package de.greensurvivors.greentreasure.legacy;

import com.github.f4b6a3.ulid.Ulid;
import com.mojang.authlib.GameProfile;
import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import io.papermc.paper.configuration.GlobalConfiguration;
import net.minecraft.core.UUIDUtil;
import net.minecraft.server.MinecraftServer;
import org.apache.commons.io.FilenameUtils;
import org.bukkit.block.Container;
import org.bukkit.util.NumberConversions;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.configurate.ConfigurationNode;
import org.spongepowered.configurate.yaml.YamlConfigurationLoader;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.Instant;
import java.util.Collections;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class PlayerFileVisitor extends ALegacyFileVisitor {
    protected final static @NotNull PathMatcher PLAYER_PATH_MATCHER = FileSystems.getDefault().getPathMatcher("regex:^(?i).*?[!-~]{1,16}\\.(?:yml|yaml)$");
    protected final static @NotNull Pattern PLAYER_COORDS_PATTERN = Pattern.compile("^(?<x>[+-]?\\d+)_(?<y>[+-]?\\d+)_(?<z>[+-]?\\d+)$");
    protected final @NotNull UUIDFetchScheduler uuidFetchScheduler;

    protected PlayerFileVisitor(final @NotNull GreenTreasure plugin, final @NotNull UUIDFetchScheduler uuidFetchScheduler, final @NotNull AtomicInteger importProcessId, final @NotNull ForkingSuccessCollector successCollector) {
        super(plugin, importProcessId, successCollector);
        this.uuidFetchScheduler = uuidFetchScheduler;
    }

    @Override
    public @NotNull FileVisitResult visitFile(final @NotNull Path filePath, final @NotNull BasicFileAttributes attrs) {
        if (attrs.isRegularFile() && PLAYER_PATH_MATCHER.matches(filePath)) {
            successCollector.fork();
            plugin.getComponentLogger().debug("trying to import player data with path: {}", filePath);

            final @NotNull String playerName = FilenameUtils.removeExtension(filePath.getFileName().toString());

            // Note: there are possible alternatives, when the nms way breaks:
            // - Bukkit#getOfflinePlayer and Bukkit#getOfflinePlayerIfCached; but they can't get called async
            // - ResolvableProfile.resolvableProfile().name(playerName).build().resolve().thenAccept(profile -> ); - but it relies on CompletableFutures I tried to slim down on, since they needed too much memory.
            // - plugin.getServer().createProfile() -> PlayerProfile#completeFromCache(false, GlobalConfiguration.get().proxies.isProxyOnlineMode()) - is thread safe and pretty much does what we are doing,
            // but our way supports finer graining for scheduling and skips texture lookups
            final @NotNull UUID uuid;
            if (GlobalConfiguration.get().proxies.isProxyOnlineMode()) {
                final @Nullable GameProfile profile = MinecraftServer.getServer().services().paper().filledProfileCache().getIfCached(playerName);
                if (profile == null) {
                    uuidFetchScheduler.registerForFetching(playerName, fetchedUUID -> {
                        if (fetchedUUID == null) { // server is in online / proxy mode but the name is still unknown.
                            successCollector.finish(false);
                            plugin.getComponentLogger().warn("Couldn't fetch uuid for player {}. Maybe never played before on this server? Ignoring!", playerName);
                        } else {
                            processPlayer(filePath, playerName, fetchedUUID);
                        }
                    });

                    return FileVisitResult.CONTINUE;
                } else {
                    uuid = profile.id();
                }
            } else {
                uuid = UUIDUtil.createOfflinePlayerUUID(playerName);
            }

            processPlayer(filePath, playerName, uuid);
        }

        return FileVisitResult.CONTINUE;
    }

    protected void processPlayer(final @NotNull Path filePath, final @NotNull String playerName, final @NotNull UUID uuid) {
        final @NotNull UUID offlineUUID = UUIDUtil.createOfflinePlayerUUID(playerName);
        final @NotNull Path playerDirPath = plugin.getServer().getLevelDirectory().resolve("players", "data");
        if (Files.exists(playerDirPath.resolve(uuid + ".dat")) ||
            Files.exists(playerDirPath.resolve(uuid + ".dat_old")) ||

            // the PlayerDataStorage determines if a player ever played before
            GlobalConfiguration.get().proxies.isProxyOnlineMode() && (
                Files.exists(playerDirPath.resolve(offlineUUID + ".dat")) ||
                    Files.exists(playerDirPath.resolve(offlineUUID + ".dat_old")))) {

            final @NotNull YamlConfigurationLoader loader = YamlConfigurationLoader.builder().path(filePath).build();
            final @NotNull ConfigurationNode configRoot;

            try {
                configRoot = loader.load();
            } catch (IOException e) {
                plugin.getComponentLogger().warn("could not read legacy player data file", e);
                successCollector.finish(false);

                return;
            }

            for (final @NotNull Map.Entry<@NotNull Object, ? extends @NotNull ConfigurationNode> worldEntry : configRoot.childrenMap().entrySet()) {
                final @NotNull String worldName = worldEntry.getKey().toString();

                for (final @NotNull Map.Entry<@NotNull Object, ? extends ConfigurationNode> treasureEntry : worldEntry.getValue().childrenMap().entrySet()) {
                    successCollector.fork();
                    final @NotNull Matcher coordsMatcher = PLAYER_COORDS_PATTERN.matcher(treasureEntry.getKey().toString());

                    if (coordsMatcher.matches()) {
                        final long lootedTimeStamp = treasureEntry.getValue().getLong(-1L);

                        if (lootedTimeStamp > 0) {
                            final int x = Integer.parseInt(coordsMatcher.group("x"));
                            final int y = Integer.parseInt(coordsMatcher.group("y"));
                            final int z = Integer.parseInt(coordsMatcher.group("z"));

                            plugin.getServer().getScheduler().runTask(plugin, () -> {
                                final int processIDCopy = importProcessId.get();
                                if (processIDCopy >= 0) {
                                    plugin.getChunkParser().registerForChunkParsing(
                                        worldName, x >> 4, z >> 4,
                                        block ->
                                            NumberConversions.square(x - block.getX()) +
                                                NumberConversions.square(y - block.getY()) +
                                                NumberConversions.square(z - block.getZ()) < 0.25,
                                        resultEither ->
                                            resultEither.consume(_ -> successCollector.finish(false),
                                                tileEntities -> {
                                                    if (tileEntities.isEmpty() || !(tileEntities.iterator().next() instanceof Container container)) {
                                                        plugin.getComponentLogger().warn("Could not load legacy player data {} because the block at world={}, x={}, y={}, z={} is not a container.", filePath, worldName, x, y, z);
                                                        successCollector.finish(false);
                                                    } else {
                                                        final @Nullable Ulid asyncTreasureId = plugin.getTreasureManager().getTreasureId(container);

                                                        if (asyncTreasureId != null) {
                                                            plugin.getDataAccessor()
                                                                .setPlayerData(
                                                                    plugin.getServer().getOfflinePlayer(uuid),
                                                                    asyncTreasureId,
                                                                    new PlayerLootDetail(Instant.ofEpochMilli(lootedTimeStamp), Collections.emptyList()))
                                                                .whenComplete((_, ex) -> {
                                                                    if (ex == null) {
                                                                        plugin.getComponentLogger().debug("Imported player data for name {} / uuid {} from path {}", playerName, uuid, filePath);

                                                                        successCollector.finish(false);
                                                                    } else {
                                                                        successCollector.finish(false);
                                                                        plugin.getComponentLogger().warn("Couldn't properly import player data name {} / uuid {} from path {}", uuid, playerName, filePath, ex);
                                                                    }
                                                            });
                                                        } else {
                                                            plugin.getComponentLogger().warn("[PlayerData] Couldn't get treasure id from block at: Location{world={},x={},y={},z={}}. Skipping.", worldName, x, y, z);
                                                            successCollector.finish(false);
                                                        }
                                                    }
                                                }), processIDCopy);
                                    }
                                }
                            );
                        } else {
                            successCollector.finish(false);
                            plugin.getComponentLogger().warn("[PlayerData] Can't extract looted timestamp for player {} in world {} from string: '{}'. Skipping.", playerName, worldName, treasureEntry.getValue().raw());
                        }
                    } else {
                        successCollector.finish(false);
                        plugin.getComponentLogger().warn("[PlayerData] Can't extract coordinates for player {} in world {} from string: '{}'. Skipping.", playerName, worldName, treasureEntry.getKey());
                    }
                }
            }
        }

        successCollector.finish(true);
    }
}
