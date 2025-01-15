package de.greensurvivors.greentreasure.config;

import de.greensurvivors.greentreasure.DatabaseManager;
import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.OfflinePlayer;
import org.bukkit.World;
import org.bukkit.block.Block;
import org.bukkit.block.Container;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.configuration.serialization.ConfigurationSerialization;
import org.bukkit.inventory.ItemStack;
import org.bukkit.plugin.Plugin;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.yaml.snakeyaml.Yaml;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.*;
import java.time.Duration;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class ImportLegacy {
    public static final @NotNull String TREASURE_CHEST = "TreasureChest", TREASURE_CHEST_X = "TreasureChestX";
    private final static @NotNull PathMatcher PATH_MATCHER = FileSystems.getDefault().getPathMatcher("regex:/^.*(?:yml|yaml)$/i");
    private final static @NotNull Pattern ITEM_NUMBER_PATTERN = Pattern.compile("^item(?<number>\\d+)$");
    private final static @NotNull Pattern PLAYER_COORDS_PATTERN = Pattern.compile("^(?<x>[+-]?\\d+)_(?<y>[+-]?\\d+)_(?<z>[+-]?\\d+)$");
    private final @NotNull Duration DEFAULT_FORGETTING_PERIOD = Duration.ofSeconds(-1);
    private final @NotNull GreenTreasure plugin;

    public ImportLegacy(final @NotNull GreenTreasure plugin) {
        this.plugin = plugin;
    }

    protected void importLegacyData() {
        final @Nullable Plugin treasureChestPlugin = Bukkit.getPluginManager().getPlugin(TREASURE_CHEST);

        if (treasureChestPlugin != null) {
            Bukkit.getPluginManager().disablePlugin(treasureChestPlugin);

            Bukkit.getServer().getCommandMap().getKnownCommands().entrySet().removeIf(s -> StringUtils.startsWithIgnoreCase(s.getKey(), "treasurechest"));
        }

        final @Nullable Plugin treasureChestXPlugin = Bukkit.getPluginManager().getPlugin(TREASURE_CHEST_X);
        if (treasureChestXPlugin != null) {
            Bukkit.getPluginManager().disablePlugin(treasureChestXPlugin);

            Bukkit.getServer().getCommandMap().getKnownCommands().entrySet().removeIf(s -> StringUtils.startsWithIgnoreCase(s.getKey(), "treasurechestx"));
        }

        final @NotNull Path pluginPath = plugin.getDataPath().getParent();
        final @NotNull Path treasureChestPath = pluginPath.resolve(TREASURE_CHEST);

        if (Files.isDirectory(treasureChestPath)) {
            plugin.getComponentLogger().info("starting import legacy process from " + TREASURE_CHEST);

            importTreasureData(treasureChestPath).thenAccept(inventorySizes -> importPlayerData(inventorySizes, treasureChestPath));

            plugin.getComponentLogger().info("importing legacy process from " + TREASURE_CHEST + " is done.");
        } else {
            final @NotNull Path treasureChestXPath = pluginPath.resolve(TREASURE_CHEST_X);

            if (Files.isDirectory(treasureChestXPath)) {
                plugin.getComponentLogger().info("starting import legacy process from " + TREASURE_CHEST_X);

                importTreasureData(treasureChestXPath).thenAccept(inventorySizes -> importPlayerData(inventorySizes, treasureChestXPath));

                plugin.getComponentLogger().info("importing legacy process from " + TREASURE_CHEST_X + " is done");
            } else {
                plugin.getComponentLogger().warn("Could not find any legacy treasures.");
            }
        }
    }

    private @NotNull Map<@NotNull String, @NotNull Object> validateMap(final @NotNull Map<?, ?> mapToValidate) {
        LinkedHashMap<@NotNull String, @NotNull Object> result = new LinkedHashMap<>(mapToValidate.size());

        for (Map.Entry<?, ?> entry : mapToValidate.entrySet()) {
            if (entry.getKey() instanceof String key && entry.getValue() != null) {
                result.put(key, entry.getValue());
            }
        }

        return result;
    }

    /**
     * import legacy treasures
     */
    private @NotNull CompletableFuture<@NotNull Map<@NotNull Location, @NotNull String>> importTreasureData(final @Nullable Path treasurePluginFolder) { // todo import double chests
        if (treasurePluginFolder == null) {
            return CompletableFuture.failedFuture(new NullPointerException("treasurePluginFolder was null"));
        }

        try (final ForkJoinPool forkJoinPool = ForkJoinPool.commonPool()) {
            final @NotNull CompletableFuture<@NotNull Map<@NotNull Location, @NotNull String>> result = new CompletableFuture<>();
            forkJoinPool.execute(() -> {
                synchronized (this) {
                    plugin.getComponentLogger().info("importing Treasures");

                    final @NotNull Path treasuresPath = treasurePluginFolder.resolve("treasure");

                    if (Files.isDirectory(treasuresPath)) {
                        try (Stream<Path> treasuresPathStream = Files.walk(treasuresPath)) {
                            final @NotNull Map<@NotNull Location, @NotNull String> resultMap = new ConcurrentHashMap<>();

                            treasuresPathStream.
                                filter(Files::isRegularFile).
                                filter(path -> PATH_MATCHER.matches(path.getFileName())).
                                forEach(path -> {
                                    try (final @NotNull BufferedReader reader = Files.newBufferedReader(path)) {
                                        final @NotNull Map<@NotNull String, ? extends @NotNull Object> configMap = new Yaml().load(reader);

                                        if (!(configMap.get("location") instanceof Map<?, ?> rootMap)) { // no I don't know why root is called location either
                                            plugin.getComponentLogger().warn("Could not load legacy treasure {} because it's file was empty.", path);
                                            return;
                                        }

                                        final @NotNull Map<@NotNull String, @NotNull Object> checkedRootMap = validateMap(rootMap);

                                        if (!(checkedRootMap.get("container") instanceof Map<?, ?> containerMap)) {
                                            plugin.getComponentLogger().warn("Could not load legacy treasure {} because it's file does not contain an container.", path);
                                            return; // invalid next path
                                        }

                                        final @NotNull Map<@NotNull String, @NotNull Object> checkedContainerMap = validateMap(containerMap);

                                        if (checkedContainerMap.get("right-side") instanceof Map<?, ?> rightMap && checkedContainerMap.get("left-side") instanceof Map<?, ?> leftMap) {
                                            final @NotNull Map<@NotNull String, @NotNull Object> checkedRightMap = validateMap(rightMap);
                                            final @NotNull Map<@NotNull String, @NotNull Object> checkedLeftMap = validateMap(leftMap);

                                            mainContainer(path, getTreasureContents(path.toString(), checkedRightMap), checkedRootMap, checkedLeftMap, resultMap);

                                        } else if (checkedContainerMap.containsKey("coords")) {
                                            mainContainer(path, null, checkedRootMap, checkedContainerMap, resultMap);
                                        }


                                    } catch (final @NotNull IOException e) {
                                        plugin.getComponentLogger().warn("could not read legacy treasure path {}", path, e);
                                    }
                                });

                            result.complete(resultMap);
                        } catch (IOException e) {
                            plugin.getComponentLogger().warn("Could not load legacy treasures!", e);

                            result.completeExceptionally(e);
                        }
                    } else {
                        plugin.getComponentLogger().warn("Could not load legacy treasures!");

                        result.completeExceptionally(new InvalidPathException(treasuresPath.toString(), "is not a dictionary!"));
                    }
                }
            });

            return result;
        }
    }

    private void mainContainer(final @NotNull Path path,
                               final @Nullable List<@NotNull ItemStack> rightContents,
                               final @NotNull Map<@NotNull String, @NotNull Object> checkedRootMap,
                               final @NotNull Map<@NotNull String, @NotNull Object> checkedContainerMap,
                               final @NotNull Map<@NotNull Location, @NotNull String> resultMap) {

        final @Nullable Location treasureLocation = getLocation(checkedContainerMap, path.toString());

        if (treasureLocation == null) {
            return;
        }

        treasureLocation.getWorld().getChunkAtAsync(treasureLocation).thenApply(chunk -> {
            final @NotNull Block treasureBlock = chunk.getBlock(treasureLocation.getBlockX(), treasureLocation.getBlockY(), treasureLocation.getBlockZ());

            if (!(treasureBlock.getState(false) instanceof Container container)) {
                plugin.getComponentLogger().warn("Could not load legacy treasure {} because the block at {} is not a container.", path, treasureBlock.getLocation());
                return null;
            }

            container = (Container) Utils.getTreasureHolder(container);

            @Nullable String treasureId = plugin.getTreasureManager().getTreasureId(container);
            if (treasureId == null) {
                treasureId = UUID.randomUUID().toString();

                plugin.getTreasureManager().setTreasureId(container, treasureId);
            }

            return treasureId;
        }).thenAccept(treasureId -> {
            final @Nullable List<@NotNull ItemStack> contents = getTreasureContents(path.toString(), checkedContainerMap);

            if (contents == null) {
                return;
            }

            if (rightContents != null) {
                contents.addAll(rightContents);
            }

            final @NotNull DatabaseManager databaseManager = plugin.getDatabaseManager();
            databaseManager.setTreasureContents(treasureId, contents);

            boolean isUnlimited = false;
            if (checkedRootMap.get("unlimited") instanceof Boolean unlimited) {
                isUnlimited = unlimited;
                databaseManager.setUnlimited(treasureId, unlimited);
            }
            if (checkedRootMap.get("shared") instanceof Boolean shared) {
                databaseManager.setShared(treasureId, shared);
            }
            if (!contents.isEmpty() && checkedRootMap.get("random") instanceof Number randomNumber) {
                final double randomChance = randomNumber.longValue() == 0 ? 100.00 : randomNumber.doubleValue() / ((double) contents.size());
                databaseManager.setRandom(treasureId, (short) (randomChance * 100));
            }
            if (checkedRootMap.get("forget-time") instanceof Number forgetTimeNumber) {
                Duration forget_time = Duration.ofMillis(forgetTimeNumber.longValue());
                if (forget_time.isZero()) {
                    forget_time = DEFAULT_FORGETTING_PERIOD;
                }
                databaseManager.setForgetDuration(treasureId, forget_time);
            }
            if (checkedRootMap.get("messages") instanceof Map<?, ?> messageMap) {
                final @NotNull Map<@NotNull String, @NotNull Object> checkedMessageMap = validateMap(messageMap);

                if (isUnlimited) {
                    if (checkedMessageMap.get("UNLIMITED") instanceof String unlimitedMessage &&
                        !unlimitedMessage.equals("Take as much as you want!")) {
                        databaseManager.setFindFreshMessageOverride(treasureId, unlimitedMessage);
                    }
                } else {
                    if (checkedMessageMap.get("FOUND") instanceof String freshFindMessage &&
                        !freshFindMessage.equals("You have found treasure!")) {
                        databaseManager.setFindFreshMessageOverride(treasureId, freshFindMessage);
                    }
                }

                if (checkedMessageMap.get("FOUND_ALREADY") instanceof String lootedMessage &&
                    !lootedMessage.equals("You have already looted this treasure...")) {
                    databaseManager.setFindLootedMessageOverride(treasureId, lootedMessage);
                }
            }

            plugin.getComponentLogger().debug("imported treasure with id {} from path {}", treasureId, path);
            resultMap.put(treasureLocation, treasureId);

            // now delete the file
            try {
                Files.delete(path);
            } catch (final @NotNull IOException e) {
                plugin.getComponentLogger().warn("Could not delete treasure file {}", path, e);
            }
        });
    }

    private @Nullable Location getLocation(final @NotNull Map<@NotNull String, @NotNull Object> checkedContainerMap, final @NotNull String path) {
        if (!(checkedContainerMap.get("world") instanceof String worldName)) {
            plugin.getComponentLogger().warn("Could not load legacy treasure {} because it's file does not contain a world.", path);
            return null;
        }

        @Nullable World world = Bukkit.getServer().getWorld(worldName);

        if (world == null) {
            plugin.getComponentLogger().warn("Could not load legacy treasure {} because I couldn't find a world named {}.", path, worldName);
            return null;
        }

        if (!(checkedContainerMap.get("coords") instanceof Map<?, ?> coordsMap)) {
            plugin.getComponentLogger().warn("Could not load legacy treasure {} because I couldn't find a position in for the world named {}.", path, worldName);
            return null;
        }

        final @NotNull Map<@NotNull String, @NotNull Object> checkedCoordsMap = validateMap(coordsMap);

        if (!(checkedCoordsMap.get("x") instanceof Number xNumber) ||
            !(checkedCoordsMap.get("y") instanceof Number yNumber) ||
            !(checkedCoordsMap.get("z") instanceof Number zNumber)) {
            plugin.getComponentLogger().warn("Could not load legacy treasure {} because I couldn't find a position in for the world named {}.", path, worldName);
            return null;
        }

        final @NotNull Location treasureLocation = new Location(world, xNumber.doubleValue(), yNumber.doubleValue(), zNumber.doubleValue());
        treasureLocation.checkFinite();

        return treasureLocation;
    }

    private @Nullable List<@NotNull ItemStack> getTreasureContents(final @NotNull String path,
                                                                   final @NotNull Map<@NotNull String, @NotNull Object> objectMap) {

        final @Nullable Location treasureLocation = getLocation(objectMap, path);

        if (treasureLocation != null) {
            // todo set id if missing
        }

        final int inventorySize;
        if (objectMap.get("size") instanceof Number sizeNumber) {
            inventorySize = sizeNumber.intValue();
        } else {
            plugin.getComponentLogger().warn("Could not get ContainerSize from {}", path);
            return null;
        }

        if (objectMap.get("contents") instanceof Map<?, ?> contentsMap) {
            final @NotNull Map<@NotNull String, @NotNull Object> checkedContentsMap = validateMap(contentsMap);

            final @Nullable ItemStack @NotNull [] contents = new ItemStack[inventorySize];

            for (Map.Entry<@NotNull String, @NotNull Object> contentsEntry : checkedContentsMap.entrySet()) {
                final @NotNull Matcher matcher = ITEM_NUMBER_PATTERN.matcher(contentsEntry.getKey());

                if (matcher.matches()) {
                    int num = Integer.parseInt(matcher.group("number"));

                    if (num >= inventorySize || num < 0) {
                        plugin.getComponentLogger().warn("Could not load legacy treasure {} because the index {} for this item is out of bounds ({}).", path, num, inventorySize);
                        continue;
                    }

                    if (contentsEntry.getValue() instanceof Map<?, ?> itemStackHolderMap) {
                        final @NotNull Map<@NotNull String, @NotNull Object> checkedItemStackHolderMap = validateMap(itemStackHolderMap);

                        if (checkedItemStackHolderMap.get(ConfigurationSerialization.SERIALIZED_TYPE_KEY) instanceof String itemStackHolderType) {
                            if (itemStackHolderType.equalsIgnoreCase("org.bukkit.inventory.ItemStack")) {
                                contents[num] = ItemStack.deserialize(checkedItemStackHolderMap);
                            } else if (checkedItemStackHolderMap.get("stack") instanceof Map<?, ?> itemStackMap) {
                                final @NotNull Map<@NotNull String, @NotNull Object> checkedItemStackMap = validateMap(itemStackMap);

                                contents[num] = ItemStack.deserialize(checkedItemStackMap);
                            } else {
                                plugin.getComponentLogger().warn("Could not load legacy treasure {} because I don't know how do deserialize item number {}.", path, num);
                                return null;
                            }
                        } else {
                            plugin.getComponentLogger().warn("Could not load legacy treasure {} because of an unexpected or missing type token: {}.", path, checkedItemStackHolderMap.get(ConfigurationSerialization.SERIALIZED_TYPE_KEY));
                            return null;
                        }
                    } else {
                        plugin.getComponentLogger().warn("Could not load legacy treasure {} because I couldn't find any items.", path);
                        return null;
                    }
                } else {
                    plugin.getComponentLogger().warn("Could not load legacy treasure {} because {} does not match {}", path, contentsEntry.getKey(), ITEM_NUMBER_PATTERN.pattern());
                    return null;
                }
            }

            return Arrays.stream(contents).
                map(itemStack -> Objects.requireNonNullElseGet(itemStack, ItemStack::empty)).
                collect(Collectors.toCollection(ArrayList::new));
        } else {
            plugin.getComponentLogger().warn("Could not load legacy treasure {} because it doesn't has any contents.", path);
            return null;
        }
    }


    /**
     * import player data
     */
    private void importPlayerData(final @NotNull Map<@NotNull Location, @NotNull String> importedTreasureIds, final @NotNull Path treasurePluginFolder) {
        try (final ForkJoinPool forkJoinPool = ForkJoinPool.commonPool()) {
            forkJoinPool.execute(() -> {
                synchronized (this) {
                    plugin.getComponentLogger().info("importing PlayerData");

                    final @NotNull Path playersPath = treasurePluginFolder.resolve("players");

                    if (Files.isDirectory(playersPath)) {
                        try (Stream<Path> playersPathStream = Files.walk(playersPath)) {
                            playersPathStream.
                                filter(Files::isRegularFile).
                                filter(path -> PATH_MATCHER.matches(path.getFileName())).
                                forEach(path -> {
                                    final @NotNull String playerName = FilenameUtils.removeExtension(path.getFileName().toString());
                                    final @NotNull OfflinePlayer offlinePlayer = Bukkit.getOfflinePlayer(playerName);

                                    if (!offlinePlayer.hasPlayedBefore()) {
                                        plugin.getComponentLogger().warn("Player '{}' has never played before. Skipping.", playerName);
                                        return;
                                    }

                                    AtomicBoolean gotNoError = new AtomicBoolean(true);

                                    try (final @NotNull BufferedReader reader = Files.newBufferedReader(path)) {
                                        final @NotNull FileConfiguration playerFile = YamlConfiguration.loadConfiguration(reader);

                                        for (String worldName : playerFile.getKeys(false)) {
                                            final @Nullable World world = Bukkit.getWorld(worldName);

                                            if (world != null) {
                                                //noinspection DataFlowIssue
                                                for (Map.Entry<String, Object> entry : playerFile.getConfigurationSection(worldName).getValues(false).entrySet()) {
                                                    final @NotNull Matcher coordsMatcher = PLAYER_COORDS_PATTERN.matcher(entry.getKey());

                                                    if (coordsMatcher.matches()) {
                                                        int x = Integer.parseInt(coordsMatcher.group("x"));
                                                        int y = Integer.parseInt(coordsMatcher.group("y"));
                                                        int z = Integer.parseInt(coordsMatcher.group("z"));

                                                        if (entry.getValue() instanceof Number timeStampNumber) {
                                                            @Nullable String treasureId = null;

                                                            for (Map.Entry<Location, String> treasureIdEntry : importedTreasureIds.entrySet()) {
                                                                Location locationOfEntry = treasureIdEntry.getKey();

                                                                if (locationOfEntry.getWorld().getUID().equals(world.getUID()) &&
                                                                    locationOfEntry.getBlockX() == x &&
                                                                    locationOfEntry.getBlockY() == y &&
                                                                    locationOfEntry.getBlockZ() == z) {

                                                                    treasureId = treasureIdEntry.getValue();
                                                                    break;
                                                                }
                                                            }

                                                            if (treasureId == null) {
                                                                world.getChunkAtAsync(x >> 4, z >> 4, false).thenAccept(chunk -> {
                                                                    final @NotNull Block treasureBlock = chunk.getBlock(x, y, z);

                                                                    if (!(treasureBlock.getState(false) instanceof Container container)) {
                                                                        plugin.getComponentLogger().warn("[playerData] Could not load legacy treasure {} because the block at {} is not a container.", path, treasureBlock.getLocation());
                                                                        return;
                                                                    }

                                                                    final @Nullable String asyncTreasureId = plugin.getTreasureManager().getTreasureId(container);

                                                                    if (asyncTreasureId != null) {
                                                                        plugin.getDatabaseManager().setPlayerData(offlinePlayer, asyncTreasureId, new PlayerLootDetail(timeStampNumber.longValue(), List.of()));
                                                                    } else {
                                                                        plugin.getComponentLogger().warn("[playerData] Couldn't get treasure id from block at: Location{world={},x={},y={},z={}}. Skipping.", world.getName(), x, y, z);
                                                                        gotNoError.set(false);
                                                                        return;
                                                                    }
                                                                });
                                                            } else {
                                                                plugin.getDatabaseManager().setPlayerData(offlinePlayer, treasureId, new PlayerLootDetail(timeStampNumber.longValue(), List.of()));
                                                            }
                                                        }
                                                    } else {
                                                        plugin.getComponentLogger().warn("[playerData] Can't extract coordinates from name: '{}'. Skipping.", entry.getKey());
                                                        gotNoError.set(false);
                                                        continue;
                                                    }
                                                }
                                            } else {
                                                plugin.getComponentLogger().warn("Unknown world: {}. Skipping.", worldName);
                                                gotNoError.set(false);
                                                return;
                                            }
                                        }
                                    } catch (IOException e) {
                                        plugin.getComponentLogger().warn("Could not read player info for {}", path, e);
                                        gotNoError.set(false);
                                    }

                                    if (gotNoError.get()) {
                                        try {
                                            Files.delete(path);

                                            plugin.getComponentLogger().debug("imported player info for {}", path);
                                        } catch (IOException e) {
                                            plugin.getComponentLogger().warn("Could not delete player info for {}", path, e);
                                        }
                                    }
                                });
                        } catch (IOException e) {
                            plugin.getComponentLogger().warn("Could not load legacy player info!", e);
                        }
                    } else {
                        plugin.getComponentLogger().warn("Could not load legacy player info!");
                    }
                }
            });
        }
    }
}
