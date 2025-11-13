package de.greensurvivors.greentreasure.config;

import com.github.f4b6a3.ulid.Ulid;
import de.greensurvivors.greentreasure.data.DatabaseManager;
import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import org.apache.commons.io.FilenameUtils;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.OfflinePlayer;
import org.bukkit.World;
import org.bukkit.block.Container;
import org.bukkit.command.Command;
import org.bukkit.command.PluginCommand;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.configuration.serialization.ConfigurationSerialization;
import org.bukkit.inventory.ItemStack;
import org.bukkit.plugin.Plugin;
import org.bukkit.util.NumberConversions;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.Range;
import org.yaml.snakeyaml.Yaml;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InvalidObjectException;
import java.io.UncheckedIOException;
import java.nio.file.*;
import java.time.Duration;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class ImportLegacy {
    public static final @NotNull String TREASURE_CHEST = "TreasureChest", TREASURE_CHEST_X = "TreasureChestX";
    private final static @NotNull PathMatcher PATH_MATCHER = FileSystems.getDefault().getPathMatcher("regex:^(?i).*(?:yml|yaml)$");
    private final static @NotNull Pattern ITEM_NUMBER_PATTERN = Pattern.compile("^item(?<number>\\d+)$");
    private final static @NotNull Pattern PLAYER_COORDS_PATTERN = Pattern.compile("^(?<x>[+-]?\\d+)_(?<y>[+-]?\\d+)_(?<z>[+-]?\\d+)$");
    private final static @NotNull Duration DEFAULT_FORGETTING_PERIOD = Duration.ofSeconds(-1);
    private final @NotNull GreenTreasure plugin;

    public ImportLegacy(final @NotNull GreenTreasure plugin) {
        this.plugin = plugin;
    }

    public static void disableLegacyPlugins() {
        disablePlugin(Bukkit.getPluginManager().getPlugin(TREASURE_CHEST));
        disablePlugin(Bukkit.getPluginManager().getPlugin(TREASURE_CHEST_X));
    }

    private static void disablePlugin(final @Nullable Plugin treasureChestPlugin) {
        if (treasureChestPlugin != null) {
            Bukkit.getPluginManager().disablePlugin(treasureChestPlugin);

            for (Command cmd : Bukkit.getCommandMap().getKnownCommands().values()) {
                if (cmd instanceof PluginCommand pluginCommand) {
                    if (pluginCommand.getPlugin().getName().equalsIgnoreCase(treasureChestPlugin.getName())) {
                        cmd.unregister(Bukkit.getCommandMap());
                    }
                }
            }
        }
    }

    protected void importLegacyData() {
        final @NotNull Path pluginPath = plugin.getDataPath().getParent();
        final @NotNull Path treasureChestPath = pluginPath.resolve(TREASURE_CHEST);

        if (Files.isDirectory(treasureChestPath)) {
            startImport(TREASURE_CHEST, treasureChestPath);
        } else {
            final @NotNull Path treasureChestXPath = pluginPath.resolve(TREASURE_CHEST_X);

            if (Files.isDirectory(treasureChestXPath)) {
                startImport(TREASURE_CHEST_X, treasureChestXPath);
            } else {
                plugin.getComponentLogger().warn("Could not find any legacy treasures.");
            }
        }
    }

    private void startImport(final @NotNull String pluginName, final @NotNull Path path) {
        plugin.getComponentLogger().info("starting import legacy process from {}", pluginName);

        importTreasureData(path).
            thenCompose(importedTreasures -> importPlayerData(importedTreasures, path)).
            thenAccept(success -> {
                plugin.getComponentLogger().info("importing legacy process from {} is done. was success: {}", pluginName, success);

                if (success) {
                    plugin.getComponentLogger().info("You may delete the old folder 'plugins/{}' now!", pluginName);
                } else {
                    plugin.getComponentLogger().info("There where some problems when importing all legacy data. You have to manually check the logs, in order to decide if deleting the old folder 'plugins/{}' is safe!", pluginName);
                }
            });
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
    private @NotNull CompletableFuture<@NotNull Map<@NotNull Location, @NotNull Ulid>> importTreasureData(final @NotNull Path treasurePluginFolder) { // todo import double chests
        final @NotNull CompletableFuture<@NotNull Map<@NotNull Location, @NotNull Ulid>> result = new CompletableFuture<>();
        // the common fork join pool doesn't need closing
        //noinspection resource
        ForkJoinPool.commonPool().execute(() -> {
            synchronized (this) {
                plugin.getComponentLogger().info("Importing Treasures. Please stand back, this may take a while!");

                final @NotNull Path treasuresPath = treasurePluginFolder.resolve("treasure");

                if (Files.isDirectory(treasuresPath)) {
                    try (Stream<Path> treasuresPathStream = Files.walk(treasuresPath)) {
                        final @NotNull Map<@NotNull Location, @NotNull Ulid> resultMap = new ConcurrentHashMap<>();

                        final @NotNull CompletableFuture<?>[] futures = treasuresPathStream.
                            filter(Files::isRegularFile).
                            filter(PATH_MATCHER::matches).
                            map(path -> {
                                plugin.getComponentLogger().debug("trying to import treasure with path: {}", path);

                                try (final @NotNull BufferedReader reader = Files.newBufferedReader(path)) {
                                    final @NotNull Map<@NotNull String, ? extends @NotNull Object> configMap = new Yaml().load(reader);

                                    if (!(configMap.get("location") instanceof Map<?, ?> rootMap)) { // no I don't know why root is called location either
                                        plugin.getComponentLogger().warn("Could not load legacy treasure {} because it's file was empty.", path);
                                        return CompletableFuture.failedFuture(new InvalidObjectException("Could not load legacy treasure " + path + " because it's file was empty."));
                                    }

                                    final @NotNull Map<@NotNull String, @NotNull Object> checkedRootMap = validateMap(rootMap);

                                    if (!(checkedRootMap.get("container") instanceof Map<?, ?> containerMap)) {// invalid next path
                                        plugin.getComponentLogger().warn("Could not load legacy treasure {} because it's file does not contain an container.", path);
                                        return CompletableFuture.failedFuture(new InvalidObjectException("Could not load legacy treasure " + path + " because it's file does not contain an container."));
                                    }

                                    final @NotNull Map<@NotNull String, @NotNull Object> checkedContainerMap = validateMap(containerMap);

                                    if (checkedContainerMap.get("right-side") instanceof Map<?, ?> rightMap && checkedContainerMap.get("left-side") instanceof Map<?, ?> leftMap) {
                                        final @NotNull Map<@NotNull String, @NotNull Object> checkedRightMap = validateMap(rightMap);
                                        final @NotNull Map<@NotNull String, @NotNull Object> checkedLeftMap = validateMap(leftMap);

                                        return mainContainer(path, getTreasureContents(path.toString(), checkedRightMap), checkedRootMap, checkedLeftMap, resultMap);

                                    } else if (checkedContainerMap.containsKey("coords")) {
                                        return mainContainer(path, null, checkedRootMap, checkedContainerMap, resultMap);
                                    } else {
                                        plugin.getComponentLogger().warn("could not read contents of legacy treasure path {}", path);

                                        return CompletableFuture.failedFuture(new InvalidObjectException("could not read contents of legacy treasure path " + path));
                                    }

                                } catch (final @NotNull IOException e) {
                                    plugin.getComponentLogger().warn("could not read legacy treasure path {}", path, e);

                                    return CompletableFuture.failedFuture(e);
                                }
                            }).
                            // since CompletableFuture#allOf fails as soon as the first future fails, just mute all exceptions and wait for all completions
                            map(f -> f.exceptionally(e -> null)).
                            toArray(CompletableFuture[]::new);

                        CompletableFuture.allOf(futures).whenComplete((voidz, ex) -> result.complete(resultMap));
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

    private @NotNull CompletableFuture<Void> mainContainer(final @NotNull Path path,
                                                           final @Nullable List<@NotNull ItemStack> rightContents,
                                                           final @NotNull Map<@NotNull String, @NotNull Object> checkedRootMap,
                                                           final @NotNull Map<@NotNull String, @NotNull Object> checkedContainerMap,
                                                           final @NotNull Map<@NotNull Location, @NotNull Ulid> resultMap) {

        final @NotNull CompletableFuture<Void> result = new CompletableFuture<>();
        final @Nullable Location treasureLocation = getLocation(checkedContainerMap, path.toString());

        if (treasureLocation == null) {
            return CompletableFuture.failedFuture(new InvalidObjectException("Could not read treasure location"));
        }

        final @NotNull CompletableFuture<@Nullable Ulid> doneFeature = new CompletableFuture<>();

        plugin.getTreasureManager().registerForChunkParsing(
            treasureLocation.getWorld().getName(), treasureLocation.getBlockX() >> 4, treasureLocation.getBlockZ() >> 4,
            block -> block.getLocation().distanceSquared(treasureLocation) < 0.25,
            tileEntities -> {
                if (tileEntities.isEmpty() || !(tileEntities.iterator().next() instanceof Container container)) {
                    plugin.getComponentLogger().warn("Could not load legacy treasure {} because the block at {} is not a container.", path, treasureLocation);

                    result.completeExceptionally(new InvalidObjectException("Block at location " + treasureLocation + " is not a container!"));
                    return null;
                }

                container = (Container) Utils.getTreasureHolder(container);

                @Nullable Ulid treasureId = plugin.getTreasureManager().getTreasureId(container);
                if (treasureId == null) {
                    treasureId = plugin.getTreasureManager().createNewMonotonicUlid();

                    plugin.getTreasureManager().setTreasureId(container, treasureId);
                }

                return treasureId;
            },
            doneFeature
        );

        doneFeature.whenCompleteAsync((treasureId, throwable) -> {
            if (throwable != null || treasureId == null) {
                plugin.getComponentLogger().debug("Could not load legacy treasure at {}", treasureLocation, throwable);

                result.complete(null);
                return;
            }

            final @Nullable List<@NotNull ItemStack> contents = getTreasureContents(path.toString(), checkedContainerMap);

            if (contents == null) {
                plugin.getComponentLogger().warn("Legacy treasure {} found at {} is empty!.", path, treasureLocation);
                final @NotNull InvalidObjectException exception = new InvalidObjectException("Block at location " + treasureLocation + " is not a container!");

                result.completeExceptionally(exception);
                throw new UncheckedIOException(exception);
            }

            if (rightContents != null) {
                contents.addAll(rightContents);
            }

            final @NotNull AtomicBoolean isUnlimited = new AtomicBoolean(false);
            final @NotNull DatabaseManager databaseManager = plugin.getDatabaseManager();
            databaseManager.setTreasureContents(treasureId, contents).
                thenCompose(voidz -> {
                    if (checkedRootMap.get("unlimited") instanceof Boolean unlimited) {
                        isUnlimited.set(unlimited);
                        return databaseManager.setUnlimited(treasureId, unlimited);
                    } else {
                        return CompletableFuture.completedFuture(null);
                    }
                }).thenCompose(voidz -> {
                    if (checkedRootMap.get("shared") instanceof Boolean shared) {
                        return databaseManager.setShared(treasureId, shared);
                    } else {
                        return CompletableFuture.completedFuture(null);
                    }
                }).thenCompose(voidz -> {
                    if (!contents.isEmpty() && checkedRootMap.get("random") instanceof Number randomNumber) {
                        final double randomChance = randomNumber.longValue() == 0 ? 100.00 : randomNumber.doubleValue() / ((double) contents.size());
                        return databaseManager.setRandom(treasureId, (short) (randomChance * 100));
                    } else {
                        return CompletableFuture.completedFuture(null);
                    }
                }).thenCompose(voidz -> {
                    if (checkedRootMap.get("forget-time") instanceof Number forgetTimeNumber) {
                        Duration forget_time = Duration.ofMillis(forgetTimeNumber.longValue());
                        if (forget_time.isZero()) {
                            forget_time = DEFAULT_FORGETTING_PERIOD;
                        }
                        return databaseManager.setForgetDuration(treasureId, forget_time);
                    } else {
                        return CompletableFuture.completedFuture(null);
                    }
                }).thenCompose(voidz -> {
                    final @NotNull List<@NotNull CompletableFuture<Void>> futures = new ArrayList<>(3);

                    if (checkedRootMap.get("messages") instanceof Map<?, ?> messageMap) {
                        final @NotNull Map<@NotNull String, @NotNull Object> checkedMessageMap = validateMap(messageMap);

                        if (isUnlimited.get()) {
                            if (checkedMessageMap.get("UNLIMITED") instanceof String unlimitedMessage &&
                                !unlimitedMessage.equals("Take as much as you want!")) {
                                futures.add(databaseManager.setFindFreshMessageOverride(treasureId, unlimitedMessage));
                            }
                        } else {
                            if (checkedMessageMap.get("FOUND") instanceof String freshFindMessage &&
                                    !freshFindMessage.equals("You have found treasure!")) {
                                futures.add(databaseManager.setFindFreshMessageOverride(treasureId, freshFindMessage));
                            }
                        }

                        if (checkedMessageMap.get("FOUND_ALREADY") instanceof String lootedMessage &&
                            !lootedMessage.equals("You have already looted this treasure...")) {
                            futures.add(databaseManager.setFindLootedMessageOverride(treasureId, lootedMessage));
                        }
                    }

                    return CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
                }).thenRun(() -> {
                    plugin.getComponentLogger().debug("imported treasure with id {} from path {}", treasureId, path);
                    resultMap.put(treasureLocation, treasureId);

                    result.complete(null);
                });
        });

        return result;
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
    private @NotNull CompletableFuture<@NotNull Boolean> importPlayerData(final @NotNull Map<@NotNull Location, @NotNull Ulid> importedTreasureIds, final @NotNull Path treasurePluginFolder) {
        final @NotNull CompletableFuture<@NotNull Boolean> result = new CompletableFuture<>();

        Bukkit.getScheduler().runTaskAsynchronously(plugin, () -> {
            synchronized (this) {
                plugin.getComponentLogger().info("importing PlayerData. Please stand back, this could take a while!");

                final @NotNull Path playersPath = treasurePluginFolder.resolve("players");
                if (Files.isDirectory(playersPath)) {
                    final @NotNull AtomicBoolean gotNoErrorAnyFile = new AtomicBoolean(true);

                    try {
                        final @NotNull DirectoryStream<@NotNull Path> stream = Files.newDirectoryStream(playersPath, pathToCheck -> {
                            return Files.isRegularFile(pathToCheck) &&
                            PATH_MATCHER.matches(pathToCheck.getFileName());});
                        final @NotNull Iterator<@NotNull Path> playerPathIterator = stream.iterator();

                        handleNextPlayerPath(playerPathIterator, importedTreasureIds, 0, gotNoErrorAnyFile, result);
                        result.whenComplete((ignored, alsoIgnored) -> {
                            try {
                                stream.close();
                            } catch (IOException ignored1) {
                            }
                        });
                    } catch (IOException e) {
                        plugin.getComponentLogger().warn("Could not load legacy player info!", e);

                        result.completeExceptionally(e);
                    }
                } else {
                    plugin.getComponentLogger().warn("Could not load legacy player info, because the path is not a directory!");

                    result.complete(Boolean.FALSE);
                }
            }
        });

        return result;
    }

    private void handleNextPlayerPath(
        final @NotNull Iterator<@NotNull Path> playerPathIterator,
        final @NotNull Map<@NotNull Location, @NotNull Ulid> importedTreasureIds,
        final @Range(from = 0, to = Integer.MAX_VALUE) int delay,
        final @NotNull AtomicBoolean gotNoErrorAnyFile,
        final @NotNull CompletableFuture<@NotNull Boolean> result) { // todo doesn't work, get's stuck and doesn't call itself at the right time, since it should do the next player after the first got the first world done.
        if (playerPathIterator.hasNext()) {
            final @NotNull AtomicInteger asyncProcessesToDo = new AtomicInteger(0);

            final @NotNull Path path = playerPathIterator.next();

            // Mojang api only allows 600 requests / 10 minutes or one per second.
            // so we throttle our requests to 400 / 10 minutes or one per 1.5 seconds == 30 ticks, to give the server some room for players and custom heads.
            // This wouldn't be necessary, if there was direct API access to the NMS PlayerDataStorage object, since we don't import unknown players anyway.
            // However, the best thing we got, is a call in the API server to get ALL OfflinePlayers at once.
            // and that call is not thread safe.
            // So that would be a very bad idea to use.
            // Also for future reference: UserCache also ins't an alternative data source, since that would boil down to
            // the UserCache.json, where every user in there may expire after a month.
            // I looked it up, the server basically does not delete any entries there but, at the same time they have an expiration date,
            // and after that date they might very well get deleted in the future.
            Bukkit.getScheduler().runTaskLaterAsynchronously(plugin, () -> {
                asyncProcessesToDo.getAndIncrement();

                final long millisAtStart = System.currentTimeMillis();

                final @NotNull String playerName = FilenameUtils.removeExtension(path.getFileName().toString());
                final @Nullable OfflinePlayer offlinePlayer = Bukkit.getOfflinePlayer(playerName);

                plugin.getComponentLogger().info("importing data for player {}", offlinePlayer.getName());

                if (!offlinePlayer.hasPlayedBefore()) {
                    plugin.getComponentLogger().warn("Player '{}' has never played before. Skipping.", playerName);
                    asyncProcessesToDo.decrementAndGet();
                    scheduleNextPlayerPath(0, playerPathIterator, importedTreasureIds, gotNoErrorAnyFile, result, millisAtStart);
                    return;
                }

                try (final @NotNull BufferedReader reader = Files.newBufferedReader(path)) {
                    final @NotNull FileConfiguration playerFile = YamlConfiguration.loadConfiguration(reader);

                    for (String worldName : playerFile.getKeys(false)) {
                        final @Nullable World world = Bukkit.getWorld(worldName);

                        if (world != null) {
                            //noinspection DataFlowIssue
                            for (Map.Entry<String, Object> entry : playerFile.getConfigurationSection(worldName).getValues(false).entrySet()) {
                                final @NotNull Matcher coordsMatcher = PLAYER_COORDS_PATTERN.matcher(entry.getKey());

                                if (coordsMatcher.matches()) {
                                    final int x = Integer.parseInt(coordsMatcher.group("x"));
                                    final int y = Integer.parseInt(coordsMatcher.group("y"));
                                    final int z = Integer.parseInt(coordsMatcher.group("z"));

                                    plugin.getComponentLogger().debug("pos: {}, {}, {}", x, y, z);

                                    if (entry.getValue() instanceof Number timeStampNumber) {
                                        @Nullable Ulid treasureId = null;

                                        for (Map.Entry<Location, Ulid> treasureIdEntry : importedTreasureIds.entrySet()) {
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
                                            CompletableFuture<Void> future = new CompletableFuture<>();

                                            asyncProcessesToDo.getAndIncrement();
                                            plugin.getTreasureManager().registerForChunkParsing(
                                                world.getName(), x >> 4, z >> 4,
                                                block ->
                                                    NumberConversions.square(x - block.getX()) +
                                                        NumberConversions.square(y - block.getY()) +
                                                        NumberConversions.square(z - block.getZ()) < 0.25,
                                                tileEntities -> {
                                                    if (tileEntities.isEmpty() || !(tileEntities.iterator().next() instanceof Container container)) {
                                                        plugin.getComponentLogger().warn("Could not load legacy player data {} because the block at {} is not a container.", path, new Location(world, x, y, z));
                                                        gotNoErrorAnyFile.set(false);
                                                        return null;
                                                    }

                                                    final @Nullable Ulid asyncTreasureId = plugin.getTreasureManager().getTreasureId(container);

                                                    if (asyncTreasureId != null) {
                                                        asyncProcessesToDo.getAndIncrement();
                                                        plugin.getDatabaseManager().setPlayerData(offlinePlayer, asyncTreasureId, new PlayerLootDetail(timeStampNumber.longValue(), List.of())).
                                                            whenComplete((ignored, ignored2) ->
                                                                scheduleNextPlayerPath(asyncProcessesToDo.decrementAndGet(), playerPathIterator, importedTreasureIds, gotNoErrorAnyFile, result, millisAtStart)
                                                            );
                                                    } else {
                                                        plugin.getComponentLogger().warn("[playerData] Couldn't get treasure id from block at: Location{world={},x={},y={},z={}}. Skipping.", world.getName(), x, y, z);
                                                        gotNoErrorAnyFile.set(false);
                                                    }

                                                    return null;
                                                },
                                                future
                                            );

                                            future.whenComplete((ignored, ignored2) ->
                                                scheduleNextPlayerPath(asyncProcessesToDo.decrementAndGet(), playerPathIterator, importedTreasureIds, gotNoErrorAnyFile, result, millisAtStart)
                                            );

                                            future.exceptionally(throwable -> {
                                                gotNoErrorAnyFile.set(false);
                                                return null;
                                            });
                                        } else {
                                            asyncProcessesToDo.getAndIncrement();
                                            plugin.getDatabaseManager().setPlayerData(offlinePlayer, treasureId, new PlayerLootDetail(timeStampNumber.longValue(), List.of())).
                                                whenComplete((ignored, ignored2) ->
                                                    scheduleNextPlayerPath(asyncProcessesToDo.decrementAndGet(), playerPathIterator, importedTreasureIds, gotNoErrorAnyFile, result, millisAtStart)
                                                );
                                        }
                                    } else {
                                        continue;
                                    }
                                } else {
                                    plugin.getComponentLogger().warn("[playerData] Can't extract coordinates from name: '{}'. Skipping.", entry.getKey());
                                    gotNoErrorAnyFile.set(false);
                                    continue;
                                }
                            }
                        } else {
                            plugin.getComponentLogger().warn("Unknown world: {}. Skipping.", worldName);
                            gotNoErrorAnyFile.set(false);
                            continue;
                        }
                    }
                } catch (IOException e) {
                    plugin.getComponentLogger().warn("Could not read player info for {}", path, e);
                    gotNoErrorAnyFile.set(false);
                    asyncProcessesToDo.decrementAndGet();
                    scheduleNextPlayerPath(0, playerPathIterator, importedTreasureIds, gotNoErrorAnyFile, result, millisAtStart);
                }

                scheduleNextPlayerPath(asyncProcessesToDo.decrementAndGet(), playerPathIterator, importedTreasureIds, gotNoErrorAnyFile, result, millisAtStart);
            }, delay);
        } else {
            result.complete(gotNoErrorAnyFile.get());
        }
    }

    private void scheduleNextPlayerPath(
        final int processesWaiting,
        final @NotNull Iterator<@NotNull Path> playerPathIterator,
        final @NotNull Map<@NotNull Location, @NotNull Ulid> importedTreasureIds,
        final @NotNull AtomicBoolean gotNoErrorAnyFile,
        final @NotNull CompletableFuture<@NotNull Boolean> result,
        final long millisAtStart) {

        if (processesWaiting <= 0) {
            // 30 ticks are enough, don't go over
            final long millisLasted = (System.currentTimeMillis() - millisAtStart);
            final float ticksPerSecond = Bukkit.getServerTickManager().getTickRate();
            final int ticks = Math.max(0, 30 - (int) (millisLasted * ticksPerSecond / 1000));

            handleNextPlayerPath(playerPathIterator, importedTreasureIds, ticks, gotNoErrorAnyFile, result);
        } else {
            plugin.getComponentLogger().debug("Tried to start next player, but the current one still has {} processes left!",  processesWaiting);
        }
    }
}
