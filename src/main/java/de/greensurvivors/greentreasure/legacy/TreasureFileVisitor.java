package de.greensurvivors.greentreasure.legacy;

import com.github.f4b6a3.ulid.Ulid;
import de.greensurvivors.greentreasure.DatabaseManager;
import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.legacy.data.*;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.block.Container;
import org.bukkit.inventory.ItemStack;
import org.bukkit.util.Vector;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.configurate.ConfigurationNode;
import org.spongepowered.configurate.serialize.Scalars;
import org.spongepowered.configurate.serialize.SerializationException;
import org.spongepowered.configurate.yaml.YamlConfigurationLoader;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.Duration;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

class TreasureFileVisitor extends ALegacyFileVisitor {
    protected final static @NotNull PathMatcher TREASURE_PATH_MATCHER = FileSystems.getDefault().getPathMatcher("glob:*.{yml,yaml}");
    protected final static @NotNull Pattern ITEM_NUMBER_PATTERN = Pattern.compile("^item(?<number>\\d+)$");
    protected final static @NotNull Duration DEFAULT_FORGETTING_PERIOD = Duration.ofSeconds(-1);
    protected final @NotNull Executor bukkitAsyncExecutor;

    protected TreasureFileVisitor(final @NotNull GreenTreasure plugin, final @NotNull AtomicInteger importProcessID, final @NotNull ForkingSuccessCollector successCollector) {
        super(plugin, importProcessID, successCollector);
        bukkitAsyncExecutor = runnable -> Bukkit.getScheduler().runTaskAsynchronously(plugin, runnable);
    }

    @Override
    public @NotNull FileVisitResult visitFile(final @NotNull Path filePath, final @NotNull BasicFileAttributes attrs) {
        if (attrs.isRegularFile() && TREASURE_PATH_MATCHER.matches(filePath)) {
            successCollector.fork();
            plugin.getComponentLogger().debug("trying to import treasure with path: {}", filePath);

            // register ItemStacks to be deserializable
            final @NotNull YamlConfigurationLoader loader = YamlConfigurationLoader.builder()
                .path(filePath)
                .defaultOptions(options -> options.serializers(builder -> {
                    builder.register(ItemStack.class, ItemStackSerializer.INSTANCE);
                    builder.register(Vector.class, VectorSerializer.INSTANCE);
                })).build();
            final @NotNull ConfigurationNode configRoot;

            try {
                configRoot = loader.load();
            } catch (IOException e) {
                plugin.getComponentLogger().warn("could not read legacy treasure file", e);
                successCollector.finish(false);
                return FileVisitResult.CONTINUE;
            }

            // no I don't know why root is called location either, or why there is a root node to begin with...
            final @NotNull ConfigurationNode containerNode = configRoot.node("location", "container");
            if (!containerNode.virtual() && containerNode.isMap()) {
                try {
                    final @Nullable IBlockInventory blockInventory;

                    final boolean rightSideExists = !containerNode.node("right-side").virtual();
                    final boolean leftSideExists = !containerNode.node("left-side").virtual();
                    if (rightSideExists && leftSideExists) {
                        blockInventory = containerNode.get(DoubleBlockInventory.class);
                    } else if (!rightSideExists && !leftSideExists) {
                        blockInventory = containerNode.get(SingleBlockInventory.class);
                    } else {
                        plugin.getComponentLogger().warn("Treasure file {} doesn't contain a valid container!", filePath);

                        return FileVisitResult.CONTINUE;
                    }

                    if (blockInventory != null) {
                        final int importProcessIDCopy = importProcessId.get();

                        if (importProcessIDCopy >= 0) {
                            processBlockInventory(blockInventory, configRoot.node("location"), importProcessIDCopy, successCollector, filePath);
                        }
                    } else {
                        plugin.getComponentLogger().warn("Could not load legacy treasure {} because it's file does not contain a valid container.", filePath);
                        successCollector.finish(false);
                    }
                } catch (final @NotNull SerializationException e) {
                    plugin.getComponentLogger().warn("Could not load legacy treasure {} because we couldn't deserialize its container.", filePath, e);
                    successCollector.finish(false);
                }
            } else {
                plugin.getComponentLogger().warn("Could not load legacy treasure {} because it's file does not contain a container.", filePath);
                successCollector.finish(false);
            }
        }

        return FileVisitResult.CONTINUE;
    }

    protected void processBlockInventory(final @NotNull IBlockInventory blockInventory,
                                         final @NotNull ConfigurationNode rootNode,
                                         final int importProcessId,
                                         final @NotNull ForkingSuccessCollector successCollector,
                                         final @NotNull Path path) {
        final @Nullable Location treasureLocation = switch (blockInventory) {
            case DoubleBlockInventory doubleBlockInventory -> deserializeLocation(doubleBlockInventory.leftSide(), path);
            case SingleBlockInventory singleBlockInventory -> deserializeLocation(singleBlockInventory, path);
        };

        if (treasureLocation == null) {
            plugin.getComponentLogger().warn("Could not read legacy treasure location for treasure at {}", path);
            successCollector.finish(false);
            return;
        }

        plugin.getChunkParser().registerForChunkParsing(
            treasureLocation.getWorld().getName(), treasureLocation.getBlockX() >> 4, treasureLocation.getBlockZ() >> 4,
            block -> block.getLocation().distanceSquared(treasureLocation) < 0.25,
            resultEither -> resultEither.consume(
                notGeneratedType -> {
                    plugin.getComponentLogger().debug("Could not load legacy treasure at {} because {}", treasureLocation, notGeneratedType);
                    successCollector.finish(false);
                }, tileEntities -> {
                    if (!tileEntities.isEmpty() && tileEntities.iterator().next() instanceof Container container) {
                        container = (Container) Utils.getTreasureHolder(container);

                        @Nullable Ulid treasureId = plugin.getTreasureManager().getTreasureId(container);
                        if (treasureId == null) {
                            treasureId = plugin.getTreasureManager().createNewMonotonicUlid();

                            plugin.getTreasureManager().setTreasureId(container, treasureId);
                        }

                        registerTreasureAsync(treasureId, treasureLocation, blockInventory, rootNode, successCollector, path);
                    } else {
                        plugin.getComponentLogger().warn("Could not load legacy treasure {} because the block at {} is not a container.", path, treasureLocation);
                        successCollector.finish(false);
                    }
                }
            ), importProcessId);
    }

    protected void registerTreasureAsync(final @NotNull Ulid treasureId, final @NotNull Location treasureLocation,
                                         final @NotNull IBlockInventory blockInventory, final @NotNull ConfigurationNode rootNode,
                                         final @NotNull ForkingSuccessCollector successCollector,
                                         final @NotNull Path path) {
        plugin.getServer().getScheduler().runTaskAsynchronously(plugin, () -> {
            final @Nullable List<@NotNull ItemStack> contents = switch (blockInventory) {
                case DoubleBlockInventory doubleBlockInventory -> {
                    final @Nullable List<@NotNull ItemStack> result2;
                    final @Nullable List<@NotNull ItemStack> leftContents = mapItemList(doubleBlockInventory.leftSide().size(), doubleBlockInventory.leftSide().contents(), path);
                    final @Nullable List<@NotNull ItemStack> rightContents = mapItemList(doubleBlockInventory.rightSide().size(), doubleBlockInventory.rightSide().contents(), path);

                    if (leftContents != null) {
                        result2 = leftContents;

                        if (rightContents != null) {
                            result2.addAll(rightContents);
                        }
                    } else {
                        result2 = rightContents;
                    }

                    yield result2;
                }
                case SingleBlockInventory singleBlockInventory -> mapItemList(singleBlockInventory.size(), singleBlockInventory.contents(), path);
            };

            if (contents == null) {
                plugin.getComponentLogger().warn("Legacy treasure {} found at {} is empty!.", path, treasureLocation);

                successCollector.finish(false);
                return;
            }

            final @NotNull AtomicBoolean isUnlimited = new AtomicBoolean(false);
            final @NotNull DatabaseManager databaseManager = plugin.getDatabaseManager();
            databaseManager.setTreasureContents(treasureId, contents).thenComposeAsync(voidz -> {
                final @Nullable Boolean unlimited = Scalars.BOOLEAN.tryDeserialize(rootNode.node("unlimited").rawScalar());
                if (unlimited != null) {
                    isUnlimited.set(unlimited);
                    return databaseManager.setUnlimited(treasureId, unlimited);
                }
                return CompletableFuture.completedStage(null);
            }, bukkitAsyncExecutor).thenComposeAsync(voidz -> {
                final @Nullable Boolean shared = Scalars.BOOLEAN.tryDeserialize(rootNode.node("shared").rawScalar());
                if (shared != null) {
                    return databaseManager.setShared(treasureId, shared);
                }
                return CompletableFuture.completedStage(null);
            }, bukkitAsyncExecutor).thenComposeAsync(voidz -> {
                final @Nullable Integer random = Scalars.INTEGER.tryDeserialize(rootNode.node("random").rawScalar());
                if (!contents.isEmpty() && random != null) {
                    final double randomChance = random == 0 ? 100.00 : random.doubleValue() / ((double) contents.size());
                    return databaseManager.setRandom(treasureId, (short) (randomChance * 100));
                }
                return CompletableFuture.completedStage(null);
            }, bukkitAsyncExecutor).thenComposeAsync(voidz -> {
                final @Nullable Long forgetTimeNumber = Scalars.LONG.tryDeserialize(rootNode.node("forget-time").rawScalar());
                if (forgetTimeNumber != null) {
                    @NotNull Duration forget_time = Duration.ofMillis(forgetTimeNumber);
                    if (forget_time.isZero()) {
                        forget_time = DEFAULT_FORGETTING_PERIOD;
                    }
                    return databaseManager.setForgetDuration(treasureId, forget_time);
                }
                return CompletableFuture.completedStage(null);
            }, bukkitAsyncExecutor).thenComposeAsync(voidz -> {
                if (isUnlimited.get()) {
                    final @Nullable String unlimitedMsg = Scalars.STRING.tryDeserialize(rootNode.node("messages", "UNLIMITED").rawScalar());

                    if (unlimitedMsg != null && !unlimitedMsg.equals("Take as much as you want!")) {
                        return databaseManager.setFindFreshMessageOverride(treasureId, unlimitedMsg);
                    }
                } else {
                    final @Nullable String findFreshMsg = Scalars.STRING.tryDeserialize(rootNode.node("messages", "FOUND").rawScalar());

                    if (findFreshMsg != null && !findFreshMsg.equals("You have found treasure!")) {
                        return databaseManager.setFindFreshMessageOverride(treasureId, findFreshMsg);
                    }
                }

                return CompletableFuture.completedStage(null);
            }, bukkitAsyncExecutor).thenComposeAsync(voidz -> {
                final @Nullable String alreadyLootedMsg = Scalars.STRING.tryDeserialize(rootNode.node("messages", "FOUND_ALREADY").rawScalar());

                if (alreadyLootedMsg != null && !alreadyLootedMsg.equals("You have already looted this treasure...")) {
                    return databaseManager.setFindLootedMessageOverride(treasureId, alreadyLootedMsg);
                }

                return CompletableFuture.completedStage(null);
            }, bukkitAsyncExecutor).whenComplete((voidz, ex) -> {
                if (ex == null) {
                    plugin.getComponentLogger().debug("imported treasure with id {} from path {}", treasureId, path);

                    successCollector.finish(true);
                } else {
                    successCollector.finish(false);
                    plugin.getComponentLogger().warn("Couldn't properly import treasure with id {} from path {}", treasureId, path, ex);
                }
            });
        });
    }

    protected @Nullable Location deserializeLocation(final @NotNull SingleBlockInventory singleBlockInventory, final @NotNull Path path) throws IllegalArgumentException {
        final @Nullable World world = Bukkit.getServer().getWorld(singleBlockInventory.world());

        if (world == null) {
            plugin.getComponentLogger().warn("Could not load legacy treasure {} because I couldn't find a world named {}.", path, singleBlockInventory.world());
            return null;
        }

        final @NotNull Location treasureLocation = new Location(world,
            singleBlockInventory.coords().getBlockX(),
            singleBlockInventory.coords().getBlockY(),
            singleBlockInventory.coords().getBlockZ());
        treasureLocation.checkFinite();

        return treasureLocation;
    }

    protected @Nullable List<@NotNull ItemStack> mapItemList(final int inventorySize, final @NotNull Map<@NotNull String, @NotNull ItemStack> serializedMap, final @NotNull Path path) {
        final @Nullable ItemStack @NotNull [] result = new ItemStack[inventorySize];

        for (Map.Entry<@NotNull String, @NotNull ItemStack> contentsEntry : serializedMap.entrySet()) {
            final @NotNull Matcher matcher = ITEM_NUMBER_PATTERN.matcher(contentsEntry.getKey());

            if (matcher.matches()) {
                int num = Integer.parseInt(matcher.group("number"));

                if (num < inventorySize && num >= 0) {
                    result[num] = contentsEntry.getValue();
                } else {
                    plugin.getComponentLogger().warn("Could not load legacy treasure {} because the index {} for this item is out of bounds ({}).", path, num, inventorySize);
                }
            } else {
                plugin.getComponentLogger().warn("Could not load legacy treasure {} because {} does not match {}", path, contentsEntry.getKey(), ITEM_NUMBER_PATTERN.pattern());
                return null;
            }
        }

        return Arrays.stream(result).
            map(itemStack -> Objects.<@NotNull ItemStack>requireNonNullElseGet(itemStack, ItemStack::empty)).
            collect(Collectors.toCollection(ArrayList::new));
    }
}
