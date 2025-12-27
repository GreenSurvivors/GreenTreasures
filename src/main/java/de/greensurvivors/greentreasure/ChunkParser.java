package de.greensurvivors.greentreasure;

import com.github.f4b6a3.ulid.Ulid;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.dataobjects.either.Either;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import it.unimi.dsi.fastutil.Pair;
import it.unimi.dsi.fastutil.longs.Long2ObjectMap;
import it.unimi.dsi.fastutil.longs.Long2ObjectMaps;
import it.unimi.dsi.fastutil.longs.Long2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.objects.Object2ObjectMap;
import it.unimi.dsi.fastutil.objects.Object2ObjectMaps;
import it.unimi.dsi.fastutil.objects.Object2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.objects.ObjectObjectImmutablePair;
import net.kyori.adventure.audience.Audience;
import net.kyori.adventure.text.minimessage.tag.resolver.Formatter;
import org.apache.logging.log4j.util.TriConsumer;
import org.bukkit.Bukkit;
import org.bukkit.Chunk;
import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.block.Block;
import org.bukkit.block.BlockState;
import org.bukkit.block.Container;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.Range;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.Predicate;

public class ChunkParser {
    // sort locations by x, then y, then z
    private final static @NotNull Comparator<Location> LOCATION_COMPARATOR = Comparator.
        comparingDouble(Location::x).
        thenComparingDouble(Location::y).
        thenComparingDouble(Location::z);
    // the amount of chunks that should get requested in the same tick.
    // in my testing, 24 chunks would take up to 1.2 GB and round about <??> ms/tick for the whole server and 700MG / <??> ms/tick for the plugin
    // for a radius of 20000 blocks
    // faster aka more chunks / tick needs more cpu / hdd read speed (given the ram is big enough to support the request)
    // and bigger aka more radius needs more ram.
    private final static int CHUNKS_TO_LOAD_PARALLEL = 8;
    private static final int CHUNKS_TO_PROCESS_BETWEEN_MESSAGES = CHUNKS_TO_LOAD_PARALLEL * 400; // 20 ticks * 20 == 20 seconds, if there is no other task interfering

    private final @NotNull Object2ObjectMap<@NotNull String, @NotNull Long2ObjectMap<@NotNull ChunkLoadInfo>> worldChunkLoadingMap = new Object2ObjectOpenHashMap<>();
    protected final @NotNull GreenTreasure plugin;

    public ChunkParser(final @NotNull GreenTreasure plugin) {
        this.plugin = plugin;

        // requesting all nearby chunks at the same time works fine for the intended case of small radii,
        // however, if the user does something stupid, the server will eat up all the ram and die, or worse will hang itself
        // so we will throttle the chunk requesting process by CHUNKS_TO_LOAD_PARALLEL per tick.
        // One "optimization" I did make, was that every time the same chunk gets requested, the request will get bundled and complete
        // at the same time as every other request for that very chunk.
        // Will it ever be worth it, instead of just requesting the chunk multiple times? Probably never.
        // That would require multiple decently big request somewhat near each other.
        Bukkit.getScheduler().runTaskTimer(plugin, () -> {
            if (!worldChunkLoadingMap.isEmpty()) {
                int i = 0;
                Iterator<Object2ObjectMap.Entry<@NotNull String, @NotNull Long2ObjectMap<@NotNull ChunkLoadInfo>>> worldIterator = Object2ObjectMaps.fastIterator(worldChunkLoadingMap);

                while (worldIterator.hasNext() && i < CHUNKS_TO_LOAD_PARALLEL) {
                    final @NotNull Map.Entry<@NotNull String, @NotNull Long2ObjectMap<@NotNull ChunkLoadInfo>> entry = worldIterator.next();

                    final @Nullable World world = Bukkit.getWorld(entry.getKey());
                    if (world != null) {
                        final @NotNull Iterator<Long2ObjectMap.@NotNull Entry<@NotNull ChunkLoadInfo>> chunkLoadInfoIterator = Long2ObjectMaps.fastIterator(entry.getValue());

                        while (chunkLoadInfoIterator.hasNext() && i < CHUNKS_TO_LOAD_PARALLEL) {
                            final @NotNull Long2ObjectMap.@NotNull Entry<@NotNull ChunkLoadInfo> chunkLoadInfoEntry = chunkLoadInfoIterator.next();
                            parseChunk(world, chunkLoadInfoEntry.getValue()); // todo if we are skipping over a chunk do to it not being generated yet, maybe we could try another like do i+=10; if the chunk was generated and i++ if not; and quadruple the "amount of chunks per tick"

                            chunkLoadInfoIterator.remove();
                            i++;
                        }

                        // free memory
                        if (!chunkLoadInfoIterator.hasNext()) {
                            worldIterator.remove();
                        }
                    } else {
                        plugin.getComponentLogger().warn("Could not complete treasure survey, because world {} was unloaded.", entry.getKey());

                        entry.getValue().values().forEach(chunkLoadInfo ->
                            chunkLoadInfo.chunkConsumers.forEach(chunkConsumer ->
                                chunkConsumer.resultConsumer.accept(Either.left(NotGeneratedType.WORLD))));
                        entry.getValue().clear();

                        worldIterator.remove();
                    }
                }
            }
        }, 20, 1);
    }

    protected double square(final double d) {
        return d * d;
    }

    // wait for all to complete, then sort by location
    protected @NotNull CompletableFuture<@NotNull SequencedMap<@NotNull TreasureInfo, @NotNull SortedSet<@NotNull Location>>> sortNearTreasureResult(final @NotNull SequencedMap<@NotNull Ulid, @NotNull List<@NotNull Location>> foundTreasures) {
        @SuppressWarnings("unchecked")
        // Arrays can't have generics, and they don't matter because of type erasure anyway.
        final CompletableFuture<@Nullable TreasureInfo>[] features = new CompletableFuture[foundTreasures.size()];
        int i = 0;
        for (@NotNull Ulid treasureId : foundTreasures.keySet()) {
            final @NotNull CompletableFuture<@Nullable TreasureInfo> treasureInfoFuture = plugin.getTreasureManager().getTreasureInfo(treasureId);

            features[i++] = treasureInfoFuture;
        }

        return CompletableFuture.allOf(features).thenApplyAsync(__ -> {
            final @NotNull List<@NotNull Pair<@NotNull TreasureInfo, @NotNull SortedSet<@NotNull Location>>> entries = new ArrayList<>(foundTreasures.size());

            final Iterator<@NotNull List<@NotNull Location>> allLocationsIterator = foundTreasures.values().iterator();
            for (final @NotNull CompletableFuture<@Nullable TreasureInfo> future : features) {
                final @Nullable TreasureInfo treasureInfo = future.join();
                final @NotNull List<@NotNull Location> locations = allLocationsIterator.next();

                if (treasureInfo != null) {
                    final @NotNull SortedSet<@NotNull Location> sortedLocations = new TreeSet<>(LOCATION_COMPARATOR);
                    sortedLocations.addAll(locations);

                    entries.add(new ObjectObjectImmutablePair<>(treasureInfo, sortedLocations));
                }
            }

            entries.sort((o1, o2) -> LOCATION_COMPARATOR.compare(o1.value().getFirst(), o2.value().getFirst()));

            final @NotNull LinkedHashMap<@NotNull TreasureInfo, @NotNull SortedSet<@NotNull Location>> resultMap = new LinkedHashMap<>();
            for (Pair<TreasureInfo, SortedSet<Location>> ulidSortedSetEntry : entries) {
                resultMap.put(ulidSortedSetEntry.key(), ulidSortedSetEntry.value());
            }

            return resultMap;
        });
    }

    /**
     * <p>Queries all Treasures in a radius around a location.<br>
     * The returned Map will be ordered by the smallest x, y, z coordinates of the TreasureInfo.<br>
     * Therefor the map, as well as the set will always be in the same order for the same location and same radius,<br>
     * as long as no Treasure was changed. </p>
     * Since much work has to be done on the main thread, this method will get throttled if the workload is too big,
     * and the audience will get informed about the ongoing process.
     */
    public @NotNull CompletableFuture<@NotNull SequencedMap<@NotNull TreasureInfo, @NotNull SortedSet<@NotNull Location>>> getNearTreasures(
        final @NotNull Location startLocation, final @Range(from = 0, to = Integer.MAX_VALUE) int radius,
        final @NotNull Audience audience) {
        final String worldName = startLocation.getWorld().getName();
        final int cxStart = startLocation.getBlockX() >> 4;
        final int czStart = startLocation.getBlockZ() >> 4;

        // result map
        final @NotNull SequencedMap<@NotNull Ulid, @NotNull List<@NotNull Location>> foundTreasures = new LinkedHashMap<>();

        final @NotNull AtomicInteger numberChunksDone = new AtomicInteger(0);
        final @NotNull AtomicInteger totalChunksToLoad = new AtomicInteger(0);
        final @NotNull CompletableFuture<@NotNull SequencedMap<@NotNull TreasureInfo, @NotNull SortedSet<@NotNull Location>>> resultFuture = new CompletableFuture<>();

        // todo this is mighty fine for small radii, but it explodes in ram usage for bigger ones. This is bad. We need to schedeule this
        // make sure we include the whole cycle, if we  don't there might be some parts of the cycle jut in the corners
        final double cRadius = (radius >> 4) + 1;
        final double radiusSquared = square(radius);
        final @NotNull Predicate<@NotNull Block> blockPredicate = block ->
            square(block.getX() - startLocation.x()) +
                square(block.getY() - startLocation.y()) +
                square(block.getZ() - startLocation.z()) <= radiusSquared;

        final @NotNull AtomicBoolean allChunksRegistered = new AtomicBoolean(false);
        final @NotNull TriConsumer<@NotNull Either<@NotNull NotGeneratedType, @NotNull Collection<@NotNull BlockState>>, @NotNull Integer, @NotNull Integer> stateConsumer = (resultEither, cx, cz) -> {
            resultEither.consume(
                notGeneratedType -> {
                },
                tileEntities -> {
                    for (BlockState tileEntity : tileEntities) {
                        if (tileEntity instanceof Container container) {
                            final @Nullable Ulid treasureId = plugin.getTreasureManager().getTreasureId(container);

                            if (treasureId != null) {
                                // using array list here instead of an already sorted set like TreeSet, because ArrayList add way less overhead and most of these list should end up empty.
                                foundTreasures.computeIfAbsent(treasureId, k -> new ArrayList<>()).add(tileEntity.getLocation());
                            }
                        }
                    }
                });

            numberChunksDone.getAndIncrement();
            messageAudience(audience, cx, cz, totalChunksToLoad, numberChunksDone);

            // since everything happens on main thread there shouldn't be any racing conditions here
            if (allChunksRegistered.get()) {
                if (numberChunksDone.get() >= totalChunksToLoad.get()) {
                    sortNearTreasureResult(foundTreasures).thenApplyAsync(resultFuture::complete, Bukkit.getScheduler().getMainThreadExecutor(plugin));
                }
            }
        };

        if (cRadius == 1) {
            allChunksRegistered.set(true);
            prepareChunkForParsing(worldName, cxStart, czStart, blockPredicate, totalChunksToLoad, stateConsumer);

            return resultFuture;
        }

        final double cRadiusSquared = cRadius * cRadius;

        for (int x = 0; x <= cRadius; ++x) {
            for (int z = 0; z <= cRadius; ++z) {
                // x^2 + z^2 > r^2
                if (square(x) + square(z) > cRadiusSquared) {
                    break;
                }

                prepareChunkForParsing(worldName, cxStart + x + 1, czStart + z + 1, blockPredicate, totalChunksToLoad, stateConsumer);
                prepareChunkForParsing(worldName, cxStart - x, czStart + z + 1, blockPredicate, totalChunksToLoad, stateConsumer);
                prepareChunkForParsing(worldName, cxStart + x + 1, czStart - z, blockPredicate, totalChunksToLoad, stateConsumer);
                prepareChunkForParsing(worldName, cxStart - x, czStart - z, blockPredicate, totalChunksToLoad, stateConsumer);
            }
        }
        allChunksRegistered.set(true);

        return resultFuture;
    }

    public void registerForChunkParsing(final @NotNull String worldName, final int chunkX, final int chunkZ,
                                        final @NotNull Predicate<? super Block> blockPredicate,
                                        @NotNull Consumer<@NotNull Either<
                                            @NotNull NotGeneratedType,
                                            @NotNull Collection<@NotNull BlockState>>> resultConsumer) {
        final long chunkKey = Chunk.getChunkKey(chunkX, chunkZ);

        final @NotNull Long2ObjectMap<@NotNull ChunkLoadInfo> chunkLoadingMap = worldChunkLoadingMap.
            // todo, the openHashMap will not retain any order. In some situations that might be irrelevant,
            //  but this does mean the loading order is pretty unpredictable
                computeIfAbsent(worldName, ignored -> new Long2ObjectOpenHashMap<>());
        // note: even though ArrayList would allow multiple entries of the same instance AND as a List remains insertion order,
        // the Type was chosen, because of its low overhead. Please treat it, like it was a HashSet.
        chunkLoadingMap.computeIfAbsent(chunkKey, k -> new ChunkLoadInfo(chunkX, chunkZ, new ArrayList<>())).
            chunkConsumers.add(new ChunkConsumer(blockPredicate, resultConsumer));
    }

    /// inform the user about the current process.
    protected void messageAudience(final @NotNull Audience audience,
                                   final int cx, final int cz,
                                   final @NotNull AtomicInteger totalChunksToLoad, final @NotNull AtomicInteger chunksToLoadDone) {
        if (totalChunksToLoad.get() > CHUNKS_TO_PROCESS_BETWEEN_MESSAGES) {
            final int chunksToLoadDoneNumber = chunksToLoadDone.get();
            if ((chunksToLoadDoneNumber % CHUNKS_TO_PROCESS_BETWEEN_MESSAGES) == 0) { // ~ every 10 seconds report back
                double percent = (double) chunksToLoadDoneNumber / (double) totalChunksToLoad.get() * 100.0d;

                plugin.getMessageManager().sendLang(audience, LangPath.CHUNK_PROCESS_WORK,
                    Formatter.number(PlaceHolderKey.X.getKey(), cx),
                    Formatter.number(PlaceHolderKey.Z.getKey(), cz),
                    Formatter.number(PlaceHolderKey.PERCENT.getKey(), percent),
                    Formatter.number(PlaceHolderKey.NUMBER.getKey(), chunksToLoadDoneNumber),
                    Formatter.number(PlaceHolderKey.MAX.getKey(), totalChunksToLoad.get()));
            }
        }
    }

    protected void prepareChunkForParsing(final @NotNull String worldName, final int cx, final int cz, final @NotNull Predicate<@NotNull Block> blockPredicate,
                                          final @NotNull AtomicInteger totalChunksToLoad,
                                          final @NotNull TriConsumer<@NotNull Either<@NotNull NotGeneratedType, @NotNull Collection<@NotNull BlockState>>, Integer, Integer> stateConsumer) {
        totalChunksToLoad.getAndIncrement();
        registerForChunkParsing(worldName, cx, cz, blockPredicate, it -> stateConsumer.accept(it, cx, cz));
    }

    protected void parseChunk(final @NotNull World world, final @NotNull ChunkLoadInfo chunkLoadInfo) {
        world.getChunkAtAsync(chunkLoadInfo.chunkX, chunkLoadInfo.chunkZ, false, chunk -> {
            if (chunk == null) { // chunk was not generated yet. I don't expect treasures there! (even though there could be using a custom generator, maybe use an optional gen parameter if it ever becomes useful)
                for (final @NotNull ChunkConsumer chunkConsumer : chunkLoadInfo.chunkConsumers) {
                    chunkConsumer.resultConsumer.accept(Either.left(NotGeneratedType.CHUNK));
                }
            } else {
                for (final @NotNull ChunkConsumer chunkConsumer : chunkLoadInfo.chunkConsumers) {
                    // it's slightly faster to use a Predicate to sort locations out before getting the BlockState
                    chunkConsumer.resultConsumer.accept(Either.right(chunk.getTileEntities(chunkConsumer.blockPredicate, false)));
                }
            }
        });
    }

    public enum NotGeneratedType {
        WORLD,
        CHUNK
    }

    protected record ChunkLoadInfo(int chunkX, int chunkZ, @NotNull Collection<@NotNull ChunkConsumer> chunkConsumers) {
    }

    protected record ChunkConsumer(@NotNull Predicate<? super Block> blockPredicate,
                                   @NotNull Consumer<@NotNull Either<
                                       @NotNull NotGeneratedType,
                                       @NotNull Collection<@NotNull BlockState>>> resultConsumer) {
    }
}
