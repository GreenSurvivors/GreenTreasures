package de.greensurvivors.greentreasure;

import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.LoadingCache;
import com.github.f4b6a3.ulid.Ulid;
import com.github.f4b6a3.ulid.UlidFactory;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import it.unimi.dsi.fastutil.longs.Long2ObjectMap;
import it.unimi.dsi.fastutil.longs.Long2ObjectMaps;
import it.unimi.dsi.fastutil.longs.Long2ObjectOpenHashMap;
import net.kyori.adventure.audience.Audience;
import net.kyori.adventure.text.minimessage.tag.resolver.Formatter;
import org.bukkit.*;
import org.bukkit.block.BlockState;
import org.bukkit.block.Container;
import org.bukkit.inventory.InventoryHolder;
import org.bukkit.inventory.InventoryView;
import org.bukkit.persistence.PersistentDataHolder;
import org.bukkit.persistence.PersistentDataType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;

public class TreasureManager {
    // sort locations by x, then y, then z
    private final static @NotNull Comparator<Location> LOCATION_COMPARATOR = Comparator.
        comparingDouble(Location::x).
        thenComparingDouble(Location::y).
        thenComparingDouble(Location::z);
    // the amount of chunks that should get requested in the same tick.
    // in my testing, 8 chunks would take up to 2 GB.
    // You could probably get higher if you would allocate more ram,
    // however our testserver with xmx 1.5GB and container size of 5.29 GiB really does struggle
    // with fairly high like a radius of 5000 blocks aka 391876 chunks
    private final static int CHUNKS_TO_LOAD_PARALLEL = 8;
    private static final int CHUNKS_TO_PROCESS_BETWEEN_MESSAGES = CHUNKS_TO_LOAD_PARALLEL * 100;

    private final @NotNull GreenTreasure plugin;
    private final @NotNull NamespacedKey idKey;
    private final @NotNull UlidFactory ulidFactory;
    // list of known treasures with its location and its information
    private final @NotNull LoadingCache<@NotNull Ulid, @Nullable TreasureInfo> treasures;

    private final @NotNull Map<@NotNull String, @NotNull Long2ObjectMap<@NotNull ChunkLoadInfo>> chunkLoadingMap;

    public TreasureManager(final @NotNull GreenTreasure plugin) {
        this.plugin = plugin;
        this.idKey = new NamespacedKey(plugin, "id");
        this.treasures = Caffeine.newBuilder().build(id -> plugin.getDatabaseManager().loadTreasure(id));


        ulidFactory = UlidFactory.newMonotonicInstance(() -> Utils.RANDOM_GENERATOR.nextLong());
        chunkLoadingMap = new HashMap<>();

        // requesting all nearby chunks at the same time works fine for the intended case of small radii,
        // however, if the user does something stupid, the server will eat up all the ram and die, or worse will hang itself
        // so we will throttle the chunk requesting process by CHUNKS_TO_LOAD_PARALLEL per second.
        // One optimisation I did make, was that every time the same chunk gets requested, the request will get bundled and complete
        // at the same time as every other request for that very chunk.
        Bukkit.getScheduler().runTaskTimer(plugin, () -> {
            if (!chunkLoadingMap.isEmpty()) {
                int i = 0;
                Iterator<Map.Entry<@NotNull String, @NotNull Long2ObjectMap<@NotNull ChunkLoadInfo>>> worldIterator = chunkLoadingMap.entrySet().iterator();

                while (worldIterator.hasNext() && i < CHUNKS_TO_LOAD_PARALLEL) {
                    final @NotNull Map.Entry<@NotNull String, @NotNull Long2ObjectMap<@NotNull ChunkLoadInfo>> entry = worldIterator.next();

                    final @Nullable World world = Bukkit.getWorld(entry.getKey());
                    if (world != null) {
                        final @NotNull Iterator<Long2ObjectMap.@NotNull Entry<@NotNull ChunkLoadInfo>> chunkLoadInfoIterator = Long2ObjectMaps.fastIterator(entry.getValue());

                        while (chunkLoadInfoIterator.hasNext() && i < CHUNKS_TO_LOAD_PARALLEL) {
                            final @NotNull Long2ObjectMap.@NotNull Entry<@NotNull ChunkLoadInfo> chunkLoadInfoEntry = chunkLoadInfoIterator.next();

                            parseChunk(world, chunkLoadInfoEntry.getValue());

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
                                chunkConsumer.chunkDoneFuture.complete(null)));
                        entry.getValue().clear();

                        worldIterator.remove();
                    }
                }
            }
        }, 20, 1);
    }

    public @NotNull Ulid createNewMonotonicUlid() {
        return ulidFactory.create();
    }

    public @Nullable Ulid getTreasureId(final @NotNull PersistentDataHolder dataHolder) {
        final byte[] bytes = dataHolder.getPersistentDataContainer().get(idKey, PersistentDataType.BYTE_ARRAY);
        return bytes == null ? null : Ulid.from(bytes);
    }

    public @Nullable Ulid getTreasureId(final @NotNull InventoryView inventoryView) {
        final @Nullable InventoryHolder holder = inventoryView.getTopInventory().getHolder(false);

        if (Utils.getTreasureHolder(holder) instanceof PersistentDataHolder persistentDataHolder) {
            return getTreasureId(persistentDataHolder);
        }

        return null;
    }

    public void setTreasureId(final @NotNull PersistentDataHolder dataHolder, final @NotNull Ulid treasureId) {
        dataHolder.getPersistentDataContainer().set(idKey, PersistentDataType.BYTE_ARRAY, treasureId.toBytes());
    }

    public @NotNull CompletableFuture<@NotNull Boolean> deleteTreasure(final @NotNull PersistentDataHolder dataHolder) { // todo this have to get reworked, if multiple treasures with the same id ever get exposed to the user
        final @Nullable Ulid treasureId = getTreasureId(dataHolder);

        if (treasureId != null) {
            dataHolder.getPersistentDataContainer().remove(idKey);

            return plugin.getDatabaseManager().deleteTreasure(treasureId).
                thenCompose(void_ -> plugin.getDatabaseManager().forgetAll(treasureId)).
                thenApply(void_ -> Boolean.TRUE);
        } else {
            return CompletableFuture.completedFuture(false);
        }
    }

    /**
     * get a treasure given its uuid
     *
     * @return all known information about the treasure itself
     */
    public @Nullable TreasureInfo getTreasureInfo(final @NotNull Ulid treasureId) {
        return treasures.get(treasureId);
    }

    public @Nullable TreasureInfo getTreasureInfo(final @NotNull PersistentDataHolder persistentDataHolder) {
        final @Nullable Ulid treasureId = getTreasureId(persistentDataHolder);

        if (treasureId == null) {
            return null;
        } else {
            return treasures.get(treasureId);
        }
    }

    public @Nullable TreasureInfo getTreasureInfo(final @NotNull InventoryView view) {
        final @Nullable Ulid treasureId = getTreasureId(view);

        if (treasureId == null) {
            return null;
        } else {
            return treasures.get(treasureId);
        }
    }

    /**
     * closes all open inventories and clears the internal hashmaps
     * (to repopulate them with updated information)
     */
    public void clearTreasures() {
        treasures.invalidateAll();
        treasures.cleanUp();
    }

    public void invalidateTreasure(final @NotNull Ulid treasureId) {
        plugin.getTreasureListener().closeInventories(treasureId);
        treasures.invalidate(treasureId);
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
        final @NotNull Location startLocation, final int radius,
        final @NotNull Audience audience) {

        final @NotNull Map<@NotNull TreasureInfo, @NotNull SortedSet<@NotNull Location>> infoToNearLocations = new HashMap<>();
        final @NotNull World world = startLocation.getWorld();
        final int maxCx = (startLocation.getBlockX() + radius) >> 4;
        final double minX = startLocation.getX() - radius;
        final double maxX = startLocation.getX() + radius;
        final double minY = Math.max(world.getMinHeight(), startLocation.getY() - radius);
        final double maxY = Math.min(world.getMaxHeight() - 1, startLocation.getY() + radius); // why does Bukkit add a 1 here? We're asking for the max height not the world height...
        final double minZ = startLocation.getZ() - radius;
        final double maxZ = startLocation.getZ() + radius;
        final int maxCz = (startLocation.getBlockZ() + radius) >> 4;

        // this list keeps track of all CompletableFutures needed to complete the search
        final @NotNull List<@NotNull CompletableFuture<Void>> futuresToComplete = new ArrayList<>();

        final @NotNull Long2ObjectMap<@NotNull ChunkLoadInfo> map2 = chunkLoadingMap.
            // todo, the openHashMap will not retain any order. In some situations that might be irrelevant,
            //  but this will mean
            computeIfAbsent(world.getName(), ignored -> new Long2ObjectOpenHashMap<>());
        // atomic int not because of concurrent access, but because java lambdas are stupid.
        final @NotNull AtomicInteger chunksStillToLoad = new AtomicInteger(0);
        final @NotNull AtomicInteger totalChunksToLoad = new AtomicInteger(0);

        for (int cx = (startLocation.getBlockX() - radius) >> 4; cx <= maxCx; cx++) {
            for (int cz = (startLocation.getBlockZ() - radius) >> 4; cz <= maxCz; cz++) {
                final long chunkKey = Chunk.getChunkKey(cx, cz);
                map2.putIfAbsent(chunkKey, new ChunkLoadInfo(cx, cz, new HashSet<>()));
                chunksStillToLoad.getAndIncrement();
                totalChunksToLoad.getAndIncrement();

                final @NotNull CompletableFuture<Void> chunkDoneFuture = new CompletableFuture<>();
                map2.get(chunkKey).chunkConsumers.add(
                    new ChunkConsumer(minX, maxX, minY, maxY, minZ, maxZ,
                        (treasureInfo, treasureLocation) ->
                            infoToNearLocations.computeIfAbsent(treasureInfo, i ->
                                new TreeSet<>(LOCATION_COMPARATOR)).add(treasureLocation), chunkDoneFuture)
                );

                // inform the user about the current process.
                int finalChunkX = cx;
                int finalChunkZ = cz;
                chunkDoneFuture.thenRun(() -> {
                    if (totalChunksToLoad.get() > CHUNKS_TO_PROCESS_BETWEEN_MESSAGES) {
                        chunksStillToLoad.getAndDecrement();

                        if ((chunksStillToLoad.get() % CHUNKS_TO_PROCESS_BETWEEN_MESSAGES) == 0) { // ~ every 5 seconds report back
                            final int chunksDone = totalChunksToLoad.get() - chunksStillToLoad.get();
                            double percent = (double) chunksDone / (double) totalChunksToLoad.get() * 100.0d;

                            plugin.getMessageManager().sendLang(audience, LangPath.CHUNK_PROCESS_WORK,
                                Formatter.number(PlaceHolderKey.X.getKey(), finalChunkX),
                                Formatter.number(PlaceHolderKey.Z.getKey(), finalChunkZ),
                                Formatter.number(PlaceHolderKey.PERCENT.getKey(), percent),
                                Formatter.number(PlaceHolderKey.NUMBER.getKey(), chunksDone),
                                Formatter.number(PlaceHolderKey.MAX.getKey(), totalChunksToLoad.get()));
                        }
                    }
                });

                futuresToComplete.add(chunkDoneFuture);
            }
        }

        // wait for all to complete, then sort by location
        return CompletableFuture.allOf(futuresToComplete.toArray(new CompletableFuture[0])).
            thenApply(void_ ->
                infoToNearLocations.entrySet().stream().sorted((o1, o2) ->
                        LOCATION_COMPARATOR.compare(o1.getValue().getFirst(), o2.getValue().first())).
                    collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, (e1, e2) -> e1, LinkedHashMap::new)));
    }

    private void parseChunk(final @NotNull World world, final @NotNull ChunkLoadInfo chunkLoadInfo) {
        world.getChunkAtAsync(chunkLoadInfo.chunkX, chunkLoadInfo.chunkZ, false, chunk -> {
            if (chunk == null) { // chunk was not generated yet. I don't expect treasures there!
                for (final @NotNull ChunkConsumer chunkConsumer : chunkLoadInfo.chunkConsumers) {
                    chunkConsumer.chunkDoneFuture.complete(null);
                }
            } else {
                for (final @NotNull ChunkConsumer chunkConsumer : chunkLoadInfo.chunkConsumers) {

                    // it's slightly faster to use a Predicate to sort locations out before getting the BlockState
                    final @NotNull Collection<@NotNull BlockState> tileEntities = chunk.getTileEntities(block ->
                            block.getX() >= chunkConsumer.minX && block.getX() <= chunkConsumer.maxX &&
                                block.getY() >= chunkConsumer.minY && block.getY() <= chunkConsumer.maxY &&
                                block.getZ() >= chunkConsumer.minZ && block.getZ() <= chunkConsumer.maxZ,
                        false
                    );

                    for (BlockState tileEntity : tileEntities) { // note: we have no guarantee of the ordering of these
                        if (tileEntity instanceof Container container) {
                            final @Nullable TreasureInfo info = getTreasureInfo(container);

                            if (info != null) {
                                chunkConsumer.treasureConsumer.accept(info, tileEntity.getLocation());
                            }
                        }
                    }

                    chunkConsumer.chunkDoneFuture.complete(null);
                }
            }
        });
    }

    private record ChunkLoadInfo(int chunkX, int chunkZ, @NotNull Set<@NotNull ChunkConsumer> chunkConsumers) {
    }

    private record ChunkConsumer(double minX, double maxX, double minY, double maxY, double minZ, double maxZ,
                                 @NotNull BiConsumer<@NotNull TreasureInfo, @NotNull Location> treasureConsumer,
                                 @NotNull CompletableFuture<Void> chunkDoneFuture) {
    }
}
