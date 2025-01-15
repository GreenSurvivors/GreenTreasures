package de.greensurvivors.greentreasure;

import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.LoadingCache;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import org.bukkit.Location;
import org.bukkit.NamespacedKey;
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
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.stream.Collectors;

public class TreasureManager {
    private final @NotNull GreenTreasure plugin;
    private final @NotNull NamespacedKey idKey;
    // list of known treasures with its location and its information
    private final @NotNull LoadingCache<@NotNull String, @Nullable TreasureInfo> treasures;

    public TreasureManager(final @NotNull GreenTreasure plugin) {
        this.plugin = plugin;
        this.idKey = new NamespacedKey(plugin, "id");
        this.treasures = Caffeine.newBuilder().build(id -> plugin.getDatabaseManager().loadTreasure(id));
    }

    public @Nullable String getTreasureId(final @NotNull PersistentDataHolder dataHolder) {
        return dataHolder.getPersistentDataContainer().get(idKey, PersistentDataType.STRING);
    }

    public @Nullable String getTreasureId(final @NotNull InventoryView inventoryView) {
        final @Nullable InventoryHolder holder = inventoryView.getTopInventory().getHolder(false);

        if (Utils.getTreasureHolder(holder) instanceof PersistentDataHolder persistentDataHolder) {
            return getTreasureId(persistentDataHolder);
        }

        return null;
    }

    public void setTreasureId(final @NotNull PersistentDataHolder dataHolder, final @NotNull String treasureId) {
        dataHolder.getPersistentDataContainer().set(idKey, PersistentDataType.STRING, treasureId);
    }

    public @NotNull CompletableFuture<@NotNull Boolean> deleteTreasure(final @NotNull PersistentDataHolder dataHolder) { // todo this have to get reworked, if multiple treasures with the same id ever get exposed to the user
        final @Nullable String treasureId = getTreasureId(dataHolder);

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
    public @Nullable TreasureInfo getTreasureInfo(final @NotNull String treasureId) {
        return treasures.get(treasureId);
    }

    public @Nullable TreasureInfo getTreasureInfo(final @NotNull PersistentDataHolder persistentDataHolder) {
        final @Nullable String treasureId = getTreasureId(persistentDataHolder);

        if (treasureId == null) {
            return null;
        } else {
            return treasures.get(treasureId);
        }
    }

    public @Nullable TreasureInfo getTreasureInfo(final @NotNull InventoryView view) {
        final @Nullable String treasureId = getTreasureId(view);

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

    public void invalidateTreasure(final @NotNull String treasureId) {
        plugin.getTreasureListener().closeInventories(treasureId);
        treasures.invalidate(treasureId);
    }

    /**
     * <p>Queries all Treasures in a radius around a location.<br>
     * The returned Map will be ordered by the smallest x, y, z coordinates of the TreasureInfo.<br>
     * Therefor the map, as well as the set will always be in the same order for the same location and same radius,<br>
     * as long as no Treasure was changed. </p>
     * Since the access to the TreasureInfo has to be sync, this is a very dangerous method in terms of lag!
     */
    public @NotNull CompletableFuture<@NotNull SequencedMap<@NotNull TreasureInfo, @NotNull SortedSet<@NotNull Location>>> getNearTreasures(
        final @NotNull Location location, final int radius) {

        final @NotNull Map<@NotNull TreasureInfo, @NotNull SortedSet<@NotNull Location>> infoToNearLocations = new ConcurrentHashMap<>();
        final int maxCx = (location.getBlockX() + radius) >> 4;
        final double minX = location.getX() - radius;
        final double maxX = location.getX() + radius;
        final double minY = location.getY() - radius;
        final double maxY = location.getY() + radius;
        final double minZ = location.getZ() - radius;
        final double maxZ = location.getZ() + radius;
        final int maxCz = (location.getBlockZ() + radius) >> 4;

        // this list keeps track of all CompletableFutures needed to complete the search
        final @NotNull List<@NotNull CompletableFuture<?>> futures = new ArrayList<>();
        final @NotNull Comparator<Location> locationComparator = Comparator.
            comparingDouble(Location::x).
            thenComparingDouble(Location::y).
            thenComparingDouble(Location::z);

        for (int cx = (location.getBlockX() - radius) >> 4; cx <= maxCx; cx++) {
            for (int cz = (location.getBlockZ() - radius) >> 4; cz <= maxCz; cz++) {
                futures.add(
                    location.getWorld().getChunkAtAsync(cx, cz, false).
                        // it's slightly faster to use a Predicate to sort locations out before getting the BlockState
                            thenApply(chunk -> chunk.getTileEntities(block ->
                            block.getX() >= minX && block.getX() <= maxX &&
                                block.getY() >= minY && block.getY() <= maxY &&
                                block.getZ() >= minZ && block.getZ() <= maxZ, false)).
                        thenAccept(tileEntities -> { // note: we have no guarantee of the ordering of these
                            for (BlockState tileEntity : tileEntities) {
                                if (tileEntity instanceof Container container) {
                                    final @Nullable TreasureInfo info = getTreasureInfo(container);

                                    if (info != null) {
                                        infoToNearLocations.computeIfAbsent(info, i ->
                                                new ConcurrentSkipListSet<>(locationComparator)).
                                            add(tileEntity.getLocation());
                                    }
                                }
                            }
                        })
                );
            }
        }

        // wait for all to complete, then sort by location
        return CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).
            thenApply(void_ -> infoToNearLocations).
            thenApply(map ->
                map.entrySet().stream().sorted((o1, o2) ->
                        locationComparator.compare(o1.getValue().getFirst(), o2.getValue().first())).
                    collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, (e1, e2) -> e1, LinkedHashMap::new)));
    }
}
