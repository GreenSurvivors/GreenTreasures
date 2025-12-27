package de.greensurvivors.greentreasure;

import com.github.benmanes.caffeine.cache.AsyncLoadingCache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.LoadingCache;
import com.github.f4b6a3.ulid.Ulid;
import com.github.f4b6a3.ulid.UlidFactory;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import org.bukkit.NamespacedKey;
import org.bukkit.inventory.InventoryHolder;
import org.bukkit.inventory.InventoryView;
import org.bukkit.persistence.PersistentDataHolder;
import org.bukkit.persistence.PersistentDataType;
import org.bukkit.plugin.Plugin;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.concurrent.CompletableFuture;

public class TreasureManager {
    private final @NotNull GreenTreasure plugin;
    private final @NotNull NamespacedKey idKey;
    private final @NotNull UlidFactory ulidFactory;
    // list of known treasures with its location and its information
    private final @NotNull AsyncLoadingCache<@NotNull Ulid, @Nullable TreasureInfo> treasures;

    public TreasureManager(final @NotNull GreenTreasure plugin) {
        this.plugin = plugin;
        this.idKey = new NamespacedKey(plugin, "id");
        this.treasures = Caffeine.newBuilder().buildAsync((id, executor) -> plugin.getDatabaseManager().loadTreasure(id));

        ulidFactory = UlidFactory.newMonotonicInstance(() -> Utils.RANDOM_GENERATOR.nextLong());
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
    public @NotNull CompletableFuture<@Nullable TreasureInfo> getTreasureInfo(final @NotNull Ulid treasureId) {
        return treasures.get(treasureId);
    }

    /**
     * fetches the treasureInfo NOW use {@link #getTreasureInfo(Ulid)} whenever possible
     * for some hot regions in this plugin we need the info now to deal with the current event, so the server can end the current tick.
     * But uncached {@link  TreasureInfo} is slow to fetch, since access the Database.
     * Well so what? just use the methods returning a {@link CompletableFuture} and call {@link CompletableFuture#join()}, right?
     * WRONG! The DatabaseHandler would catch the Data async and in order to resync it would call {@link  org.bukkit.scheduler.BukkitScheduler#runTask(Plugin, Runnable)},
     * completing the Future next tick.
     * And we would have successfully deadlocked ourselves, where the current Servertick would have to wait for the next one in order to complete...
     * <br></>
     * So I'm warning you again, just use this methode if you really have to and can't wait a tick or two!
     * That's why I didn't add any of the convenience methods like {@link #getTreasureInfo(PersistentDataHolder)} and {@link #getTreasureId(InventoryView)}
     **/
    @Contract(value = "null -> null")
    public @Nullable TreasureInfo getTreasureInfoUrgently(final @Nullable Ulid treasureId) {
        if (treasureId == null) {
            return null;
        } else {
            final @Nullable TreasureInfo treasureInfo = treasures.synchronous().getIfPresent(treasureId);

            if (treasureInfo == null) {
                final @Nullable TreasureInfo loadTreasureUrgently = plugin.getDatabaseManager().loadTreasureUrgently(treasureId);
                treasures.put(treasureId, CompletableFuture.completedFuture(loadTreasureUrgently));

                return loadTreasureUrgently;
            } else {
                return treasureInfo;
            }
        }
    }

    public @NotNull CompletableFuture<@Nullable TreasureInfo> getTreasureInfo(final @NotNull PersistentDataHolder persistentDataHolder) {
        final @Nullable Ulid treasureId = getTreasureId(persistentDataHolder);

        if (treasureId == null) {
            return CompletableFuture.completedFuture(null);
        } else {
            return getTreasureInfo(treasureId);
        }
    }

    public @NotNull CompletableFuture<@Nullable TreasureInfo> getTreasureInfo(final @NotNull InventoryView view) {
        final @Nullable Ulid treasureId = getTreasureId(view);

        if (treasureId == null) {
            return CompletableFuture.completedFuture(null);
        } else {
            return getTreasureInfo(treasureId);
        }
    }

    /**
     * closes all open inventories and clears the internal hashmaps
     * (to repopulate them with updated information)
     */
    public void clearTreasures() {
        LoadingCache<@NotNull Ulid, @Nullable TreasureInfo> syncView = treasures.synchronous();

        syncView.invalidateAll();
        syncView.cleanUp();
    }

    public void invalidateTreasure(final @NotNull Ulid treasureId) {
        plugin.getTreasureListener().closeInventories(treasureId);

        if (treasures.getIfPresent(treasureId) != null) {
            final LoadingCache<@NotNull Ulid, @Nullable TreasureInfo> synchronous = treasures.synchronous();
            synchronous.invalidate(treasureId);
            synchronous.cleanUp();
        }
    }
}
