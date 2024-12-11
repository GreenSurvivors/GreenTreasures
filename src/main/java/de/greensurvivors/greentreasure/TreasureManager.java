package de.greensurvivors.greentreasure;

import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.LoadingCache;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import org.bukkit.NamespacedKey;
import org.bukkit.persistence.PersistentDataHolder;
import org.bukkit.persistence.PersistentDataType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

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

    public void setTreasureId(final @NotNull PersistentDataHolder dataHolder, final @NotNull String treasureId) {
        dataHolder.getPersistentDataContainer().set(idKey, PersistentDataType.STRING, treasureId);
    }

    public void removeTreasureId(final @NotNull PersistentDataHolder dataHolder) {
        dataHolder.getPersistentDataContainer().remove(idKey);
    }

    /**
     * get a treasure given its uuid
     *
     * @return all known information about the treasure itself
     */
    public @Nullable TreasureInfo getTreasure(final @NotNull String treasureId) {
        return treasures.get(treasureId);
    }

    public @Nullable TreasureInfo getTreasure(final @NotNull PersistentDataHolder persistentDataHolder) {
        final @Nullable String treasureId = getTreasureId(persistentDataHolder);

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
}
