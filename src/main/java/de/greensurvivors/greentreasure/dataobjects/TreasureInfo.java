package de.greensurvivors.greentreasure.dataobjects;

import com.github.f4b6a3.ulid.Ulid;
import de.greensurvivors.greentreasure.dataobjects.refreshInfo.ARefreshInfo;
import de.greensurvivors.greentreasure.dataobjects.refreshInfo.InstantUnlock;
import de.greensurvivors.greentreasure.dataobjects.refreshInfo.NoForget;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.Range;
import org.jetbrains.annotations.Unmodifiable;

import java.util.List;
import java.util.Objects;

public final class TreasureInfo {
    private final @NotNull Ulid treasureId;
    private final @NotNull @Unmodifiable List<@Nullable ItemStack> itemLoot;
    private final @NotNull ARefreshInfo refreshInfo;
    private final @Range(from = 0, to = 10000) int nonEmptyPermyriad;
    private final boolean isUnlimited;
    private final boolean isShared;
    private final @Nullable String rawFindFreshMessageOverride;
    private final @Nullable String rawFindLootedMessageOverride;

    /**
     * @param itemLoot          the inventory with its loot items
     //* @param timeUntilForget   the time period the treasure has to be not opened until it restocks
     * @param nonEmptyPermyriad the chance a slot has to appear when opening the treasure for the fist time
     * @param isUnlimited       if the treasure is lootable an unlimited amount of times
     * @param isShared          if the inventory a treasure has is globally shared
     */
    public TreasureInfo(final @NotNull Ulid treasureId, final @NotNull List<@Nullable ItemStack> itemLoot,
                        @NotNull ARefreshInfo refreshInfo,
                        @Range(from = 0, to = 10000) int nonEmptyPermyriad,
                        boolean isUnlimited, boolean isShared,
                        @Nullable String rawFindFreshMessageOverride, @Nullable String rawFindLootedMessageOverride) {
        this.treasureId = treasureId;
        this.itemLoot = itemLoot;
        this.refreshInfo = refreshInfo;
        this.nonEmptyPermyriad = nonEmptyPermyriad;
        this.isUnlimited = isUnlimited;
        this.isShared = isShared;
        this.rawFindFreshMessageOverride = rawFindFreshMessageOverride;
        this.rawFindLootedMessageOverride = rawFindLootedMessageOverride;
    }

    public @NotNull Ulid treasureId() {
        return treasureId;
    }

    public @NotNull @Unmodifiable List<@Nullable ItemStack> itemLoot() {
        return itemLoot;
    }

    public boolean doesForget () {
        return !(refreshInfo instanceof NoForget);
    }

    public boolean isUnlocked() {
        if (refreshInfo instanceof InstantUnlock timedUnlock) {
            return timedUnlock.canOpenFresh(null);
        } else {
            return true;
        }
    }

    public @NotNull ARefreshInfo getRefreshInfo() {
        return refreshInfo;
    }

    public @Range(from = 0, to = 10000) int nonEmptyPermyriad() {
        return nonEmptyPermyriad;
    }

    public boolean isUnlimited() {
        return isUnlimited;
    }

    public boolean isShared() {
        return isShared;
    }

    public @Nullable String rawFindFreshMessageOverride() {
        return rawFindFreshMessageOverride;
    }

    public @Nullable String rawFindLootedMessageOverride() {
        return rawFindLootedMessageOverride;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        final @NotNull TreasureInfo that = (TreasureInfo) obj;

        return Objects.equals(this.treasureId, that.treasureId) &&
            Objects.equals(this.itemLoot, that.itemLoot) &&
            Objects.equals(this.refreshInfo, that.refreshInfo) &&
            this.nonEmptyPermyriad == that.nonEmptyPermyriad &&
            this.isUnlimited == that.isUnlimited &&
            this.isShared == that.isShared &&
            Objects.equals(this.rawFindFreshMessageOverride, that.rawFindFreshMessageOverride) &&
            Objects.equals(this.rawFindLootedMessageOverride, that.rawFindLootedMessageOverride);
    }

    @Override
    public int hashCode() {
        return Objects.hash(treasureId, itemLoot, refreshInfo, nonEmptyPermyriad, isUnlimited, isShared, rawFindFreshMessageOverride, rawFindLootedMessageOverride);
    }

    @Override
    public String toString() {
        return "TreasureInfo[" +
            "treasureId=" + treasureId + ", " +
            "itemLoot=" + itemLoot + ", " +
            "forgetContainer=" + refreshInfo + ", " +
            "nonEmptyPermyriad=" + nonEmptyPermyriad + ", " +
            "isUnlimited=" + isUnlimited + ", " +
            "isShared=" + isShared + ", " +
            "rawFindFreshMessageOverride=" + rawFindFreshMessageOverride + ", " +
            "rawFindLootedMessageOverride=" + rawFindLootedMessageOverride + ']';
    }
}
