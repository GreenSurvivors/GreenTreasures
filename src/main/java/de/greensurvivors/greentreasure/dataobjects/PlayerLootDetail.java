package de.greensurvivors.greentreasure.dataobjects;

import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Objects;

/**
 * all important information about a treasure a player may or may not have opened before
 */
public final class PlayerLootDetail {
    private final long lastChangedTimeStamp;
    private final long firstLootedTimeStamp;
    private final @Nullable List<@NotNull ItemStack> unLootedStuff;

    /**
     * @param lastChangedTimeStamp the unix time in milliseconds a player last has opened the treasure
     *                             This will set the firstLootedTimeStamp to lastChangedTimeStamp
     * @param unLootedStuff        the list of items a player has not looted yet.
     *                             Might be null if the player never opened the inventory
     */
    public PlayerLootDetail(long lastChangedTimeStamp, @Nullable List<@NotNull ItemStack> unLootedStuff) {
        this(null, lastChangedTimeStamp, unLootedStuff);
    }

    /**
     * @param firstLootedTimeStamp the unix time in milliseconds a player fist has opened the treasure
     * @param lastChangedTimeStamp the unix time in milliseconds a player last has opened the treasure
     * @param unLootedStuff        the list of items a player has not looted yet.
     *                             Might be null if the player never opened the inventory
     */
    public PlayerLootDetail(@Nullable Long firstLootedTimeStamp, long lastChangedTimeStamp, @Nullable List<@NotNull ItemStack> unLootedStuff) {
        this.firstLootedTimeStamp = Objects.requireNonNullElse(firstLootedTimeStamp, lastChangedTimeStamp);
        this.lastChangedTimeStamp = lastChangedTimeStamp;
        this.unLootedStuff = unLootedStuff;
    }

    public long lastChangedTimeStamp() {
        return lastChangedTimeStamp;
    }

    public @Nullable List<@NotNull ItemStack> unLootedStuff() {
        return unLootedStuff;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        var that = (PlayerLootDetail) obj;
        return this.firstLootedTimeStamp == that.firstLootedTimeStamp &&
            this.lastChangedTimeStamp == that.lastChangedTimeStamp &&
            Objects.equals(this.unLootedStuff, that.unLootedStuff);
    }

    @Override
    public int hashCode() {
        return Objects.hash(firstLootedTimeStamp, lastChangedTimeStamp, unLootedStuff);
    }

    @Override
    public String toString() {
        return "PlayerLootDetail[" +
            "firstLootedTimeStamp=" + firstLootedTimeStamp + ", " +
            "lastChangedTimeStamp=" + lastChangedTimeStamp + ", " +
            "unLootedStuff=" + unLootedStuff + ']';
    }

    public long firstLootedTimeStamp() {
        return firstLootedTimeStamp;
    }
}
