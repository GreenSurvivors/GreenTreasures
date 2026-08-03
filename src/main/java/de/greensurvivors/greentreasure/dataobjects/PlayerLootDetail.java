package de.greensurvivors.greentreasure.dataobjects;

import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.Unmodifiable;

import java.time.Instant;
import java.util.List;
import java.util.Objects;

/// all important information about a treasure a player may or may not have opened before
public final class PlayerLootDetail {
    private final @NotNull Instant lastChangedInstant;
    private final @NotNull Instant firstLootedInstant;
    private final @Nullable List<@NotNull ItemStack> unLootedStuff;

    /**
     * @param lastChangedInstant the unix time in milliseconds a player last has opened the treasure
     *                           This will set the firstLootedTimeStamp to lastChangedInstant
     * @param unLootedStuff      the list of items a player has not looted yet.
     *                           Might be null if the player never opened the inventory
     */
    public PlayerLootDetail(final @NotNull Instant lastChangedInstant, final @Nullable List<@NotNull ItemStack> unLootedStuff) {
        this(null, lastChangedInstant, unLootedStuff);
    }

    /**
     * @param firstLootedInstant the unix time in milliseconds a player fist has opened the treasure
     * @param lastChangedInstant the unix time in milliseconds a player last has opened the treasure
     * @param unLootedStuff      the list of items a player has not looted yet.
     *                           Might be null if the player never opened the inventory
     */
    public PlayerLootDetail(final @Nullable Instant firstLootedInstant, final @NotNull Instant lastChangedInstant,
                            final @Nullable List<@NotNull ItemStack> unLootedStuff) {
        this.firstLootedInstant = Objects.requireNonNullElse(firstLootedInstant, lastChangedInstant);
        this.lastChangedInstant = lastChangedInstant;
        this.unLootedStuff = unLootedStuff;
    }

    public @NotNull Instant lastChangedInstant() {
        return lastChangedInstant;
    }

    public @Nullable @Unmodifiable List<@NotNull ItemStack> unLootedStuff() {
        return unLootedStuff;
    }

    public boolean isEmpty() {
        return unLootedStuff() == null || unLootedStuff().isEmpty();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        final @NotNull PlayerLootDetail that = (PlayerLootDetail) obj;
        return this.firstLootedInstant == that.firstLootedInstant &&
            this.lastChangedInstant == that.lastChangedInstant &&
            Objects.equals(this.unLootedStuff, that.unLootedStuff);
    }

    @Override
    public int hashCode() {
        return Objects.hash(firstLootedInstant, lastChangedInstant, unLootedStuff);
    }

    @Override
    public String toString() {
        return "PlayerLootDetail[" +
            "firstLootedInstant=" + firstLootedInstant + ", " +
            "lastChangedInstant=" + lastChangedInstant + ", " +
            "unLootedStuff=" + unLootedStuff + ']';
    }

    public @NotNull Instant firstLootedInstant() {
        return firstLootedInstant;
    }
}
