package de.greensurvivors.greentreasure.dataobjects;

import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.Range;

import java.time.Duration;
import java.util.List;

/**
 * the information a treasure has
 *
 * @param itemLoot        the inventory with its loot items
 * @param timeUntilForget the time period the treasure has to be not opened until it restocks
 * @param slotChance      the chance a slot has to appear when opening the treasure for the fist time
 * @param isUnlimited     if the treasure is lootable an unlimited amount of times
 * @param isShared        if the inventory a treasure has is globally shared
 */
public record TreasureInfo(@NotNull String treasureId, @NotNull List<@Nullable ItemStack> itemLoot,
                           @NotNull Duration timeUntilForget, @Range(from = 0, to = 10000) int slotChance, boolean isUnlimited, boolean isShared) {
}
