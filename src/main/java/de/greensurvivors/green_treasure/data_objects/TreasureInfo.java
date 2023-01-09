package de.greensurvivors.green_treasure.data_objects;

import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;

import java.util.List;

/**
 * the information a treasure has
 * @param itemLoot the inventory with its loot items
 * @param timeUntilForget the time period the treasure has to be not opened until it restocks
 * @param slotChance the chance a slot has to appear when opening the treasure for the fist time
 * @param isUnlimited if the treasure is lootable an unlimited amount of times
 * @param isGlobal if the inventory a treasure has is globally shared
 * @param type the type a container has, important check when opening an inventory
 */
public record TreasureInfo(@NotNull List<ItemStack> itemLoot, long timeUntilForget, int slotChance, boolean isUnlimited, boolean isGlobal, @NotNull String type) {
}
