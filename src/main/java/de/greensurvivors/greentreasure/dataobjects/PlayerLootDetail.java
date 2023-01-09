package de.greensurvivors.greentreasure.dataobjects;

import org.bukkit.inventory.ItemStack;

import javax.annotation.Nullable;
import java.util.List;

/**
 * all important information about a treasure a player may or may not have opened before
 * @param lastLootedTimeStamp the unix time in milliseconds a player last has opened the treasure
 * @param unLootedStuff the list of items a player has not looted yet.
 *                      Might be null if the player never opened the inventory
 */
public record PlayerLootDetail(long lastLootedTimeStamp, @Nullable List<ItemStack> unLootedStuff) {
}
