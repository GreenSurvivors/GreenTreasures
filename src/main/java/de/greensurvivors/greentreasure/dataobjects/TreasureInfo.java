package de.greensurvivors.greentreasure.dataobjects;

import com.github.f4b6a3.ulid.Ulid;
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
 * @param nonEmptyPermyriad      the chance a slot has to appear when opening the treasure for the fist time
 * @param isUnlimited     if the treasure is lootable an unlimited amount of times
 * @param isShared        if the inventory a treasure has is globally shared
 */
public record TreasureInfo(@NotNull Ulid treasureId, @NotNull List<@Nullable ItemStack> itemLoot,
                           @NotNull Duration timeUntilForget,
                           @Range(from = 0, to = 10000) int nonEmptyPermyriad,
                           boolean isUnlimited, boolean isShared,
                           @Nullable String rawFindFreshMessageOverride, @Nullable String rawFindLootedMessageOverride) {
}
