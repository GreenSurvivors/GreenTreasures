package de.greensurvivors.greentreasure.legacy.data;

import org.bukkit.event.inventory.InventoryType;
import org.bukkit.inventory.ItemStack;
import org.bukkit.util.Vector;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.configurate.objectmapping.ConfigSerializable;
import org.spongepowered.configurate.objectmapping.meta.Required;

import java.util.Map;

@ConfigSerializable
public record SingleBlockInventory(@Required @NotNull String world,
                                   @Required @NotNull Vector coords,
                                   @Required @NotNull Map<String, ItemStack> contents,
                                   @Required int size,
                                   @Nullable InventoryType type) implements IBlockInventory {
}
