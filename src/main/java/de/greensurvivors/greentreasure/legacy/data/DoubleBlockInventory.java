package de.greensurvivors.greentreasure.legacy.data;

import org.jetbrains.annotations.NotNull;
import org.spongepowered.configurate.objectmapping.ConfigSerializable;
import org.spongepowered.configurate.objectmapping.meta.Required;

@ConfigSerializable
public record DoubleBlockInventory(@Required @NotNull SingleBlockInventory leftSide,
                                   @Required @NotNull SingleBlockInventory rightSide) implements IBlockInventory {
}
