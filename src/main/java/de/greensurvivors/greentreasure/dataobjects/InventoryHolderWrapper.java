package de.greensurvivors.greentreasure.dataobjects;

import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryHolder;
import org.bukkit.persistence.PersistentDataContainer;
import org.bukkit.persistence.PersistentDataHolder;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class InventoryHolderWrapper<T extends InventoryHolder & PersistentDataHolder> implements InventoryHolder, PersistentDataHolder {
    private final @NotNull T holder;
    private final boolean perCommand;
    private final @Nullable Long fistLootedTimeStamp;

    public InventoryHolderWrapper(final @NotNull T holder, boolean perCommand) {
        this.holder = holder;
        this.perCommand = perCommand;
        this.fistLootedTimeStamp = null;
    }

    public InventoryHolderWrapper(final @NotNull T holder, boolean perCommand, final @Nullable Long fistLootedTimeStamp) {
        this.holder = holder;
        this.perCommand = perCommand;
        this.fistLootedTimeStamp = fistLootedTimeStamp;
    }

    @Override
    public @NotNull PersistentDataContainer getPersistentDataContainer() {
        return holder.getPersistentDataContainer();
    }

    @Override
    public @NotNull Inventory getInventory() {
        return holder.getInventory();
    }

    public boolean isPerCommand() {
        return perCommand;
    }

    public @Nullable Long getFistLootedTimeStamp() {
        return fistLootedTimeStamp;
    }
}

