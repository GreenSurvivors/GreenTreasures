package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermissionManager;
import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.dataobjects.InventoryHolderWrapper;
import de.greensurvivors.greentreasure.language.LangKey;
import de.greensurvivors.greentreasure.language.PlaceHolder;
import net.kyori.adventure.text.Component;
import org.bukkit.Bukkit;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.InventoryType;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryHolder;
import org.bukkit.inventory.InventoryView;
import org.bukkit.permissions.Permissible;
import org.bukkit.persistence.PersistentDataHolder;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class EditSubCommand extends ASubCommand {

    public EditSubCommand(final @NotNull GreenTreasure plugin) {
        super(plugin);
    }

    @Override
    protected boolean checkPermission(final @NotNull Permissible permissible) {
        return permissible.hasPermission(PermissionManager.TREASURE_EDIT_CONTENT.get());
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("edit", "content");
    }

    @Override
    public @NotNull Component getHelpText() {
        return null;
    }

    /**
     * lets the commandSender edit a treasure inventory
     * note: two 2 players editing the same inventory at the same time is NOT supported right now
     * /gt edit
     *
     * @param sender sender of this command
     */
    public boolean onCommand(final @NotNull CommandSender sender, final @NotNull String @NotNull [] args) {
        if (checkPermission(sender)) {
            final @Nullable Container container = plugin.getMainCommand().getContainer(sender);

            if (container != null) {
                if (sender instanceof Player player) {
                    plugin.getTreasureManager().getTreasureInfo(container).thenAccept(treasureInfo -> {
                        if (treasureInfo != null) {
                            final @NotNull Component title = LangKey.TREASURE_TITLE_EDIT.create(
                                PlaceHolder.TREASURE_ID.component(Utils.getDisplayName(container)));

                            final @NotNull InventoryHolderWrapper<?> wrapper = new InventoryHolderWrapper<>((InventoryHolder & PersistentDataHolder) Utils.getTreasureHolder(container), true);

                            final @NotNull Inventory inventory;
                            if (container.getInventory().getType() == InventoryType.CHEST) {
                                inventory = Bukkit.createInventory(wrapper, container.getInventory().getSize(), title);
                            } else {
                                inventory = Bukkit.createInventory(wrapper, container.getInventory().getType(), title);
                            }

                            Utils.setContents(inventory, treasureInfo.itemLoot());
                            final @Nullable InventoryView view = player.openInventory(inventory);

                            if (view != null) {
                                plugin.getCommandInventoriesListener().addEditingTreasure(view, treasureInfo.treasureId());
                            } else {
                                plugin.getMessageManager().sendPrefixed(sender, LangKey.ERROR_UNKNOWN);
                                plugin.getComponentLogger().warn("Could not open Inventory {} for Player {}", inventory, player);
                            }
                        } else {
                            plugin.getMessageManager().sendPrefixed(sender, LangKey.ERROR_NOT_LOOKING_AT_TREASURE);
                        }
                    });
                } else {
                    plugin.getMessageManager().sendPrefixed(sender, LangKey.ERROR_SENDER_NOT_PLAYER);
                }
            } else {
                plugin.getMessageManager().sendPrefixed(sender, LangKey.ERROR_NOT_LOOKING_AT_CONTAINER);
            }
        } else {
            plugin.getMessageManager().sendPrefixed(sender, LangKey.NO_PERMISSION);
        }

        return true;
    }

    public @NotNull List<@NotNull String> onTabComplete(final @NotNull CommandSender sender, final @NotNull String @NotNull [] args) {
        if (args.length == 1) {
            return List.copyOf(getAliases());
        } else {
            return new ArrayList<>();
        }
    }
}
