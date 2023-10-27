package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.language.Lang;
import de.greensurvivors.greentreasure.listener.CommandInventoriesListener;
import de.greensurvivors.greentreasure.listener.TreasureListener;
import de.greensurvivors.greentreasure.permission.Perm;
import net.kyori.adventure.text.Component;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static de.greensurvivors.greentreasure.comands.TreasureCommands.EDIT;

public class EditSubCommand {
    private static EditSubCommand instance;

    public static EditSubCommand inst() {
        if (instance == null)
            instance = new EditSubCommand();
        return instance;
    }

    /**
     * lets the commandSender edit a treasure inventory
     * note: two 2 players editing the same inventory at the same time is NOT supported right now
     * /gt edit
     *
     * @param commandSender sender of this command
     */
    protected void handleEdit(CommandSender commandSender) {
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_EDIT)) {
            Container container = TreasureCommands.getContainer(commandSender);

            if (container != null) {
                if (commandSender instanceof Player player) {
                    Component eTitle = Lang.build("EDIT TREASURE");
                    final Location location = Utils.cleanLocation(container.getLocation());
                    TreasureInfo treasureInfo = TreasureListener.inst().getTreasure(location);

                    if (treasureInfo != null) {
                        Inventory nowEditing = Bukkit.createInventory(null, container.getInventory().getType(), eTitle);
                        // clone every item stack and put it into the new inventory
                        nowEditing.setContents(treasureInfo.itemLoot().stream().map(s -> s == null ? null : s.clone()).toArray(ItemStack[]::new));

                        CommandInventoriesListener.inst().addEditingTreasure(player.openInventory(nowEditing), location);
                    } else {
                        commandSender.sendMessage(Lang.build(Lang.NO_TREASURE.get()));
                    }
                } else {
                    commandSender.sendMessage(Lang.build(Lang.NO_PLAYER.get()));
                }
            } else {
                commandSender.sendMessage(Lang.build(Lang.NOT_LOOKINGAT_CONTAINER.get()));
            }
        } else {
            commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
        }
    }

    /**
     * @param args The arguments passed to the command, including final
     *             partial argument to be completed
     * @return suggestion of arguments
     */
    protected List<String> handleTabComplete(@NotNull String[] args) {
        if (args.length == 1) {
            return Collections.singletonList(EDIT);
        } else {
            return new ArrayList<>();
        }
    }
}
