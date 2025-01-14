package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermmissionManager;
import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
import org.bukkit.Bukkit;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryView;
import org.bukkit.permissions.Permissible;
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
    protected boolean checkPermission(@NotNull Permissible permissible) {
        return permissible.hasPermission(PermmissionManager.TREASURE_EDIT_CONTENT.get());
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
            Container container = plugin.getTreasureCommands().getContainer(sender);

            if (container != null) {
                if (sender instanceof Player player) {
                    final @Nullable TreasureInfo treasureInfo = plugin.getTreasureManager().getTreasureInfo(container);

                    if (treasureInfo != null) {
                        final @NotNull Component title = plugin.getMessageManager().getLang(LangPath.TREASURE_TITLE_EDIT,
                            Placeholder.component(PlaceHolderKey.NAME.getKey(), Utils.getDisplayName(container)));

                        final @NotNull Inventory inventory = Bukkit.createInventory(container, container.getInventory().getType(), title);
                        Utils.setContents(inventory, treasureInfo.itemLoot());
                        InventoryView view = player.openInventory(inventory);

                        if (view != null) {
                            plugin.getCommandInventoriesListener().addEditingTreasure(view, treasureInfo.treasureId());
                        } else {
                            plugin.getMessageManager().sendLang(sender, LangPath.ERROR_UNKNOWN);
                        }
                    } else {
                        plugin.getMessageManager().sendLang(sender, LangPath.ERROR_NOT_LOOKING_AT_TREASURE);
                    }
                } else {
                    plugin.getMessageManager().sendLang(sender, LangPath.ERROR_SENDER_NOT_PLAYER);
                }
            } else {
                plugin.getMessageManager().sendLang(sender, LangPath.ERROR_NOT_LOOKING_AT_CONTAINER);
            }
        } else {
            plugin.getMessageManager().sendLang(sender, LangPath.NO_PERMISSION);
        }

        return true;
    }

    public @NotNull List<@NotNull String> onTabComplete(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        if (args.length == 1) {
            return List.copyOf(getAliases());
        } else {
            return new ArrayList<>();
        }
    }
}
