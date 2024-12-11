package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermmissionManager;
import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class ForgetAllSubCommand extends ASubCommand {

    public ForgetAllSubCommand(final @NotNull GreenTreasure plugin) {
        super(plugin);
    }

    @Override
    protected boolean checkPermission(@NotNull Permissible permissible) {
        return permissible.hasPermission(PermmissionManager.TREASURE_FORGET.get());
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("forget-all");
    }

    @Override
    public @NotNull Component getHelpText() {
        return null;
    }

    /**
     * the treasure forgets anybody has ever looted it.
     * /gt forget-all
     *
     * @param sender sender of this command
     */
    public boolean onCommand(final @NotNull CommandSender sender, final @NotNull String @NotNull [] args) {
        if (checkPermission(sender)) {
            Container container = plugin.getTreasureCommands().getContainer(sender);

            if (container != null) {
                final @Nullable TreasureInfo treasureInfo = plugin.getTreasureManager().getTreasure(container);

                if (treasureInfo != null) {
                    plugin.getDatabaseManager().forgetAll(treasureInfo.treasureId()).thenRun(() ->
                        plugin.getMessageManager().sendLang(sender, LangPath.CMD_FORGET_ALL_SUCCESS,
                            Placeholder.component(PlaceHolderKey.NAME.getKey(), Utils.getDisplayName(container))));
                } else {
                    plugin.getMessageManager().sendLang(sender, LangPath.ERROR_NOT_LOOKING_AT_TREASURE);
                }
            } else {
                plugin.getMessageManager().sendLang(sender, LangPath.ERROR_NOT_LOOKING_AT_CONTAINER);
            }
        } else {
            plugin.getMessageManager().sendLang(sender, LangPath.NO_PERMISSION);
        }

        return true;
    }

    /**
     * @param sender
     * @param args   The arguments passed to the command, including final
     *               partial argument to be completed
     * @return suggestion of arguments
     */
    public @NotNull List<@NotNull String> onTabComplete(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        if (args.length == 1) {
            return List.copyOf(getAliases());
        } else {
            return new ArrayList<>();
        }
    }
}
