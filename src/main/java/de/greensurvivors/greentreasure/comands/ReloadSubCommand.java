package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermmissionManager;
import de.greensurvivors.greentreasure.language.LangPath;
import net.kyori.adventure.text.Component;
import org.bukkit.command.CommandSender;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class ReloadSubCommand extends ASubCommand {

    public ReloadSubCommand(@NotNull GreenTreasure plugin) {
        super(plugin);
    }

    @Override
    protected boolean checkPermission(@NotNull Permissible permissible) {
        return permissible.hasPermission(PermmissionManager.TREASURE_RELOAD.get());
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("reload");
    }

    @Override
    @NotNull
    public Component getHelpText() {
        return null;
    }

    /**
     * reloads the plugin
     * /gt reload
     *
     * @param sender sender of this command
     */
    public boolean onCommand(final @NotNull CommandSender sender, final @NotNull String @NotNull [] args) {
        if (checkPermission(sender)) {
            plugin.getConfigHandler().reloadMain();

            plugin.getMessageManager().sendLang(sender, LangPath.CMD_RELOAD_SUCCESS);
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
