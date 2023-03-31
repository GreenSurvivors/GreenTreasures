package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.config.TreasureConfig;
import de.greensurvivors.greentreasure.language.Lang;
import de.greensurvivors.greentreasure.permission.Perm;
import org.bukkit.command.CommandSender;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static de.greensurvivors.greentreasure.comands.TreasureCommands.RELOAD;

public class ReloadSubCommand {
    private static ReloadSubCommand instance;

    public static ReloadSubCommand inst() {
        if (instance == null)
            instance = new ReloadSubCommand();
        return instance;
    }

    /**
     * reloads the plugin
     * /gt reload
     * @param commandSender sender of this command
     */
    protected void handleReload(CommandSender commandSender) {
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_RELOAD)) {
            TreasureConfig.inst().reloadMain();

            commandSender.sendMessage(Lang.build(Lang.RELOAD.get()));
        } else {
            commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
        }
    }

    /**
     * @param args The arguments passed to the command, including final
     *     partial argument to be completed
     * @return suggestion of arguments
     */
    protected List<String> handleTabCompleate(@NotNull String[] args){
        if (args.length == 1) {
            return Collections.singletonList(RELOAD);
        } else {
            return new ArrayList<>();
        }
    }
}
