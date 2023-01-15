package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.config.TreasureConfig;
import de.greensurvivors.greentreasure.language.Lang;
import de.greensurvivors.greentreasure.permission.Perm;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static de.greensurvivors.greentreasure.comands.TreasureCommands.FORGET_ALL;

public class ForgetAllSubCommand {
    private static ForgetAllSubCommand instance;

    public static ForgetAllSubCommand inst() {
        if (instance == null)
            instance = new ForgetAllSubCommand();
        return instance;
    }

    /**
     * the treasure forgets anybody has ever looted it.
     * /gt forget-all
     * @param commandSender sender of this command
     */
    protected void handleForgetAll(CommandSender commandSender){
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_FORGET)) {
            Container container = TreasureCommands.getContainer(commandSender);

            if (container != null){
                TreasureConfig.inst().forgetAllAsync(container.getBlock().getLocation());

                commandSender.sendMessage(Lang.build(Lang.FORGET_ALL.get()));
            } else {
                commandSender.sendMessage(Lang.build(Lang.NOT_LOOKINGAT_CONTAINER.get()));
            }
        } else {
            commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
        }
    }

    /**
     * @param args The arguments passed to the command, including final
     *     partial argument to be completed
     * @return suggestion of arguments
     */
    protected List<String> handleTapCompleate(@NotNull String[] args){
        if (args.length == 1) {
            return Collections.singletonList(FORGET_ALL);
        } else {
            return new ArrayList<>();
        }
    }
}
