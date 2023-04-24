package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.language.Lang;
import de.greensurvivors.greentreasure.permission.Perm;
import net.kyori.adventure.text.Component;
import org.bukkit.command.CommandSender;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static de.greensurvivors.greentreasure.comands.TreasureCommands.ABOUT;

public class AboutSubCommand {
    private static AboutSubCommand instance;

    public static AboutSubCommand inst() {
        if (instance == null)
            instance = new AboutSubCommand();
        return instance;
    }

    /**
     * get basic information about this plugin like its version
     * /gt about
     *
     * @param commandSender sender of this command
     */
    protected void handleAbout(CommandSender commandSender) {
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_ABOUT)) {
            // collect all messages to send at once
            List<Component> components = new ArrayList<>();
            // header
            components.add(Lang.build(Lang.PLUGIN_HEADER.get()));
            // version (update ?)
            components.add(Lang.build(Lang.PLUGIN_VERSION.get().replace(Lang.VALUE, GreenTreasure.inst().getDescription().getVersion())));

            //description
            components.add(Lang.build(Lang.PLUGIN_DESCRIPTION.get().replace(Lang.VALUE, GreenTreasure.inst().getDescription().getDescription())));

            // send components
            commandSender.sendMessage(Lang.join(components));
        } else {
            commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
        }
    }

    /**
     * @param args The arguments passed to the command, including final
     *             partial argument to be completed
     * @return suggestion of arguments
     */
    protected List<String> handleTabCompleate(@NotNull String[] args) {
        if (args.length == 1) {
            return Collections.singletonList(ABOUT);
        } else {
            return new ArrayList<>();
        }
    }
}
