package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.language.Lang;
import de.greensurvivors.greentreasure.permission.Perm;
import net.kyori.adventure.text.Component;
import org.bukkit.command.CommandSender;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

import static de.greensurvivors.greentreasure.comands.TreasureCommands.*;

public class HelpSubCommand {
    private static HelpSubCommand instance;

    public static HelpSubCommand inst() {
        if (instance == null)
            instance = new HelpSubCommand();
        return instance;
    }

    /**
     * get a help-text about this plugin and it's commands
     *
     * @param commandSender sender of this command
     * @param args          given arguments
     */
    protected void handleHelp(CommandSender commandSender, String[] args) {
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_HELP)) {
            // collect all messages to send at once
            ArrayList<Component> components = new ArrayList<>();
            //wiki
            components.add(Lang.build(Lang.HELP_WIKI.get()));

            //todo explain commands

            // send components
            commandSender.sendMessage(Lang.join(components));
        } else {
            commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
        }
    }

    protected List<String> handleTabComplete(@NotNull CommandSender commandSender, @NotNull String[] args) {
        switch (args.length) {
            case 1 -> {
                return List.of(HELP_LONG, HELP_SHORT);
            }
            case 2 -> {
                if (args[0].equalsIgnoreCase(HELP_SHORT) || args[0].equalsIgnoreCase(HELP_LONG)) {
                    final List<String> suggestions = new ArrayList<>();

                    if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_CREATE)) {
                        suggestions.add(CREATE);
                    }
                    if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_DELETE)) {
                        suggestions.add(DELETE);
                        suggestions.add(REMOVE);
                    }
                    if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_EDIT)) {
                        suggestions.add(EDIT);
                    }
                    if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_LIST_PLAYERS, Perm.TREASURE_LIST_TREASURES, Perm.TREASURE_LIST_WHO)) {
                        suggestions.add(LIST);
                    }
                    if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_PEEK)) {
                        suggestions.add(PEEK);
                    }
                    if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_FORGET)) {
                        suggestions.add(FORGET);
                        suggestions.add(FORGET_ALL);
                    }
                    if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_FORGET, Perm.TREASURE_SET_UNLIMITED, Perm.TREASURE_SET_RANDOM, Perm.TREASURE_SET_GLOBAL)) {
                        suggestions.add(SET);
                    }
                    if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_RANDOM)) {
                        suggestions.add(RANDOM);
                    }
                    if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_UNLIMITED)) {
                        suggestions.add(UNLIMITED);
                    }
                    if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_GLOBAL)) {
                        suggestions.add(GLOBAL);
                        suggestions.add(SHARED);
                    }
                    if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_FORGET)) {
                        suggestions.add(FORGET);
                    }
                    if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_ABOUT)) {
                        suggestions.add(ABOUT);
                    }
                    if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_RELOAD)) {
                        suggestions.add(RELOAD);
                    }
                    if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_HELP)) {
                        suggestions.add(HELP_LONG);
                    }
                    return suggestions;
                }
            }
        }

        return new ArrayList<>();
    }
}
