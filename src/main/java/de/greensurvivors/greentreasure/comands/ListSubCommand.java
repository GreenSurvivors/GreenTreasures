package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.comands.list.ListNearbyCommand;
import de.greensurvivors.greentreasure.comands.list.ListPlayerSubCommand;
import de.greensurvivors.greentreasure.comands.list.ListTreasuresSubCommand;
import de.greensurvivors.greentreasure.comands.list.ListWhoSubCommand;
import de.greensurvivors.greentreasure.language.LangKey;
import de.greensurvivors.greentreasure.language.PlaceHolder;
import net.kyori.adventure.text.Component;
import org.apache.commons.lang3.Strings;
import org.bukkit.command.CommandSender;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;

import java.util.*;

public class ListSubCommand extends ASubCommand {
    public static final byte ENTRIES_PER_PAGE = 10;
    private final @NotNull Map<@NotNull String, @NotNull ASubCommand> subCommands = new HashMap<>();

    public ListSubCommand(@NotNull GreenTreasure plugin) {
        super(plugin);

        registerSubCommand(new ListPlayerSubCommand(plugin));
        registerSubCommand(new ListTreasuresSubCommand(plugin));
        registerSubCommand(new ListWhoSubCommand(plugin));
        registerSubCommand(new ListNearbyCommand(plugin));
    }

    private void registerSubCommand(final @NotNull ASubCommand subCommand) {
        for (@NotNull String alias : subCommand.getAliases()) {
            subCommands.put(alias, subCommand);
        }
    }

    @Override
    protected boolean checkPermission(@NotNull Permissible permissible) {
        return subCommands.values().stream().anyMatch(sub -> sub.checkPermission(permissible));
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("list");
    }

    @Override
    public @NotNull Component getHelpText() {
        return null;
    }

    /**
     * list all treasures on this server,
     * or if a player was given lists all looted treasures
     * /gt list(0) treasures(1) [num - optional](2) - list all treasures
     * /gt list(0) player(1) playerName(2) [num- optional](3) - detail of all treasures of a player
     * /gt list(0) player(1) uuid(2) [num - optional](3) - detail of all treasures of a player
     * /gt list(0) who(1) [num - optional](2) (while looking at a container) - detail of all players of a treasure
     *
     * @param sender sender of this command
     */
    @Override
    public boolean onCommand(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        if (args.length > 1) {
            for (Map.Entry<String, ASubCommand> entry : subCommands.entrySet()) {
                if (entry.getKey().equalsIgnoreCase(args[1])) {
                    if (entry.getValue().checkPermission(sender)) {
                        return entry.getValue().onCommand(sender, args);
                    } else {
                        plugin.getMessageManager().sendPrefixed(sender, LangKey.NO_PERMISSION);
                    }

                    return true;
                }
            }

            plugin.getMessageManager().sendPrefixed(sender, LangKey.ARG_UNKNOWN.create(
                PlaceHolder.TEXT.string(args[1])));
            return false;
        } else {
            plugin.getMessageManager().sendPrefixed(sender, LangKey.CMD_ERROR_NOT_ENOUGH_ARGS);
        }

        return true;
    }

    /**
     * @param args The arguments passed to the command, including final
     *             partial argument to be completed
     *             /gt list(0) treasures(1) [num - optional](2) - list all treasures
     *             /gt list(0) player(1) playerName(2) [num- optional](3) - detail of all treasures of a player
     *             /gt list(0) player(1) uuid(2) [num - optional](3) - detail of all treasures of a player
     *             /gt list(0) who(1) [num - optional](2) (while looking at a container) - detail of all players of a treasure
     * @return suggestion of arguments, filtert by what's already written
     */
    @Override
    public @NotNull List<@NotNull String> onTabComplete(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        if (args.length <= 2) {
            final Set<String> suggestions = new HashSet<>();

            for (Map.Entry<String, ASubCommand> entry : subCommands.entrySet()) {
                if (entry.getValue().checkPermission(sender)) {
                    suggestions.add(entry.getKey());
                }
            }

            return suggestions.stream().filter(s -> Strings.CI.startsWith(s, args[1])).toList();
        } else {
            for (Map.Entry<String, ASubCommand> entry : subCommands.entrySet()) {
                if (entry.getKey().equalsIgnoreCase(args[1]) && entry.getValue().checkPermission(sender)) {
                    return entry.getValue().onTabComplete(sender, args).stream().filter(s -> Strings.CI.startsWith(s, args[args.length - 1])).toList();
                }
            }

            return List.of();
        }
    }
}
