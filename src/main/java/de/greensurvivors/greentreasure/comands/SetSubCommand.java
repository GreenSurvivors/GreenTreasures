package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.comands.set.*;
import de.greensurvivors.greentreasure.language.LangKey;
import de.greensurvivors.greentreasure.language.PlaceHolder;
import net.kyori.adventure.text.Component;
import org.bukkit.command.CommandSender;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;

import java.util.*;

public class SetSubCommand extends ASubCommand {
    private final @NotNull Map<@NotNull String, @NotNull ASubCommand> subCommands = new HashMap<>();

    public SetSubCommand(final @NotNull GreenTreasure plugin) {
        super(plugin);

        registerSubCommand(new SetForgetSubCommand(plugin));
        registerSubCommand(new SetSharedSubCommand(plugin));
        registerSubCommand(new SetRandomSubCommand(plugin));
        registerSubCommand(new SetUnlimitedSubCommand(plugin));
        registerSubCommand(new SetFindFreshMessageOverrideSubCommand(plugin));
        registerSubCommand(new SetFindLootedMessageOverrideSubCommand(plugin));
    }

    private void registerSubCommand(final @NotNull ASubCommand subCommand) {
        for (@NotNull String alias : subCommand.getAliases()) {
            subCommands.put(alias, subCommand);
        }
    }

    @Override
    protected boolean checkPermission(final @NotNull Permissible permissible) {
        return subCommands.values().stream().anyMatch(sub -> sub.checkPermission(permissible));
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("set");
    }

    @Override
    public @NotNull Component getHelpText() {
        return null;
    }

    @Override
    public boolean onCommand(final @NotNull CommandSender sender, final @NotNull String @NotNull [] args) {
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

            plugin.getMessageManager().sendPrefixed(sender, LangKey.ARG_UNKNOWN
                .create(PlaceHolder.TEXT.string(args[1])));
            return false;
        } else {
            plugin.getMessageManager().sendPrefixed(sender, LangKey.CMD_ERROR_NOT_ENOUGH_ARGS);
        }

        return true;
    }

    @Override
    public @NotNull List<@NotNull String> onTabComplete(final @NotNull CommandSender sender, final @NotNull String @NotNull [] args) {
        if (args.length <= 2) {
            final Set<String> suggestions = new HashSet<>();

            for (Map.Entry<String, ASubCommand> entry : subCommands.entrySet()) {
                if (entry.getValue().checkPermission(sender)) {
                    suggestions.add(entry.getKey());
                }
            }

            return new ArrayList<>(suggestions);
        } else {
            for (Map.Entry<String, ASubCommand> entry : subCommands.entrySet()) {
                if (entry.getKey().equalsIgnoreCase(args[1]) && entry.getValue().checkPermission(sender)) {
                    return entry.getValue().onTabComplete(sender, args);
                }
            }

            return List.of();
        }
    }
}
