package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.comands.set.SetForgetSubCommand;
import de.greensurvivors.greentreasure.comands.set.SetRandomSubCommand;
import de.greensurvivors.greentreasure.comands.set.SetSharedSubCommand;
import de.greensurvivors.greentreasure.comands.set.SetUnlimitedSubCommand;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
import org.apache.commons.lang3.StringUtils;
import org.bukkit.command.CommandSender;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;

import java.util.*;

public class SetSubCommand extends ASubCommand {
    private final @NotNull Map<@NotNull String, @NotNull ASubCommand> subCommands = new HashMap<>();

    public SetSubCommand(@NotNull GreenTreasure plugin) {
        super(plugin);

        registerSubCommand(new SetForgetSubCommand(plugin));
        registerSubCommand(new SetSharedSubCommand(plugin));
        registerSubCommand(new SetRandomSubCommand(plugin));
        registerSubCommand(new SetUnlimitedSubCommand(plugin));
    }

    private void registerSubCommand (final @NotNull ASubCommand subCommand) {
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
        return Set.of("set");
    }

    @Override
    @NotNull
    public Component getHelpText() {
        return null;
    }

    @Override
    public boolean onCommand(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        if (args.length > 1) {
            for (Map.Entry<String, ASubCommand> entry : subCommands.entrySet()) {
                if (entry.getKey().equalsIgnoreCase(args[1])) {
                    if (entry.getValue().checkPermission(sender)) {
                        entry.getValue().onCommand(sender, args);
                    } else {
                        plugin.getMessageManager().sendLang(sender, LangPath.NO_PERMISSION);
                    }

                    return true;
                }
            }

            plugin.getMessageManager().sendLang(sender, LangPath.ARG_UNKNOWN,
                Placeholder.unparsed(PlaceHolderKey.TEXT.getKey(), args[1]));
            return false;
        } else {
            plugin.getMessageManager().sendLang(sender, LangPath.ERROR_NOT_ENOUGH_ARGS);
        }

        return true;
    }

    @Override
    public @NotNull List<@NotNull String> onTabComplete(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        if (args.length <= 2) {
            final Set<String> suggestions = new HashSet<>();

            for (Map.Entry<String, ASubCommand> entry : subCommands.entrySet()) {
                if (entry.getValue().checkPermission(sender)) {
                    suggestions.add(entry.getKey());
                }
            }

            return suggestions.stream().filter(s -> StringUtils.startsWithIgnoreCase(s, args[1])).toList();
        } else {
            for (Map.Entry<String, ASubCommand> entry : subCommands.entrySet()) {
                if (entry.getKey().equalsIgnoreCase(args[1]) && entry.getValue().checkPermission(sender)) {
                    return entry.getValue().onTabComplete(sender, args).stream().filter(s -> StringUtils.startsWithIgnoreCase(s, args[args.length - 1])).toList();
                }
            }

            return List.of();
        }
    }
}
