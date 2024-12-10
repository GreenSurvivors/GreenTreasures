package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.language.LangPath;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.JoinConfiguration;
import org.apache.commons.lang3.StringUtils;
import org.bukkit.command.CommandSender;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class HelpSubCommand extends ASubCommand {

    public HelpSubCommand(@NotNull GreenTreasure plugin) {
        super(plugin);
    }

    @Override
    protected boolean checkPermission(@NotNull Permissible permissible) {
        return plugin.getTreasureCommands().getSubCommands().values().stream().anyMatch(sub -> !(sub instanceof HelpSubCommand) && sub.checkPermission(permissible));
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("?", "help");
    }

    @Override
    public @NotNull Component getHelpText() {
        return null;
    }

    /**
     * get a help-text about this plugin and it's commands
     *
     * @param sender sender of this command
     * @param args   given arguments
     */
    public boolean onCommand(@NotNull CommandSender sender, String @NotNull [] args) {
        if (checkPermission(sender)) {
            // collect all messages to send at once
            ArrayList<Component> components = new ArrayList<>();
            //wiki
            components.add(plugin.getMessageManager().getLang(LangPath.CMD_HELP_WIKI));

            //todo explain commands

            // send components
            plugin.getMessageManager().sendMessage(sender, Component.join(JoinConfiguration.newlines(), components));
        } else {
            plugin.getMessageManager().sendLang(sender, LangPath.NO_PERMISSION);
        }

        return true;
    }

    @Override
    public @NotNull List<@NotNull String> onTabComplete(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        if (args.length == 2) {
            final List<String> suggestions = new ArrayList<>();

            for (Map.Entry<String, ASubCommand> entry : plugin.getTreasureCommands().getSubCommands().entrySet()) {
                if (entry.getValue().checkPermission(sender) && StringUtils.startsWithIgnoreCase(entry.getKey(), args[1])) {
                    suggestions.add(entry.getKey());
                }
            }

            return suggestions;
        }

        return new ArrayList<>();
    }
}
