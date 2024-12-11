package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermmissionManager;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.JoinConfiguration;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
import org.bukkit.command.CommandSender;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class AboutSubCommand extends ASubCommand {

    public AboutSubCommand(@NotNull GreenTreasure plugin) {
        super(plugin);
    }

    @Override
    protected boolean checkPermission(@NotNull Permissible permissible) {
        return permissible.hasPermission(PermmissionManager.TREASURE_ABOUT.get());
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("about");
    }

    @Override
    @NotNull
    public Component getHelpText() {
        return null;
    }

    /**
     * get basic information about this plugin like its version
     * /gt about
     *
     * @param sender sender of this command
     */
    @Override
    public boolean onCommand(final @NotNull CommandSender sender, final @NotNull String @NotNull [] args) {
        if (checkPermission(sender)) {
            // collect all messages to send at once
            List<Component> components = new ArrayList<>();
            // header
            components.add(plugin.getMessageManager().getLang(LangPath.CMD_ABOUT_PLUGIN_HEADER));
            // version (update ?)
            components.add(plugin.getMessageManager().getLang(LangPath.CMD_ABOUT_PLUGIN_VERSION,
                Placeholder.unparsed(PlaceHolderKey.NUMBER.getKey(), plugin.getPluginMeta().getVersion())));

            //description
            components.add(plugin.getMessageManager().getLang(LangPath.CMD_ABOUT_PLUGIN_DESCRIPTION,
                Placeholder.unparsed(PlaceHolderKey.TEXT.getKey(), plugin.getPluginMeta().getDescription())));

            // send components
            sender.sendMessage(Component.join(JoinConfiguration.newlines(), components));
        } else {
            plugin.getMessageManager().sendLang(sender, LangPath.NO_PERMISSION);
        }

        return true;
    }

    @Override
    public @NotNull List<@NotNull String> onTabComplete(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        if (args.length == 1) {
            return List.copyOf(getAliases());
        } else {
            return new ArrayList<>();
        }
    }
}
