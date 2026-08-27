package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermissionManager;
import de.greensurvivors.greentreasure.language.LangKey;
import de.greensurvivors.greentreasure.language.PlaceHolder;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.JoinConfiguration;
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
        return permissible.hasPermission(PermissionManager.TREASURE_ABOUT.get());
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("about");
    }

    @Override
    public @NotNull Component getHelpText() {
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
            components.add(LangKey.CMD_ABOUT_PLUGIN_HEADER.create());
            // version (update ?)
            components.add(LangKey.CMD_ABOUT_PLUGIN_VERSION.create(
                PlaceHolder.NUMBER.string(plugin.getPluginMeta().getVersion())));

            //description
            components.add(LangKey.CMD_ABOUT_PLUGIN_DESCRIPTION.create(
                PlaceHolder.TEXT.string(plugin.getPluginMeta().getDescription())));

            // send components
            sender.sendMessage(Component.join(JoinConfiguration.newlines(), components));
        } else {
            plugin.getMessageManager().sendPrefixed(sender, LangKey.NO_PERMISSION);
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
