package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.GreenTreasure;
import net.kyori.adventure.text.Component;
import org.bukkit.command.CommandSender;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Set;

public abstract class ASubCommand {
    protected final @NotNull GreenTreasure plugin;

    public ASubCommand(@NotNull GreenTreasure plugin) {
        this.plugin = plugin;
    }

    /**
     * This checks, if the permissible has permission to see
     * (like in tab-completion/help) and use the subcommand.
     * <p>
     * Note: The subcommand can perform additional checks once the
     * {@link #onCommand(CommandSender, String[])} method was called and therefor fail at this point.
     * Example: The method checks if the command sender is a player and owner of a sign.
     *
     * @param permissible to check the permission for
     * @return true if the permissible can see/use the subcommand.
     */
    abstract protected boolean checkPermission(@NotNull Permissible permissible);

    /**
     * get all the names a subcommand can get called.
     * If subcommand1 has alias "subcommand1-alias1" and "subcommand1-alias2"
     * and the command /command registers them they can be called via
     * <p>/command subcommand1-alias1
     * <p>/command subcommand1-alias2
     * If two or more subcommands register the same alias the outcome is unknown.
     * Please be aware of that. It really shouldn't be all that hard to not have
     * two subcommands called the same in one single plugins command.
     * <p> Every alias has to registered as lower case, so the casing when called
     * doesn't matter.
     */
    public abstract @NotNull Set<@NotNull String> getAliases();

    /**
     * get information how the command works, like
     * when used in a /help command wink wink
     */
    public abstract @NotNull Component getHelpText();


    /**
     * Executes the given command, returning its success.
     * <br> If false is returned, then the "usage" plugin.yml entry for this command
     * (if defined) will be sent to the player.
     *
     * @param sender Source of the command
     * @param args   Passed command arguments.
     *               Please Note: The subcommand will ALWAYS be the first argument aka arg[0].
     * @return true if a valid command, otherwise false
     */
    public abstract boolean onCommand(final @NotNull CommandSender sender, final @NotNull String @NotNull [] args);

    /**
     * @param args The arguments passed to the command, including final
     *             partial argument to be completed
     * @return suggestion of arguments
     */
    public abstract @NotNull List<@NotNull String> onTabComplete(@NotNull CommandSender sender, @NotNull String @NotNull [] args);
}
