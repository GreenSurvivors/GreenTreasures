package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.language.Lang;
import de.greensurvivors.greentreasure.permission.Perm;
import org.bukkit.block.Block;
import org.bukkit.block.Container;
import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;
import org.bukkit.command.TabCompleter;
import org.bukkit.entity.LivingEntity;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class TreasureCommands implements CommandExecutor, TabCompleter {
    public static final String
            CMD = "greentresure",
            CREATE = "create",
            DELETE = "delete", REMOVE = "remove",
            ABOUT = "about",
            HELP_SHORT = "?", HELP_LONG = "help",
            RELOAD = "reload",
            FORGET = "forget",
            FORGET_ALL = "forget-all",
            EDIT = "edit",
            LIST = "list",
            PEEK = "peek",
            SET = "set",
            RANDOM = "random",
            UNLIMITED = "unlimited",
            GLOBAL = "global", SHARED = "shared";

    private static TreasureCommands instance;

    public static TreasureCommands inst() {
        if (instance == null)
            instance = new TreasureCommands();
        return instance;
    }

    /**
     * Get container a living entity is looking at
     *
     * @param commandSender sender of this command looking at a container
     * @return container or null if the living entity is not looking at a container
     */
    protected static Container getContainer(CommandSender commandSender) {
        if (commandSender instanceof LivingEntity livingEntity) {
            // is the player looking at a container?
            Block block = livingEntity.getTargetBlockExact(5);

            if (block != null && block.getState() instanceof Container container) {
                return container;
            } else {
                return null;
            }
        } else {
            commandSender.sendMessage(Lang.build(Lang.NO_PLAYER.get()));
            return null;
        }
    }

    /**
     * @param commandSender Source of the command
     * @param command       Command which was executed
     * @param label         Alias of the command which was used
     * @param args          Passed command arguments
     * @return always true, except if the subcommand was unknown, because most subcommands have their own handling of errors
     */
    @Override
    public boolean onCommand(@NotNull CommandSender commandSender, @NotNull Command command, @NotNull String label, @NotNull String[] args) {
        if (args.length > 0) {
            switch (args[0].toLowerCase()) {
                case CREATE -> CreateSubCommand.inst().handleCreate(commandSender);
                case DELETE, REMOVE -> DeleteSubCommand.inst().handleDelete(commandSender);
                case EDIT -> EditSubCommand.inst().handleEdit(commandSender);
                case LIST -> ListSubCommand.inst().handleList(commandSender, args);
                case PEEK -> PeekSubCommand.inst().handlePeek(commandSender, args);
                case FORGET -> ForgetSubCommand.inst().handleForget(commandSender, args);
                case FORGET_ALL -> ForgetAllSubCommand.inst().handleForgetAll(commandSender);
                case SET -> {
                    if (args.length > 1) {
                        switch (args[1].toLowerCase()) {
                            case RANDOM -> SetRandomSubCommand.inst().handleSetRandom(commandSender, args);
                            case UNLIMITED -> SetUnlimitedSubCommand.inst().handleSetUnlimited(commandSender, args);
                            case GLOBAL, SHARED -> SetGlobalSubCommand.inst().handleSetGlobal(commandSender, args);
                            case FORGET -> SetForgetSubCommand.inst().handleSetForget(commandSender, args);
                            default -> {
                                return false;
                            }
                        }
                    } else {
                        commandSender.sendMessage(Lang.build(Lang.NOT_ENOUGH_ARGS.get()));
                    }
                }
                case ABOUT -> AboutSubCommand.inst().handleAbout(commandSender);
                case RELOAD -> ReloadSubCommand.inst().handleReload(commandSender);
                case HELP_SHORT, HELP_LONG -> HelpSubCommand.inst().handleHelp(commandSender, args);
                default -> {
                    return false;
                }
            }
        } else {
            // plugin
            AboutSubCommand.inst().handleAbout(commandSender);
        }

        return true;
    }

    /**
     * @param commandSender Source of the command. For players tab-completing a
     *                      command inside a command block, this will be the player, not
     *                      the command block.
     * @param command       Command which was executed
     * @param label         Alias of the command which was used
     * @param args          The arguments passed to the command, including final
     *                      partial argument to be completed
     * @return suggestion of arguments, filtert by what's already written
     */
    @Override
    public @Nullable List<String> onTabComplete(@NotNull CommandSender commandSender, @NotNull Command command, @NotNull String label, @NotNull String[] args) {
        final Set<String> suggestions = new HashSet<>();

        //check permissions and add subCommands to be suggested for the commandSender
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_CREATE)) {
            suggestions.addAll(CreateSubCommand.inst().handleTabCompleate(args));
        }
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_DELETE)) {
            suggestions.addAll(DeleteSubCommand.inst().handleTabCompleate(args));
        }
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_EDIT)) {
            suggestions.addAll(EditSubCommand.inst().handleTabCompleate(args));
        }
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_LIST_PLAYERS, Perm.TREASURE_LIST_TREASURES)) {
            suggestions.addAll(ListSubCommand.inst().handleTabCompleate(args));
        }
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_PEEK)) {
            suggestions.addAll(PeekSubCommand.inst().handleTabCompleate(args));
        }
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_FORGET)) {
            suggestions.addAll(ForgetSubCommand.inst().handleTabCompleate(args));
            suggestions.addAll(ForgetAllSubCommand.inst().handleTabCompleate(args));
        }
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_FORGET)) {
            suggestions.addAll(SetForgetSubCommand.inst().handleTabCompleate(args));
        }
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_UNLIMITED)) {
            suggestions.addAll(SetUnlimitedSubCommand.inst().handleTabComplete(args));
        }
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_RANDOM)) {
            suggestions.addAll(SetRandomSubCommand.inst().handleTabComplete(args));
        }
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_GLOBAL)) {
            suggestions.addAll(SetGlobalSubCommand.inst().handleTabComplete(args));
        }
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_ABOUT)) {
            suggestions.addAll(AboutSubCommand.inst().handleTabCompleate(args));
        }
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_RELOAD)) {
            suggestions.addAll(ReloadSubCommand.inst().handleTabCompleate(args));
        }
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_HELP)) {
            suggestions.addAll(HelpSubCommand.inst().handleTabComplete(commandSender, args));
        }

        return suggestions.stream().filter(s -> s.toLowerCase().startsWith(args[args.length - 1].toLowerCase())).toList();
    }
}
