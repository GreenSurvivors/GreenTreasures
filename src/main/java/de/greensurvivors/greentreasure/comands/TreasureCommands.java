package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
import org.apache.commons.lang3.StringUtils;
import org.bukkit.Bukkit;
import org.bukkit.block.Block;
import org.bukkit.block.Container;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.LivingEntity;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public class TreasureCommands extends Command {
    public static final String CMD = "greentresure";

    private final @NotNull Map<@NotNull String, @NotNull ASubCommand> subCommands = new HashMap<>();
    private final @NotNull GreenTreasure plugin;
    private final @NotNull AboutSubCommand aboutSubCommand;
    private final @NotNull CreateSubCommand createSubCommand;
    private final @NotNull ListSubCommand listSubCommand;

    public TreasureCommands(final @NotNull GreenTreasure plugin) {
        super(CMD);
        super.setAliases(List.of("gt", "tchest", "treasurechest"));
        this.plugin = plugin;

        Bukkit.getServer().getCommandMap().register(plugin.getName().toLowerCase(Locale.ENGLISH), this);

        aboutSubCommand = new AboutSubCommand(plugin);
        registerSubCommand(aboutSubCommand);
        createSubCommand = new CreateSubCommand(plugin);
        registerSubCommand(createSubCommand);
        registerSubCommand(new DeleteSubCommand(plugin));
        registerSubCommand(new EditSubCommand(plugin));
        registerSubCommand(new ForgetAllSubCommand(plugin));
        registerSubCommand(new ForgetSubCommand(plugin));
        registerSubCommand(new HelpSubCommand(plugin));
        listSubCommand = new ListSubCommand(plugin);
        registerSubCommand(listSubCommand);
        registerSubCommand(new PeekSubCommand(plugin));
        registerSubCommand(new ReloadSubCommand(plugin));
        registerSubCommand(new SetSubCommand(plugin));
    }

    private void registerSubCommand(final @NotNull ASubCommand subCommand) {
        for (@NotNull String alias : subCommand.getAliases()) {
            subCommands.put(alias, subCommand);
        }
    }

    /// only visible for help subcommand!
    protected @NotNull Map<String, ASubCommand> getSubCommands() {
        return subCommands;
    }

    /**
     * Get container a living entity is looking at
     *
     * @param sender sender of this command looking at a container
     * @return container or null if the living entity is not looking at a container
     */
    @Nullable
    public Container getContainer(final @NotNull CommandSender sender) { // todo move
        if (sender instanceof LivingEntity livingEntity) {
            // is the player looking at a container?
            Block block = livingEntity.getTargetBlockExact(5);

            if (block != null && block.getState(false) instanceof Container container) {
                return (Container)Utils.getTreasureHolder(container);
            } else {
                return null;
            }
        } else {
            plugin.getMessageManager().sendLang(sender, LangPath.ERROR_SENDER_NOT_PLAYER);
            return null;
        }
    }

    @Override
    public boolean execute(@NotNull CommandSender sender, @NotNull String commandLabel, @NotNull String @NotNull [] args) {
        if (args.length > 0) {
            for (Map.Entry<String, ASubCommand> entry : subCommands.entrySet()) {
                if (entry.getKey().equalsIgnoreCase(args[0])) {
                    if (entry.getValue().checkPermission(sender)) {
                        entry.getValue().onCommand(sender, args);
                    } else {
                        plugin.getMessageManager().sendLang(sender, LangPath.NO_PERMISSION);
                    }

                    return true;
                }
            }

            plugin.getMessageManager().sendLang(sender, LangPath.ARG_UNKNOWN,
                Placeholder.unparsed(PlaceHolderKey.TEXT.getKey(), args[0]));
            return false;
        } else {
            // plugin
            aboutSubCommand.onCommand(sender, args);
        }

        return true;
    }

    @Override
    public @NotNull List<String> tabComplete(@NotNull CommandSender sender, @NotNull String label, @NotNull String[] args) {
        if (args.length <= 1) {
            final Set<String> suggestions = new HashSet<>();

            for (Map.Entry<String, ASubCommand> entry : subCommands.entrySet()) {
                if (entry.getValue().checkPermission(sender)) {
                    suggestions.add(entry.getKey());
                }
            }

            return suggestions.stream().filter(s -> StringUtils.startsWithIgnoreCase(s, args[0])).toList();
        } else {
            for (Map.Entry<String, ASubCommand> entry : subCommands.entrySet()) {
                if (entry.getKey().equalsIgnoreCase(args[0]) && entry.getValue().checkPermission(sender)) {
                    return entry.getValue().onTabComplete(sender, args).stream().filter(s -> StringUtils.startsWithIgnoreCase(s, args[args.length - 1])).toList();
                }
            }

            return List.of();
        }
    }

    public @NotNull ASubCommand getCreateSubCmd() {
        return createSubCommand;
    }

    public @NotNull ASubCommand getListSubCmd() {
        return listSubCommand;
    }
}
