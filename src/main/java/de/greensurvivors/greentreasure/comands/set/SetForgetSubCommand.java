package de.greensurvivors.greentreasure.comands.set;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermissionManager;
import de.greensurvivors.greentreasure.comands.ASubCommand;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.MessageManager;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class SetForgetSubCommand extends ASubCommand {
    // time suffixes
    private static final String
        SUFFIX_YEAR = "y",
        SUFFIX_WEEK = "w",
        SUFFIX_DAY = "d",
        SUFFIX_HOUR = "h",
        SUFFIX_MINUTE = "m",
        SUFFIX_SECOND = "s",
        SUFFIX_TICK = "t",
        SUFFIX_MILLISECOND = "ms";

    public SetForgetSubCommand(@NotNull GreenTreasure plugin) {
        super(plugin);
    }

    @Override
    protected boolean checkPermission(@NotNull Permissible permissible) {
        return permissible.hasPermission(PermissionManager.TREASURE_SET_FORGET.get());
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("forget");
    }

    @Override
    @NotNull
    public Component getHelpText() {
        return null;
    }

    /**
     * set's the time period a treasure forgets it was ever opened
     * /gt set(0) forget(1) <1y 2w 3d 4h 5m 6s 7t 8ms>(2..)
     *
     * @param sender sender of this command
     * @param args   given arguments
     */
    public boolean onCommand(final @NotNull CommandSender sender, final @NotNull String @NotNull [] args) {
        if (checkPermission(sender)) {
            final @Nullable Container container = plugin.getTreasureCommands().getContainer(sender);

            if (container != null) {
                final @Nullable TreasureInfo treasureInfo = plugin.getTreasureManager().getTreasureInfo(container);

                if (treasureInfo != null) {
                    if (args.length > 2) {
                        @NotNull Duration forgetDuration = Duration.ZERO;

                        for (int i = 2; i < args.length; i++) {
                            final @Nullable Duration temp = plugin.getMessageManager().parseDuration(args[i]);

                            if (temp != null) {
                                forgetDuration = forgetDuration.plus(temp);
                            } else {
                                plugin.getMessageManager().sendLang(sender, LangPath.ARG_NOT_TIME,
                                    Placeholder.unparsed(PlaceHolderKey.TEXT.getKey(), args[i]));
                                return false;
                            }
                        }

                        if (forgetDuration.isZero() || forgetDuration.isNegative()) { //negative values turn forget off
                            plugin.getDatabaseManager().setForgetDuration(treasureInfo.treasureId(), null).thenRun(() ->
                                plugin.getMessageManager().sendLang(sender, LangPath.CMD_SET_FORGET_REMOVE_DURATION)
                            );
                        } else {
                            final @NotNull Component formattedTime = MessageManager.formatTime(forgetDuration);

                            plugin.getDatabaseManager().setForgetDuration(treasureInfo.treasureId(), forgetDuration).thenRun(() ->
                                plugin.getMessageManager().sendLang(sender, LangPath.SET_FORGET_DURATION,
                                    Placeholder.component(PlaceHolderKey.TIME.getKey(), formattedTime)));
                        }
                    } else {
                        plugin.getMessageManager().sendLang(sender, LangPath.ERROR_NOT_ENOUGH_ARGS);
                    }
                } else {
                    plugin.getMessageManager().sendLang(sender, LangPath.ERROR_NOT_LOOKING_AT_TREASURE);
                }
            } else {
                plugin.getMessageManager().sendLang(sender, LangPath.ERROR_NOT_LOOKING_AT_CONTAINER);
            }
        } else {
            plugin.getMessageManager().sendLang(sender, LangPath.NO_PERMISSION);
        }

        return true;
    }

    public @NotNull List<@NotNull String> onTabComplete(@NotNull CommandSender sender, final @NotNull String @NotNull [] args) {
        if (args.length > 2) {
            return List.of(SUFFIX_YEAR, SUFFIX_WEEK, SUFFIX_DAY, SUFFIX_HOUR, SUFFIX_MINUTE, SUFFIX_SECOND, SUFFIX_TICK, SUFFIX_MILLISECOND);
        }

        return new ArrayList<>();
    }
}
