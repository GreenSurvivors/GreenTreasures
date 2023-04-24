package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.config.TreasureConfig;
import de.greensurvivors.greentreasure.language.Lang;
import de.greensurvivors.greentreasure.listener.TreasureListener;
import de.greensurvivors.greentreasure.permission.Perm;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static de.greensurvivors.greentreasure.comands.TreasureCommands.FORGET;
import static de.greensurvivors.greentreasure.comands.TreasureCommands.SET;

public class SetForgetSubCommand {
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

    private static SetForgetSubCommand instance;

    public static SetForgetSubCommand inst() {
        if (instance == null)
            instance = new SetForgetSubCommand();
        return instance;
    }

    /**
     * set's the time period a treasure forgets it was ever opened
     * /gt set(0) forget(1) <1y 2w 3d 4h 5m 6s 7t 8ms>(2..)
     *
     * @param commandSender sender of this command
     * @param args          given arguments
     */
    protected void handleSetForget(CommandSender commandSender, String[] args) {
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_FORGET)) {
            Container container = TreasureCommands.getContainer(commandSender);

            if (container != null) {
                if (TreasureListener.inst().getTreasure(Utils.cleanLocation(container.getLocation())) != null) {
                    if (args.length > 2) {
                        long forgetPeriod = -1L;

                        for (int i = 2; i < args.length; i++) {
                            if (args[i].endsWith(SUFFIX_YEAR)) {
                                String periodWithoutSuffix = args[i].replace(SUFFIX_YEAR, "");

                                if (Utils.isInt(periodWithoutSuffix)) {
                                    forgetPeriod += Long.parseLong(periodWithoutSuffix) * Utils.YEAR_IN_MILLISECONDS;
                                } else {
                                    commandSender.sendMessage(Lang.build(Lang.NO_NUMBER.get().replace(Lang.VALUE, args[i])));
                                    return;
                                }
                            } else if (args[i].endsWith(SUFFIX_WEEK)) {
                                String periodWithoutSuffix = args[i].replace(SUFFIX_WEEK, "");

                                if (Utils.isInt(periodWithoutSuffix)) {
                                    forgetPeriod += Long.parseLong(periodWithoutSuffix) * Utils.WEAK_IN_MILLISECONDS;
                                } else {
                                    commandSender.sendMessage(Lang.build(Lang.NO_NUMBER.get().replace(Lang.VALUE, args[i])));
                                    return;
                                }
                            } else if (args[i].endsWith(SUFFIX_DAY)) {
                                String periodWithoutSuffix = args[i].replace(SUFFIX_DAY, "");

                                if (Utils.isInt(periodWithoutSuffix)) {
                                    forgetPeriod += Long.parseLong(periodWithoutSuffix) * Utils.DAY_IN_MILLISECONDS;
                                } else {
                                    commandSender.sendMessage(Lang.build(Lang.NO_NUMBER.get().replace(Lang.VALUE, args[i])));
                                    return;
                                }
                            } else if (args[i].endsWith(SUFFIX_HOUR)) {
                                String periodWithoutSuffix = args[i].replace(SUFFIX_HOUR, "");

                                if (Utils.isInt(periodWithoutSuffix)) {
                                    forgetPeriod += Long.parseLong(periodWithoutSuffix) * Utils.HOUR_IN_MILLISECONDS;
                                } else {
                                    commandSender.sendMessage(Lang.build(Lang.NO_NUMBER.get().replace(Lang.VALUE, args[i])));
                                    return;
                                }
                            } else if (args[i].endsWith(SUFFIX_MINUTE)) {
                                String periodWithoutSuffix = args[i].replace(SUFFIX_MINUTE, "");

                                if (Utils.isInt(periodWithoutSuffix)) {
                                    forgetPeriod += Long.parseLong(periodWithoutSuffix) * Utils.MINUTE_IN_MILLISECONDS;
                                } else {
                                    commandSender.sendMessage(Lang.build(Lang.NO_NUMBER.get().replace(Lang.VALUE, args[i])));
                                    return;
                                }
                            } else if (args[i].endsWith(SUFFIX_SECOND)) {
                                String periodWithoutSuffix = args[i].replace(SUFFIX_SECOND, "");

                                if (Utils.isInt(periodWithoutSuffix)) {
                                    forgetPeriod += Long.parseLong(periodWithoutSuffix) * Utils.SECONDS_IN_MILLISECONDS;
                                } else {
                                    commandSender.sendMessage(Lang.build(Lang.NO_NUMBER.get().replace(Lang.VALUE, args[i])));
                                    return;
                                }
                            } else if (args[i].endsWith(SUFFIX_TICK)) {
                                String periodWithoutSuffix = args[i].replace(SUFFIX_TICK, "");

                                if (Utils.isInt(periodWithoutSuffix)) {
                                    forgetPeriod += Long.parseLong(periodWithoutSuffix) * Utils.TICKS_IN_MILLISECONDS;
                                } else {
                                    commandSender.sendMessage(Lang.build(Lang.NO_NUMBER.get().replace(Lang.VALUE, args[i])));
                                    return;
                                }
                            } else if (args[i].endsWith(SUFFIX_MILLISECOND)) {
                                String periodWithoutSuffix = args[i].replace(SUFFIX_MILLISECOND, "");

                                if (Utils.isInt(periodWithoutSuffix)) {
                                    forgetPeriod += Long.parseLong(periodWithoutSuffix);
                                } else {
                                    commandSender.sendMessage(Lang.build(Lang.NO_NUMBER.get().replace(Lang.VALUE, args[i])));
                                    return;
                                }
                            } else {
                                commandSender.sendMessage(Lang.build(Lang.UNKNOWN_ARGUMENT.get().replace(Lang.VALUE, args[i])));
                                return;
                            }
                        }

                        commandSender.sendMessage(Lang.build(Lang.SET_FORGET_START.get()));

                        long finalForgetPeriod = forgetPeriod;
                        TreasureConfig.inst().setForgetAsync(container.getLocation(), forgetPeriod, () -> {
                            //negative values turn forget off
                            if (finalForgetPeriod < 0) {
                                commandSender.sendMessage(Lang.build(Lang.REMOVE_FORGET_END.get()));
                            } else {
                                commandSender.sendMessage(Lang.build(Lang.SET_FORGET_END.get().replace(Lang.VALUE, Lang.formatTimePeriod(finalForgetPeriod))));
                            }
                        });
                    } else {
                        commandSender.sendMessage(Lang.build(Lang.NOT_ENOUGH_ARGS.get()));
                    }
                } else {
                    commandSender.sendMessage(Lang.build(Lang.NO_TREASURE.get()));
                }
            } else {
                commandSender.sendMessage(Lang.build(Lang.NOT_LOOKINGAT_CONTAINER.get()));
            }
        } else {
            commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
        }
    }

    /**
     * @param args The arguments passed to the command, including final
     *             partial argument to be completed
     * @return suggestion of arguments
     */
    protected List<String> handleTabCompleate(@NotNull String[] args) {
        switch (args.length) {
            case 1 -> {
                return Collections.singletonList(SET);
            }
            case 2 -> {
                if (args[0].equalsIgnoreCase(SET)) {
                    return Collections.singletonList(FORGET);
                }
            }
            default -> {
                if (args[0].equalsIgnoreCase(SET) && args[1].equalsIgnoreCase(FORGET)) {
                    return List.of(SUFFIX_YEAR, SUFFIX_WEEK, SUFFIX_DAY, SUFFIX_HOUR, SUFFIX_MINUTE, SUFFIX_SECOND, SUFFIX_TICK, SUFFIX_MILLISECOND);
                }
            }
        }

        return new ArrayList<>();
    }
}
