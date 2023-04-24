package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.config.TreasureConfig;
import de.greensurvivors.greentreasure.language.Lang;
import de.greensurvivors.greentreasure.listener.TreasureListener;
import de.greensurvivors.greentreasure.permission.Perm;
import org.apache.commons.lang.BooleanUtils;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static de.greensurvivors.greentreasure.comands.TreasureCommands.*;

public class SetGlobalSubCommand {
    private static SetGlobalSubCommand instance;

    public static SetGlobalSubCommand inst() {
        if (instance == null)
            instance = new SetGlobalSubCommand();
        return instance;
    }

    /**
     * set's if all player share the same loot inventory or not
     * /gt set(0) global(1) <true/false>(2)
     * @param commandSender sender of this command
     * @param args given arguments
     */
    protected void handleSetGlobal(CommandSender commandSender, String[] args) {
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_GLOBAL)) {
            Container container = TreasureCommands.getContainer(commandSender);

            if (container != null){
                if (TreasureListener.inst().getTreasure(Utils.cleanLocation(container.getLocation())) != null){
                    if (args.length > 2){
                        Boolean isUnLimited = BooleanUtils.toBooleanObject(args[2]);

                        commandSender.sendMessage(Lang.build(Lang.SET_GLOBAL_START.get()));

                        if (isUnLimited != null){
                            TreasureConfig.inst().setGlobalAsync(container.getLocation(), isUnLimited, () ->
                                    commandSender.sendMessage(Lang.build(Lang.SET_GLOBAL_END.get().replace(Lang.VALUE, String.valueOf(isUnLimited)))));
                        } else {
                            commandSender.sendMessage(Lang.build(Lang.NO_BOOL.get().replace(Lang.VALUE, args[2])));
                        }
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
     *     partial argument to be completed
     * @return suggestion of arguments
     */
    protected List<String> handleTabComplete(@NotNull String[] args){
        switch (args.length) {
            case 1 -> {
                return Collections.singletonList(SET);
            }
            case 2 -> {
                if (args[1].equalsIgnoreCase(SET) ) {
                    return List.of(GLOBAL, SHARED);
                }
            }
            case 3 -> {
                if (args[1].equalsIgnoreCase(SET) && (args[2].equalsIgnoreCase(GLOBAL) || args[2].equalsIgnoreCase(SHARED))) {
                    return List.of(Boolean.TRUE.toString(), Boolean.FALSE.toString());
                }
            }
        }

        return new ArrayList<>();
    }
}
