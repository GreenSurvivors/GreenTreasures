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

import static de.greensurvivors.greentreasure.comands.TreasureCommands.RANDOM;
import static de.greensurvivors.greentreasure.comands.TreasureCommands.SET;

public class SetRandomSubCommand {
    private static SetRandomSubCommand instance;

    public static SetRandomSubCommand inst() {
        if (instance == null)
            instance = new SetRandomSubCommand();
        return instance;
    }

    /**
     * sets how probable it is for an inventory slot to be lootable
     * /gt set(0) random(1) <slotChance>(2)
     * @param commandSender sender of this command
     * @param args given arguments
     */
    protected void handleSetRandom(CommandSender commandSender, String[] args) {
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_RANDOM)) {
            Container container = TreasureCommands.getContainer(commandSender);

            if (container != null){
                if (TreasureListener.inst().getTreasure(Utils.cleanLocation(container.getLocation())) != null){
                    if (args.length > 2){
                        if (Utils.isDouble(args[2])){
                            int slotPercentage = (int)(Double.parseDouble(args[2]) * 100);

                            slotPercentage = Math.max(Math.min(slotPercentage, 10000), 0);

                            TreasureConfig.inst().setRandomAsync(container.getLocation(), slotPercentage);

                            commandSender.sendMessage(Lang.build(Lang.SET_RANDOM.get().replace(Lang.VALUE, String.valueOf(((float)slotPercentage) / 100.0f))));
                        } else {
                            commandSender.sendMessage(Lang.build(Lang.NO_NUMBER.get().replace(Lang.VALUE, args[2])));
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

    //gt set(0) forget(1) <1y 2w 3d 4h 5m 6s 7t 8ms>(2..)
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
                if (args[0].equalsIgnoreCase(SET)) {
                    return Collections.singletonList(RANDOM);
                }
            }
            case 3 -> {
                if (args[1].equalsIgnoreCase(SET) && args[2].equalsIgnoreCase(RANDOM)) {
                    return List.of("100.0", "75.0", "50.0", "25.0", "10.0", "5.0", "2.0");
                }
            }
        }

        return new ArrayList<>();
    }
}
