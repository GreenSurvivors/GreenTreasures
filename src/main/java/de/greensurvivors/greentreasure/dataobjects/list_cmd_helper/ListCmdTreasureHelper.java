package de.greensurvivors.greentreasure.dataobjects.list_cmd_helper;

import de.greensurvivors.greentreasure.TreasureLogger;
import de.greensurvivors.greentreasure.comands.TreasureCommands;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.language.Lang;
import org.bukkit.Location;
import org.bukkit.command.CommandSender;

import java.util.logging.Level;

public class ListCmdTreasureHelper extends ListCmdHelper{
    public ListCmdTreasureHelper(final CommandSender commandSender, final int pageNow, final int lastPage, final int numEntries) {
        super(commandSender, pageNow, lastPage, numEntries);

        //page will be added by super
        super.command = "/" + TreasureCommands.CMD + " " + TreasureCommands.LIST + " ";

        // header
        super.componentResult.add(Lang.build(Lang.LIST_TREASURES_HEADER.get().replace(Lang.VALUE, String.valueOf(pageNow)).replace(Lang.VALUE2, String.valueOf(lastPage))));
    }

    public void addEntry(final TreasureInfo treasureInfo, final Location location){
        super.NumOfPagesStillToDo--;

        TreasureLogger.log(Level.INFO, "numOfPages now: " + NumOfPagesStillToDo);

        //build treasureInfo
        String treasureInfoStr = Lang.LIST_TREASURE_BODY.get().
                replace(Lang.LOCATION, Lang.locationToString(location)).
                replace(Lang.VALUE, String.valueOf(((double)treasureInfo.slotChance()) / 100.0d)).
                replace(Lang.GLOBAL, treasureInfo.isGlobal() ? Lang.TRUE.get() : Lang.FALSE.get()).
                replace(Lang.UNLIMITED, treasureInfo.isUnlimited() ? Lang.TRUE.get() : Lang.FALSE.get());

        if (treasureInfo.timeUntilForget() > 0){
            treasureInfoStr = treasureInfoStr + Lang.LIST_TREASURE_FORGETPERIOD.get().replace(Lang.VALUE, Lang.formatTimePeriod(treasureInfo.timeUntilForget()));
        }

        super.componentResult.add(Lang.build(treasureInfoStr));

        if (super.NumOfPagesStillToDo <= 0){
            sendMessage();
        }
    }
}
