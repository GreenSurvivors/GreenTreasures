package de.greensurvivors.greentreasure.dataobjects.list_cmd_helper;

import de.greensurvivors.greentreasure.comands.TreasureCommands;
import de.greensurvivors.greentreasure.config.TreasureConfig;
import de.greensurvivors.greentreasure.language.Lang;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.command.CommandSender;

import java.text.DateFormat;
import java.util.Date;
import java.util.UUID;

public class ListCmdPlayerDetailHelper extends ListCmdHelper {
    private final UUID UUID_TO_GET_LIST_OF;

    public ListCmdPlayerDetailHelper(final CommandSender commandSender, final int pageNow, final int lastPage, final int numEntries, final UUID uuidToGetListOf) {
        super(commandSender, pageNow, lastPage, numEntries);

        this.UUID_TO_GET_LIST_OF = uuidToGetListOf;
        //page will be added by super
        super.command = "/" + TreasureCommands.CMD + " " + TreasureCommands.LIST + " " + UUID_TO_GET_LIST_OF.toString() + " ";

        String name = Bukkit.getOfflinePlayer(uuidToGetListOf).getName();
        if (name == null) {
            name = "?";
        }

        // header
        super.componentResult.add(Lang.build(Lang.LIST_PLAYER_HEADER.get().replace(Lang.NAME, name).replace(Lang.VALUE, String.valueOf(pageNow)).replace(Lang.VALUE2, String.valueOf(lastPage))));
    }

    public void addEntry(final Location treasureLocation) {
        TreasureConfig.inst().getPlayerLootDetailAsync(UUID_TO_GET_LIST_OF, treasureLocation, playerLootDetail_result -> {
            synchronized (super.MUTEX) {
                super.NumOfPagesStillToDo--;

                //build treasureInfo
                super.componentResult.add(Lang.build(Lang.LIST_PLAYER_BODY.get().
                        replace(Lang.LOCATION, Lang.locationToString(treasureLocation)).
                        replace(Lang.VALUE, playerLootDetail_result.unLootedStuff() == null ? Lang.LIST_PLAYER_NEVER.get() : DateFormat.getDateTimeInstance().format(new Date(playerLootDetail_result.lastLootedTimeStamp())))));

                if (super.NumOfPagesStillToDo <= 0) {
                    sendMessage();
                }
            }
        });
    }
}
