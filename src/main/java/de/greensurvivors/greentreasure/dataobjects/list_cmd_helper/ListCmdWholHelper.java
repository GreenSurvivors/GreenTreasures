package de.greensurvivors.greentreasure.dataobjects.list_cmd_helper;

import de.greensurvivors.greentreasure.comands.ListSubCommand;
import de.greensurvivors.greentreasure.comands.TreasureCommands;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import de.greensurvivors.greentreasure.language.Lang;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.command.CommandSender;
import org.jetbrains.annotations.NotNull;

import java.text.DateFormat;
import java.util.Date;
import java.util.UUID;

public class ListCmdWholHelper extends ListCmdHelper {
    public ListCmdWholHelper(final CommandSender commandSender, final int pageNow, final int lastPage, final int numEntries, @NotNull Location location) {
        super(commandSender, pageNow, lastPage, numEntries);

        //page will be added by super
        super.command = "/" + TreasureCommands.CMD + " " + TreasureCommands.LIST + " " + ListSubCommand.LIST_TYPE.WHO.getSubCommand() + " ";

        // header
        super.componentResult.add(Lang.build(Lang.LIST_WHO_HEADER.get().replace(Lang.LOCATION, Lang.locationToString(location)).replace(Lang.VALUE, String.valueOf(pageNow)).replace(Lang.VALUE2, String.valueOf(lastPage))));
    }

    public void addEntry(final @NotNull UUID uuid, final @NotNull PlayerLootDetail playerLootDetail) {
        super.NumOfEntriesStillToDo--;

        String who = Bukkit.getOfflinePlayer(uuid).getName();

        super.componentResult.add(Lang.build(Lang.LIST_WHO_BODY.get()
                .replace(Lang.NAME, who == null ? Lang.LIST_WHO_GLOBAL.get() : who)
                .replace(Lang.VALUE, (playerLootDetail.unLootedStuff() == null) ? Lang.LIST_PLAYER_NEVER.get() : DateFormat.getDateTimeInstance().format(new Date(playerLootDetail.lastLootedTimeStamp())))));

        if (super.NumOfEntriesStillToDo <= 0) {
            sendMessage();
        }
    }
}
