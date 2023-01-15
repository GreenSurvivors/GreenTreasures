package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.config.TreasureConfig;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.language.Lang;
import de.greensurvivors.greentreasure.listener.TreasureListener;
import de.greensurvivors.greentreasure.permission.Perm;
import net.kyori.adventure.text.Component;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.OfflinePlayer;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;

import java.text.DateFormat;
import java.util.*;

import static de.greensurvivors.greentreasure.comands.TreasureCommands.LIST;

public class ListSubCommand {
    private static ListSubCommand instance;

    public static ListSubCommand inst() {
        if (instance == null)
            instance = new ListSubCommand();
        return instance;
    }

    /**
     * list all treasures on this server,
     * or if a player was given lists all looted treasures
     * /gt list (list all treasures)
     * /gt list playerName
     * /gt list uuid
     * @param commandSender sender of this command
     */
    protected void handleList(CommandSender commandSender, String[] args){
        UUID uuidToGetListOf;
        if (args.length >= 2){
            if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_LIST_PLAYERS)) {

                OfflinePlayer offlinePlayer = Bukkit.getOfflinePlayer(args[1]);
                if (offlinePlayer.hasPlayedBefore()){
                    uuidToGetListOf = offlinePlayer.getUniqueId();
                } else {
                    try{
                        uuidToGetListOf = UUID.fromString(args[1]);
                    }catch (IllegalArgumentException ignored){
                        commandSender.sendMessage(Lang.build(Lang.NO_SUCH_PLAYER.get().replace(Lang.VALUE, args[1])));
                        return;
                    }
                }

                Set<Location> treasureLocations = TreasureListener.inst().getTreasureLocations();
                final ArrayList<PlayerLootDetail> playerLootDetails = new ArrayList<>();

                for(Iterator<Location> locationIterator = treasureLocations.iterator(); locationIterator.hasNext();) {
                    final Location treasureLocation = locationIterator.next();
                    final boolean lastEntry = !locationIterator.hasNext();

                    TreasureConfig.inst().getPlayerLootDetailAsync(uuidToGetListOf, treasureLocation, playerLootDetail_result -> {
                        playerLootDetails.add(playerLootDetail_result);

                        if (lastEntry){
                            // collect all messages to send at once
                            List<Component> components = new ArrayList<>();
                            // header
                            components.add(Lang.build(Lang.LIST_HEADER_PLAYER.get().replace(Lang.VALUE, uuidToGetListOf.toString())));

                            //build treasureInfo
                            for (PlayerLootDetail playerLootDetail: playerLootDetails){
                                components.add(Lang.build(Lang.LIST_PLAYER.get().
                                        replace(Lang.LOCATION, Lang.locationToString(treasureLocation)).
                                        replace(Lang.VALUE, playerLootDetail.unLootedStuff() == null ? Lang.LIST_NEVER.get() : DateFormat.getDateTimeInstance().format(new Date(playerLootDetail.lastLootedTimeStamp())))));
                            }

                            // send components
                            commandSender.sendMessage(Lang.join(components));
                        }
                    });
                }
            } else {
                commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
            }

        } else {
            if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_LIST_TREASURES)) {
                // collect all messages to send at once
                List<Component> components = new ArrayList<>();
                // header
                components.add(Lang.build(Lang.LIST_HEADER_TREASURES.get()));

                //build list
                for (Location treasureLocation : TreasureListener.inst().getTreasureLocations()){
                    TreasureInfo treasureInfo = TreasureListener.inst().getTreasure(treasureLocation);

                    String treasureInfoStr = Lang.LIST_TREASURE_BODY.get().
                            replace(Lang.LOCATION, Lang.locationToString(treasureLocation)).
                            replace(Lang.VALUE, String.valueOf(((double)treasureInfo.slotChance()) / 100.0d)).
                            replace(Lang.GLOBAL, treasureInfo.isGlobal() ? Lang.TRUE.get() : Lang.FALSE.get()).
                            replace(Lang.UNLIMITED, treasureInfo.isUnlimited() ? Lang.TRUE.get() : Lang.FALSE.get());

                    if (treasureInfo.timeUntilForget() > 0){
                        treasureInfoStr = treasureInfoStr + Lang.LIST_TREASURE_FORGETPERIOD.get().replace(Lang.VALUE, Lang.formatTimePeriod(treasureInfo.timeUntilForget()));
                    }

                    components.add(Lang.build(treasureInfoStr));
                }

                // send components
                commandSender.sendMessage(Lang.join(components));
            } else {
                commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
            }
        }
    }

    /**
     * @param args The arguments passed to the command, including final
     *     partial argument to be completed
     * @return suggestion of arguments, filtert by what's already written
     */
    protected List<String> handleTapCompleate(@NotNull String[] args){
        switch (args.length){
            case 1 -> {
                return Collections.singletonList(LIST);
            }
            case 2 -> {
                if (args[0].equalsIgnoreCase(LIST)) {
                    Collection<? extends Player> onlinePlayers =  Bukkit.getOnlinePlayers();
                    if (!onlinePlayers.isEmpty()){
                        return onlinePlayers.stream().map(Player::getName).toList();
                    }
                }
            }
        }


        return new ArrayList<>();
    }
}
