package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.config.TreasureConfig;
import de.greensurvivors.greentreasure.dataobjects.PeekedTreasure;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.language.Lang;
import de.greensurvivors.greentreasure.listener.CommandInventoriesListener;
import de.greensurvivors.greentreasure.listener.TreasureListener;
import de.greensurvivors.greentreasure.permission.Perm;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.OfflinePlayer;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;

import java.util.*;

import static de.greensurvivors.greentreasure.comands.TreasureCommands.PEEK;

public class PeekSubCommand {
    private static PeekSubCommand instance;

    public static PeekSubCommand inst() {
        if (instance == null)
            instance = new PeekSubCommand();
        return instance;
    }

    /**
     * let's the command sender open the treasure-inventory for another player
     * /gt peek (takes the commandSender self)
     * /gt peek playerName
     * /gt peek uuid
     * @param commandSender sender of this command
     * @param args given arguments
     */
    protected void handlePeek(CommandSender commandSender, String[] args){
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_PEEK)) {
            if (commandSender instanceof Player player){
                Container container = TreasureCommands.getContainer(commandSender);

                if (container != null){
                    Location location = Utils.cleanLocation(container.getBlock().getLocation());
                    TreasureInfo treasureInfo = TreasureListener.inst().getTreasure(location);

                    if (treasureInfo != null){
                        // unlimited treasure
                        if (treasureInfo.isUnlimited()){
                            commandSender.sendMessage(Lang.build(Lang.PEEK_UNLIMITED.get()));
                        }

                        if (treasureInfo.isGlobal()){
                            //load global treasure async
                            TreasureConfig.inst().getPlayerLootDetailAsync(null, location, playerLootDetail -> {
                                Inventory nowPeeking;
                                long timeStamp= playerLootDetail.lastLootedTimeStamp();

                                if ((playerLootDetail.unLootedStuff() == null || playerLootDetail.unLootedStuff().isEmpty())){
                                    commandSender.sendMessage(Lang.build(Lang.PEEK_GENERATE.get()));

                                    nowPeeking = Bukkit.createInventory(null, container.getInventory().getType(), Lang.build(Lang.PEEK_GLOBAL.get()));

                                    int slotChance = treasureInfo.slotChance();
                                    Random random = new Random();

                                    // clone every item stack and put it into the new inventory
                                    nowPeeking.setContents(TreasureListener.inst().getTreasure(location).itemLoot().stream().map(s -> s == null ? null : random.nextInt(0, 1000) > slotChance ? null : s.clone()).toArray(ItemStack[]::new));

                                    player.sendMessage(Lang.build(Lang.PEEK_WARNING.get()));
                                } else {
                                    nowPeeking = Bukkit.createInventory(null, container.getInventory().getType(), Lang.build(Lang.PEEK_GLOBAL.get()));
                                    // get items left there last time
                                    nowPeeking.setContents(playerLootDetail.unLootedStuff().toArray(new ItemStack[0]));

                                    player.sendMessage(Lang.build(Lang.PEEK_WARNING.get()));
                                }

                                CommandInventoriesListener.inst().addPeekingTreasure(player.openInventory(nowPeeking), new PeekedTreasure(null, location, timeStamp));
                            });
                        } else {
                            UUID uuidToPeek;
                            if (args.length >= 2){
                                OfflinePlayer offlinePlayer = Bukkit.getOfflinePlayer(args[1]);
                                if (offlinePlayer.hasPlayedBefore()){
                                    uuidToPeek = offlinePlayer.getUniqueId();
                                } else {
                                    try{
                                        uuidToPeek = UUID.fromString(args[1]);
                                    }catch (IllegalArgumentException ignored){
                                        commandSender.sendMessage(Lang.build(Lang.NO_SUCH_PLAYER.get().replace(Lang.VALUE, args[1])));
                                        return;
                                    }
                                }
                            } else {
                                uuidToPeek = player.getUniqueId();
                            }

                            TreasureConfig.inst().getPlayerLootDetailAsync(uuidToPeek, location, playerLootDetail -> {
                                Inventory nowPeeking;

                                if ((playerLootDetail.unLootedStuff() == null || playerLootDetail.unLootedStuff().isEmpty())){
                                    commandSender.sendMessage(Lang.build(Lang.PEEK_GENERATE.get()));

                                    nowPeeking = Bukkit.createInventory(null, container.getInventory().getType(), Lang.build(Lang.PEEK_PLAYER.get().replace(Lang.VALUE, uuidToPeek.toString())));

                                    int slotChance = treasureInfo.slotChance();
                                    Random random = new Random();

                                    // clone every item stack and put it into the new inventory
                                    nowPeeking.setContents(TreasureListener.inst().getTreasure(location).itemLoot().stream().map(s -> s == null ? null : random.nextInt(0, 1000) > slotChance ? null : s.clone()).toArray(ItemStack[]::new));
                                } else {
                                    nowPeeking = Bukkit.createInventory(null, container.getInventory().getType(), Lang.build(Lang.PEEK_PLAYER.get().replace(Lang.VALUE, uuidToPeek.toString())));
                                    // get items left there last time
                                    nowPeeking.setContents(playerLootDetail.unLootedStuff().toArray(new ItemStack[0]));
                                }

                                CommandInventoriesListener.inst().addPeekingTreasure(player.openInventory(nowPeeking), new PeekedTreasure(uuidToPeek, location, playerLootDetail.lastLootedTimeStamp()));
                                commandSender.sendMessage(Lang.build(Lang.PEEK_WARNING.get()));
                            });
                        }
                    } else {
                        commandSender.sendMessage(Lang.build(Lang.NO_TREASURE.get()));
                    }
                } else {
                    commandSender.sendMessage(Lang.build(Lang.NOT_LOOKINGAT_CONTAINER.get()));
                }
            } else {
                commandSender.sendMessage(Lang.build(Lang.NO_PLAYER.get()));
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
    protected List<String> handleTabCompleate(@NotNull String[] args){
        switch (args.length) {
            case 1 -> {
                return Collections.singletonList(PEEK);
            }
            case 2 -> {
                if (args[0].equalsIgnoreCase(PEEK)) {
                    Collection<? extends Player> onlinePlayers = Bukkit.getOnlinePlayers();

                    if (!onlinePlayers.isEmpty()){
                        return onlinePlayers.stream().map(Player::getName).toList();
                    }
                }
            }
        }

        return new ArrayList<>();
    }
}
