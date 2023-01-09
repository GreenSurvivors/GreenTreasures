package de.greensurvivors.greentreasure.listener;

import de.greensurvivors.greentreasure.config.TreasureConfig;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.event.TreasureBreakEvent;
import de.greensurvivors.greentreasure.event.TreasureCloseEvent;
import de.greensurvivors.greentreasure.event.TreasureOpenEvent;
import de.greensurvivors.greentreasure.language.Lang;
import de.greensurvivors.greentreasure.permission.Perm;
import net.kyori.adventure.text.Component;
import net.minecraft.world.entity.vehicle.ContainerEntity;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.entity.HumanEntity;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.block.BlockBreakEvent;
import org.bukkit.event.inventory.InventoryCloseEvent;
import org.bukkit.event.inventory.InventoryOpenEvent;
import org.bukkit.inventory.BlockInventoryHolder;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryView;
import org.bukkit.inventory.ItemStack;

import java.util.*;

import static de.greensurvivors.greentreasure.Utils.cleanLocation;

public class TreasureListener implements Listener {
    private static TreasureListener instance;

    // list of known treasures with its location and its information
    private final HashMap<Location, TreasureInfo> treasures = new HashMap<>();

    //list of open inventories, needed to have global treasures and saving the contents of a treasure after the inventory view was closed
    private final HashMap<InventoryView, Location> openInventories = new HashMap<>();

    private TreasureListener() {}

    public static TreasureListener inst() {
        if (instance == null) {
            instance = new TreasureListener();
        }
        return instance;
    }

    /**
     * adds a treasure
     * @param location location to identify a treasure
     * @param treasureInfo all important information a treasure has
     */
    public void addTreasure(Location location, TreasureInfo treasureInfo) {
        treasures.put(location, treasureInfo);
    }

    /**
     * get a treasure given its location
     * @param location location to identify a treasure
     * @return all known information about the treasure itself
     */
    public TreasureInfo getTreasure (Location location){
       return treasures.get(location);
    }

    /**
     * get all locations of all treasueres known. might be used with getTreasure() to get all treasures
     * @return set of locations or an empty list if no treasure was loaded
     */
    public Set<Location> getTreasureLocations(){
        return treasures.keySet();
    }

    /**
     * closes all open inventories and clears the internal hashmaps
     * (to repopulate them with updated information)
     */
    public void clearTreasures() {
        closeAllInventories();

        treasures.clear();
        openInventories.clear();
    }

    /**
     * closes all open inventories to update them
     */
    public void closeAllInventories(){
        for (InventoryView inventoryView : openInventories.keySet()){
            inventoryView.close();
        }
    }

    /**
     * if a treasure was closed update the timestamp and unloosed inventory in the player file
     */
    @EventHandler
    private void onCloseTreasure(InventoryCloseEvent event){
        UUID eUUID = event.getPlayer().getUniqueId();
        Inventory eInventory = event.getInventory();
        Location eLocation = openInventories.get(event.getView());

        TreasureInfo treasureInfo = treasures.get(eLocation);

        //if the treasure wasn't deleted while the inventory was open call the close event
        if (treasureInfo != null){
            new TreasureCloseEvent((Player) event.getPlayer(), treasureInfo).callEvent();

            if (eLocation != null){
                openInventories.remove(event.getView());
                TreasureConfig.inst().savePlayerDetailAsync(treasureInfo.isGlobal() ? null : eUUID, eLocation, System.currentTimeMillis(), Arrays.asList(eInventory.getContents()));
            }
        }
    }

    /**
     * opens a treasure inventory depending on if it was ever looted by the player,
     * if its global, unlimited, forgetting period and random slot chance
     * @param event
     */
    @EventHandler
    private void onOpenTreasure (InventoryOpenEvent event){
        if (!event.isCancelled()) {
            HumanEntity ePlayer = event.getPlayer();

            //test permission
            if (Perm.hasPermission(ePlayer, Perm.TREASURE_ADMIN, Perm.TREASURE_OPEN)) {

                Inventory eInventory = event.getInventory();
                Component eTitle = event.getView().title();
                final Location eLocation;
                String type;

                // get location and type
                // note: this makes it already compatible should ever support for entity's like mine-carts be needed,
                // however the commands aren't
                if (eInventory.getHolder() instanceof BlockInventoryHolder blockInventoryHolder){
                    eLocation = cleanLocation(blockInventoryHolder.getBlock().getLocation());
                    type = blockInventoryHolder.getBlock().getType().name();
                } else if (eInventory.getHolder() instanceof ContainerEntity containerEntity){
                    eLocation = cleanLocation(containerEntity.getLocation());
                    type = containerEntity.getEntity().getType().toString();
                } else {
                    eLocation = null;
                    type = null;
                }

                TreasureInfo treasureInfo = treasures.get(eLocation);

                if (eLocation != null && treasureInfo != null) {
                    // test on type in case wrong entity is clicked
                    if (treasureInfo.type().equalsIgnoreCase(type)){
                        // don't open the original block inventory
                        event.setCancelled(true);

                        // call api event: TreasureOpenEvent
                        TreasureOpenEvent treasureOpenEvent = new TreasureOpenEvent((Player) ePlayer, treasureInfo);
                        treasureOpenEvent.callEvent();

                        // evaluate result
                        switch (treasureOpenEvent.getResult()){
                            case DEFAULT -> event.setCancelled(true);
                            case ORIGINAL -> {
                                return;
                            }
                            case CANCELED -> {
                                event.setCancelled(true);
                                return;
                            }
                        }

                        if (treasureInfo.isGlobal()){
                            InventoryView inventoryView = openInventories.keySet().stream().filter(s -> openInventories.get(s) == eLocation).findAny().orElse(null);

                            if (inventoryView != null && !treasureInfo.isUnlimited()) {
                                ePlayer.openInventory(inventoryView.getTopInventory());

                            } else if (treasureInfo.isUnlimited()){
                                Inventory nowLooting = Bukkit.createInventory(null, eInventory.getType(), eTitle);
                                // clone every item stack and put it into the new inventory

                                int slotChance = treasureInfo.slotChance();
                                Random random = new Random();

                                nowLooting.setContents(treasures.get(eLocation).itemLoot().stream().map(s -> s == null ? null : random.nextInt(0, 1000) > slotChance ? null : s.clone()).toArray(ItemStack[]::new));

                                ePlayer.openInventory(nowLooting);
                                ePlayer.sendMessage(Lang.build(Lang.TREASURE_FOUND_UNLIMITED.get()));
                            } else {
                                //load global treasure async
                                TreasureConfig.inst().getPlayerLootDetailAsync(null, eLocation, playerLootDetail -> {
                                    Inventory nowLooting;

                                    // automatically forget after a given time
                                    if ((playerLootDetail.unLootedStuff() == null || playerLootDetail.unLootedStuff().isEmpty()) &&
                                            !(treasureInfo.timeUntilForget() > 0 && System.currentTimeMillis() - playerLootDetail.lastLootedTimeStamp() > treasureInfo.timeUntilForget())){

                                        nowLooting = Bukkit.createInventory(null, eInventory.getType(), eTitle);

                                        int slotChance = treasureInfo.slotChance();
                                        Random random = new Random();

                                        // clone every item stack and put it into the new inventory
                                        nowLooting.setContents(treasures.get(eLocation).itemLoot().stream().map(s -> s == null ? null : random.nextInt(0, 1000) > slotChance ? null : s.clone()).toArray(ItemStack[]::new));

                                        ePlayer.sendMessage(Lang.build(Lang.TREASURE_FOUND_LIMITED.get()));
                                    } else if (playerLootDetail.unLootedStuff() != null) {
                                        nowLooting = Bukkit.createInventory(null, eInventory.getType(), eTitle);
                                        // get items left there last time
                                        nowLooting.setContents(playerLootDetail.unLootedStuff().toArray(new ItemStack[0]));

                                        ePlayer.sendMessage(Lang.build(Lang.TREASURE_ALREADYLOOTED.get()));
                                    } else {
                                        //not saved inventory but timer is still running. Should never occur but better be safe than sorry
                                        nowLooting = Bukkit.createInventory(null, eInventory.getType(), eTitle);

                                        ePlayer.sendMessage(Lang.build(Lang.TREASURE_FOUND_LIMITED.get()));
                                    }

                                    openInventories.put(ePlayer.openInventory(nowLooting), eLocation);
                                });
                            }
                        } else { // not globally shared
                            TreasureConfig.inst().getPlayerLootDetailAsync(ePlayer.getUniqueId(), eLocation, playerLootDetail -> {
                                Inventory nowLooting;

                                if ((playerLootDetail.unLootedStuff() == null || playerLootDetail.unLootedStuff().isEmpty() ||
                                        // unlimited treasure
                                        treasureInfo.isUnlimited()) &&
                                        // automatically forget after a given time
                                        !((treasureInfo.timeUntilForget() > 0) && (System.currentTimeMillis() - playerLootDetail.lastLootedTimeStamp()) > treasureInfo.timeUntilForget())){

                                    nowLooting = Bukkit.createInventory(null, eInventory.getType(), eTitle);

                                    int slotChance = treasureInfo.slotChance();
                                    Random random = new Random();

                                    // clone every item stack and put it into the new inventory
                                    nowLooting.setContents(treasures.get(eLocation).itemLoot().stream().map(s -> s == null ? null : random.nextInt(0, 1000) > slotChance ? null : s.clone()).toArray(ItemStack[]::new));

                                    ePlayer.sendMessage(Lang.build(Lang.TREASURE_FOUND_LIMITED.get()));
                                } else if (playerLootDetail.unLootedStuff() != null) {
                                    nowLooting = Bukkit.createInventory(null, eInventory.getType(), eTitle);
                                    // get items left there last time
                                    nowLooting.setContents(playerLootDetail.unLootedStuff().toArray(new ItemStack[0]));

                                    ePlayer.sendMessage(Lang.build(Lang.TREASURE_ALREADYLOOTED.get()));
                                } else {
                                    //not saved inventory but timer is still running. Should never occur but better be safe than sorry
                                    nowLooting = Bukkit.createInventory(null, eInventory.getType(), eTitle);

                                    ePlayer.sendMessage(Lang.build(Lang.TREASURE_FOUND_LIMITED.get()));
                                }

                                openInventories.put(ePlayer.openInventory(nowLooting), eLocation);
                            });
                        }
                    }
                }
            } else {
                ePlayer.sendMessage(Lang.build(Lang.NO_PERMISSION_SOMETHING.get()));
            }
        }
    }

    /**
     * in case a treasure is about to be broken cancel the event.
     * to get rid of a treasure use /gt delete
     */
    @EventHandler
    private void onTreasureBreak(BlockBreakEvent event){
        Location eLocation = cleanLocation(event.getBlock().getLocation());
        TreasureInfo treasureInfo = treasures.get(eLocation);

        if (treasureInfo != null && treasureInfo.type().equalsIgnoreCase(event.getBlock().getType().name())){
            if (new TreasureBreakEvent(event.getBlock(), event.getPlayer()).callEvent()){
                event.setCancelled(true);

                Player ePlayer = event.getPlayer();
                if (Perm.hasPermission(ePlayer, Perm.TREASURE_ADMIN, Perm.TREASURE_DELETE)) {
                    ePlayer.sendMessage(Lang.build(Lang.DONT_BREAK_CONTAINER_ADMIN.get()));
                } else {
                    ePlayer.sendMessage(Lang.build(Lang.DONT_BREAK_CONTAINER_USER.get()));
                }
            }
        }
    }
}
