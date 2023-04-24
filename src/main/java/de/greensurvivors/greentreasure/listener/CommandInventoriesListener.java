package de.greensurvivors.greentreasure.listener;

import de.greensurvivors.greentreasure.config.TreasureConfig;
import de.greensurvivors.greentreasure.dataobjects.PeekedTreasure;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.event.TreasureCloseEvent;
import de.greensurvivors.greentreasure.language.Lang;
import org.bukkit.Location;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.inventory.InventoryCloseEvent;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryView;

import java.util.Arrays;
import java.util.HashMap;

/**
 * this is a technical class to handle the inventories opened by commands
 */
public class CommandInventoriesListener implements Listener {
    private static CommandInventoriesListener instance;

    private final HashMap<InventoryView, Location> editingTreasures = new HashMap<>();
    private final HashMap<InventoryView, PeekedTreasure> peekingTreasures = new HashMap<>();

    public static CommandInventoriesListener inst() {
        if (instance == null) {
            instance = new CommandInventoriesListener();
        }
        return instance;
    }

    /**
     * add an inventory view to the list of actively edited inventory's
     * note: two 2 players editing the same inventory at the same time is NOT supported right now
     *
     * @param editingView the view given back by player.openInventory when /gt edit was called
     * @param location    location to identify the treasure
     */
    public void addEditingTreasure(InventoryView editingView, Location location) {
        editingTreasures.put(editingView, location);
    }

    /**
     * add an inventory view to the list of actively peeked player-treasure inventory's
     *
     * @param peekingView    the view given back by player.openInventory when /gt peek was called
     * @param peekedTreasure location to identify the treasure
     */
    public void addPeekingTreasure(InventoryView peekingView, PeekedTreasure peekedTreasure) {
        peekingTreasures.put(peekingView, peekedTreasure);
    }

    /**
     * closes all open inventories and clears the internal hashmaps
     */
    public void clearInventories() {
        closeInventories();

        editingTreasures.clear();
        peekingTreasures.clear();
    }

    /**
     * closes all open inventories that where the result of a command
     */
    public void closeInventories() {
        for (InventoryView editingView : editingTreasures.keySet()) {
            editingView.close();
        }
        for (InventoryView peekingView : peekingTreasures.keySet()) {
            peekingView.close();
        }
    }

    /**
     * saves the new contents of a treasure inventory if an editing inventory view was closed
     */
    @EventHandler(priority = EventPriority.MONITOR)
    private void onEditingDone(InventoryCloseEvent event) {
        Inventory eInventory = event.getInventory();
        Location location = editingTreasures.get(event.getView());

        if (location != null) {
            TreasureInfo treasureInfo = TreasureListener.inst().getTreasure(location);

            //if the treasure wasn't deleted while the inventory was open
            if (treasureInfo != null) {
                editingTreasures.remove(event.getView());

                event.getPlayer().sendMessage(Lang.build(Lang.TREASURE_EDITED_START.get()));

                TreasureConfig.inst().saveTreasureAsync(location, Arrays.asList(eInventory.getContents()), treasureInfo.type(), () ->
                        event.getPlayer().sendMessage(Lang.build(Lang.TREASURE_EDITED_END.get())));
            } else {
                event.getPlayer().sendMessage(Lang.build(Lang.UNKNOWN_ERROR.get()));
            }
        }
    }

    /**
     * saves the new contents of a treasure inventory to a player file if a peeking inventory view was closed
     */
    @EventHandler(priority = EventPriority.MONITOR)
    private void onPeekingDone(InventoryCloseEvent event) {
        PeekedTreasure peekedTreasure = peekingTreasures.get(event.getView());

        if (peekedTreasure != null) {
            Inventory eInventory = event.getInventory();
            Location location = peekedTreasure.locationOfTreasure();

            TreasureInfo treasureInfo = TreasureListener.inst().getTreasure(location);

            //if the treasure wasn't deleted while the inventory was open call the close event
            if (treasureInfo != null) {
                new TreasureCloseEvent((Player) event.getPlayer(), treasureInfo).callEvent();

                peekingTreasures.remove(event.getView());
                TreasureConfig.inst().savePlayerDetailAsync(treasureInfo.isGlobal() ? null : peekedTreasure.playerPeeked(), location, peekedTreasure.timeStamp(), Arrays.asList(eInventory.getContents()));
            }
        }
    }
}
