package de.greensurvivors.greentreasure.listener;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermmissionManager;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.event.TreasureBreakEvent;
import de.greensurvivors.greentreasure.event.TreasureCloseEvent;
import de.greensurvivors.greentreasure.event.TreasureOpenEvent;
import de.greensurvivors.greentreasure.language.LangPath;
import io.papermc.paper.block.TileStateInventoryHolder;
import net.kyori.adventure.text.Component;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.block.BlockBreakEvent;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.event.inventory.InventoryCloseEvent;
import org.bukkit.event.inventory.InventoryDragEvent;
import org.bukkit.event.inventory.InventoryOpenEvent;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryView;
import org.bukkit.inventory.ItemStack;
import org.bukkit.persistence.PersistentDataHolder;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

public class TreasureListener implements Listener {
    //list of open inventories, needed to have shared treasures and saving the contents of a treasure after the inventory view was closed
    private final @NotNull Map<@NotNull InventoryView, @NotNull String> openInventories = new HashMap<>();
     private final @NotNull GreenTreasure plugin;

    public TreasureListener(final @NotNull GreenTreasure plugin) {
        this.plugin = plugin;

        Bukkit.getPluginManager().registerEvents(this, plugin);
    }

    public void closeInventories(final @NotNull String treasureId) {
        for (Iterator<Map.Entry<InventoryView, String>> iterator = openInventories.entrySet().iterator(); iterator.hasNext(); ) {
            Map.Entry<InventoryView, String> entry = iterator.next();

            if (entry.getValue() == treasureId) {
                entry.getKey().close();
                iterator.remove();
            }
        }
    }

    /**
     * closes all open inventories to update them
     */
    public void closeAllInventories() {
        for (Iterator<InventoryView> iterator = openInventories.keySet().iterator(); iterator.hasNext();) {
            InventoryView view = iterator.next();
            view.close(); // will call inventory close event first before moving to next line
            iterator.remove();
        }
    }

    /**
     * if a treasure was closed update the timestamp and unlooted inventory in the player file
     */
    @EventHandler(priority = EventPriority.MONITOR)
    private void onCloseTreasure(final @NotNull InventoryCloseEvent event) {
        if (event.getPlayer() instanceof Player ePlayer) {
            Inventory eInventory = event.getInventory();

            if (event.getInventory().getHolder(false) instanceof PersistentDataHolder persistentDataHolder) {
                TreasureInfo treasureInfo = plugin.getTreasureManager().getTreasure(persistentDataHolder);

                //if the treasure wasn't deleted while the inventory was open call the close event
                if (treasureInfo != null) {
                    new TreasureCloseEvent((Player) event.getPlayer(), treasureInfo).callEvent();

                    openInventories.remove(event.getView());

                    // everything is fine. The IDE is just confused with the two annotations of the array
                    plugin.getDatabaseManager().setPlayerData(treasureInfo.isShared() ? null : ePlayer, treasureInfo.treasureId(),
                        new PlayerLootDetail(System.currentTimeMillis(), Arrays.stream(eInventory.getContents()).collect(Collectors.toCollection(ArrayList::new))));
                }
            }
        }
    }

    /**
     * don't let players put items into the treasure
     */
    @EventHandler(ignoreCancelled = true)
    private void onInventoryDrag(final @NotNull InventoryDragEvent event) {
        if (openInventories.get(event.getView()) != null) {
            for (int slot : event.getRawSlots()) {
                if (slot >= 0 && slot < event.getView().getTopInventory().getSize()) {
                    event.setCancelled(true);
                }
            }
        }
    }

    /**
     * don't let players put items into the treasure
     */
    @EventHandler(ignoreCancelled = true)
    private void onInventoryClick(final @NotNull InventoryClickEvent event) {
        if (openInventories.get(event.getView()) != null) {

            switch (event.getAction()) {
                case PLACE_ALL, PLACE_SOME, PLACE_ONE, SWAP_WITH_CURSOR, HOTBAR_SWAP, COLLECT_TO_CURSOR -> {
                    if (event.getRawSlot() >= 0 && event.getRawSlot() < event.getView().getTopInventory().getSize()) {
                        event.setCancelled(true);
                    }
                }
                case MOVE_TO_OTHER_INVENTORY -> {
                    if (event.getRawSlot() < 0 || event.getRawSlot() >= event.getView().getTopInventory().getSize()) {
                        event.setCancelled(true);
                    }
                }
            }
        }
    }

    /**
     * opens a treasure inventory depending on if it was ever looted by the player,
     * if its global, unlimited, forgetting period and random slot chance
     */
    @EventHandler(ignoreCancelled = true)
    private void onOpenTreasure(final @NotNull InventoryOpenEvent event) {
        if (event.getPlayer() instanceof Player ePlayer) {
            Inventory eInventory = event.getInventory();

            if (eInventory.getHolder(false) instanceof PersistentDataHolder dataHolder) {
                final @Nullable TreasureInfo treasureInfo = plugin.getTreasureManager().getTreasure(dataHolder);

                if (treasureInfo != null) {
                    //test permission
                    if (ePlayer.hasPermission(PermmissionManager.TREASURE_OPEN.get())) {
                        Component eTitle = event.getView().title();
                        // don't open the original block inventory
                        event.setCancelled(true);

                        // call api event: TreasureOpenEvent
                        TreasureOpenEvent treasureOpenEvent = new TreasureOpenEvent(ePlayer, treasureInfo, true);
                        treasureOpenEvent.callEvent();

                        // evaluate result
                        switch (treasureOpenEvent.getResult()) {
                            case DEFAULT -> event.setCancelled(true);
                            case ORIGINAL -> {
                                return;
                            }
                            case CANCELED -> {
                                event.setCancelled(true);
                                return;
                            }
                        }

                        if (treasureInfo.isShared()) {
                            InventoryView inventoryView = openInventories.entrySet().stream().
                                filter(entry -> entry.getValue() == treasureInfo.treasureId()).
                                map(Map.Entry::getKey).findAny().orElse(null);

                            if (inventoryView != null && !treasureInfo.isUnlimited()) {
                                ePlayer.openInventory(inventoryView.getTopInventory());

                            } else if (treasureInfo.isUnlimited()) {
                                Inventory nowLooting = Bukkit.createInventory(null, eInventory.getType(), eTitle);
                                // clone every item stack and put it into the new inventory

                                int slotChance = treasureInfo.slotChance();
                                Random random = new Random();

                                nowLooting.setContents(treasureInfo.itemLoot().stream()
                                    .map(s -> s == null || s.isEmpty() ? null : random.nextInt(0, 1000) > slotChance ? null : s.clone()).
                                    toArray(ItemStack[]::new));

                                ePlayer.openInventory(nowLooting);
                                plugin.getMessageManager().sendLang(ePlayer, LangPath.ACTION_FIND_UNLIMITED);
                            } else {
                                //load global treasure async
                                plugin.getDatabaseManager().getPlayerData(null, treasureInfo.treasureId()).thenAccept(playerLootDetail -> {
                                    Inventory nowLooting;

                                    // automatically forget after a given time
                                    if (playerLootDetail == null || ((playerLootDetail.unLootedStuff() == null || playerLootDetail.unLootedStuff().isEmpty()) &&
                                        !(treasureInfo.timeUntilForget().isPositive() && System.currentTimeMillis() - playerLootDetail.lastChangedTimeStamp() > treasureInfo.timeUntilForget().toMillis()))) {

                                        nowLooting = Bukkit.createInventory(null, eInventory.getType(), eTitle);

                                        int slotChance = treasureInfo.slotChance();
                                        Random random = new Random();

                                        // clone every item stack and put it into the new inventory
                                        nowLooting.setContents(treasureInfo.itemLoot().stream().map(s -> s == null || s.isEmpty() ? null : random.nextInt(0, 1000) > slotChance ? null : s.clone()).toArray(ItemStack[]::new));

                                        plugin.getMessageManager().sendLang(ePlayer, LangPath.ACTION_FIND_LIMITED);
                                    } else if (playerLootDetail.unLootedStuff() != null) {
                                        nowLooting = Bukkit.createInventory(null, eInventory.getType(), eTitle);
                                        // get items left there last time
                                        nowLooting.setContents(playerLootDetail.unLootedStuff().toArray(new ItemStack[0]));

                                        plugin.getMessageManager().sendLang(ePlayer, LangPath.ACTION_FIND_ALREADY_LOOTED);
                                    } else {
                                        //not saved inventory but timer is still running. Should never occur but better be safe than sorry
                                        nowLooting = Bukkit.createInventory(null, eInventory.getType(), eTitle);

                                        plugin.getMessageManager().sendLang(ePlayer, LangPath.ACTION_FIND_LIMITED);
                                    }

                                    openInventories.put(ePlayer.openInventory(nowLooting), treasureInfo.treasureId());
                                });
                            }
                        } else { // not globally shared
                            plugin.getDatabaseManager().getPlayerData(ePlayer, treasureInfo.treasureId()).thenAccept(playerLootDetail -> {
                                Inventory nowLooting;

                                if (playerLootDetail == null || ((playerLootDetail.unLootedStuff() == null || playerLootDetail.unLootedStuff().isEmpty() ||
                                    // unlimited treasure
                                    treasureInfo.isUnlimited()) &&
                                    // automatically forget after a given time
                                    !((treasureInfo.timeUntilForget().isPositive()) && (System.currentTimeMillis() - playerLootDetail.lastChangedTimeStamp()) > treasureInfo.timeUntilForget().toMillis()))) {

                                    nowLooting = Bukkit.createInventory(null, eInventory.getType(), eTitle);

                                    int slotChance = treasureInfo.slotChance();
                                    Random random = new Random();

                                    // clone every item stack and put it into the new inventory
                                    nowLooting.setContents(treasureInfo.itemLoot().stream().map(s -> s == null || s.isEmpty() ? null : random.nextInt(0, 1000) > slotChance ? null : s.clone()).toArray(ItemStack[]::new));

                                    plugin.getMessageManager().sendLang(ePlayer, LangPath.ACTION_FIND_LIMITED);
                                } else if (playerLootDetail.unLootedStuff() != null) {
                                    nowLooting = Bukkit.createInventory(null, eInventory.getType(), eTitle);
                                    // get items left there last time
                                    nowLooting.setContents(playerLootDetail.unLootedStuff().toArray(new ItemStack[0]));

                                    plugin.getMessageManager().sendLang(ePlayer, LangPath.ACTION_FIND_ALREADY_LOOTED);
                                } else {
                                    //not saved inventory but timer is still running. Should never occur but better be safe than sorry
                                    nowLooting = Bukkit.createInventory(null, eInventory.getType(), eTitle);

                                    plugin.getMessageManager().sendLang(ePlayer, LangPath.ACTION_FIND_LIMITED);
                                }

                                openInventories.put(ePlayer.openInventory(nowLooting), treasureInfo.treasureId());
                            });
                        }
                    } else {
                        TreasureOpenEvent treasureOpenEvent = new TreasureOpenEvent(ePlayer, treasureInfo, true);
                        treasureOpenEvent.callEvent();

                        switch (treasureOpenEvent.getResult()) {
                            case DEFAULT -> {
                                plugin.getMessageManager().sendLang(ePlayer, LangPath.NO_PERMISSION);
                                // don't open the original block inventory
                                event.setCancelled(true);
                            }
                            case ORIGINAL -> {
                            }
                            case CANCELED -> event.setCancelled(true);
                        }
                    }
                }
            }
        }
    }

    /**
     * in case a treasure is about to be broken cancel the event.
     * to get rid of a treasure use /gt delete
     */
    @EventHandler
    private void onTreasureBreak(final @NotNull BlockBreakEvent event) {
        if (event.getBlock().getState(false) instanceof TileStateInventoryHolder inventoryHolder) {
            final @Nullable TreasureInfo treasureInfo = plugin.getTreasureManager().getTreasure(inventoryHolder);

            if (treasureInfo != null) {
                if (new TreasureBreakEvent(event.getBlock(), event.getPlayer()).callEvent()) {
                    event.setCancelled(true);

                    Player ePlayer = event.getPlayer();
                    if (ePlayer.hasPermission(PermmissionManager.TREASURE_DELETE.get())) {
                        plugin.getMessageManager().sendLang(ePlayer, LangPath.ACTION_BREAK_CONTAINER_ADMIN);
                    } else {
                        plugin.getMessageManager().sendLang(ePlayer, LangPath.ACTION_BREAK_CONTAINER_USER);
                    }
                }
            }
        }
    }
}
