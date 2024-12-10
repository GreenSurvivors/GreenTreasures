package de.greensurvivors.greentreasure.listener;

import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.LoadingCache;
import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.event.TreasureBreakEvent;
import de.greensurvivors.greentreasure.event.TreasureCloseEvent;
import de.greensurvivors.greentreasure.event.TreasureOpenEvent;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.permission.PermmissionManager;
import io.papermc.paper.block.TileStateInventoryHolder;
import net.kyori.adventure.text.Component;
import org.bukkit.Bukkit;
import org.bukkit.NamespacedKey;
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
import org.bukkit.persistence.PersistentDataType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

// todo the last rework removed the ability to get all treasureLocatoins. Might be usefull to list them in commands.
public class TreasureListener implements Listener {
    // list of known treasures with its location and its information
    private final @NotNull LoadingCache<@NotNull String, @Nullable TreasureInfo> treasures;

    //list of open inventories, needed to have shared treasures and saving the contents of a treasure after the inventory view was closed
    private final Map<InventoryView, String> openInventories = new HashMap<>();

    private final @NotNull NamespacedKey idKey;
    private final @NotNull GreenTreasure plugin;


    public TreasureListener(final @NotNull GreenTreasure plugin) {
        this.plugin = plugin;
        this.idKey = new NamespacedKey(plugin, "id");
        this.treasures = Caffeine.newBuilder().build(id -> plugin.getConfigHandler().loadTreasure(id));

        Bukkit.getPluginManager().registerEvents(this, plugin);
    }

    /**
     * get a treasure given its uuid
     *
     * @return all known information about the treasure itself
     */
    public @Nullable TreasureInfo getTreasure(final @NotNull String treasureId) {
        return treasures.get(treasureId);
    }

    public @Nullable TreasureInfo getTreasure(final @NotNull PersistentDataHolder persistentDataHolder) {
        final @Nullable String treasureId = getTreasureId(persistentDataHolder);

        if (treasureId == null) {
            return null;
        } else {
            return treasures.get(treasureId);
        }
    }

    /**
     * closes all open inventories and clears the internal hashmaps
     * (to repopulate them with updated information)
     */
    public void clearTreasures() {
        treasures.invalidateAll();
        treasures.cleanUp();

        openInventories.clear();
    }

    public void invalidateTreasure(final @NotNull String treasureId) {
        treasures.invalidate(treasureId);
        openInventories.values().removeIf(id -> id == treasureId);
    }

    /**
     * closes all open inventories to update them
     */
    public void closeAllInventories() {
        openInventories.keySet().forEach(InventoryView::close);
    }

    /**
     * if a treasure was closed update the timestamp and unloosed inventory in the player file
     */
    @EventHandler(priority = EventPriority.MONITOR)
    private void onCloseTreasure(final @NotNull InventoryCloseEvent event) {
        if (event.getPlayer() instanceof Player ePlayer) {
            Inventory eInventory = event.getInventory();
            String treasureId = openInventories.get(event.getView());

            if (treasureId != null) {
                TreasureInfo treasureInfo = treasures.get(treasureId);

                //if the treasure wasn't deleted while the inventory was open call the close event
                if (treasureInfo != null) {
                    new TreasureCloseEvent((Player) event.getPlayer(), treasureInfo).callEvent();

                    openInventories.remove(event.getView());

                    // everything is fine. The IDE is just confused with the two annotations of the array
                    //noinspection NullableProblems
                    plugin.getConfigHandler().savePlayerDetail(treasureInfo.isShared() ? null : ePlayer, treasureId, System.currentTimeMillis(), Arrays.asList(eInventory.getContents()));
                }
            }
        }
    }

    /**
     * don't let players put items into the treasure
     */
    @EventHandler(ignoreCancelled = true)
    private void onInventoryDrag(InventoryDragEvent event) {
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
        if (event.getPlayer() instanceof Player ePlayer){
            Inventory eInventory = event.getInventory();

            if (eInventory.getHolder(false) instanceof PersistentDataHolder dataHolder){
                @Nullable String id = getTreasureId(dataHolder);

                if (id != null) {
                    TreasureInfo treasureInfo = treasures.get(id);

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
                                InventoryView inventoryView = openInventories.entrySet().stream().filter(entry -> entry.getValue() == id).map(Map.Entry::getKey).findAny().orElse(null);

                                if (inventoryView != null && !treasureInfo.isUnlimited()) {
                                    ePlayer.openInventory(inventoryView.getTopInventory());

                                } else if (treasureInfo.isUnlimited()) {
                                    Inventory nowLooting = Bukkit.createInventory(null, eInventory.getType(), eTitle);
                                    // clone every item stack and put it into the new inventory

                                    int slotChance = treasureInfo.slotChance();
                                    Random random = new Random();

                                    nowLooting.setContents(treasureInfo.itemLoot().stream().map(s -> s == null || s.isEmpty() ? null : random.nextInt(0, 1000) > slotChance ? null : s.clone()).toArray(ItemStack[]::new));

                                    ePlayer.openInventory(nowLooting);
                                    plugin.getMessageManager().sendLang(ePlayer, LangPath.ACTION_FIND_UNLIMITED);
                                } else {
                                    //load global treasure async
                                    plugin.getConfigHandler().getPlayerLootDetail(null, id).thenAccept(playerLootDetail -> {
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

                                        openInventories.put(ePlayer.openInventory(nowLooting), id);
                                    });
                                }
                            } else { // not globally shared
                                plugin.getConfigHandler().getPlayerLootDetail(ePlayer, id).thenAccept(playerLootDetail -> {
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

                                    openInventories.put(ePlayer.openInventory(nowLooting), id);
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
    }

    public @Nullable String getTreasureId(final @NotNull PersistentDataHolder dataHolder) {
        return dataHolder.getPersistentDataContainer().get(idKey, PersistentDataType.STRING);
    }

    public void setTreasureId(final @NotNull PersistentDataHolder dataHolder, final @NotNull String treasureId) {
        dataHolder.getPersistentDataContainer().set(idKey, PersistentDataType.STRING, treasureId);
    }

    public void removeTreasureId(final @NotNull PersistentDataHolder dataHolder) {
        dataHolder.getPersistentDataContainer().remove(idKey);
    }

    /**
     * in case a treasure is about to be broken cancel the event.
     * to get rid of a treasure use /gt delete
     */
    @EventHandler
    private void onTreasureBreak(final @NotNull BlockBreakEvent event) {
        if (event.getBlock().getState(false) instanceof TileStateInventoryHolder inventoryHolder) {
            final @Nullable  String id = getTreasureId(inventoryHolder);

            if (id != null) {
                @Nullable TreasureInfo treasureInfo = treasures.get(id);

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
                } else {
                    plugin.getComponentLogger().warn("Treasure with id {} was broken, but no instance was known!", id);
                }
            }
        }
    }
}
