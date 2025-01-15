package de.greensurvivors.greentreasure.listener;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermissionManager;
import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.dataobjects.InventoryHolderWrapper;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.event.TreasureBreakEvent;
import de.greensurvivors.greentreasure.event.TreasureCloseEvent;
import de.greensurvivors.greentreasure.event.TreasureOpenEvent;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import io.papermc.paper.block.TileStateInventoryHolder;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.MiniMessage;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
import org.bukkit.Bukkit;
import org.bukkit.block.Container;
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
import org.bukkit.inventory.InventoryHolder;
import org.bukkit.inventory.InventoryView;
import org.bukkit.persistence.PersistentDataHolder;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

public class TreasureListener implements Listener {
    //list of open inventories, needed to have shared treasures and saving the contents of a treasure after the inventory view was closed
    private final @NotNull Map<@NotNull String, @NotNull Set<@NotNull InventoryView>> openInventories = new HashMap<>();
    private final @NotNull GreenTreasure plugin;

    public TreasureListener(final @NotNull GreenTreasure plugin) {
        this.plugin = plugin;

        Bukkit.getPluginManager().registerEvents(this, plugin);
    }

    public void closeInventories(final @NotNull String treasureId) {
        final @Nullable Collection<@NotNull InventoryView> views = openInventories.remove(treasureId);

        if (views != null) {
            views.forEach(InventoryView::close);
        }
    }

    /**
     * closes all open inventories to update them
     */
    public void closeAllInventories() {
        for (Collection<InventoryView> views : openInventories.values()) {
            views.forEach(InventoryView::close); // will call inventory close event first before removing this entry
        }
        openInventories.clear();
    }

    /**
     * if a treasure was closed update the timestamp and unlooted inventory in the player file
     */
    @EventHandler(priority = EventPriority.MONITOR)
    private void onCloseTreasure(final @NotNull InventoryCloseEvent event) {
        if (event.getPlayer() instanceof Player ePlayer) {
            final @NotNull Inventory eInventory = event.getInventory();

            // skip peeked treasures
            if (event.getView().getTopInventory().getHolder(false) instanceof InventoryHolderWrapper<?> wrapper && wrapper.isPerCommand()) {
                return;
            }

            final @Nullable String treasureId = plugin.getTreasureManager().getTreasureId(event.getView());
            if (treasureId != null) {
                TreasureInfo treasureInfo = plugin.getTreasureManager().getTreasureInfo(treasureId);

                //if the treasure wasn't deleted while the inventory was open call the close event
                if (treasureInfo != null) {
                    new TreasureCloseEvent((Player) event.getPlayer(), treasureInfo).callEvent();

                    final @Nullable Collection<@NotNull InventoryView> views = openInventories.get(treasureId);
                    if (views != null) {
                        views.remove(event.getView());
                    }

                    // everything is fine. The IDE is just confused with the two annotations of the array
                    plugin.getDatabaseManager().setPlayerData(treasureInfo.isShared() ? null : ePlayer, treasureId,
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
        if (event.getView().getTopInventory().getHolder(false) instanceof InventoryHolderWrapper<?> wrapper && wrapper.isPerCommand()) {
            return;
        }

        if (plugin.getTreasureManager().getTreasureId(event.getView()) != null) {
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
        if (event.getView().getTopInventory().getHolder(false) instanceof InventoryHolderWrapper<?> wrapper && wrapper.isPerCommand()) {
            return;
        }

        if (plugin.getTreasureManager().getTreasureId(event.getView()) != null) {

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
            final @NotNull Inventory eInventory = event.getInventory();

            final @Nullable TreasureInfo treasureInfo = plugin.getTreasureManager().getTreasureInfo(event.getView());

            if (treasureInfo != null) {
                final @Nullable Collection<@NotNull InventoryView> views = openInventories.get(treasureInfo.treasureId());

                if (eInventory.getHolder(false) instanceof InventoryHolderWrapper<?>) {
                    return;
                }

                if (views != null) {
                    plugin.getComponentLogger().info("found entry in openInventories.");

                    if (views.contains(event.getView())) {
                        plugin.getComponentLogger().info("same view!.");
                        return;
                    }
                }

                //test permission
                if (ePlayer.hasPermission(PermissionManager.TREASURE_OPEN.get())) {
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

                    final @NotNull InventoryHolderWrapper<?> owner = new InventoryHolderWrapper<>((InventoryHolder & PersistentDataHolder)
                        Utils.getTreasureHolder(eInventory.getHolder(false)), false);

                    if (treasureInfo.isShared()) {
                        final @Nullable InventoryView inventoryView;
                        if (views == null || views.isEmpty()) {
                            inventoryView = null;
                        } else {
                            inventoryView = views.iterator().next();
                        }

                        if (inventoryView != null && !treasureInfo.isUnlimited()) {
                            ePlayer.openInventory(inventoryView.getTopInventory());

                        } else if (treasureInfo.isUnlimited()) {
                            Inventory nowLooting = Bukkit.createInventory(owner, eInventory.getType(), eTitle);
                            Utils.setContents(nowLooting, treasureInfo.itemLoot(), treasureInfo.slotChance());

                            ePlayer.openInventory(nowLooting);

                            if (treasureInfo.rawFindFreshMessageOverride() != null) {
                                plugin.getMessageManager().sendMessage(ePlayer,
                                    MiniMessage.miniMessage().deserialize(treasureInfo.rawFindFreshMessageOverride(),
                                        Placeholder.component(PlaceHolderKey.PLAYER.getKey(), ePlayer.displayName()),
                                        Placeholder.component(PlaceHolderKey.TEXT.getKey(), eTitle),
                                        Placeholder.component(PlaceHolderKey.UNLIMITED.getKey(), plugin.getMessageManager().getLang(LangPath.BOOLEAN_TRUE))
                                    ));
                            } else {
                                plugin.getMessageManager().sendLang(ePlayer, LangPath.ACTION_FIND_UNLIMITED);
                            }
                        } else {
                            //load global treasure async
                            plugin.getDatabaseManager().getPlayerData(null, treasureInfo.treasureId()).thenAccept(playerLootDetail -> {
                                Inventory nowLooting;

                                // automatically forget after a given time
                                if (playerLootDetail == null || ((playerLootDetail.unLootedStuff() == null || playerLootDetail.unLootedStuff().isEmpty()) &&
                                    !(treasureInfo.timeUntilForget().isPositive() && System.currentTimeMillis() - playerLootDetail.lastChangedTimeStamp() > treasureInfo.timeUntilForget().toMillis()))) {

                                    nowLooting = Bukkit.createInventory(owner, eInventory.getType(), eTitle);
                                    Utils.setContents(nowLooting, treasureInfo.itemLoot(), treasureInfo.slotChance());

                                    if (treasureInfo.rawFindFreshMessageOverride() != null) {
                                        plugin.getMessageManager().sendMessage(ePlayer,
                                            MiniMessage.miniMessage().deserialize(treasureInfo.rawFindFreshMessageOverride(),
                                                Placeholder.component(PlaceHolderKey.PLAYER.getKey(), ePlayer.displayName()),
                                                Placeholder.component(PlaceHolderKey.TEXT.getKey(), eTitle),
                                                Placeholder.component(PlaceHolderKey.UNLIMITED.getKey(), plugin.getMessageManager().getLang(LangPath.BOOLEAN_FALSE))
                                            ));
                                    } else {
                                        plugin.getMessageManager().sendLang(ePlayer, LangPath.ACTION_FIND_LIMITED);
                                    }
                                } else if (playerLootDetail.unLootedStuff() != null) {
                                    nowLooting = Bukkit.createInventory(owner, eInventory.getType(), eTitle);
                                    // get items left there last time
                                    Utils.setContents(nowLooting, playerLootDetail.unLootedStuff());

                                    if (treasureInfo.rawFindLootedMessageOverride() != null) {
                                        plugin.getMessageManager().sendMessage(ePlayer,
                                            MiniMessage.miniMessage().deserialize(treasureInfo.rawFindLootedMessageOverride(),
                                                Placeholder.component(PlaceHolderKey.PLAYER.getKey(), ePlayer.displayName()),
                                                Placeholder.component(PlaceHolderKey.TEXT.getKey(), eTitle),
                                                Placeholder.component(PlaceHolderKey.UNLIMITED.getKey(), plugin.getMessageManager().getLang(LangPath.BOOLEAN_FALSE))
                                            ));
                                    } else {
                                        plugin.getMessageManager().sendLang(ePlayer, LangPath.ACTION_FIND_ALREADY_LOOTED);
                                    }
                                } else {
                                    //not saved inventory but timer is still running. Should never occur but better be safe than sorry
                                    nowLooting = Bukkit.createInventory(owner, eInventory.getType(), eTitle);

                                    if (treasureInfo.rawFindFreshMessageOverride() != null) {
                                        plugin.getMessageManager().sendMessage(ePlayer,
                                            MiniMessage.miniMessage().deserialize(treasureInfo.rawFindFreshMessageOverride(),
                                                Placeholder.component(PlaceHolderKey.PLAYER.getKey(), ePlayer.displayName()),
                                                Placeholder.component(PlaceHolderKey.TEXT.getKey(), eTitle),
                                                Placeholder.component(PlaceHolderKey.UNLIMITED.getKey(), plugin.getMessageManager().getLang(LangPath.BOOLEAN_FALSE))
                                            ));
                                    } else {
                                        plugin.getMessageManager().sendLang(ePlayer, LangPath.ACTION_FIND_LIMITED);
                                    }
                                }

                                final InventoryView view = ePlayer.openInventory(nowLooting);
                                if (view != null) {
                                    openInventories.computeIfAbsent(treasureInfo.treasureId(), ignored -> new HashSet<>()).add(view);
                                }
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

                                nowLooting = Bukkit.createInventory(owner, eInventory.getType(), eTitle);
                                Utils.setContents(nowLooting, treasureInfo.itemLoot(), treasureInfo.slotChance());

                                if (treasureInfo.rawFindFreshMessageOverride() != null) {
                                    plugin.getMessageManager().sendMessage(ePlayer,
                                        MiniMessage.miniMessage().deserialize(treasureInfo.rawFindFreshMessageOverride(),
                                            Placeholder.component(PlaceHolderKey.PLAYER.getKey(), ePlayer.displayName()),
                                            Placeholder.component(PlaceHolderKey.TEXT.getKey(), eTitle),
                                            Placeholder.component(PlaceHolderKey.UNLIMITED.getKey(), plugin.getMessageManager().getLang(LangPath.BOOLEAN_FALSE))
                                        ));
                                } else {
                                    plugin.getMessageManager().sendLang(ePlayer, LangPath.ACTION_FIND_LIMITED);
                                }
                            } else if (playerLootDetail.unLootedStuff() != null) {
                                nowLooting = Bukkit.createInventory(owner, eInventory.getType(), eTitle);
                                Utils.setContents(nowLooting, playerLootDetail.unLootedStuff());

                                if (treasureInfo.rawFindLootedMessageOverride() != null) {
                                    plugin.getMessageManager().sendMessage(ePlayer,
                                        MiniMessage.miniMessage().deserialize(treasureInfo.rawFindLootedMessageOverride(),
                                            Placeholder.component(PlaceHolderKey.PLAYER.getKey(), ePlayer.displayName()),
                                            Placeholder.component(PlaceHolderKey.TEXT.getKey(), eTitle),
                                            Placeholder.component(PlaceHolderKey.UNLIMITED.getKey(), plugin.getMessageManager().getLang(LangPath.BOOLEAN_FALSE))
                                        ));
                                } else {
                                    plugin.getMessageManager().sendLang(ePlayer, LangPath.ACTION_FIND_ALREADY_LOOTED);
                                }
                            } else {
                                //not saved inventory but timer is still running. Should never occur but better be safe than sorry
                                nowLooting = Bukkit.createInventory(owner, eInventory.getType(), eTitle);

                                if (treasureInfo.rawFindFreshMessageOverride() != null) {
                                    plugin.getMessageManager().sendMessage(ePlayer,
                                        MiniMessage.miniMessage().deserialize(treasureInfo.rawFindFreshMessageOverride(),
                                            Placeholder.component(PlaceHolderKey.PLAYER.getKey(), ePlayer.displayName()),
                                            Placeholder.component(PlaceHolderKey.TEXT.getKey(), eTitle),
                                            Placeholder.component(PlaceHolderKey.UNLIMITED.getKey(), plugin.getMessageManager().getLang(LangPath.BOOLEAN_FALSE))
                                        ));
                                } else {
                                    plugin.getMessageManager().sendLang(ePlayer, LangPath.ACTION_FIND_LIMITED);
                                }
                            }

                            final InventoryView view = ePlayer.openInventory(nowLooting);
                            if (view != null) {
                                openInventories.computeIfAbsent(treasureInfo.treasureId(), ignored -> new HashSet<>()).add(view);
                            }
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

    /**
     * in case a treasure is about to be broken cancel the event.
     * to get rid of a treasure use /gt delete
     */
    @EventHandler
    private void onTreasureBreak(final @NotNull BlockBreakEvent event) {
        if (event.getBlock().getState(false) instanceof TileStateInventoryHolder inventoryHolder) {
            final @Nullable TreasureInfo treasureInfo;

            // double chests are wierd.
            if (Utils.getTreasureHolder(inventoryHolder.getInventory().getHolder()) instanceof PersistentDataHolder persistentDataHolder) {
                treasureInfo = plugin.getTreasureManager().getTreasureInfo(persistentDataHolder);

                if (treasureInfo != null) {
                    if (new TreasureBreakEvent(event.getBlock(), event.getPlayer()).callEvent()) {
                        event.setCancelled(true);

                        Player ePlayer = event.getPlayer();
                        if (ePlayer.hasPermission(PermissionManager.TREASURE_DELETE.get())) {
                            plugin.getTreasureManager().deleteTreasure(persistentDataHolder).thenAccept(success ->
                                plugin.getMessageManager().sendLang(ePlayer, LangPath.ACTION_BREAK_CONTAINER_SUCCESS,
                                    Placeholder.component(PlaceHolderKey.NAME.getKey(),
                                        Utils.getDisplayName((Container) inventoryHolder.getInventory().getHolder())),
                                    Placeholder.component(PlaceHolderKey.BOOL.getKey(),
                                        plugin.getMessageManager().getLang(success ? LangPath.BOOLEAN_TRUE : LangPath.BOOLEAN_FALSE)))
                            );
                        } else {
                            plugin.getMessageManager().sendLang(ePlayer, LangPath.ACTION_BREAK_CONTAINER_DENIED);
                        }
                    }
                }
            }
        }
    }
}
