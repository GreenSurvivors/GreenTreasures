package de.greensurvivors.greentreasure.listener;

import com.github.f4b6a3.ulid.Ulid;
import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermissionManager;
import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.comands.MainCommand;
import de.greensurvivors.greentreasure.config.TreasureConfig;
import de.greensurvivors.greentreasure.dataobjects.InventoryHolderWrapper;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.event.TreasureBreakEvent;
import de.greensurvivors.greentreasure.event.TreasureCloseEvent;
import de.greensurvivors.greentreasure.event.TreasureOpenEvent;
import de.greensurvivors.greentreasure.language.LangKey;
import de.greensurvivors.greentreasure.language.MessageManager;
import de.greensurvivors.greentreasure.language.PlaceHolder;
import io.papermc.paper.block.TileStateInventoryHolder;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.event.ClickEvent;
import net.kyori.adventure.text.minimessage.MiniMessage;
import net.kyori.adventure.text.minimessage.tag.resolver.Formatter;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
import net.kyori.adventure.text.minimessage.tag.resolver.TagResolver;
import net.kyori.adventure.text.minimessage.translation.Argument;
import org.bukkit.Bukkit;
import org.bukkit.block.Container;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.block.BlockBreakEvent;
import org.bukkit.event.inventory.*;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryHolder;
import org.bukkit.inventory.InventoryView;
import org.bukkit.inventory.ItemStack;
import org.bukkit.persistence.PersistentDataHolder;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.time.Duration;
import java.time.Instant;
import java.util.*;

public class TreasureListener implements Listener {
    //list of open inventories, needed to have shared treasures and saving the contents of a treasure after the inventory view was closed
    private final @NotNull Map<@NotNull Ulid, @NotNull Set<@NotNull InventoryView>> openInventories = new HashMap<>();
    private final @NotNull GreenTreasure plugin;

    public TreasureListener(final @NotNull GreenTreasure plugin) {
        this.plugin = plugin;

        Bukkit.getPluginManager().registerEvents(this, plugin);
    }

    public void closeInventories(final @NotNull Ulid treasureId) {
        final @Nullable Collection<@NotNull InventoryView> views = openInventories.remove(treasureId);

        if (views != null) {
            views.forEach(InventoryView::close);
        }
        openInventories.remove(treasureId);
    }

    /**
     * closes all open inventories to update them
     */
    public void closeAllInventories() {
        for (final @NotNull Collection<@NotNull InventoryView> views : openInventories.values()) {
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
            if (event.getView().getTopInventory().getHolder(false) instanceof InventoryHolderWrapper<?> wrapper) {
                if (wrapper.isPerCommand()) {
                    return;
                }

                final @Nullable Ulid treasureId = plugin.getTreasureManager().getTreasureId(event.getView());
                if (treasureId != null) {
                    TreasureInfo treasureInfo = plugin.getTreasureManager().getTreasureInfoUrgently(treasureId);

                    //if the treasure wasn't deleted while the inventory was open call the close event
                    if (treasureInfo != null) {
                        new TreasureCloseEvent((Player) event.getPlayer(), treasureInfo).callEvent();

                        final @Nullable Collection<@NotNull InventoryView> views = openInventories.get(treasureId);
                        if (views != null) {
                            views.remove(event.getView());

                            if (views.isEmpty()) {
                                openInventories.remove(treasureId);
                            }
                        }

                        // shut up, if a stack is null it will get mapped to an empty stack instead
                        //noinspection NullableProblems
                        plugin.getDataAccessor().setPlayerData(treasureInfo.isShared() ? null : ePlayer, treasureId,
                            new PlayerLootDetail(
                                wrapper.getFistLootedInstant(),
                                Instant.now(),
                                Arrays.stream(eInventory.getContents())
                                    .map(stack -> Objects.requireNonNullElseGet(stack, ItemStack::empty))
                                    .toList()
                            )
                        );
                    }
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
        if (event.getPlayer() instanceof Player player) {
            final @NotNull Inventory eInventory = event.getInventory();
            final @Nullable TreasureInfo treasureInfo = plugin.getTreasureManager().getTreasureInfoUrgently(plugin.getTreasureManager().getTreasureId(event.getView()));

            if (treasureInfo != null) {
                final @Nullable Collection<@NotNull InventoryView> views = openInventories.get(treasureInfo.treasureId());

                // we will cancel this event and open a new inventory, retriggering this event.
                // ignore them as well as the views created by our commands
                if (eInventory.getHolder(false) instanceof InventoryHolderWrapper<?>) {
                    return;
                }
                // everything should get sorted out above, but just to be sure, ignore all already tracked views
                // should basically never happen.
                if (views != null && views.contains(event.getView())) {
                    return;
                }

                //test permission
                if (player.hasPermission(PermissionManager.TREASURE_OPEN.get())) {
                    if (treasureInfo.isUnlocked()) {
                        final @NotNull Component eTitle = event.getView().title();

                        // call api event: TreasureOpenEvent
                        TreasureOpenEvent treasureOpenEvent = new TreasureOpenEvent(player, treasureInfo, true);
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
                            final @Nullable InventoryView inventoryView;
                            if (views == null || views.isEmpty()) {
                                inventoryView = null;
                            } else {
                                inventoryView = views.iterator().next();
                            }

                            if (inventoryView != null && !treasureInfo.isUnlimited()) {
                                // shared and already open inventory with our custom owner and with limited stock.
                                // Just share the inventory to keep it sync across all players
                                player.openInventory(inventoryView.getTopInventory());
                            } else if (treasureInfo.isUnlimited()) {
                                handleTreasureOpen(player, treasureInfo, eInventory, eTitle, null);
                            } else {
                                //load global treasure async
                                plugin.getDataAccessor().getPlayerData(null, treasureInfo.treasureId()).thenAccept(playerLootDetail ->
                                    handleTreasureOpen(player, treasureInfo, eInventory, eTitle, playerLootDetail));
                            }
                        } else { // not globally shared
                            plugin.getDataAccessor().getPlayerData(player, treasureInfo.treasureId()).thenAccept(playerLootDetail ->
                                handleTreasureOpen(player, treasureInfo, eInventory, eTitle, playerLootDetail));
                        }
                    } else {
                        TreasureOpenEvent treasureOpenEvent = new TreasureOpenEvent(player, treasureInfo, true);
                        treasureOpenEvent.callEvent();

                        switch (treasureOpenEvent.getResult()) {
                            case DEFAULT -> {
                                plugin.getMessageManager().sendPrefixed(player, LangKey.ACTION_FIND_LOCKED.create(
                                    PlaceHolder.TEXT.component(event.getView().title()),
                                    PlaceHolder.TIME.component(
                                        MessageManager.formatDuration(treasureInfo.getRefreshInfo().getTimeUntilFresh(null)))
                                ));
                                // don't open the original block inventory
                                event.setCancelled(true);
                            }
                            case ORIGINAL -> {
                            }
                            case CANCELED -> event.setCancelled(true);
                        }
                    }
                } else {
                    TreasureOpenEvent treasureOpenEvent = new TreasureOpenEvent(player, treasureInfo, false);
                    treasureOpenEvent.callEvent();

                    switch (treasureOpenEvent.getResult()) {
                        case DEFAULT -> {
                            plugin.getMessageManager().sendPrefixed(player, LangKey.NO_PERMISSION);
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

    private void handleTreasureOpen(final @NotNull Player player, final @NotNull TreasureInfo treasureInfo,
                                    final @NotNull Inventory inventory, final @NotNull Component eTitle,
                                    final @Nullable PlayerLootDetail playerLootDetail) {
        final @NotNull Inventory nowLooting;

        if ( // never opened or unexpected empty
            (playerLootDetail == null || playerLootDetail.unLootedStuff() == null) ||
                // unlimited treasure
                treasureInfo.isUnlimited() ||
                // automatically forget after a given time
                treasureInfo.getRefreshInfo().canOpenFresh(playerLootDetail)) {

            nowLooting = createInventory(inventory, eTitle, null);
            Utils.setContents(nowLooting, treasureInfo.itemLoot(), treasureInfo.nonEmptyPermyriad());

            sendLootMessage(player, treasureInfo, eTitle, null, true);
        } else {
            nowLooting = createInventory(inventory, eTitle, playerLootDetail);
            // get items left there last time
            Utils.setContents(nowLooting, playerLootDetail.unLootedStuff());

            sendLootMessage(player, treasureInfo, eTitle, playerLootDetail, false);
        }

        final InventoryView view = player.openInventory(nowLooting);
        if (view != null) {
            openInventories.computeIfAbsent(treasureInfo.treasureId(), ignored -> new HashSet<>()).add(view);
        }
    }

    private static @NotNull Inventory createInventory(final @NotNull Inventory eInventory, final @NotNull Component title,
                                                      final @Nullable PlayerLootDetail playerLootDetail) {
        final @NotNull InventoryHolderWrapper<?> owner = new InventoryHolderWrapper<>(
            (InventoryHolder & PersistentDataHolder) Utils.getTreasureHolder(eInventory.getHolder(false)), false,
            playerLootDetail != null ? playerLootDetail.firstLootedInstant() : null
        );

        if (eInventory.getType() == InventoryType.CHEST) {
            return Bukkit.createInventory(owner, eInventory.getSize(), title);
        } else {
            return Bukkit.createInventory(owner, eInventory.getType(), title);
        }
    }

    private void sendLootMessage(final @NotNull Player player, final @NotNull TreasureInfo treasureInfo, final @NotNull Component title,
                                 final @Nullable PlayerLootDetail playerLootDetail, final boolean isFresh) {
        final @Nullable String messageOverride = isFresh ? treasureInfo.rawFindFreshMessageOverride() : treasureInfo.rawFindLootedMessageOverride();

        if (messageOverride != null) {
            plugin.getMessageManager().sendPrefixed(player, MiniMessage.miniMessage().deserialize(messageOverride,
                getTagResolvers(player, treasureInfo, title, playerLootDetail)));
        } else if (!isFresh) { // already looted limited
            plugin.getMessageManager().sendPrefixed(player, LangKey.ACTION_FIND_ALREADY_LOOTED.create(
                Argument.tagResolver(getTagResolvers(player, treasureInfo, title, playerLootDetail))));
        } else {
            if (treasureInfo.isUnlimited()) {
                plugin.getMessageManager().sendPrefixed(player, LangKey.ACTION_FIND_UNLIMITED);
            } else {
                plugin.getMessageManager().sendPrefixed(player, LangKey.ACTION_FIND_LIMITED);
            }
        }
    }

    private static @NotNull TagResolver @NotNull [] getTagResolvers(final @NotNull Player player, final @NotNull TreasureInfo treasureInfo, final @NotNull Component title,
                                                                    final @Nullable PlayerLootDetail playerLootDetail) {
        final @NotNull TagResolver @NotNull [] resolvers;
        final @Nullable Duration timeUntilFresh = treasureInfo.getRefreshInfo().getTimeUntilFresh(playerLootDetail);
        // add time until forget if we got one
        if ((playerLootDetail == null && treasureInfo.isUnlocked()) || treasureInfo.isUnlimited() || timeUntilFresh == null) {
            resolvers = new TagResolver[3];
        } else {
            resolvers = new TagResolver[4];
            resolvers[3] = Placeholder.component(PlaceHolder.TIME.getKey(), MessageManager.formatDuration(timeUntilFresh));
        }

        resolvers[0] = Placeholder.component(PlaceHolder.PLAYER.getKey(), player.displayName());
        resolvers[1] = Placeholder.component(PlaceHolder.TEXT.getKey(), title);
        resolvers[2] = Formatter.booleanChoice(PlaceHolder.UNLIMITED.getKey(), treasureInfo.isUnlimited());

        return resolvers;
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
            if (Utils.getTreasureHolder(inventoryHolder.getInventory().getHolder(false)) instanceof PersistentDataHolder persistentDataHolder) {
                treasureInfo = plugin.getTreasureManager().getTreasureInfoUrgently(plugin.getTreasureManager().getTreasureId(persistentDataHolder));

                if (treasureInfo != null) {
                    final @NotNull Player ePlayer = event.getPlayer();

                    // java being java again. I easily could do this in the switch, but then java thinks it would require a default branch,
                    // and I don't want a default branch. I want the compiler to scream at me, when (if ever) any other BreakBehaviors get added!
                    // Not yet because there could any get added somewhere in the future. The switch is always exclusive, shut up!
                    if (plugin.getConfigHandler().getBreakBehavior() == TreasureConfig.BreakBehavior.ONLY_SHIFT_BREAKS_GLOBAL) {
                        if (!ePlayer.isSneaking()) {
                            return;
                        }
                    }

                    final boolean isGlobal = switch (plugin.getConfigHandler().getBreakBehavior()) {
                        case SHIFT_BREAKS_LOCAL -> !ePlayer.isSneaking();
                        case ALL_BREAK_GLOBAL, ONLY_SHIFT_BREAKS_GLOBAL ->  true;
                    };
                    final boolean hasPermission = ePlayer.hasPermission(PermissionManager.TREASURE_DELETE.get());

                    final TreasureBreakEvent treasureBreakEvent = new TreasureBreakEvent(event.getBlock(), event.getPlayer(), isGlobal);
                    treasureBreakEvent.setCancelled(!hasPermission);

                    if (treasureBreakEvent.callEvent() || !hasPermission) {
                        event.setCancelled(true);

                        if (hasPermission) {
                            if (isGlobal) {
                                plugin.getTreasureManager().deleteTreasure(persistentDataHolder).thenAccept(success -> {
                                    if (success) {
                                        plugin.getMessageManager().sendPrefixed(ePlayer, LangKey.REMOVE_GLOBAL_SUCCESS.create(
                                            PlaceHolder.TREASURE_ID.component(
                                                Utils.getDisplayName((Container) persistentDataHolder))));
                                    } else {
                                        final @NotNull String command = "/" + MainCommand.CMD + " " + plugin.getMainCommand().getDeleteSubCmd().getAliases().iterator().next();
                                        plugin.getMessageManager().sendPrefixed(ePlayer, LangKey.REMOVE_ERROR.create(
                                            PlaceHolder.TREASURE_ID.component(
                                                Utils.getDisplayName((Container) persistentDataHolder)),
                                            PlaceHolder.CMD.component(
                                                Component.text()
                                                    .content(command)
                                                    .clickEvent(ClickEvent.suggestCommand(command))
                                            )
                                        ));
                                    }
                                });
                            } else {
                                if (plugin.getTreasureManager().deleteTreasureLocal(persistentDataHolder)) {
                                    plugin.getMessageManager().sendPrefixed(ePlayer, LangKey.REMOVE_LOCAL_SUCCESS.create(
                                        PlaceHolder.TREASURE_ID.component(
                                            Utils.getDisplayName((Container) persistentDataHolder))));
                                }  else {
                                    final @NotNull String command = "/" + MainCommand.CMD + " " + plugin.getMainCommand().getDeleteSubCmd().getAliases().iterator().next();
                                    plugin.getMessageManager().sendPrefixed(ePlayer, LangKey.REMOVE_ERROR.create(
                                        PlaceHolder.TREASURE_ID.component(
                                            Utils.getDisplayName((Container) persistentDataHolder)),
                                        PlaceHolder.CMD.component(
                                            Component.text().
                                                content(command).
                                                clickEvent(ClickEvent.suggestCommand(command))
                                        )
                                    ));
                                }
                            }
                        } else {
                            plugin.getMessageManager().sendPrefixed(ePlayer, LangKey.ACTION_REMOVE_DENIED);
                        }
                    }
                }
            }
        }
    }
}
