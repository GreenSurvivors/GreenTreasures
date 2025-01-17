package de.greensurvivors.greentreasure.listener;

import com.github.f4b6a3.ulid.Ulid;
import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.dataobjects.PeekedTreasure;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.event.PeekingDoneEvent;
import de.greensurvivors.greentreasure.language.LangPath;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.inventory.InventoryCloseEvent;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryView;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 * this is a technical class to handle the inventories opened by commands
 */
public class CommandInventoriesListener implements Listener {
    private final @NotNull GreenTreasure plugin;

    private final Map<@NotNull InventoryView, @NotNull Ulid> editingTreasures = new HashMap<>();
    private final Map<@NotNull InventoryView, @NotNull PeekedTreasure> peekingTreasures = new HashMap<>();

    public CommandInventoriesListener(final @NotNull GreenTreasure plugin) {
        this.plugin = plugin;

        Bukkit.getPluginManager().registerEvents(this, plugin);
    }

    /**
     * add an inventory view to the list of actively edited inventory's
     * note: two 2 players editing the same inventory at the same time is NOT supported right now
     *
     * @param editingView the view given back by player.openInventory when /gt edit was called
     */
    public void addEditingTreasure(final @NotNull InventoryView editingView, final @NotNull Ulid treasureId) {
        editingTreasures.put(editingView, treasureId);
    }

    /**
     * add an inventory view to the list of actively peeked player-treasure inventory's
     *
     * @param peekingView    the view given back by player.openInventory when /gt peek was called
     * @param peekedTreasure location to identify the treasure
     */
    public void addPeekingTreasure(final @NotNull InventoryView peekingView, final @NotNull PeekedTreasure peekedTreasure) {
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
    private void onEditingDone(final @NotNull InventoryCloseEvent event) {
        Inventory eInventory = event.getInventory();
        final @Nullable Ulid treasureId = editingTreasures.get(event.getView());

        if (treasureId != null) {
            TreasureInfo treasureInfo = plugin.getTreasureManager().getTreasureInfo(treasureId);

            //if the treasure wasn't deleted while the inventory was open
            if (treasureInfo != null) {
                editingTreasures.remove(event.getView());

                // the IDE is confused with two annotations
                //noinspection NullableProblems
                plugin.getDatabaseManager().setTreasureContents(treasureId, Arrays.asList(eInventory.getContents())).
                    thenRun(() -> plugin.getMessageManager().sendLang(event.getPlayer(), LangPath.ACTION_TREASURE_EDITED));
            } else {
                plugin.getMessageManager().sendLang(event.getPlayer(), LangPath.ERROR_UNKNOWN);
            }
        }
    }

    /**
     * saves the new contents of a treasure inventory to a player file if a peeking inventory view was closed
     */
    @EventHandler(priority = EventPriority.MONITOR)
    private void onPeekingDone(final @NotNull InventoryCloseEvent event) {
        PeekedTreasure peekedTreasure = peekingTreasures.get(event.getView());

        if (peekedTreasure != null) {
            Inventory eInventory = event.getInventory();
            final @NotNull Ulid treasureId = peekedTreasure.treasureId();

            final @Nullable TreasureInfo treasureInfo = plugin.getTreasureManager().getTreasureInfo(treasureId);

            //if the treasure wasn't deleted while the inventory was open call the close event
            if (treasureInfo != null) {
                new PeekingDoneEvent((Player) event.getPlayer(), treasureInfo, peekedTreasure.playerPeekedUUID()).callEvent();

                peekingTreasures.remove(event.getView());
                if (treasureInfo.isShared() || peekedTreasure.playerPeekedUUID() == null) {
                    plugin.getDatabaseManager().setPlayerData(null, treasureId,
                        new PlayerLootDetail(peekedTreasure.timeStamp(), Arrays.stream(eInventory.getContents()).map(s -> s == null ? ItemStack.empty() : s).toList()));
                } else {
                    plugin.getDatabaseManager().setPlayerData(Bukkit.getOfflinePlayer(peekedTreasure.playerPeekedUUID()), treasureId,
                        new PlayerLootDetail(peekedTreasure.timeStamp(), Arrays.stream(eInventory.getContents()).map(s -> s == null ? ItemStack.empty() : s).toList()));
                }
            }
        }
    }
}
