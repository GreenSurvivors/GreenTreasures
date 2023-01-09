package de.greensurvivors.green_treasure.event;

import de.greensurvivors.green_treasure.data_objects.TreasureInfo;
import org.bukkit.entity.Player;
import org.bukkit.event.HandlerList;
import org.bukkit.event.player.PlayerEvent;
import org.jetbrains.annotations.NotNull;

/**
 * called if a treasure inventory view is closed
 */
public class TreasureCloseEvent extends PlayerEvent {
    private final HandlerList handlers = new HandlerList();
    private final TreasureInfo treasureInfo;

    /**
     * @param who the player who closes a treasure
     * @param treasureInfo the information about a treasure
     */
    public TreasureCloseEvent(@NotNull Player who, @NotNull TreasureInfo treasureInfo) {
        super(who);

        this.treasureInfo = treasureInfo;
    }

    @Override
    public @NotNull HandlerList getHandlers() {
        return handlers;
    }

    public TreasureInfo getTreasureInfo() {
        return treasureInfo;
    }
}
