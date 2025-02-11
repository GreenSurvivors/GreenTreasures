package de.greensurvivors.greentreasure.event;

import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import org.bukkit.entity.Player;
import org.bukkit.event.HandlerList;
import org.bukkit.event.player.PlayerEvent;
import org.jetbrains.annotations.NotNull;

/**
 * called if a treasure inventory view is closed
 */
public class TreasureCloseEvent extends PlayerEvent {
    private final static HandlerList handlers = new HandlerList();
    private final @NotNull TreasureInfo treasureInfo;

    /**
     * @param who          the player who closes a treasure
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

    public static @NotNull HandlerList getHandlerList() {
        return handlers;
    }

    public @NotNull TreasureInfo getTreasureInfo() {
        return treasureInfo;
    }
}
