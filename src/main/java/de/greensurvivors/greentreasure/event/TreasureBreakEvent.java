package de.greensurvivors.greentreasure.event;

import org.bukkit.block.Block;
import org.bukkit.entity.Player;
import org.bukkit.event.Cancellable;
import org.bukkit.event.HandlerList;
import org.bukkit.event.block.BlockEvent;
import org.jetbrains.annotations.NotNull;

/**
 * called if a player tries to break a treasure Block.
 * canceling this event means the original block break event gets NOT canceled.
 * if the player doesn't have the required permission to break the block, this event will get called in a canceled state
 */
@SuppressWarnings("unused") // used as api
public class TreasureBreakEvent extends BlockEvent implements Cancellable {
    private static final @NotNull HandlerList handlers = new HandlerList();
    private final @NotNull Player player;
    private final boolean breaksGlobal;
    private boolean cancel;

    /**
     * @param theBlock the treasure block a player tries to break
     * @param player   the player in action
     */
    public TreasureBreakEvent(final @NotNull Block theBlock, final @NotNull Player player, final boolean breaksGlobal) {
        super(theBlock);
        this.player = player;
        this.breaksGlobal = breaksGlobal;
    }

    @Override
    public @NotNull HandlerList getHandlers() {
        return handlers;
    }

    public static @NotNull HandlerList getHandlerList() {
        return handlers;
    }

    /**
     * Gets the Player that is breaking the block involved in this event.
     *
     * @return The Player that is breaking the block involved in this event
     */
    public @NotNull Player getPlayer() {
        return player;
    }

    @Override
    public boolean isCancelled() {
        return cancel;
    }

    @Override
    public void setCancelled(boolean cancel) {
        this.cancel = cancel;
    }

    public boolean breaksGlobal() {
        return breaksGlobal;
    }
}
