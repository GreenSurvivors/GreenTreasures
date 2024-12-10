package de.greensurvivors.greentreasure.event;

import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import org.bukkit.entity.Player;
import org.bukkit.event.Cancellable;
import org.bukkit.event.HandlerList;
import org.bukkit.event.player.PlayerEvent;
import org.jetbrains.annotations.NotNull;

/**
 * called if a player opens a treasure
 * Note: this Event doesn't include PlayerLootDetail, since it's loaded async,
 * and therefore we couldn't cancel the open-Inventory-event afterward.
 * If you need the info, you have to call TreasureConfig.inst().getPlayerLootDetail() yourself.
 */
public class TreasureOpenEvent extends PlayerEvent implements Cancellable {
    private static final @NotNull HandlerList handlers = new HandlerList();
    private final @NotNull TreasureInfo treasureInfo;
    private final boolean hasPermission;
    private @NotNull Result result = Result.DEFAULT;
    public TreasureOpenEvent(final @NotNull Player who, final @NotNull TreasureInfo what, final boolean hasPermission) {
        super(who);

        this.treasureInfo = what;
        this.hasPermission = hasPermission;
    }

    public @NotNull TreasureInfo getTreasureInfo() {
        return treasureInfo;
    }

    @Override
    public @NotNull HandlerList getHandlers() {
        return handlers;
    }

    public @NotNull Result getResult() {
        return this.result;
    }

    public void setResult(@NotNull Result result) {
        this.result = result;
    }

    @Deprecated
    @Override
    public boolean isCancelled() {
        return result == Result.CANCELED;
    }

    @Deprecated
    @Override
    public void setCancelled(boolean isCanceled) {
        this.result = Result.CANCELED;
    }

    public boolean hasPermission() {
        return hasPermission;
    }

    public enum Result {
        /// default means the original open-inventory-event gets canceled
        DEFAULT,
        /// original means the event gets canceled and the original open-inventory-event not
        ORIGINAL,
        /// canceled means the event as well as the original open-inventory-event is canceled
        CANCELED
    }
}
