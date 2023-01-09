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
 * and therefore we couldn't cancel the open-Inventory-event afterwards.
 * If you need the info, you have to call TreasureConfig.inst().getPlayerLootDetail() yourself.
 */
public class TreasureOpenEvent extends PlayerEvent implements Cancellable {
    private static final HandlerList handlers = new HandlerList();
    private Result result = Result.DEFAULT;
    private final TreasureInfo treasureInfo;

    public enum Result {
        /**
         * default means the original open-inventory-event gets canceled
         */
        DEFAULT,
        /**
         * original means the event gets canceled and the original open-inventory-event not
         */
        ORIGINAL,
        /**
         * canceled means the event as well as the original open-inventory-event is canceled
         */
        CANCELED
    }

    public TreasureOpenEvent(@NotNull Player who, @NotNull TreasureInfo what) {
        super(who);

        this.treasureInfo = what;
    }

    public @NotNull TreasureInfo getTreasureInfo() {
        return treasureInfo;
    }

    @Override
    public @NotNull HandlerList getHandlers() {
        return handlers;
    }

    public Result getResult(){
        return this.result;
    }

    public void setResult(Result result){
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
}
