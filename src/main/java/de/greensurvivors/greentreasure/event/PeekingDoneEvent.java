package de.greensurvivors.greentreasure.event;

import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import org.bukkit.entity.Player;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.UUID;

public class PeekingDoneEvent extends TreasureCloseEvent {
    private final static HandlerList handlers = new HandlerList();
    private final @Nullable UUID peekedUUID;

    /**
     * @param who          the player who peeked a treasure
     * @param treasureInfo the information about a treasure
     */
    public PeekingDoneEvent(@NotNull Player who, @NotNull TreasureInfo treasureInfo, @Nullable UUID peekedUUID) {
        super(who, treasureInfo);
        this.peekedUUID = peekedUUID;
    }

    @Override
    public @NotNull HandlerList getHandlers() {
        return handlers;
    }

    public static @NotNull HandlerList getHandlerList() {
        return handlers;
    }

    public @Nullable UUID getPeekedUUID() {
        return peekedUUID;
    }
}
