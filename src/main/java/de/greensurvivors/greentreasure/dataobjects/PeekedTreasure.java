package de.greensurvivors.greentreasure.dataobjects;

import com.github.f4b6a3.ulid.Ulid;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.UUID;

/**
 * holds the needed information if a player is peeking a treasure
 *
 * @param playerPeekedUUID the uuid of the player whose treasure inventory is shown or null if the treasure is shared.
 * @param timeStamp        the last time stamp a player has opened the treasure. Peeking doesn't overwrite it, but saves the inventory contents
 */
public record PeekedTreasure(@Nullable UUID playerPeekedUUID, @NotNull Ulid treasureId, long timeStamp) {
}
