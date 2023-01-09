package de.greensurvivors.green_treasure.data_objects;

import org.bukkit.Location;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.UUID;

/**
 * holds the needed information if a player is peeking a treasure
 * @param playerPeeked the uuid of the player whose treasure inventory is shown or null if the treasure is global.
 * @param locationOfTreasure the location of the treasure, imported to identify it.
 * @param timeStamp the last time stamp a player has opened the treasure. Peeking doesn't overwrite it, but saves the inventory contents
 */
public record PeekedTreasure(@Nullable UUID playerPeeked, @NotNull Location locationOfTreasure, long timeStamp) {
}
