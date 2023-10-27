package de.greensurvivors.greentreasure.dataobjects.callback;

import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import org.jetbrains.annotations.NotNull;

import java.util.Map;
import java.util.UUID;

/**
 * technical interface to give information form async back to the main thread via the function "onQueryDone"
 */
public interface PlayerLootDetailMapCallback {
    void onQueryDone(@NotNull Map<UUID, PlayerLootDetail> result);
}
