package de.greensurvivors.greentreasure.dataobjects.callback;

import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import org.jetbrains.annotations.Nullable;

/**
 * technical interface to give information form async back to the main thread via the function "onQueryDone"
 */
public interface PlayerLootDetailCallback {
    void onQueryDone(@Nullable PlayerLootDetail result);
}
