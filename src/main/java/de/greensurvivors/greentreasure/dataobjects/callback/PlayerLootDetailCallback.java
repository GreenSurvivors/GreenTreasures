package de.greensurvivors.greentreasure.dataobjects.callback;

import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;

/**
 * technical interface to give information form async back to the main thread via the function "onQueryDone"
 */
public interface PlayerLootDetailCallback {
    public void onQueryDone(PlayerLootDetail result);
}
