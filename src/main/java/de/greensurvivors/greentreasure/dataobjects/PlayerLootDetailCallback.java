package de.greensurvivors.greentreasure.dataobjects;

/**
 * technical interface to give information form async back to the main thread via the function "onQueryDone"
 */
public interface PlayerLootDetailCallback {
    public void onQueryDone(PlayerLootDetail result);
}
