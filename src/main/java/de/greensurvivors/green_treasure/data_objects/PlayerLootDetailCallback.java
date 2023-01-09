package de.greensurvivors.green_treasure.data_objects;

/**
 * technical interface to give information form async back to the main thread via the function "onQueryDone"
 */
public interface PlayerLootDetailCallback {
    public void onQueryDone(PlayerLootDetail result);
}
