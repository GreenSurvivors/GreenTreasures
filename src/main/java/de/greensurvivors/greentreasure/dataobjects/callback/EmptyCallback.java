package de.greensurvivors.greentreasure.dataobjects.callback;

/**
 * technical interface to give information form async back to the main thread via the function "onQueryDone"
 */
public interface EmptyCallback {
    void onQueryDone();
}
