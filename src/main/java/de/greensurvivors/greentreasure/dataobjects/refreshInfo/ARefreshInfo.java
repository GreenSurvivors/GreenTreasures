package de.greensurvivors.greentreasure.dataobjects.refreshInfo;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import net.kyori.adventure.text.Component;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.time.Duration;

public abstract class ARefreshInfo {
    protected final @NotNull GreenTreasure plugin;

    protected ARefreshInfo(final @NotNull GreenTreasure plugin) {
        this.plugin = plugin;
    }

    public abstract @NotNull Component infoMessage();

    public abstract boolean canOpenFresh(final @NotNull PlayerLootDetail lootDetail);

    public abstract @Nullable Duration getTimeUntilFresh(final @Nullable PlayerLootDetail lootDetail);
}
