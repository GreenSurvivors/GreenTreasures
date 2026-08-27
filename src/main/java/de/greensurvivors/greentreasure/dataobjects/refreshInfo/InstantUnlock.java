package de.greensurvivors.greentreasure.dataobjects.refreshInfo;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import de.greensurvivors.greentreasure.language.LangKey;
import de.greensurvivors.greentreasure.language.PlaceHolder;
import net.kyori.adventure.text.Component;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.time.Duration;
import java.time.Instant;

// technically this is a misuse of ARefreshInfo. But at the same time I can't think of any use of a one time refresh.
public class InstantUnlock extends ARefreshInfo {
    protected final @NotNull Instant start;

    public InstantUnlock(final @NotNull GreenTreasure plugin, final @NotNull Instant start) {
        super(plugin);
        this.start = start;
    }

    @Override
    public @NotNull Component infoMessage() {
        return LangKey.REFRESH_MSG_INFO_INSTANT_UNLOCK.create(
            PlaceHolder.START.temporal(start));
    }

    @Override
    public boolean canOpenFresh(final @Nullable PlayerLootDetail lootDetail) {
        return Instant.now().isAfter(start);
    }

    @Override
    public @Nullable Duration getTimeUntilFresh(@Nullable PlayerLootDetail lootDetail) {
        return Duration.between(Instant.now(), start);
    }
}
