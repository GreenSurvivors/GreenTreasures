package de.greensurvivors.greentreasure.dataobjects.refreshInfo;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import de.greensurvivors.greentreasure.language.LangKey;
import de.greensurvivors.greentreasure.language.MessageManager;
import de.greensurvivors.greentreasure.language.PlaceHolder;
import net.kyori.adventure.text.Component;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.time.Duration;
import java.time.Instant;
import java.util.Objects;

public class PeriodicForget extends ARefreshInfo {
    protected final @NotNull Duration duration;

    public PeriodicForget(final @NotNull GreenTreasure plugin, final @NotNull Duration duration) {
        super(plugin);
        this.duration = duration;
    }

    @Override
    public @NotNull Component infoMessage() {
        return LangKey.REFRESH_MSG_INFO_PERIODIC.create(
            PlaceHolder.TIME.component(MessageManager.formatDuration(duration)));
    }

    @Override
    public boolean canOpenFresh(final @NotNull PlayerLootDetail lootDetail) {
        return Duration.between(lootDetail.firstLootedInstant(), Instant.now()).compareTo(duration) >= 0;
    }

    @Override
    @Contract("null -> null")
    public @Nullable Duration getTimeUntilFresh(final @Nullable PlayerLootDetail lootDetail) {
        if (lootDetail == null) {
            return null;
        } else {
            return duration.minus(Duration.between(Instant.now(), lootDetail.firstLootedInstant()));
        }
    }

    @Override
    public int hashCode() {
        return Objects.hash(duration);
    }

    @Override
    public boolean equals(final @Nullable Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        final @NotNull de.greensurvivors.greentreasure.dataobjects.refreshInfo.PeriodicForget that = (de.greensurvivors.greentreasure.dataobjects.refreshInfo.PeriodicForget) obj;

        return Objects.equals(this.duration, that.duration);
    }

    @Override
    public String toString() {
        return "DurationForget[" + duration + "]";
    }
}
