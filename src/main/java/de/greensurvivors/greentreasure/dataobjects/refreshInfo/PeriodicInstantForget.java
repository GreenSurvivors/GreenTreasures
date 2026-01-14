package de.greensurvivors.greentreasure.dataobjects.refreshInfo;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.MessageManager;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.time.Duration;
import java.time.Instant;
import java.util.Objects;

public class PeriodicInstantForget extends ARefreshInfo {
    protected final @NotNull Instant start;
    protected final @NotNull Duration forgetDuration;

    public PeriodicInstantForget(final @NotNull GreenTreasure plugin, final @NotNull Instant start, final @NotNull Duration forgetDuration) {
        super(plugin);
        this.start = start;
        this.forgetDuration = forgetDuration;
    }

    @Override
    public @NotNull Component infoMessage() {
        return plugin.getMessageManager().getLang(LangPath.REFRESH_MSG_INFO_PERIODIC_INSTANT,
            Placeholder.unparsed(PlaceHolderKey.START.getKey(), plugin.getMessageManager().formatTime(start)),
            Placeholder.component(PlaceHolderKey.TIME.getKey(), MessageManager.formatDuration(forgetDuration)));
    }

    @Override
    public boolean canOpenFresh(final @NotNull PlayerLootDetail lootDetail) {
        if (Duration.between(start, lootDetail.firstLootedInstant()).isNegative()) {
            return true;
        }

        return (lootDetail.firstLootedInstant().compareTo(calcLastResetInstant()) <= 0);
    }

    protected @NotNull Instant calcLastResetInstant() { // todo better calculation
        final @NotNull Duration durationStartNow = Duration.between(start, Instant.now());
        final long mod = durationStartNow.dividedBy(forgetDuration);

        return start.plus(forgetDuration.multipliedBy(mod));
    }

    @Override
    @Contract("null -> null")
    public @Nullable Duration getTimeUntilFresh(final @Nullable PlayerLootDetail lootDetail) {
        if (lootDetail == null) {
            return null;
        } else {
            if (Duration.between(start, lootDetail.firstLootedInstant()).isNegative()) {
                return Duration.ZERO;
            } else {
                final @NotNull Instant lastResetInstant = calcLastResetInstant();

                if (lootDetail.firstLootedInstant().compareTo(calcLastResetInstant()) <= 0) {
                    return Duration.ZERO;
                } else {
                    return Duration.between(Instant.now(), lastResetInstant.plus(forgetDuration));
                }
            }
        }
    }

    @Override
    public int hashCode() {
        return Objects.hash(start, forgetDuration);
    }

    @Override
    public boolean equals(final @Nullable Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        final @NotNull de.greensurvivors.greentreasure.dataobjects.refreshInfo.PeriodicInstantForget that = (de.greensurvivors.greentreasure.dataobjects.refreshInfo.PeriodicInstantForget) obj;

        return Objects.equals(this.start, that.start) && Objects.equals(this.forgetDuration, that.forgetDuration);
    }

    @Override
    public String toString() {
        return "TimedForget[" +
            "start:" + plugin.getMessageManager().formatTime(start) +
            "duration:" + forgetDuration + "]";
    }
}
