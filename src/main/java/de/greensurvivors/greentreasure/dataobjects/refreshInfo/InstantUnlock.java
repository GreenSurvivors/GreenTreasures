package de.greensurvivors.greentreasure.dataobjects.refreshInfo;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
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
        return plugin.getMessageManager().getLang(LangPath.REFRESH_MSG_INFO_INSTANT_UNLOCK,
            Placeholder.unparsed(PlaceHolderKey.START.getKey(), plugin.getMessageManager().formatTime(start)));
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
