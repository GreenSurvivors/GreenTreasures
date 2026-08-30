package de.greensurvivors.greentreasure.dataobjects.refreshInfo;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import de.greensurvivors.greentreasure.language.LangKey;
import net.kyori.adventure.text.Component;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.time.Duration;

public class NoForget extends ARefreshInfo {
    public NoForget(final @NotNull GreenTreasure plugin) {
        super(plugin);
    }

    @Override
    public @NotNull Component infoMessage() { // unused
        return LangKey.REFRESH_MSG_INFO_NEVER.create();
    }

    @Override
    public boolean canOpenFresh(final @NotNull PlayerLootDetail lootDetail) {
        return false;
    }

    @Override
    @Contract("_ -> null")
    public @Nullable Duration getTimeUntilFresh(final @Nullable PlayerLootDetail lootDetail) {
        return null;
    }

    @Override
    public String toString() {
        return "NoForget[]";
    }
}
