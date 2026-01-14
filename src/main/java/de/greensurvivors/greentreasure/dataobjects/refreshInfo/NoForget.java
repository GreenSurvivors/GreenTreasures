package de.greensurvivors.greentreasure.dataobjects.refreshInfo;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import de.greensurvivors.greentreasure.language.LangPath;
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
        return plugin.getMessageManager().getLang(LangPath.REFRESH_MSG_INFO_NEVER);
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
