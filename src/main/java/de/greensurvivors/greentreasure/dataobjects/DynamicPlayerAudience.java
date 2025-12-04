package de.greensurvivors.greentreasure.dataobjects;

import net.kyori.adventure.audience.Audience;
import net.kyori.adventure.audience.ForwardingAudience;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.UUID;

public class DynamicPlayerAudience implements ForwardingAudience.Single {
    private final @NotNull UUID uuid;

    public static @NotNull Audience fromAudience(final @NotNull Audience other) {
        return other instanceof Player player ? new DynamicPlayerAudience(player.getUniqueId()) : other;
    }

    private DynamicPlayerAudience(final @NotNull UUID uuid) {
        this.uuid = uuid;
    }

    @Override
    public @NotNull Audience audience() {
        final @Nullable Player player = Bukkit.getServer().getPlayer(uuid);
        return player != null ? player : Audience.empty();
    }
}
