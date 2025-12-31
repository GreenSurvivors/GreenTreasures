package de.greensurvivors.greentreasure.legacy;

import de.greensurvivors.greentreasure.GreenTreasure;
import io.papermc.paper.threadedregions.scheduler.ScheduledTask;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.players.NameAndId;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Queue;
import java.util.UUID;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

public class UUIDFetchScheduler {
    protected final @NotNull GreenTreasure plugin;
    protected final @NotNull Queue<@NotNull UUIDFetchRequest> queuedIds = new ConcurrentLinkedQueue<>();
    protected @Nullable ScheduledTask task = null;

    public UUIDFetchScheduler(final @NotNull GreenTreasure plugin) {
        this.plugin = plugin;
    }

    public void registerForFetching(final @NotNull String playerName, final @NotNull Consumer<@Nullable UUID> resultConsumer) {
        queuedIds.add(new UUIDFetchRequest(playerName, resultConsumer));

        // Mojang api only allows 600 requests / 10 minutes or one per second.
        // so we throttle our requests to 400 / 10 minutes or one per 1.5 seconds, to give other plugins some room
        if (task == null) {
            task = plugin.getServer().getAsyncScheduler().runAtFixedRate(plugin, it -> {
                final @NotNull UUIDFetchRequest uuidFetchRequest = queuedIds.remove();
                // regardless of name, the nameToIdCache does make a request to mojang
                uuidFetchRequest.resultConsumer().accept(MinecraftServer.getServer().services().nameToIdCache().get(uuidFetchRequest.playerName()).map(NameAndId::id).orElse(null));

                if (queuedIds.isEmpty()) {
                    it.cancel();
                }

                // wait half-time initiallyto let other requests flood in and not rapidly start and finish the task circumventing the period
            }, 750, 1500, TimeUnit.MILLISECONDS);
        }
    }

    public void cancel () {
        queuedIds.removeIf( fetchRequest -> {
            fetchRequest.resultConsumer.accept(null);
            return true;
        });

        queuedIds.clear();
        if (task != null) {
            task.cancel();
            task = null;
        }
    }

    protected record UUIDFetchRequest(@NotNull String playerName, @NotNull Consumer<@Nullable UUID> resultConsumer) {
    }
}
