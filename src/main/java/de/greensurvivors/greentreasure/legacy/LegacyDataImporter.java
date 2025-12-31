package de.greensurvivors.greentreasure.legacy;

import de.greensurvivors.greentreasure.GreenTreasure;
import org.bukkit.Bukkit;
import org.bukkit.plugin.Plugin;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.atomic.AtomicInteger;

@Deprecated
public class LegacyDataImporter {
    public static final @NotNull String TREASURE_CHEST = "TreasureChest", TREASURE_CHEST_X = "TreasureChestX";
    protected final @NotNull GreenTreasure plugin;
    protected final @NotNull UUIDFetchScheduler uuidFetchScheduler;
    protected final @NotNull AtomicInteger importProcessId = new AtomicInteger(-1);

    public LegacyDataImporter(final @NotNull GreenTreasure plugin) {
        this.plugin = plugin;

        uuidFetchScheduler = new UUIDFetchScheduler(plugin);
    }

    public static void disableLegacyPlugins() {
        disablePlugin(Bukkit.getPluginManager().getPlugin(TREASURE_CHEST));
        disablePlugin(Bukkit.getPluginManager().getPlugin(TREASURE_CHEST_X));
    }

    private static void disablePlugin(final @Nullable Plugin treasureChestPlugin) {
        if (treasureChestPlugin != null) {
            Bukkit.getPluginManager().disablePlugin(treasureChestPlugin);
        }
    }

    public void cancelImport () {
        uuidFetchScheduler.cancel();
        importProcessId.set(-1);
    }

    public boolean importLegacyData() {
        if (importProcessId.get() >= 0) {
            return false;
        }

        importProcessId.set(plugin.getChunkParser().requestProcessId());

        final @NotNull Path pluginPath = plugin.getDataPath().getParent();
        final @NotNull Path treasureChestPath = pluginPath.resolve(TREASURE_CHEST);

        if (Files.isDirectory(treasureChestPath)) {
            startImport(TREASURE_CHEST, treasureChestPath);
        } else {
            final @NotNull Path treasureChestXPath = pluginPath.resolve(TREASURE_CHEST_X);

            if (Files.isDirectory(treasureChestXPath)) {
                startImport(TREASURE_CHEST_X, treasureChestXPath);
            } else {
                plugin.getComponentLogger().warn("Could not find any legacy treasures.");
            }
        }

        return true;
    }

    protected void startImport(final @NotNull String pluginName, final @NotNull Path path) {
        plugin.getComponentLogger().info("starting import legacy process from {}", pluginName);

        importTreasureData(path)
            .thenCombineAsync(importPlayerData(path), (firstSuccess, secondSuccess) -> firstSuccess && secondSuccess,
                runnable -> Bukkit.getScheduler().runTaskAsynchronously(plugin, runnable))
            .whenComplete((success, ex) -> {
                plugin.getComponentLogger().info("importing legacy process from {} is done. was success: {}", pluginName, ex == null ? success : false);

                if (ex == null ? success : false) {
                    plugin.getComponentLogger().info("You may delete the old folder 'plugins/{}' now!", pluginName);
                } else {
                    plugin.getComponentLogger().info("There where some problems when importing all legacy data. You have to manually check the logs, in order to decide if deleting the old folder 'plugins/{}' is safe!", pluginName);
                }

                importProcessId.set(-1);
            });
    }

    /// returned boolean is success for all stages!
    protected @NotNull CompletionStage<@NotNull Boolean> importTreasureData(final @NotNull Path treasurePluginFolder) {
        plugin.getComponentLogger().info("Importing Treasures. Please stand back, this may take a while!");

        final @NotNull Path treasuresPath = treasurePluginFolder.resolve("treasure");
        if (Files.isDirectory(treasuresPath)) {
            final @NotNull CompletableFuture<@NotNull Boolean> result = new CompletableFuture<>();

            Bukkit.getScheduler().runTaskAsynchronously(plugin, () -> {
                try {
                    final @NotNull ForkingSuccessCollector successCollector = new ForkingSuccessCollector(result::complete);

                    Files.walkFileTree(treasuresPath, new TreasureFileVisitor(plugin, importProcessId, successCollector));
                    successCollector.markAllStarted();
                } catch (final @NotNull IOException e) {
                    plugin.getComponentLogger().warn("could not read legacy treasure path", e);
                    result.complete(Boolean.FALSE);
                }
            });

            return result;
        } else {
            plugin.getComponentLogger().warn("Could not load legacy treasures!");
            return CompletableFuture.completedStage(Boolean.FALSE);
        }
    }
    /**
     * import player data
     */
    protected @NotNull CompletionStage<@NotNull Boolean> importPlayerData(final @NotNull Path treasurePluginFolder) {
        plugin.getComponentLogger().info("importing PlayerData. Please stand back, this could take a while!");

        final @NotNull Path playersPath = treasurePluginFolder.resolve("players");
        if (Files.isDirectory(playersPath)) {

            final @NotNull CompletableFuture<@NotNull Boolean> result = new CompletableFuture<>();

            Bukkit.getScheduler().runTaskAsynchronously(plugin, () -> {
                try {
                    final @NotNull ForkingSuccessCollector successCollector = new ForkingSuccessCollector(result::complete);

                    Files.walkFileTree(playersPath, new PlayerFileVisitor(plugin, uuidFetchScheduler, importProcessId, successCollector));
                    successCollector.markAllStarted();
                } catch (final @NotNull IOException e) {
                    plugin.getComponentLogger().warn("Could not load legacy player info!", e);
                    result.complete(Boolean.FALSE);
                }
            });

            return result;
        } else {
            plugin.getComponentLogger().warn("Could not load legacy player info, because the path is not a directory!");
            return CompletableFuture.completedStage(Boolean.FALSE);
        }
    }
}
