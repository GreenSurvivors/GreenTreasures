package de.greensurvivors.greentreasure;

import de.greensurvivors.greentreasure.comands.MainCommand;
import de.greensurvivors.greentreasure.config.TreasureConfig;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.language.MessageManager;
import de.greensurvivors.greentreasure.legacy.LegacyDataImporter;
import de.greensurvivors.greentreasure.listener.CommandInventoriesListener;
import de.greensurvivors.greentreasure.listener.TreasureListener;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.command.Command;
import org.bukkit.command.PluginCommand;
import org.bukkit.plugin.java.JavaPlugin;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.jetbrains.annotations.NotNull;

public class GreenTreasure extends JavaPlugin {
    private final @NotNull DatabaseManager databaseManager;
    private final @NotNull TreasureManager treasureManager;
    private final @NotNull MessageManager messageManager;
    private final @NotNull TreasureConfig configHandler;
    private @MonotonicNonNull ChunkParser chunkParser;
    private @MonotonicNonNull TreasureListener treasureListener;
    private @MonotonicNonNull CommandInventoriesListener commandInventoriesListener;
    private @MonotonicNonNull MainCommand treasureCommands;
    private @MonotonicNonNull DependencyHelper dependencyHelper;
    private @MonotonicNonNull LegacyDataImporter legacyDataImporter = null;
    private volatile @MonotonicNonNull LoadedTreasureLogger loadedTreasureLogger = null;

    public GreenTreasure() {
        databaseManager = new DatabaseManager(this);
        treasureManager = new TreasureManager(this);
        messageManager = new MessageManager(getPluginMeta().namespace(), getComponentLogger(), getDataPath());
        configHandler = new TreasureConfig(this);
    }

    @Override
    public void onLoad() {
        dependencyHelper = new DependencyHelper(this);
    }

    @Override
    public void onEnable() {
        // order is important, the config depends on the database and treasure
        commandInventoriesListener = new CommandInventoriesListener(this);
        reload();

        chunkParser = new ChunkParser(this);

        treasureCommands = new MainCommand(this);
        treasureListener = new TreasureListener(this);
        dependencyHelper.enable();

        // disable legacy plugins and their commands
        LegacyDataImporter.disableLegacyPlugins();
    }

    public void shutdownForcefully() {
        getComponentLogger().error("Something went wrong, force disabling the plugin, brace for impact and send thoughts and prayers!");

        // unregister our cmds
        for (Command cmd : Bukkit.getCommandMap().getKnownCommands().values()) {
            if (cmd instanceof PluginCommand pluginCommand) {
                if (pluginCommand.getPlugin().getName().equalsIgnoreCase(getName())) {
                    cmd.unregister(Bukkit.getCommandMap());
                }
            }
        }

        getServer().getPluginManager().disablePlugin(this);
    }

    @Override
    public void onDisable() {
        //clean up
        treasureListener.closeAllInventories();
        commandInventoriesListener.clearInventories();

        treasureManager.clearTreasures();
        databaseManager.closeConnection();
    }

    public void reload() {
        configHandler.reload();
        messageManager.reload();
        commandInventoriesListener.clearInventories();
    }

    public @NotNull DatabaseManager getDatabaseManager() {
        return databaseManager;
    }

    public @NotNull TreasureConfig getConfigHandler() {
        return configHandler;
    }

    public @NotNull TreasureManager getTreasureManager() {
        return treasureManager;
    }

    public ChunkParser getChunkParser() {
        return chunkParser;
    }

    public @NotNull CommandInventoriesListener getCommandInventoriesListener() {
        return commandInventoriesListener;
    }

    public @NotNull MessageManager getMessageManager() {
        return messageManager;
    }

    public @NotNull TreasureListener getTreasureListener() {
        return treasureListener;
    }

    public MainCommand getMainCommand() {
        return treasureCommands;
    }

    @Deprecated
    public @NotNull LegacyDataImporter getLegacyDataImporter() {
        if (legacyDataImporter == null) {
            legacyDataImporter = new LegacyDataImporter(this);
        }

        return legacyDataImporter;
    }

    public void log(final @NotNull TreasureInfo treasureInfo, final @NotNull Location location) {
        if (getConfigHandler().shouldLogLoadedTreasures()) {
            if (loadedTreasureLogger == null) {
                loadedTreasureLogger = new LoadedTreasureLogger(this);
            }

            loadedTreasureLogger.log(treasureInfo, location);
        }
    }
}
