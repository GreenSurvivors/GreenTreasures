package de.greensurvivors.greentreasure;

import de.greensurvivors.greentreasure.comands.MainCommand;
import de.greensurvivors.greentreasure.config.ImportLegacy;
import de.greensurvivors.greentreasure.config.TreasureConfig;
import de.greensurvivors.greentreasure.language.MessageManager;
import de.greensurvivors.greentreasure.listener.CommandInventoriesListener;
import de.greensurvivors.greentreasure.listener.TreasureListener;
import org.bukkit.Bukkit;
import org.bukkit.command.Command;
import org.bukkit.command.PluginCommand;
import org.bukkit.plugin.java.JavaPlugin;
import org.jetbrains.annotations.NotNull;

public class GreenTreasure extends JavaPlugin {
    private static GreenTreasure instance;
    private DatabaseManager databaseManager;
    private TreasureManager treasureManager;
    private ChunkParser chunkParser;
    private TreasureConfig configHandler;
    private TreasureListener treasureListener;
    private CommandInventoriesListener commandInventoriesListener;
    private MessageManager messageManager;
    private MainCommand treasureCommands;
    private DependencyHelper dependencyHelper;

    public GreenTreasure() {
        super();

        instance = this;
    }

    @Deprecated // only use if you really have to!
    public static GreenTreasure inst() {
        return instance;
    }

    @Override
    public void onLoad() {
        dependencyHelper = new DependencyHelper(this);
    }

    @Override
    public void onEnable() {
        // order is important, the config depends on the database, treasure and messages
        databaseManager = new DatabaseManager(this);
        treasureManager = new TreasureManager(this);
        chunkParser = new ChunkParser(this);
        messageManager = new MessageManager(this);
        // configuration
        configHandler = new TreasureConfig(this);
        configHandler.reload();

        // command
        treasureCommands = new MainCommand(this);
        treasureListener = new TreasureListener(this);
        commandInventoriesListener = new CommandInventoriesListener(this);
        dependencyHelper.enable();

        // disable legacy plugins and their commands
        ImportLegacy.disableLegacyPlugins();
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

    public @NotNull DatabaseManager getDatabaseManager() {
        return databaseManager;
    }

    public @NotNull TreasureConfig getConfigHandler() {
        return configHandler;
    }

    public TreasureManager getTreasureManager() {
        return treasureManager;
    }

    public ChunkParser getChunkParser() {
        return chunkParser;
    }

    public @NotNull CommandInventoriesListener getCommandInventoriesListener() {
        return commandInventoriesListener;
    }

    public MessageManager getMessageManager() {
        return messageManager;
    }

    public @NotNull TreasureListener getTreasureListener() {
        return treasureListener;
    }

    public MainCommand getMainCommand() {
        return treasureCommands;
    }
}
