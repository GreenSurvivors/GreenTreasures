package de.greensurvivors.greentreasure;

import de.greensurvivors.greentreasure.comands.MainCommand;
import de.greensurvivors.greentreasure.config.ImportLegacy;
import de.greensurvivors.greentreasure.config.TreasureConfig;
import de.greensurvivors.greentreasure.language.MessageManager;
import de.greensurvivors.greentreasure.listener.CommandInventoriesListener;
import de.greensurvivors.greentreasure.listener.TreasureListener;
import org.bukkit.plugin.java.JavaPlugin;
import org.jetbrains.annotations.NotNull;

public class GreenTreasure extends JavaPlugin {
    private static GreenTreasure instance;
    private DatabaseManager databaseManager;
    private TreasureManager treasureManager;
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
    public void onEnable() {
        // order is important, the config depends on the database and treasureManager
        databaseManager = new DatabaseManager(this);
        treasureManager = new TreasureManager(this);
        // configuration
        configHandler = new TreasureConfig(this);
        configHandler.reloadMain();

        // command
        treasureCommands = new MainCommand(this);
        treasureListener = new TreasureListener(this);
        commandInventoriesListener = new CommandInventoriesListener(this);
        messageManager = new MessageManager(this);
        dependencyHelper = new DependencyHelper(this);

        // disable legacy plugins and their commands
        ImportLegacy.disableLegacyPlugins();
    }

    @Override
    public void onDisable() {
        //clean up
        treasureListener.closeAllInventories();
        commandInventoriesListener.clearInventories();

        treasureManager.clearTreasures();
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
