package de.greensurvivors.greentreasure;

import de.greensurvivors.greentreasure.comands.TreasureCommands;
import de.greensurvivors.greentreasure.config.TreasureConfig;
import de.greensurvivors.greentreasure.language.MessageManager;
import de.greensurvivors.greentreasure.listener.CommandInventoriesListener;
import de.greensurvivors.greentreasure.listener.TreasureListener;
import org.bukkit.plugin.java.JavaPlugin;
import org.jetbrains.annotations.NotNull;

public class GreenTreasure extends JavaPlugin {
    private static GreenTreasure instance;
    private DatabaseManager databaseManager;
    private TreasureConfig configHandler;
    private TreasureListener treasureListener;
    private CommandInventoriesListener commandInventoriesListener;
    private MessageManager messageManager;
    private TreasureCommands treasureCommands;

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
        databaseManager = new DatabaseManager(this);
        // configuration
        configHandler = new TreasureConfig(this);
        configHandler.reloadMain();

        // command
        treasureCommands = new TreasureCommands(this);
        treasureListener = new TreasureListener(this);
        commandInventoriesListener = new CommandInventoriesListener(this);
        messageManager = new MessageManager(this);
    }

    @Override
    public void onDisable() {
        //clean up
        treasureListener.closeAllInventories();
        commandInventoriesListener.clearInventories();

        treasureListener.clearTreasures();
    }

    public @NotNull DatabaseManager getDatabaseManager () {
        return databaseManager;
    }

    public @NotNull TreasureConfig getConfigHandler() {
        return configHandler;
    }

    public @NotNull TreasureListener getTreasureListener() {
        return treasureListener;
    }

    public @NotNull CommandInventoriesListener getCommandInventoriesListener() {
        return commandInventoriesListener;
    }

    public MessageManager getMessageManager() {
        return messageManager;
    }

    public TreasureCommands getTreasureCommands() {
        return treasureCommands;
    }
}
