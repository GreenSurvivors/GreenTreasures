package de.greensurvivors.greentreasure;

import de.greensurvivors.greentreasure.comands.TreasureCommands;
import de.greensurvivors.greentreasure.config.TreasureConfig;
import de.greensurvivors.greentreasure.listener.CommandInventoriesListener;
import de.greensurvivors.greentreasure.listener.TreasureListener;
import org.bukkit.Bukkit;
import org.bukkit.plugin.PluginManager;
import org.bukkit.plugin.java.JavaPlugin;

public class GreenTreasure extends JavaPlugin {

	private static GreenTreasure instance;

	public static GreenTreasure inst() {
		return instance;
	}

	@Override
	public void onEnable() {
		// set instance
		instance = this;
		// set logger
		TreasureLogger.setLogger(getLogger());

		// configuration
		TreasureConfig.inst().reloadMain();
		// command
		getCommand(TreasureCommands.CMD).setExecutor(TreasureCommands.inst());
		// listener
		PluginManager pm = Bukkit.getPluginManager();
		pm.registerEvents(TreasureListener.inst(), this);
		pm.registerEvents(CommandInventoriesListener.inst(), this);
	}

	@Override
	public void onDisable() {
		//clean up
		TreasureListener.inst().closeAllInventories();
		CommandInventoriesListener.inst().clearInventories();

		TreasureListener.inst().clearTreasures();
	}
}
