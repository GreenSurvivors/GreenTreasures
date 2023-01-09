package de.greensurvivors.green_treasure;

import de.greensurvivors.green_treasure.comands.TreasureCommands;
import de.greensurvivors.green_treasure.config.TreasureConfig;
import de.greensurvivors.green_treasure.listener.CommandInventoriesListener;
import de.greensurvivors.green_treasure.listener.TreasureListener;
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
		TreasureListener.inst().clearTreasures();
		CommandInventoriesListener.inst().clearInventories();
	}
}
