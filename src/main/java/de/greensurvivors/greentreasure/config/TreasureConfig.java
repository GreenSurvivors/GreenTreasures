package de.greensurvivors.greentreasure.config;

import de.greensurvivors.greentreasure.GreenTreasure;
import org.bukkit.Bukkit;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.file.FileConfiguration;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Locale;

public class TreasureConfig {
    private static final @NotNull String
        CONFIG_KEY_SQL = "SQL",
        CONFIG_KEY_LANG = "language",
        CONFIG_KEY_IMPORT_LEGACY = "import_legacy";
    //ensures only one thread works with the file at a time
    private final @NotNull Object mutexMainConfig = new Object();
    private final @NotNull GreenTreasure plugin;

    public TreasureConfig(final @NotNull GreenTreasure plugin) {
        this.plugin = plugin;
    }

    /**
     * Load main configurations.
     */
    public void reloadMain() {
        plugin.reloadConfig();
        loadDatabase();
        loadLanguage();
        loadLegacy();
    }

    /**
     * Save default configuration values.
     */
    private void setDefaults(final @NotNull FileConfiguration cfg) {
        cfg.options().setHeader(List.of(plugin.getName() + " " + plugin.getPluginMeta().getVersion()));
        cfg.options().copyDefaults(true);
        cfg.options().parseComments(true);
    }

    /**
     * Load language configuration.
     */
    private void loadLanguage() {
        Bukkit.getScheduler().runTaskAsynchronously(plugin, () -> {
            synchronized (mutexMainConfig) {
                FileConfiguration mainCfg = plugin.getConfig();

                Locale locale = Locale.forLanguageTag(mainCfg.getString(CONFIG_KEY_LANG, "en-en").replace("_", "-"));
                Bukkit.getScheduler().runTask(plugin, () -> plugin.getMessageManager().reload(locale));
            }
        });
    }

    private void loadDatabase() {
        Bukkit.getScheduler().runTaskAsynchronously(plugin, () -> {
            synchronized (mutexMainConfig) {
                FileConfiguration mainCfg = plugin.getConfig();
                setDefaults(mainCfg);

                final @Nullable ConfigurationSection section = mainCfg.getConfigurationSection(CONFIG_KEY_SQL);

                if (section != null) {
                    Bukkit.getScheduler().runTask(plugin, () -> {
                        plugin.getDatabaseManager().reload(section.getValues(false));
                        mainCfg.set(CONFIG_KEY_SQL, plugin.getDatabaseManager().serializeDatabaseConnectionConfig());
                        plugin.saveConfig();
                    });
                } else {
                    plugin.getComponentLogger().error("Could not load database!");
                }
            }
        });
    }

    private void loadLegacy() {
        final @NotNull FileConfiguration config = plugin.getConfig();
        if (config.getBoolean(CONFIG_KEY_IMPORT_LEGACY)) {
            Bukkit.getScheduler().runTaskLater(plugin, () -> { // run later to give the database time to connect
                new ImportLegacy(plugin).importLegacyData();

                config.set(CONFIG_KEY_IMPORT_LEGACY, Boolean.FALSE);
                plugin.saveConfig();
            }, plugin.getDatabaseManager().hasConnection() ? 20 : 300);
        }
    }
}
