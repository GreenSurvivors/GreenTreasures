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
    private final @NotNull GreenTreasure plugin;

    public TreasureConfig(final @NotNull GreenTreasure plugin) {
        this.plugin = plugin;
    }

    /**
     * Load main configurations.
     */
    public void reload() {
        plugin.reloadConfig();

        final FileConfiguration config = plugin.getConfig();
        // set defaults
        config.options().setHeader(List.of(plugin.getName() + " " + plugin.getPluginMeta().getVersion()));
        config.options().copyDefaults(true);
        config.options().parseComments(true);

        loadDatabase();

        // load language
        Locale locale = Locale.forLanguageTag(config.getString(CONFIG_KEY_LANG, "en-en").replace("_", "-"));
        plugin.getMessageManager().reload(locale);

        // import legacy
        if (config.getBoolean(CONFIG_KEY_IMPORT_LEGACY)) {
            Bukkit.getScheduler().runTaskLater(plugin, () -> { // run later to give the database time to connect
                new ImportLegacy(plugin).importLegacyData();

                config.set(CONFIG_KEY_IMPORT_LEGACY, Boolean.FALSE);
                plugin.saveConfig();
            }, plugin.getDatabaseManager().hasConnection() ? 20 : 300);
        }

        plugin.saveConfig();
    }

    private void loadDatabase() {
        final FileConfiguration mainCfg = plugin.getConfig();
        final @Nullable ConfigurationSection section = mainCfg.getConfigurationSection(CONFIG_KEY_SQL);

        if (section != null) {
            Bukkit.getScheduler().runTask(plugin, () -> {
                plugin.getDatabaseManager().reload(section.getValues(false));
                mainCfg.set(CONFIG_KEY_SQL, plugin.getDatabaseManager().serializeDatabaseConnectionConfig());
            });
        } else {
            plugin.getComponentLogger().error("Could not load database!");
        }
    }
}
