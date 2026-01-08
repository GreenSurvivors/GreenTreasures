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
    protected final @NotNull ConfigOption<@NotNull String> language = new ConfigOption<>("language", "en-en");
    protected final @NotNull ConfigOption<@NotNull Boolean> importLegacy = new ConfigOption<>("import_legacy", false);
    protected static final @NotNull String CONFIG_KEY_SQL = "SQL";
    protected final @NotNull GreenTreasure plugin;

    public TreasureConfig(final @NotNull GreenTreasure plugin) {
        this.plugin = plugin;
    }

    /**
     * Load main configurations.
     */
    public void reload() {
        plugin.reloadConfig();

        final @NotNull FileConfiguration config = plugin.getConfig();
        // set defaults
        config.options().setHeader(List.of(plugin.getName() + " " + plugin.getPluginMeta().getVersion()));
        config.options().copyDefaults(true);
        config.options().parseComments(true);

        final FileConfiguration mainCfg = plugin.getConfig();
        final @Nullable ConfigurationSection section = mainCfg.getConfigurationSection(CONFIG_KEY_SQL);

        if (section != null) {
            plugin.getDatabaseManager().reload(section.getValues(false));
            mainCfg.set(CONFIG_KEY_SQL, plugin.getDatabaseManager().serializeDatabaseConnectionConfig());
        } else {
            plugin.getComponentLogger().error("Could not load database!");
        }

        // load language
        language.setValue(config.getString(language.getPath()));
        Locale locale = Locale.forLanguageTag(language.getValueOrFallback().replace("_", "-"));
        plugin.getMessageManager().reload(locale);

        // import legacy
        if (config.getBoolean(importLegacy.getPath(), importLegacy.getValueOrFallback())) {
            Bukkit.getScheduler().runTaskLater(plugin, () -> { // run later to give the database time to connect
                if (plugin.getLegacyDataImporter().importLegacyData()) {
                    config.set(importLegacy.getPath(), Boolean.FALSE);
                    plugin.saveConfig();
                } else {
                    plugin.getComponentLogger().warn("Could not import legacy data, since a import process is already running!");
                }
            }, plugin.getDatabaseManager().hasConnection() ? 20 : 300);
        }

        plugin.saveConfig();
    }
}
