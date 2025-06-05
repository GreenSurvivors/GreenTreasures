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
    protected final @NotNull ConfigOption<@NotNull Boolean> importLegacy = new ConfigOption<>("import_legacy", false);
    protected final @NotNull ConfigOption<@NotNull BreakBehavior> breakBehavior = new ConfigOption<>("break_behavior", BreakBehavior.ALL_BREAK_GLOBAL);
    protected static final @NotNull String CONFIG_KEY_SQL = "SQL";
    protected final @NotNull GreenTreasure plugin;

    public TreasureConfig(final @NotNull GreenTreasure plugin) {
        this.plugin = plugin;
    }

    /// Load main configuration.
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
            plugin.getDataAccessor().reload(section.getValues(false));
            mainCfg.set(CONFIG_KEY_SQL, plugin.getDataAccessor().serializeDatabaseConnectionConfig());
        } else {
            plugin.getComponentLogger().error("Could not load database!");
        }

        // import legacy
        if (config.getBoolean(importLegacy.getPath(), importLegacy.getValueOrFallback())) {
            Bukkit.getScheduler().runTaskLater(plugin, () -> { // run later to give the database time to connect
                if (plugin.getLegacyDataImporter().importLegacyData()) {
                    config.set(importLegacy.getPath(), Boolean.FALSE);
                    plugin.saveConfig();
                } else {
                    plugin.getComponentLogger().warn("Could not import legacy data, since a import process is already running!");
                }
            }, plugin.getDataAccessor().hasConnection() ? 20 : 300);
        }

        @Nullable BreakBehavior newBehavior = null;
        final @Nullable String strBehavior = config.getString(breakBehavior.getPath());

        if (strBehavior != null) {
            try {
                newBehavior = BreakBehavior.valueOf(strBehavior.toUpperCase(Locale.ENGLISH));
            } catch (final @NotNull IllegalArgumentException e) {
                plugin.getComponentLogger().warn("Could not load break behavior config option (invalid). Falling back to default.", e);
            }
        }

        breakBehavior.setValue(newBehavior);

        plugin.saveConfig();
    }

    public @NotNull BreakBehavior getBreakBehavior() {
        return breakBehavior.getValueOrFallback();
    }

    public enum BreakBehavior {
        SHIFT_BREAKS_LOCAL,
        ONLY_SHIFT_BREAKS_GLOBAL,
        ALL_BREAK_GLOBAL
    }
}
