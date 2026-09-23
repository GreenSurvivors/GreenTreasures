package de.greensurvivors.greentreasure;

import de.greensurvivors.corelib.CoreLibPlugin;
import de.greensurvivors.corelib.config.CoreConfigManager;
import de.greensurvivors.corelib.database.IConnectionProvider;
import de.greensurvivors.greentreasure.comands.MainCommand;
import de.greensurvivors.greentreasure.config.ConfigData;
import de.greensurvivors.greentreasure.language.LangKey;
import de.greensurvivors.greentreasure.language.MessageManager;
import de.greensurvivors.greentreasure.legacy.LegacyDataImporter;
import de.greensurvivors.greentreasure.listener.CommandInventoriesListener;
import de.greensurvivors.greentreasure.listener.TreasureListener;
import io.leangen.geantyref.TypeToken;
import org.bukkit.plugin.java.JavaPlugin;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.jetbrains.annotations.NotNull;
import org.spongepowered.configurate.transformation.ConfigurationTransformation;
import org.spongepowered.configurate.yaml.NodeStyle;
import org.spongepowered.configurate.yaml.YamlConfigurationLoader;

import java.nio.file.Path;

public class GreenTreasure extends JavaPlugin {
    private final @NotNull DatabaseManager databaseManager;
    private final @NotNull TreasureManager treasureManager;
    private final @NotNull MessageManager messageManager;
    private final @NotNull CoreConfigManager<ConfigData> configHandler;
    private final @NotNull CoreLibPlugin coreLib;
    private @MonotonicNonNull ChunkParser chunkParser;
    private @MonotonicNonNull TreasureListener treasureListener;
    private @MonotonicNonNull CommandInventoriesListener commandInventoriesListener;
    private @MonotonicNonNull MainCommand treasureCommands;
    private @MonotonicNonNull DependencyHelper dependencyHelper;
    private @MonotonicNonNull LegacyDataImporter legacyDataImporter = null;

    public GreenTreasure() {
        coreLib = (CoreLibPlugin) getServer().getPluginManager().getPlugin("CoreLib");
        databaseManager = new DatabaseManager(this);
        treasureManager = new TreasureManager(this);
        messageManager = new MessageManager(getPluginMeta().namespace(), getComponentLogger(), getDataPath(), LangKey.BUNDLE_NAME);

        final @NotNull Path configFile = getDataPath().resolve("config.yml");
        final @NotNull YamlConfigurationLoader loader = YamlConfigurationLoader.builder()
            .path(configFile)
            // not setting node style defaults to auto, that somewhy prefers flow aka json style.
            .nodeStyle(NodeStyle.BLOCK)
            .defaultOptions(configOptions ->
                configOptions.shouldCopyDefaults(true)
            ).build();

        final @NotNull ConfigurationTransformation.Versioned updateTransformation =
            ConfigurationTransformation.versionedBuilder()
                .versionKey("data-version")
                .addVersion(ConfigData.CURRENT_DATA_VERSION,
                    ConfigurationTransformation.builder()
                        .build()
                ).build();

        configHandler = new CoreConfigManager<>(getComponentLogger(),
            getDataPath(), this.getClassLoader(),
            loader, updateTransformation, TypeToken.get(ConfigData.class), ConfigData.CURRENT_DATA_VERSION);
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
        getServer().getPluginManager().disablePlugin(this);
    }

    @Override
    public void onDisable() {
        //clean up
        treasureListener.closeAllInventories();
        commandInventoriesListener.clearInventories();

        treasureManager.clearTreasures();
    }

    public void reload() {
        configHandler.reload()
            .thenAccept(configData -> {
                // import legacy
                if (configData.importLegacy()) {
                    if (getLegacyDataImporter().importLegacyData()) {
                        configData.setImportLegacy(false);
                        configHandler.saveConfig();
                    } else {
                        getComponentLogger().warn("Could not import legacy data, since a import process is already running!");
                    }
                }
        });
        messageManager.reload();
        commandInventoriesListener.clearInventories();
    }

    public @NotNull DatabaseManager getDatabaseManager() {
        return databaseManager;
    }

    public @NotNull ConfigData getConfigData() {
        return configHandler.getConfigData();
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

    public @NotNull IConnectionProvider connectionProvider() {
        return coreLib.connectionProvider();
    }

    @Deprecated
    public @NotNull LegacyDataImporter getLegacyDataImporter() {
        if (legacyDataImporter == null) {
            legacyDataImporter = new LegacyDataImporter(this);
        }

        return legacyDataImporter;
    }
}
