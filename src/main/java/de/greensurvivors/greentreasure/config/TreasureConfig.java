package de.greensurvivors.greentreasure.config;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.dataobjects.DatabaseManager;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.*;
import java.util.concurrent.CompletableFuture;

public class TreasureConfig {
    private static final @NotNull String
            ITEM_LIST = "items",
            FORGETTING_DURATION = "forgettingDurationInMillis",
            SLOT_CHANCE = "slotChance",
            IS_UNLIMITED = "isUnlimited",
            IS_SHARED = "isShared";
    private static final @NotNull OfflinePlayer
        // in case a treasure was shared (@ is not permitted as a valid char and therefor always unique)
        SHARED_PROFILE = Bukkit.getOfflinePlayer("@SHARED");
    private static final @NotNull String
        CONFIG_KEY_SQL = "SQL",
        CONFIG_KEY_LANG = "language",
        CONFIG_KEY_IMPORT_LEGACY = "import_legacy";
    private final long DEFAULT_FORGETTING_PERIOD = -1;
    private final int DEFAULT_SLOT_CHANCE = 10000;
    private final boolean DEFAULT_IS_UNLIMITED = false;
    private final boolean DEFAULT_IS_SHARED = false;
    //ensures only one thread works with the file at a time
    private final @NotNull Object mutexMainConfig = new Object();
    private final @NotNull DatabaseManager databaseManager;
    private final @NotNull GreenTreasure plugin;
    private final @NotNull Path treasureFolder;

    public TreasureConfig (final @NotNull GreenTreasure plugin) {
        this.plugin = plugin;
        this.databaseManager = new DatabaseManager(plugin);
        this.treasureFolder = Path.of(plugin.getDataFolder().getPath()).resolve("treasures");
    }

    /**
     * saves the params to a player file
     *
     * @param player             the UUID of a looting player null means shared
     * @param treasureId         the treasureId of a treasure, the information for identify a treasure
     * @param timeStamp        when has the player last opened the treasure?
     * @param unlootedTreasure the inventory with all the remaining items. null means the player will be reset
     */
    public void savePlayerDetail(final @Nullable OfflinePlayer player, final @NotNull String treasureId, final long timeStamp, final @Nullable List<@NotNull ItemStack> unlootedTreasure) {
        databaseManager.setPlayerData(player == null ? SHARED_PROFILE : player, treasureId, new PlayerLootDetail(timeStamp, unlootedTreasure));
    }

    /**
     * forget a player has ever looted a treasure
     *
     * @param player     the UUID of a looting player null means shared
     */
    public @NotNull CompletableFuture<Void> forgetPlayer(final @Nullable OfflinePlayer player, final @NotNull String treasureID) {
        return databaseManager.forgetPlayer(player == null ? SHARED_PROFILE : player, treasureID);
    }

    /**
     * forget anybody has ever opened the treasure at the given location.
     *
     * @param treasureID information to identify a treasure
     */
    public @NotNull CompletableFuture<Void> forgetAll(final @NotNull String treasureID) {
        return databaseManager.forgetAll(treasureID);
    }


    /**
     * load the status of a player about a treasure and calls a function with the information back in the main thread.
     *
     * @param player                     the UUID of a looting player null means shared
     * @param treasureId                 the treasureId of a treasure, the information for identify a treasure
     */
    public @NotNull CompletableFuture<@Nullable PlayerLootDetail> getPlayerLootDetail(final @Nullable OfflinePlayer player, final @NotNull String treasureId) {
        return databaseManager.getPlayerData(player == null ? SHARED_PROFILE : player, treasureId);
    }

    public @NotNull CompletableFuture<@NotNull Map<UUID, @NotNull PlayerLootDetail>> getAllPlayerDetail(@NotNull String treasureId) {
        return databaseManager.getAllPlayerData(treasureId);
    }

    /**
     * Load main configurations.
     */
    public void reloadMain() {
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

    public @NotNull CompletableFuture<Void> deleteTreasure(@NotNull String treasureId) {
        CompletableFuture<Void> result = new CompletableFuture<>();

        Bukkit.getScheduler().runTaskAsynchronously(plugin, () -> {
            synchronized (treasureFolder) {
                Path treasureFile = treasureFolder.resolve(treasureId + ".yml");

                if (Files.isRegularFile(treasureFile)) {
                    try {
                        Files.deleteIfExists(treasureFile);

                        Bukkit.getScheduler().runTask(plugin, () -> {
                            databaseManager.forgetAll(treasureId);
                            plugin.getTreasureListener().invalidateTreasure(treasureId);
                            result.complete(null);
                        });
                    } catch (IOException e) {
                        plugin.getComponentLogger().error("Could not delete {} treasure file.", treasureFile, e);
                    }
                }
            }
        });

        return result;
    }

    /**
     * @param treasureId  information to identify a treasure
     * @param lootItems list of items a player should receive upon opening the treasure.
     */
    public @NotNull CompletableFuture<Void> saveTreasureLoot(final @NotNull String treasureId, final @NotNull List<@NotNull ItemStack> lootItems) {
        CompletableFuture<Void> result = new CompletableFuture<>();

        Bukkit.getScheduler().runTaskAsynchronously(plugin, () -> {
            synchronized (treasureFolder) {
                Path treasurePath = treasureFolder.resolve(treasureId + ".yml");

                if (Files.notExists(treasurePath)) {
                    try {
                        Files.createFile(treasurePath);
                    } catch (IOException e) {
                        plugin.getComponentLogger().error("Could not create missing {} treasure file.", treasurePath, e);
                    }
                }

                if (Files.isRegularFile(treasurePath)) {
                    try {
                        final File treasureFile = treasurePath.toFile();
                        FileConfiguration cfg = YamlConfiguration.loadConfiguration(treasureFile);
                        setDefaults(cfg);
                        cfg.set(ITEM_LIST, ItemStack.serializeItemsAsBytes(lootItems));

                        cfg.save(treasureFile);

                        Bukkit.getScheduler().runTask(plugin, () -> {
                            plugin.getTreasureListener().invalidateTreasure(treasureId);
                            result.complete(null);
                        });
                    } catch (IOException e) {
                        plugin.getComponentLogger().error("Could not save {} treasure file.", treasurePath, e);
                    }
                }
            }
        });

        return result;
    }

    /**
     * set the random slot chance
     *
     * @param treasureId  information to identify a treasure
     * @param slotChance how probable it is for a slot to be in the freshly opened treasure
     */
    public @NotNull CompletableFuture<Void> setRandom(final @NotNull String treasureId, final int slotChance) {
        CompletableFuture<Void> result = new CompletableFuture<>();

        Bukkit.getScheduler().runTaskAsynchronously(plugin, () -> {
            synchronized (treasureFolder) {
                Path treasurePath = treasureFolder.resolve(treasureId + ".yml");

                if (Files.notExists(treasurePath)) {
                    try {
                        Files.createFile(treasurePath);
                    } catch (IOException e) {
                        plugin.getComponentLogger().error("Could not create missing {} treasure file.", treasurePath, e);
                    }
                }

                if (Files.isRegularFile(treasurePath)) {
                    try {
                        final File treasureFile = treasurePath.toFile();
                        FileConfiguration cfg = YamlConfiguration.loadConfiguration(treasureFile);
                        setDefaults(cfg);
                        cfg.set(SLOT_CHANCE, slotChance);

                        cfg.save(treasureFile);

                        Bukkit.getScheduler().runTask(plugin, () -> {
                            plugin.getTreasureListener().invalidateTreasure(treasureId);
                            result.complete(null);
                        });
                    } catch (IOException e) {
                        plugin.getComponentLogger().error("Could not save {} treasure file.", treasurePath, e);
                    }
                }
            }
        });

        return result;
    }

    /**
     * set the shared status
     *
     * @param treasureId information to identify a treasure
     * @param isShared if the Inventory is globally shared across all players
     */
    public @NotNull CompletableFuture<Void> setShared(@NotNull String treasureId, final boolean isShared) {
        CompletableFuture<Void> result = new CompletableFuture<>();

        Bukkit.getScheduler().runTaskAsynchronously(plugin, () -> {
            synchronized (treasureFolder) {
                Path treasurePath = treasureFolder.resolve(treasureId + ".yml");

                if (Files.notExists(treasurePath)) {
                    try {
                        Files.createFile(treasurePath);
                    } catch (IOException e) {
                        plugin.getComponentLogger().error("Could not create missing {} treasure file.", treasurePath, e);
                    }
                }

                if (Files.isRegularFile(treasurePath)) {
                    try {
                        final File treasureFile = treasurePath.toFile();
                        FileConfiguration cfg = YamlConfiguration.loadConfiguration(treasureFile);
                        setDefaults(cfg);
                        cfg.set(IS_SHARED, isShared);

                        cfg.save(treasureFile);

                        Bukkit.getScheduler().runTask(plugin, () -> {
                            plugin.getTreasureListener().invalidateTreasure(treasureId);
                            result.complete(null);
                        });
                    } catch (IOException e) {
                        plugin.getComponentLogger().error("Could not save {} treasure file.", treasurePath, e);
                    }
                }
            }
        });

        return result;
    }

    /**
     * set the unlimited status
     *
     * @param treasureId  information to identify a treasure
     * @param isUnLimited if one can loot the treasure as often as one wants
     */
    public @NotNull CompletableFuture<Void> setUnlimited(@NotNull String treasureId, boolean isUnLimited) {
        CompletableFuture<Void> result = new CompletableFuture<>();

        Bukkit.getScheduler().runTaskAsynchronously(plugin, () -> {
            synchronized (treasureFolder) {
                Path treasurePath = treasureFolder.resolve(treasureId + ".yml");

                if (Files.notExists(treasurePath)) {
                    try {
                        Files.createFile(treasurePath);
                    } catch (IOException e) {
                        plugin.getComponentLogger().error("Could not create missing {} treasure file.", treasurePath, e);
                    }
                }

                if (Files.isRegularFile(treasurePath)) {
                    try {
                        final File treasureFile = treasurePath.toFile();
                        FileConfiguration cfg = YamlConfiguration.loadConfiguration(treasureFile);
                        setDefaults(cfg);
                        cfg.set(IS_UNLIMITED, isUnLimited);

                        cfg.save(treasureFile);

                        Bukkit.getScheduler().runTask(plugin, () -> {
                            plugin.getTreasureListener().invalidateTreasure(treasureId);
                            result.complete(null);
                        });
                    } catch (IOException e) {
                        plugin.getComponentLogger().error("Could not save {} treasure file.", treasurePath, e);
                    }
                }
            }
        });

        return result;
    }

    /**
     * set the forget-period
     *
     * @param treasureId        information to identify a treasure
     * @param forgettingPeriod  how long a Treasure has to be not looted until it is filled again.
     *                         negative values mean the Treasure will never restock.
     */
    public @NotNull CompletableFuture<Void> setForget(final @NotNull String treasureId, final @Nullable Duration forgettingPeriod) {
        CompletableFuture<Void> result = new CompletableFuture<>();

        Bukkit.getScheduler().runTaskAsynchronously(plugin, () -> {
            synchronized (treasureFolder) {
                Path treasurePath = treasureFolder.resolve(treasureId + ".yml");

                if (Files.notExists(treasurePath)) {
                    try {
                        Files.createFile(treasurePath);
                    } catch (IOException e) {
                        plugin.getComponentLogger().error("Could not create missing {} treasure file.", treasurePath, e);
                    }
                }

                if (Files.isRegularFile(treasurePath)) {
                    try {
                        final File treasureFile = treasurePath.toFile();
                        FileConfiguration cfg = YamlConfiguration.loadConfiguration(treasureFile);
                        setDefaults(cfg);
                        cfg.set(FORGETTING_DURATION,  forgettingPeriod == null ? null : forgettingPeriod.toMillis());

                        cfg.save(treasureFile);

                        Bukkit.getScheduler().runTask(plugin, () -> {
                            plugin.getTreasureListener().invalidateTreasure(treasureId);
                            result.complete(null);
                        });
                    } catch (IOException e) {
                        plugin.getComponentLogger().error("Could not save {} treasure file.", treasurePath, e);
                    }
                }
            }
        });

        return result;
    }

    public @Nullable TreasureInfo loadTreasure(final @NotNull String treasureId) {
        synchronized (treasureFolder) {
            Path treasurePath = treasureFolder.resolve(treasureId + ".yml");
            if (Files.isRegularFile(treasurePath)) {
                final File treasureFile = treasurePath.toFile();
                FileConfiguration cfg = YamlConfiguration.loadConfiguration(treasureFile);

                final @Nullable String rawItemStr = cfg.getString(ITEM_LIST);
                final long forgetting_time = cfg.getLong(FORGETTING_DURATION, DEFAULT_FORGETTING_PERIOD);
                final int slotChance = cfg.getInt(SLOT_CHANCE, DEFAULT_SLOT_CHANCE);
                final boolean isUnlimited = cfg.getBoolean(IS_UNLIMITED, DEFAULT_IS_UNLIMITED);
                final boolean isShared = cfg.getBoolean(IS_SHARED, DEFAULT_IS_SHARED);

                if (rawItemStr != null && ! rawItemStr.isBlank()) {
                    final @NotNull List<ItemStack> items = new ArrayList<> (List.of(ItemStack.deserializeItemsFromBytes(Base64.getDecoder().decode(rawItemStr))));
                    return new TreasureInfo(treasureId, items, Duration.ofMillis(forgetting_time), slotChance, isUnlimited, isShared);
                } else {
                    plugin.getComponentLogger().warn("No or malformed item list found. Skipping.");
                }
            }
        }

        return null;
    }

    private void loadDatabase() {
        Bukkit.getScheduler().runTaskAsynchronously(plugin, () -> {
            synchronized (mutexMainConfig) {
                FileConfiguration mainCfg = plugin.getConfig();

                final @Nullable ConfigurationSection section = mainCfg.getConfigurationSection(CONFIG_KEY_SQL);

                if (section != null) {
                    Bukkit.getScheduler().runTask(plugin, () -> databaseManager.reload(section.getValues(false)));
                }

                mainCfg.set(CONFIG_KEY_SQL, databaseManager.serializeDatabaseConnectionConfig());
                plugin.saveConfig();
            }
        });
    }

    private void loadLegacy () {
        final @NotNull FileConfiguration config = plugin.getConfig();
        if (config.getBoolean(CONFIG_KEY_IMPORT_LEGACY)) {
            new ImportLegacy(plugin).importLegacyData();

            config.set(CONFIG_KEY_IMPORT_LEGACY, Boolean.FALSE);
            plugin.saveConfig();
        }
    }
}
