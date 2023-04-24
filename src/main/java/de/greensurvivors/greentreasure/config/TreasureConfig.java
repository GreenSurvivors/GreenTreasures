package de.greensurvivors.greentreasure.config;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.TreasureLogger;
import de.greensurvivors.greentreasure.dataobjects.DatabaseManager;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.dataobjects.callback.EmptyCallback;
import de.greensurvivors.greentreasure.dataobjects.callback.PlayerLootDetailCallback;
import de.greensurvivors.greentreasure.language.Lang;
import de.greensurvivors.greentreasure.listener.TreasureListener;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.OfflinePlayer;
import org.bukkit.World;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;

import javax.annotation.Nullable;
import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.logging.Level;
import java.util.regex.Pattern;

public class TreasureConfig {
    private static final Pattern COORD_PATTERN = Pattern.compile("^[+-]?\\d+;[+-]?\\d+;[+-]?\\d+$");
    private static TreasureConfig instance;
    private final String
            TREASURE_FOLDER = "treasures";
    private final String
            WORLDS = "worlds",
            COORDS = "coords",
            TIME_STAMP = "timeStamp",
            ITEM_LIST = "items",
            FORGETTING_PERIOD = "forgetting_period",
            SLOT_CHANCE = "slot_chance",
            IS_UNLIMITED = "is_unlimited",
            IS_GLOBAL = "is_global",
            TREASURE_TYPE = "type";
    private final UUID
            // in case a treasure was shared (herobrine)
            GLOBAL_UUID = UUID.fromString("f84c6a79-0a4e-45e0-879b-cd49ebd4c4e2");
    private final long DEFAULT_TIME_STAMP = 0;
    private final long DEFAULT_FORGETTING_PERIOD = -1;
    private final int DEFAULT_SLOT_CHANCE = 10000;
    private final boolean DEFAULT_IS_UNLIMITED = false;
    private final boolean DEFAULT_IS_GLOBAL = false;
    private final String DEFAULT_TYPE = "unknown";
    //ensures only one thread works with a file at a time
    private final Object mutexDatabase = new Object();
    private final Object mutexTreasureFile = new Object();
    private final Object mutexMainConfig = new Object();
    private DatabaseManager databaseManager = null;

    public static TreasureConfig inst() {
        if (instance == null)
            instance = new TreasureConfig();
        return instance;
    }

    /**
     * Combining all arguments to a single FileConfiguration key.
     *
     * @param args key arguments in order
     * @return String
     */
    private static @NotNull String buildKey(String... args) {
        return String.join(".", args);
    }

    /**
     * takes a list of itemStack and trys to serialize all of them into a List of Maps (or null)
     *
     * @param itemList a list of ItemStack
     * @return serialized list of Map<String, Object>
     */
    public static List<Map<String, Object>> serializeItemList(@Nullable List<ItemStack> itemList) {
        if (itemList == null) {
            return null;
        }

        List<Map<String, Object>> serializedOutput = new LinkedList<>();

        for (ItemStack itemStack : itemList) {
            if (itemStack == null) {
                serializedOutput.add(null);
            } else {
                serializedOutput.add(itemStack.serialize());
            }
        }

        return serializedOutput;
    }

    /**
     * takes a list of Objects and trys to convert it into a List of ItemStack (or null)
     *
     * @param itemObjs a list of Map<String, Object>, as ItemStack.serialize() outputs
     * @return deserialized List of ItemStack or null, if the input is null
     */
    public static List<ItemStack> deserializeItemList(@Nullable List<?> itemObjs) {
        if (itemObjs != null) {
            List<ItemStack> items = new ArrayList<>();

            // go through every object end try to convert it into a map
            for (Object itemObj : itemObjs) {
                //default value if deserializing was impossible
                ItemStack itemStack = null;
                if (itemObj instanceof Map<?, ?> itemMapObj) {
                    //test every item property
                    Map<String, Object> itemMap = new HashMap<>();
                    for (Object itemPropObj : itemMapObj.keySet()) {
                        if (itemPropObj instanceof String itemPropStr) {
                            itemMap.put(itemPropStr, itemMapObj.get(itemPropObj));
                        }
                    }

                    //deserialize the map into an itemStack
                    itemStack = ItemStack.deserialize(itemMap);
                }

                items.add(itemStack);
            }

            return items;
        } else {
            return null;
        }
    }

    /**
     * saves the params to a player file
     *
     * @param uuid             the UUID of a looting player null means global
     * @param location         the location of a treasure, the information for identify a treasure
     * @param timeStamp        when has the player last opened the treasure?
     * @param unlootedTreasure the inventory with all the remaining items. null means the player will be reset
     */
    public void savePlayerDetailAsync(@Nullable UUID uuid, @NotNull Location location, long timeStamp, @Nullable List<ItemStack> unlootedTreasure) {
        Bukkit.getScheduler().runTaskAsynchronously(GreenTreasure.inst(), () -> {
            //get offline player of uuid
            OfflinePlayer player = Bukkit.getOfflinePlayer(uuid == null ? GLOBAL_UUID : uuid);
            String id = location.getWorld().getName() + "_" + location.getBlockX() + "_" + location.getBlockY() + "_" + location.getBlockZ();

            synchronized (mutexDatabase) {
                databaseManager.setPlayerData(player, id, new PlayerLootDetail(timeStamp, unlootedTreasure));
            }
        });
    }

    /**
     * forget a player has ever looted a treasure
     *
     * @param uuid     the UUID of a looting player null means global
     * @param location the location of a treasure, the information for identify a treasure
     */
    public void forgetPlayerAsync(@Nullable UUID uuid, @NotNull Location location) {
        Bukkit.getScheduler().runTaskAsynchronously(GreenTreasure.inst(), () -> {
            /*path = "playerFile_" + (uuid == null ? GLOBAL_FILE_NAME : uuid.toString());
            fileName = location.getWorld().getName() + "_" + location.getBlockX() + "_" + location.getBlockY() + "_" + location.getBlockZ();

            synchronized (mutexDatabase) {

                String keyFirstPart = buildKey(WORLDS, location.getWorld().getName(),
                        COORDS, location.getBlockX() + ";" + location.getBlockY() + ";" + location.getBlockZ());

                cfg.set(buildKey(keyFirstPart, ITEM_LIST), null);
                cfg.set(buildKey(keyFirstPart, TIME_STAMP), null);

                playerFile.saveCfg();
            }*/
        });
    }

    /**
     * forget anybody has ever opened the treasure at the given location.
     *
     * @param location the location of a treasure, the information for identify a treasure
     */
    public void forgetAllAsync(@NotNull Location location, @NotNull EmptyCallback emptyCallback) {
        Bukkit.getScheduler().runTaskAsynchronously(GreenTreasure.inst(), () -> {
            synchronized (mutexDatabase) {
                String id = location.getWorld().getName() + "_" + location.getBlockX() + "_" + location.getBlockY() + "_" + location.getBlockZ();
                databaseManager.forgetAll(id);

                emptyCallback.onQueryDone();
            }
        });
    }

    /**
     * load the status of a player about a treasure and calls a function with the information back in the main thread.
     *
     * @param uuid                     the UUID of a looting player null means global
     * @param location                 the location of a treasure, the information for identify a treasure
     * @param playerLootDetailCallback a function to be called after the loading is done, to receive the playerLootDetail
     */
    public void getPlayerLootDetailAsync(@Nullable UUID uuid, @NotNull Location location, @NotNull PlayerLootDetailCallback playerLootDetailCallback) {
        Bukkit.getScheduler().runTaskAsynchronously(GreenTreasure.inst(), () -> {
            //get offline player of uuid
            OfflinePlayer player = Bukkit.getOfflinePlayer(uuid == null ? GLOBAL_UUID : uuid);
            String id = location.getWorld().getName() + "_" + location.getBlockX() + "_" + location.getBlockY() + "_" + location.getBlockZ();

            synchronized (mutexDatabase) {
                // load player loot detail information async
                PlayerLootDetail playerLootDetail = databaseManager.getPlayerData(player, id);

                // give the data back to the main thread
                Bukkit.getScheduler().scheduleSyncDelayedTask(GreenTreasure.inst(), () ->
                        playerLootDetailCallback.onQueryDone(playerLootDetail));
            }
        });
    }

    /**
     * get a treasureFile from a location
     */
    private File getTreasureFile(@NotNull Location location) {
        String pathAndFile = TREASURE_FOLDER + File.separator +
                location.getWorld().getName() + File.separator +
                location.getBlockX() + "_" + location.getBlockY() + "_" + location.getBlockZ() + ".yml";

        return new File(GreenTreasure.inst().getDataFolder(), pathAndFile);
    }

    /**
     * Load main configurations.
     */
    public void reloadMain() {
        loadDatabase();
        loadLanguage();
        loadTreasuresSave(() -> {});
    }

    /**
     * Load language configuration.
     */
    private void loadLanguage() {
        File file = new File(GreenTreasure.inst().getDataFolder(), "language.yml");
        FileConfiguration cfg = YamlConfiguration.loadConfiguration(file);
        String k;
        // check Config
        for (Lang l : Lang.values()) {
            k = l.name().replaceAll("_", ".");
            // load value, if value not exist add
            if (cfg.contains(k))
                l.set(cfg.getString(k));
            else
                cfg.set(k, l.get());
        }
        // save modified configuration
        cfg.options().setHeader(Collections.singletonList(String.format(
                "Language configuration for %s (%s)",
                GreenTreasure.inst().getName(),
                GreenTreasure.inst().getDescription().getVersion())));
        cfg.options().parseComments(true);
        try {
            cfg.save(file);
        } catch (IOException e) {
            TreasureLogger.log(Level.SEVERE, "Could not save language configuration.", e);
        }
    }

    /**
     * Save default configuration values.
     */
    private void setDefaults(FileConfiguration cfg) {
        cfg.options().setHeader(List.of(GreenTreasure.inst().getName() + " " + GreenTreasure.inst().getDescription().getVersion()));
        cfg.options().copyDefaults(true);
        cfg.options().parseComments(true);
    }

    /**
     * deletes a treasure
     *
     * @param location the location of a treasure, the information for identify a treasure
     */
    public void deleteTreasureAsync(@NotNull Location location, @NotNull EmptyCallback emptyCallback) {
        Bukkit.getScheduler().runTaskAsynchronously(GreenTreasure.inst(), () -> {
            synchronized (mutexTreasureFile) {
                File treasureFile = getTreasureFile(location);
                FileConfiguration cfg = YamlConfiguration.loadConfiguration(treasureFile);
                setDefaults(cfg);

                try {
                    treasureFile.delete();

                    //reload treasures
                    loadTreasuresSave(emptyCallback);
                } catch (Exception e) {
                    TreasureLogger.log(Level.SEVERE, "Could not save " + treasureFile.getName() + " treausre file.", e);
                }
            }
        });
    }

    /**
     * save a treasure
     *
     * @param location  the location of a treasure, the information for identify a treasure
     * @param lootItems list of items a player should receive upon opening the treasure. Size must match the inventory size of the given type.
     * @param type      type of treasure. it's imported to identify the opened Inventory
     */
    public void saveTreasureAsync(@NotNull Location location, @Nullable List<ItemStack> lootItems, @Nullable String type, @NotNull EmptyCallback emptyCallback) {
        Bukkit.getScheduler().runTaskAsynchronously(GreenTreasure.inst(), () -> {
            synchronized (mutexTreasureFile) {
                File treasureFile = getTreasureFile(location);
                FileConfiguration cfg = YamlConfiguration.loadConfiguration(treasureFile);
                setDefaults(cfg);

                String cfgKey = buildKey(WORLDS, location.getWorld().getName(),
                        COORDS, location.getBlockX() + ";" + location.getBlockY() + ";" + location.getBlockZ());

                cfg.set(buildKey(cfgKey, ITEM_LIST), serializeItemList(lootItems));
                cfg.set(buildKey(cfgKey, TREASURE_TYPE), type);

                try {
                    cfg.save(treasureFile);

                    //reload treasures
                    loadTreasuresSave(emptyCallback);
                } catch (IOException e) {
                    TreasureLogger.log(Level.SEVERE, "Could not save " + treasureFile.getName() + " treausre file.", e);
                }
            }
        });
    }

    /**
     * set the random slot chance
     *
     * @param location   the location of a treasure, the information for identify a treasure
     * @param slotChance how probable it is for a slot to be in the freshly opened treasure
     */
    public void setRandomAsync(@NotNull Location location, int slotChance, @NotNull EmptyCallback emptyCallback) {
        Bukkit.getScheduler().runTaskAsynchronously(GreenTreasure.inst(), () -> {
            synchronized (mutexTreasureFile) {
                File treasureFile = getTreasureFile(location);
                FileConfiguration cfg = YamlConfiguration.loadConfiguration(treasureFile);
                setDefaults(cfg);

                String cfgKey = buildKey(WORLDS, location.getWorld().getName(),
                        COORDS, location.getBlockX() + ";" + location.getBlockY() + ";" + location.getBlockZ(), SLOT_CHANCE);

                cfg.set(cfgKey, slotChance);

                try {
                    cfg.save(treasureFile);

                    //reload treasures
                    loadTreasuresSave(emptyCallback);
                } catch (IOException e) {
                    TreasureLogger.log(Level.SEVERE, "Could not save " + treasureFile.getName() + " treausre file.", e);
                }
            }
        });
    }

    /**
     * set the global status
     *
     * @param location the location of a treasure, the information for identify a treasure
     * @param isGlobal if the Inventory is globally shared across all players
     */
    public void setGlobalAsync(@NotNull Location location, boolean isGlobal, @NotNull EmptyCallback emptyCallback) {
        Bukkit.getScheduler().runTaskAsynchronously(GreenTreasure.inst(), () -> {
            synchronized (mutexTreasureFile) {
                File treasureFile = getTreasureFile(location);
                FileConfiguration cfg = YamlConfiguration.loadConfiguration(treasureFile);
                setDefaults(cfg);

                String cfgKey = buildKey(WORLDS, location.getWorld().getName(),
                        COORDS, location.getBlockX() + ";" + location.getBlockY() + ";" + location.getBlockZ(), IS_GLOBAL);

                cfg.set(cfgKey, isGlobal);

                try {
                    cfg.save(treasureFile);

                    //reload treasures
                    loadTreasuresSave(emptyCallback);
                } catch (IOException e) {
                    TreasureLogger.log(Level.SEVERE, "Could not save " + treasureFile.getName() + " treausre file.", e);
                }
            }
        });
    }

    /**
     * set the unlimited status
     *
     * @param location    the location of a treasure, the information for identify a treasure
     * @param isUnLimited if one can loot the treasure as often as one wants
     */
    public void setUnlimitedAsync(@NotNull Location location, boolean isUnLimited, @NotNull EmptyCallback emptyCallback) {
        Bukkit.getScheduler().runTaskAsynchronously(GreenTreasure.inst(), () -> {
            synchronized (mutexTreasureFile) {
                File treasureFile = getTreasureFile(location);
                FileConfiguration cfg = YamlConfiguration.loadConfiguration(treasureFile);
                setDefaults(cfg);

                String cfgKey = buildKey(WORLDS, location.getWorld().getName(),
                        COORDS, location.getBlockX() + ";" + location.getBlockY() + ";" + location.getBlockZ(), IS_UNLIMITED);

                cfg.set(cfgKey, isUnLimited);

                try {
                    cfg.save(treasureFile);

                    //reload treasures
                    loadTreasuresSave(emptyCallback);
                } catch (IOException e) {
                    TreasureLogger.log(Level.SEVERE, "Could not save " + treasureFile.getName() + " treausre file.", e);
                }
            }
        });
    }

    /**
     * set the forget-period
     *
     * @param location         the location of a treasure, the information for identify a treasure
     * @param forgettingPeriod the period in milliseconds how long a Treasure has to be not looted until it is filled again.
     *                         negative values mean the Treasure will never restock.
     */
    public void setForgetAsync(Location location, long forgettingPeriod, EmptyCallback emptyCallback) {
        Bukkit.getScheduler().runTaskAsynchronously(GreenTreasure.inst(), () -> {
            synchronized (mutexTreasureFile) {
                File treasureFile = getTreasureFile(location);
                FileConfiguration cfg = YamlConfiguration.loadConfiguration(treasureFile);
                setDefaults(cfg);

                String cfgKey = buildKey(WORLDS, location.getWorld().getName(),
                        COORDS, location.getBlockX() + ";" + location.getBlockY() + ";" + location.getBlockZ(), FORGETTING_PERIOD);

                cfg.set(cfgKey, forgettingPeriod);

                try {
                    cfg.save(treasureFile);

                    //reload treasures
                    loadTreasuresSave(emptyCallback);
                } catch (IOException e) {
                    TreasureLogger.log(Level.SEVERE, "Could not save " + treasureFile.getName() + " treausre file.", e);
                }
            }
        });
    }

    /**
     * load the loot of the containers
     */
    private void loadTreasures(EmptyCallback emptyCallback) {
        File treasureMotherFolder = new File(GreenTreasure.inst().getDataFolder(), TREASURE_FOLDER);

        //get worldFolders
        File[] worldFolders = treasureMotherFolder.listFiles();
        if (worldFolders != null) {
            for (File worldFolder : worldFolders) {

                //get treasureFiles
                File[] treasureFiles = worldFolder.listFiles();
                if (treasureFiles != null) {
                    for (File treasureFile : treasureFiles) {
                        FileConfiguration cfg = YamlConfiguration.loadConfiguration(treasureFile);

                        // load worlds
                        if (cfg.contains(WORLDS)) {
                            ConfigurationSection cfgSection_Worlds = cfg.getConfigurationSection(WORLDS);

                            if (cfgSection_Worlds != null) {
                                cfgSection_Worlds.getKeys(false).forEach(worldName -> {
                                    World world = Bukkit.getWorld(worldName);

                                    if (world != null) {
                                        //load coordinates of  containers
                                        if (cfg.contains(buildKey(WORLDS, worldName, COORDS))) {
                                            ConfigurationSection cfgSection_coords = cfg.getConfigurationSection(buildKey(WORLDS, worldName, COORDS));

                                            if (cfgSection_coords != null) {
                                                cfgSection_coords.getKeys(false).forEach(coordStr -> {
                                                    if (COORD_PATTERN.matcher(coordStr).find()) {
                                                        String[] coordsArray = coordStr.split(";");
                                                        int x = Integer.parseInt(coordsArray[0]);
                                                        int y = Integer.parseInt(coordsArray[1]);
                                                        int z = Integer.parseInt(coordsArray[2]);
                                                        Location location = new Location(world, x, y, z);

                                                        //load loot
                                                        List<?> itemObjs = cfg.getList(buildKey(WORLDS, worldName, COORDS, coordStr, ITEM_LIST));

                                                        long forgetting_time = cfg.getLong(buildKey(WORLDS, worldName, COORDS, coordStr, FORGETTING_PERIOD), DEFAULT_FORGETTING_PERIOD);
                                                        int slotChance = cfg.getInt(buildKey(WORLDS, worldName, COORDS, coordStr, SLOT_CHANCE), DEFAULT_SLOT_CHANCE);
                                                        boolean isUnlimited = cfg.getBoolean(buildKey(WORLDS, worldName, COORDS, coordStr, IS_UNLIMITED), DEFAULT_IS_UNLIMITED);
                                                        boolean isShared = cfg.getBoolean(buildKey(WORLDS, worldName, COORDS, coordStr, IS_GLOBAL), DEFAULT_IS_GLOBAL);
                                                        String TreasureType = cfg.getString(buildKey(buildKey(WORLDS, worldName, COORDS, coordStr, TREASURE_TYPE)), DEFAULT_TYPE);

                                                        if (itemObjs != null) {
                                                            TreasureListener.inst().addTreasure(location, new TreasureInfo(deserializeItemList(itemObjs), forgetting_time, slotChance, isUnlimited, isShared, TreasureType));
                                                        } else {
                                                            TreasureLogger.log(Level.WARNING, "No or malformed item list found. Skipping.");
                                                        }
                                                    } else {
                                                        TreasureLogger.log(Level.WARNING, String.format("Malformed coordinates string was found: '%s'. Skipping.", coordStr));
                                                    }
                                                });
                                            } else {
                                                TreasureLogger.log(Level.WARNING, "coordinate keyword was given but no coordinates where found.");
                                            }
                                        } else {
                                            TreasureLogger.log(Level.FINE, "World was given but no coordinates where defined.");
                                        }
                                    } else {
                                        TreasureLogger.log(Level.WARNING, "No such world '%s' was found. Skipping.");
                                    }
                                });
                            } else {
                                TreasureLogger.log(Level.FINE, "Treasure keyword was given but no worlds where found.");
                            }
                        } else {
                            TreasureLogger.log(Level.FINE, "No Treasures to load.");
                        }
                    }
                }
            }
        }

        emptyCallback.onQueryDone();
    }

    private void loadDatabase() {
        HashMap<String, Object> serializedDatabaseData = new HashMap<>();

        Bukkit.getScheduler().runTaskAsynchronously(GreenTreasure.inst(), () -> {
            synchronized (mutexMainConfig) {
                FileConfiguration mainCfg = GreenTreasure.inst().getConfig();

                if (mainCfg.contains("SQL")) {
                    ConfigurationSection section = mainCfg.getConfigurationSection("SQL");

                    if (section != null) {
                        for (String key : section.getKeys(false)) {
                            serializedDatabaseData.put(key, mainCfg.get("SQL." + key));
                        }
                    }
                }

                synchronized (mutexDatabase) {
                    databaseManager = new DatabaseManager(GreenTreasure.inst(), serializedDatabaseData);

                    mainCfg.set("SQL", databaseManager.getDatabaseData());
                }

                GreenTreasure.inst().saveConfig();
            }
        });
    }

    /**
     * Load treasures and clears cached ones.
     */
    public void loadTreasuresSave(@NotNull EmptyCallback emptyCallback) {
        Bukkit.getScheduler().scheduleSyncDelayedTask(GreenTreasure.inst(), () -> {
            // clear cache
            TreasureListener.inst().clearTreasures();
            Bukkit.getScheduler().runTaskAsynchronously(GreenTreasure.inst(), () -> {
                synchronized (mutexTreasureFile) {
                    loadTreasures(emptyCallback);
                }
                ImportLegacy.inst().importLegacyData();
            });
        });
    }
}
