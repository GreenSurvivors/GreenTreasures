package de.greensurvivors.greentreasure.config;

import com.mtihc.minecraft.treasurechest.v8.core.ITreasureChest;
import com.mtihc.minecraft.treasurechest.v8.plugin.TreasureChestPlugin;
import de.greensurvivors.greentreasure.TreasureLogger;
import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.listener.TreasureListener;
import org.apache.commons.io.FilenameUtils;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.OfflinePlayer;
import org.bukkit.World;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;

import static org.bukkit.Bukkit.getServer;

public class ImportLegacy {
    private final String
            TREASURE_FOLDER = "treasure",
            PLAYER_FOLDER = "players";

    private final long DEFAULT_FORGETTING_PERIOD = -1;

    private final long DEFAULT_TIME_STAMP = 0;

    private TreasureChestPlugin treasureChestPlugin;
    private File treasurePluginFolder;

    private final HashMap<Location, Integer> inventorySizes = new HashMap<>();

    private boolean hasAlreadyImported = false;

    private static ImportLegacy instance;

    public static ImportLegacy inst() {
        if (instance == null)
            instance = new ImportLegacy();
        return instance;
    }

    public void importLegacyData(){
        if (!hasAlreadyImported){
            hasAlreadyImported = true;

            TreasureLogger.log(Level.INFO, "starting import process");

            treasureChestPlugin = (TreasureChestPlugin)getServer().getPluginManager().getPlugin("TreasureChest");
            if (treasureChestPlugin == null){
                treasureChestPlugin = (TreasureChestPlugin)getServer().getPluginManager().getPlugin("TreasureChestX");

                if (treasureChestPlugin == null){
                    TreasureLogger.log(Level.INFO, "No TreasurePlugin enabled. No importing could be done.");
                    return;
                }
            }

            if (!treasureChestPlugin.isEnabled()){
                TreasureLogger.log(Level.INFO, "No TreasurePlugin enabled. No importing could be done.");

                return;
            }

            treasurePluginFolder = treasureChestPlugin.getDataFolder();

            importTreasureData();
            importPlayerData();
        }
    }

    /**
     * Combining all arguments to a single FileConfiguration key.
     * @param args key arguments in order
     * @return String
     */
    private static @NotNull String buildKey(String...args) {
        return String.join(".", args);
    }

    /**
     * import legacy treasures
     */
    private void importTreasureData(){
        TreasureLogger.log(Level.INFO, "import Treasures");
        File[] legacyWorldFolders = new File(treasurePluginFolder.getPath() + File.separator + TREASURE_FOLDER).listFiles();

        if (legacyWorldFolders != null){
            for (File worldFolder : legacyWorldFolders){
                if (worldFolder.isDirectory()){
                    File[] legacyTresureFiles = worldFolder.listFiles();

                    if (legacyTresureFiles != null){
                        for (File tresureFile : legacyTresureFiles){
                            String worldName = FilenameUtils.removeExtension(worldFolder.getName());

                            World world = Bukkit.getWorld(worldName);
                            if (world != null){
                                String coordinatesStr = FilenameUtils.removeExtension(tresureFile.getName());

                                String[] coordStrArray = coordinatesStr.split("_");

                                //convert to integer
                                Integer[] coordIntArray = Arrays.stream(coordStrArray).map(coordStr -> {
                                    if (Utils.isInt(coordStr)){
                                        return Integer.parseInt(coordStr);
                                    } else {
                                        return null;
                                    }
                                }).toArray(Integer[]::new);

                                // can't get coordinates form name. skipping
                                if (Arrays.stream(coordIntArray).anyMatch(Objects::isNull) || coordIntArray.length != 3){
                                    TreasureLogger.log(Level.WARNING, "[treasure] Can't extract coordinates from name: '" + tresureFile.getPath() + "'. Skipping.");
                                    break;
                                }

                                Location location = new Location(world, coordIntArray[0], coordIntArray[1], coordIntArray[2]);


                                ITreasureChest treasureChest = treasureChestPlugin.getManager().getTreasure(location);

                                boolean isUnLimited = treasureChest.isUnlimited();
                                boolean isGLobal = treasureChest.isUnlimited();
                                long forget_time = treasureChest.getForgetTime();
                                if (forget_time == 0){
                                    forget_time = DEFAULT_FORGETTING_PERIOD;
                                }

                                int inventorySize = treasureChest.getContainer().getSize();
                                double randomChance = treasureChest.getAmountOfRandomlyChosenStacks();
                                randomChance = randomChance == 0 ? 100.00 : randomChance / ((double)inventorySize);

                                ItemStack[] contents = treasureChest.getContainer().getContents();

                                String type = treasureChest.getContainer().getType().name().toUpperCase();


                                TreasureConfig.inst().saveTreasureAsync(location, Arrays.stream(contents).toList(), type, () -> {});

                                TreasureConfig.inst().setForgetAsync(location, forget_time, () -> {});
                                TreasureConfig.inst().setGlobalAsync(location, isGLobal, () -> {});
                                TreasureConfig.inst().setUnlimitedAsync(location, isUnLimited, () -> {});
                                TreasureConfig.inst().setRandomAsync(location, (int)(randomChance * 100), () -> {});

                                //remove the treasure form the plugin and then v it
                                treasureChestPlugin.getManager().removeTreasure(location);
                                tresureFile.delete();

                                inventorySizes.put(location, contents.length);
                                TreasureLogger.log(Level.INFO, "imported treasure" + tresureFile.getPath());
                            } else {
                                TreasureLogger.log(Level.WARNING, "Can't extract world from name : " + worldName + " in '" + tresureFile.getPath() + "'. Skipping.");
                            }
                        }
                    }
                }
            }
        }
    }


    /**
     * import player data
     */
    private void importPlayerData(){
        TreasureLogger.log(Level.INFO, "import PlayerData");

        File[] legacyPlayerFiles = new File(treasurePluginFolder.getPath() + File.separator + PLAYER_FOLDER).listFiles();

        if (legacyPlayerFiles != null){
            for (File playerFile : legacyPlayerFiles){
                AtomicBoolean gotNoError = new AtomicBoolean(true);

                String playerName = FilenameUtils.removeExtension(playerFile.getName());

                OfflinePlayer offlinePlayer = Bukkit.getOfflinePlayer(playerName);

                if (offlinePlayer.hasPlayedBefore()){
                    UUID uuid = offlinePlayer.getUniqueId();

                    FileConfiguration cfg = YamlConfiguration.loadConfiguration(playerFile);
                    ConfigurationSection cfgSection_Worlds = cfg.getConfigurationSection("");

                    if (cfgSection_Worlds != null){
                        cfgSection_Worlds.getKeys(false).forEach(worldName -> {
                            World world = Bukkit.getWorld(worldName);

                            if (world != null){
                                ConfigurationSection cfgSection_coords = cfg.getConfigurationSection(worldName);

                                if (cfgSection_coords != null){
                                    cfgSection_coords.getKeys(false).forEach(coordinatesStr -> {
                                        String[] coordStrArray = coordinatesStr.split("_");

                                        //convert to integer
                                        Integer[] coordIntArray = Arrays.stream(coordStrArray).map(coordStr -> {
                                            if (Utils.isInt(coordStr)){
                                                return Integer.parseInt(coordStr);
                                            } else {
                                                return null;
                                            }
                                        }).toArray(Integer[]::new);

                                        // can't get coordinates form name. skipping
                                        if (Arrays.stream(coordIntArray).anyMatch(Objects::isNull) || coordIntArray.length != 3){
                                            TreasureLogger.log(Level.WARNING, "[playerData] Can't extract coordinates from name: '" + coordinatesStr + "'. Skipping.");
                                            gotNoError.set(false);
                                        } else {
                                            Location location = new Location(world, coordIntArray[0], coordIntArray[1], coordIntArray[2]);

                                            long timeStamp = cfg.getLong(buildKey(worldName, coordinatesStr), DEFAULT_TIME_STAMP);

                                            Integer inventorySize = inventorySizes.get(location);
                                            //if treasure was not imported this instance, try to get it via already loaded ones
                                            if (inventorySize == null){
                                                TreasureInfo treasureInfo = TreasureListener.inst().getTreasure(location);
                                                if (treasureInfo != null){
                                                    inventorySize = treasureInfo.itemLoot().size();
                                                }
                                            }

                                            if (inventorySize != null) {
                                                List<ItemStack> unLootedItems = Arrays.stream((new ItemStack[inventorySize])).toList();

                                                TreasureConfig.inst().savePlayerDetailAsync(uuid, location, timeStamp, unLootedItems);

                                                //remove from cfg
                                                cfg.set(buildKey(worldName, coordinatesStr), null);
                                            }
                                        }
                                    });
                                }
                            } else {
                                TreasureLogger.log(Level.WARNING, "Unknown world: " + worldName + ". Skipping.");
                                gotNoError.set(false);
                            }
                        });
                    } else {
                        TreasureLogger.log(Level.WARNING, "No worlds found in " + playerFile.getPath() + " Skipping.");
                        gotNoError.set(false);
                    }
                } else {
                    TreasureLogger.log(Level.WARNING, "Player '" + playerName + "' has never played before. Skipping.");
                    gotNoError.set(false);
                }

                if (gotNoError.get()){
                    playerFile.delete();
                    TreasureLogger.log(Level.INFO, "imported player info for " + playerFile.getPath());
                }
            }
        }
    }
}
