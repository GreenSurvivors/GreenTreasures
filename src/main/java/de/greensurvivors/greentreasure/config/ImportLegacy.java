package de.greensurvivors.greentreasure.config;

import com.mtihc.minecraft.treasurechest.v8.core.ItemStackWrapper;
import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.TreasureLogger;
import de.greensurvivors.greentreasure.Utils;
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
import java.util.logging.Level;

public class ImportLegacy {
    private final String
            MOTHER_FOLDER = "importLegacy",
            TREASURE_FOLDER = "treasure",
            PLAYER_FOLDER = "players";

    private final String
            STANDART_LEGACY_KEY = "location",
            UNLIMITED = "unlimited",
            GLOBAL = "shared",
            FORGET = "forget-time",
            RANDOM = "random",
            CONTAINER = "container",
            SIZE = buildKey(CONTAINER, "size"),
            TYPE = buildKey(CONTAINER, "type"),
            ITEMS = (buildKey(CONTAINER, "contents"));

    //random = legacyRandom/slots

    private final boolean DEFAULT_IS_UNLIMITED = false;
    private  final boolean DEFAULT_IS_GLOBAL = false;
    private final long DEFAULT_FORGETTING_PERIOD = -1;
    private final int DEFAULT_RANDOM_SLOTS = 0;

    private final long DEFAULT_TIME_STAMP = 0;

    private final HashMap<Location, Integer> inventorySizes = new HashMap<>();

    private static ImportLegacy instance;

    public static ImportLegacy inst() {
        if (instance == null)
            instance = new ImportLegacy();
        return instance;
    }

    public void importLegacyData(){
        importTreasureData();
        importPlayerData();
    }

    /**
     * Combining all arguments to a single FileConfiguration key.
     * @param args key arguments in order
     * @return String
     */
    private static @NotNull String buildKey(String...args) {
        return String.join(".", args);
    }

    private void importTreasureData(){
        File[] legacyWorldFolders = new File(GreenTreasure.inst().getDataFolder().getAbsolutePath() + File.separator + MOTHER_FOLDER + File.separator + TREASURE_FOLDER).listFiles();

        if (legacyWorldFolders != null){
            for (File worldFolder : legacyWorldFolders){
                if (worldFolder.isDirectory()){
                    File[] legacyTresureFiles = worldFolder.listFiles();

                    if (legacyTresureFiles != null){
                        for (File tresureFile : legacyTresureFiles){
                            String worldName = worldFolder.getName();

                            World world = Bukkit.getWorld(worldName);
                            if (world != null){
                                String coordinatesStr = tresureFile.getName();

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
                                    TreasureLogger.log(Level.WARNING, "Can't extract coordinates from name : '" + tresureFile.getPath() + "'. Skipping.");
                                    break;
                                }

                                Location location = new Location(world, coordIntArray[0], coordIntArray[1], coordIntArray[2]);

                                FileConfiguration cfg = YamlConfiguration.loadConfiguration(tresureFile);

                                boolean isUnLimited = cfg.getBoolean(buildKey(STANDART_LEGACY_KEY, UNLIMITED), DEFAULT_IS_UNLIMITED);
                                boolean isGLobal = cfg.getBoolean(buildKey(STANDART_LEGACY_KEY, GLOBAL), DEFAULT_IS_GLOBAL);
                                long forget_time = cfg.getLong(buildKey(STANDART_LEGACY_KEY, FORGET), DEFAULT_FORGETTING_PERIOD);
                                if (forget_time == 0){
                                    forget_time = DEFAULT_FORGETTING_PERIOD;
                                }

                                double randomChance = cfg.getInt(buildKey(STANDART_LEGACY_KEY, RANDOM), DEFAULT_RANDOM_SLOTS);
                                int inventorySize = cfg.getInt(buildKey(STANDART_LEGACY_KEY, SIZE));
                                randomChance = randomChance == 0 ? 100.00 : randomChance / ((double)inventorySize);
                                String type = cfg.getString(buildKey(STANDART_LEGACY_KEY, TYPE));


                                /*
                                 *  original deserialization code by Mitch Mtihic,
                                 *  under GNU General Public License
                                 *  taken from https://github.com/HenrikoMagnifico/TreasureChestX/blob/main/src/main/java/com/mtihc/minecraft/treasurechest/v8/core/BlockInventory.java
                                 */
                                ItemStack[] contents = new ItemStack[inventorySize];
                                Map<?, ?> contentsSection = (Map<?, ?>) cfg.get(buildKey(STANDART_LEGACY_KEY, ITEMS));
                                Set<?> contentsEntries = contentsSection.entrySet();
                                int substringBeginIndex = "item".length();
                                for (Object object : contentsEntries) {
                                    Map.Entry<?, ?> entry = (Map.Entry<?, ?>) object;
                                    String key = (String) entry.getKey();

                                    ItemStack item;
                                    if(entry.getValue() instanceof ItemStack) {
                                        item = (ItemStack) entry.getValue();
                                    }
                                    else if(entry.getValue() instanceof ItemStackWrapper itemStackWrapper){
                                        item = itemStackWrapper.getItemStack();
                                    }
                                    else {
                                        item = null;
                                    }
                                    int index = Integer.parseInt(key.substring(substringBeginIndex));

                                    contents[index] = item;
                                }
                                // original code end

                                TreasureConfig.inst().saveTreasureAsync(location, Arrays.stream(contents).toList(), type);

                                TreasureConfig.inst().setForgetAsync(location, forget_time);
                                TreasureConfig.inst().setGlobalAsync(location, isGLobal);
                                TreasureConfig.inst().setUnlimitedAsync(location, isUnLimited);
                                TreasureConfig.inst().setRandomAsync(location, (int)(randomChance * 100));

                                tresureFile.delete();
                                inventorySizes.put(location, inventorySize);
                            } else {
                                TreasureLogger.log(Level.WARNING, "Can't extract world from name : '" + tresureFile.getPath() + "'. Skipping.");
                            }
                        }
                    }
                }
            }
        }
    }

    private void importPlayerData(){
        File[] legacyPlayerFiles = new File(GreenTreasure.inst().getDataFolder().getAbsolutePath() + File.separator + MOTHER_FOLDER + File.separator + TREASURE_FOLDER).listFiles();

        if (legacyPlayerFiles != null){
            for (File playerFile : legacyPlayerFiles){
                String playerName = playerFile.getName();

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
                                    cfgSection_Worlds.getKeys(false).forEach(coordinatesStr -> {
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
                                            TreasureLogger.log(Level.WARNING, "Can't extract coordinates from name : '" + coordinatesStr + "'. Skipping.");
                                        } else {
                                            Location location = new Location(world, coordIntArray[0], coordIntArray[1], coordIntArray[2]);

                                            long timeStamp = cfg.getLong(buildKey(worldName, coordinatesStr), DEFAULT_TIME_STAMP);

                                            Integer inventorySize = inventorySizes.get(location);
                                            if (inventorySize != null) {
                                                List<ItemStack> unLootedItems = Arrays.stream((new ItemStack[inventorySize])).toList();

                                                TreasureConfig.inst().savePlayerDetailAsync(uuid, location, timeStamp, unLootedItems);

                                                playerFile.delete();
                                            }
                                        }
                                    });
                                }
                            } else {
                                TreasureLogger.log(Level.WARNING, "Unknown world: " + worldName + ". Skipping.");
                            }
                        });
                    } else {
                        TreasureLogger.log(Level.WARNING, "No worlds found in " + playerFile.getPath() + " Skipping.");
                    }
                } else {
                    TreasureLogger.log(Level.WARNING, "Player '" + playerName + "' has never played before. Skipping.");
                }
            }
        }






    }
}
