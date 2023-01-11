package de.greensurvivors.greentreasure.config;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.TreasureLogger;
import org.bukkit.Location;
import org.bukkit.configuration.file.YamlConfiguration;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.logging.Level;

public class PlayerFile {
    private YamlConfiguration cfg = null;

    private final String fileName;
    public final String path;
    public static final String FOLDER = "playerFiles";

    public PlayerFile(String playerID, Location location) {
        // for some reason linux doesn't handle '-' very well in folderNames.
        this.path = (FOLDER + File.separator + playerID + File.separator + location.getWorld().getName() + File.separator).replace("-", "_");

        this.fileName = location.getBlockX() + "_" + location.getBlockY() + "_" + location.getBlockZ() + ".yml";
    }


    /**
     * Save configuration to file.
     */
    public void saveCfg() {
        File file = new File(GreenTreasure.inst().getDataFolder(), path + fileName);

        //make sure the cfg was loaded
        getCfg();

        // save modified configuration
        cfg.options().setHeader(Collections.singletonList(String.format(
                fileName.replace(" .yml", "").replace("_", " ") + " playerFile for %s (%s)",
                GreenTreasure.inst().getName(),
                GreenTreasure.inst().getDescription().getVersion())));
        cfg.options().parseComments(true);
        try {
            cfg.save(file);
        } catch (IOException e) {
            TreasureLogger.log(Level.SEVERE, "Could not save " + fileName + " player file.", e);
        }
    }

    /**
     * get the config, if null was the last value try to load
     * @return cfg
     */
    public YamlConfiguration getCfg() {
        if (cfg == null) {
            File file = new File(GreenTreasure.inst().getDataFolder(), path + fileName);
            cfg = YamlConfiguration.loadConfiguration(file);
        }
        return cfg;
    }
}
