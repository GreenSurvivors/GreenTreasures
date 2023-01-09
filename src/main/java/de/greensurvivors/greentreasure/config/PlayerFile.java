package de.greensurvivors.greentreasure.config;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.TreasureLogger;
import org.bukkit.configuration.file.YamlConfiguration;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.logging.Level;

public class PlayerFile {
    private YamlConfiguration cfg = null;

    private final String fileName;
    public static final String FOLDER = "playerFiles";

    public PlayerFile(String fileName) {
        this.fileName = FOLDER + File.separator + fileName;
    }


    /**
     * Save configuration to file.
     */
    public void saveCfg() {
        File file = new File(GreenTreasure.inst().getDataFolder(), fileName);

        //make sure the cfg was loaded
        getCfg();

        // save modified configuration
        cfg.options().setHeader(Collections.singletonList(String.format(
                fileName.replace(".yml", "").replace("_", " ") + " playerFile for %s (%s)",
                GreenTreasure.inst().getName(),
                GreenTreasure.inst().getDescription().getVersion())));
        cfg.options().parseComments(true);
        try {
            cfg.save(file);
        } catch (IOException e) {
            TreasureLogger.log(Level.SEVERE, "Could not save " + fileName + " configuration file.", e);
        }
    }

    /**
     * get the config, if null was the last value try to load
     * @return cfg
     */
    public YamlConfiguration getCfg() {
        if (cfg == null) {
            File file = new File(GreenTreasure.inst().getDataFolder(), fileName);
            cfg = YamlConfiguration.loadConfiguration(file);
        }
        return cfg;
    }
}
