package de.greensurvivors.greentreasure.config;

import de.greensurvivors.corelib.config.IVersionedConfigData;
import de.greensurvivors.greentreasure.BreakBehavior;
import org.jetbrains.annotations.NotNull;
import org.spongepowered.configurate.objectmapping.ConfigSerializable;

@ConfigSerializable
public class ConfigData implements IVersionedConfigData {
    public static final int CURRENT_DATA_VERSION = 1;

    protected int dataversion = CURRENT_DATA_VERSION;
    protected boolean importLegacy = false;
    protected @NotNull BreakBehavior breakBehavior = BreakBehavior.ALL_BREAK_GLOBAL;

    public int dataVersion() {
        return dataversion;
    }

    public boolean importLegacy() {
        return importLegacy;
    }

    public void setImportLegacy(final boolean importLegacy) {
        this.importLegacy = importLegacy;
    }

    public @NotNull BreakBehavior breakBehavior() {
        return breakBehavior;
    }
}
