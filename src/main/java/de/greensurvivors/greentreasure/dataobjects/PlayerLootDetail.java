package de.greensurvivors.greentreasure.dataobjects;

import de.greensurvivors.greentreasure.TreasureLogger;
import de.greensurvivors.greentreasure.config.TreasureConfig;
import org.bukkit.configuration.serialization.ConfigurationSerializable;
import org.bukkit.configuration.serialization.ConfigurationSerialization;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.Serial;
import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

/**
 * all important information about a treasure a player may or may not have opened before
 *
 * @param lastLootedTimeStamp the unix time in milliseconds a player last has opened the treasure
 * @param unLootedStuff       the list of items a player has not looted yet.
 *                            Might be null if the player never opened the inventory
 */
public record PlayerLootDetail(long lastLootedTimeStamp,
                               @Nullable List<ItemStack> unLootedStuff) implements ConfigurationSerializable, Serializable {
    @Serial
    private static final long serialVersionUID = -3305276997530613807L;

    public PlayerLootDetail {
        ConfigurationSerialization.registerClass(PlayerLootDetail.class);
    }

    @Nullable
    public static PlayerLootDetail deserialize(Map<String, Object> objMap) {

        Long lastLootedTimeStamp = null;
        Object tempObj = objMap.get("lastLootedTimeStamp");

        //don't ask me why, it's fucktup.
        //basically the problem is, json doesn't differentiate between number types, and it could be really anything.
        //gson typically makes it a Double, Jackson BigInteger. But nothing is guaranteed
        if (tempObj instanceof Number num) {
            lastLootedTimeStamp = num.longValue();
        }

        if (lastLootedTimeStamp != null) {
            if (objMap.get("unLootedStuff") != null && objMap.get("unLootedStuff") instanceof List<?> itemList) {
                return new PlayerLootDetail(lastLootedTimeStamp, TreasureConfig.deserializeItemList(itemList));
            } else {
                TreasureLogger.log(Level.WARNING, "Couldn't deserialize PlayerLootDetail: unLootedStuff not list" + objMap.get("unLootedStuff"));
                return null;
            }
        } else {
            TreasureLogger.log(Level.WARNING, "Couldn't deserialize PlayerLootDetail: lastLootedTimeStamp not long" + tempObj);
            return null;
        }
    }

    @NotNull
    public Map<String, Object> serialize() {
        HashMap<String, Object> objMap = new HashMap<>();

        objMap.put("lastLootedTimeStamp", lastLootedTimeStamp);

        objMap.put("unLootedStuff", TreasureConfig.serializeItemList(unLootedStuff));
        return objMap;
    }
}
