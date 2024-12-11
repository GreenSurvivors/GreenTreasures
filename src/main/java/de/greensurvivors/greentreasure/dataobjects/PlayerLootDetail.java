package de.greensurvivors.greentreasure.dataobjects;

import de.greensurvivors.greentreasure.GreenTreasure;
import org.bukkit.configuration.serialization.ConfigurationSerializable;
import org.bukkit.configuration.serialization.ConfigurationSerialization;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

/**
 * all important information about a treasure a player may or may not have opened before
 *
 * @param lastChangedTimeStamp the unix time in milliseconds a player last has opened the treasure
 * @param unLootedStuff        the list of items a player has not looted yet.
 *                             Might be null if the player never opened the inventory
 */
public record PlayerLootDetail(long lastChangedTimeStamp, @Nullable List<@NotNull ItemStack> unLootedStuff) implements ConfigurationSerializable {
    private static final String UNLOOTED_KEY = "unlooted";
    private static final String LAST_CHANGED_TIME_STAMP_KEY = "lastChangedTimeStamp";
    private static final String NULL = "NULL";

    public PlayerLootDetail {
        ConfigurationSerialization.registerClass(PlayerLootDetail.class);
    }

    @Nullable
    public static PlayerLootDetail deserialize(Map<String, Object> objMap) {
        Long lastLootedTimeStamp = null;
        Object tempObj = objMap.get(LAST_CHANGED_TIME_STAMP_KEY);

        //don't ask me why, it's fucktup.
        //basically the problem is, json doesn't differentiate between number types, and it could be really anything.
        //gson typically makes it a Double, Jackson BigInteger. But nothing is guaranteed
        if (tempObj instanceof Number num) {
            lastLootedTimeStamp = num.longValue();
        }

        if (lastLootedTimeStamp != null) {
            if (objMap.get(UNLOOTED_KEY) instanceof String rawItemStr) {
                //get list from string
                final @Nullable List<ItemStack> items;
                if (rawItemStr.equalsIgnoreCase(NULL)) {
                    items = null;
                } else {
                    items = new ArrayList<>(List.of(ItemStack.deserializeItemsFromBytes(Base64.getDecoder().decode(rawItemStr))));
                }

                return new PlayerLootDetail(lastLootedTimeStamp, items);
            } else {
                GreenTreasure.inst().getComponentLogger().warn("Couldn't deserialize PlayerLootDetail: unLootedStuff not list {}", objMap.get(UNLOOTED_KEY));
                return null;
            }
        } else {
            GreenTreasure.inst().getComponentLogger().warn("Couldn't deserialize PlayerLootDetail: lastChangedTimeStamp not long {}", tempObj);
            return null;
        }
    }

    @NotNull
    public Map<String, Object> serialize() {
        HashMap<String, Object> objMap = new HashMap<>();

        objMap.put(LAST_CHANGED_TIME_STAMP_KEY, lastChangedTimeStamp);

        objMap.put(UNLOOTED_KEY, unLootedStuff == null ? NULL : Base64.getEncoder().encodeToString(ItemStack.serializeItemsAsBytes(unLootedStuff)));
        return objMap;
    }
}
