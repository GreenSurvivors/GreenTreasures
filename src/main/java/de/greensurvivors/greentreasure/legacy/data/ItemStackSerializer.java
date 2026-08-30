package de.greensurvivors.greentreasure.legacy.data;

import io.leangen.geantyref.TypeToken;
import org.bukkit.configuration.serialization.ConfigurationSerialization;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.configurate.ConfigurationNode;
import org.spongepowered.configurate.serialize.SerializationException;
import org.spongepowered.configurate.serialize.TypeSerializer;

import java.lang.reflect.Type;
import java.util.Map;

public final class ItemStackSerializer implements TypeSerializer<@NotNull ItemStack> {
    private static final @NotNull TypeToken<Map<String, Object>> MAP_TOKEN = new TypeToken<> (){};
    public static final @NotNull ItemStackSerializer INSTANCE = new ItemStackSerializer();

    private ItemStackSerializer() {
    }

    @Override
    public ItemStack deserialize(final @NotNull Type type, final @NotNull ConfigurationNode node) throws SerializationException {
        if (ItemStack.class.getName().equals(node.node(ConfigurationSerialization.SERIALIZED_TYPE_KEY).getString())) {
            return ItemStack.deserialize(node.require(MAP_TOKEN));
        } else {
            throw new SerializationException(node, String.class, "The serialized type key is missing or not an ItemStack key!");
        }
    }

    @Override
    public void serialize(final @NotNull Type type, final @Nullable ItemStack itemStack, final @NotNull ConfigurationNode node) throws SerializationException {
        if (itemStack == null) {
            node.raw(null);
            return;
        }

        node.set(itemStack.serialize());
        node.node(ConfigurationSerialization.SERIALIZED_TYPE_KEY).set(ItemStack.class.getName());
    }
}
