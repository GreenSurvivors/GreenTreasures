package de.greensurvivors.greentreasure.legacy.data;

import io.leangen.geantyref.TypeToken;
import org.bukkit.configuration.serialization.ConfigurationSerialization;
import org.bukkit.util.Vector;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.configurate.ConfigurationNode;
import org.spongepowered.configurate.serialize.SerializationException;
import org.spongepowered.configurate.serialize.TypeSerializer;

import java.lang.reflect.Type;
import java.util.Map;

public final class VectorSerializer implements TypeSerializer<@NotNull Vector> {
    private static final @NotNull TypeToken<Map<String, Object>> MAP_TOKEN = new TypeToken<> (){};
    public static final @NotNull VectorSerializer INSTANCE = new VectorSerializer();

    private VectorSerializer() {
    }

    @Override
    public @NotNull Vector deserialize(final @NotNull Type type, final @NotNull ConfigurationNode node) throws SerializationException {
        if ("Vector".equals(node.node(ConfigurationSerialization.SERIALIZED_TYPE_KEY).getString())) {
            return Vector.deserialize(node.require(MAP_TOKEN));
        } else {
            throw new SerializationException(node, String.class, "The serialized type key is missing or not an Vector key!");
        }
    }

    @Override
    public void serialize(final @NotNull Type type, final @Nullable Vector vector, final @NotNull ConfigurationNode node) throws SerializationException {
        if (vector == null) {
            node.raw(null);
            return;
        }

        node.set(vector.serialize());
        node.node(ConfigurationSerialization.SERIALIZED_TYPE_KEY).set("Vector");
    }
}
