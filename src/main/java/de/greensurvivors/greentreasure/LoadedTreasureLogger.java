package de.greensurvivors.greentreasure;

import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import io.leangen.geantyref.TypeToken;
import org.bukkit.Location;
import org.bukkit.configuration.serialization.ConfigurationSerialization;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.configurate.CommentedConfigurationNode;
import org.spongepowered.configurate.ConfigurateException;
import org.spongepowered.configurate.ConfigurationNode;
import org.spongepowered.configurate.serialize.SerializationException;
import org.spongepowered.configurate.serialize.TypeSerializer;
import org.spongepowered.configurate.yaml.YamlConfigurationLoader;

import java.lang.reflect.Type;
import java.util.Collections;
import java.util.List;
import java.util.Map;

public class LoadedTreasureLogger {
    private static final @NotNull TypeToken<Map<String, Object>> MAP_TOKEN = new TypeToken<> (){};
    protected final @NotNull GreenTreasure plugin;
    protected final @NotNull YamlConfigurationLoader loader;
    protected final @NotNull CommentedConfigurationNode rootNode;

    public LoadedTreasureLogger(final @NotNull GreenTreasure plugin) {
        this.plugin = plugin;

        loader = YamlConfigurationLoader.builder()
            .path(plugin.getDataPath().resolve("loadedTreasuresLog.yml"))
            .defaultOptions(options -> options.serializers(builder -> {
                builder.register(Location.class, LocationSerializer.INSTANCE);
            })).build();
        rootNode = loader.createNode();
    }

    public void log(final @NotNull TreasureInfo treasureInfo, final @NotNull Location location) {
        plugin.getServer().getAsyncScheduler().runNow(plugin, task -> {
            synchronized (rootNode) {
                final @NotNull CommentedConfigurationNode node = rootNode.node(treasureInfo.treasureId().toString());
                node.node("data").raw(treasureInfo.toString());
                final @NotNull CommentedConfigurationNode locationNode = node.node("locations");
                try {
                    final @NotNull List<@NotNull Location> locations = locationNode.getList(Location.class, Collections.emptyList());
                    locations.add(location);
                    locationNode.setList(Location.class, locations.stream().distinct().toList());
                } catch (final @NotNull SerializationException e) {
                    plugin.getComponentLogger().error("Could not log treasure at {} with id {}", location, treasureInfo.treasureId(), e);
                }

                try {
                    loader.save(rootNode);
                } catch (final @NotNull ConfigurateException e) {
                    plugin.getComponentLogger().error("Could not safe loaded Treasure log file.", e);
                }
            }
        });
    }

    public static final class LocationSerializer implements TypeSerializer<@NotNull Location> {
        public static final @NotNull LocationSerializer INSTANCE = new LocationSerializer();

        private LocationSerializer() {
        }

        @Override
        public @NotNull Location deserialize(final @NotNull Type type, final @NotNull ConfigurationNode node) throws SerializationException {
            if (Location.class.getName().equals(node.node(ConfigurationSerialization.SERIALIZED_TYPE_KEY).getString())) {
                return Location.deserialize(node.require(MAP_TOKEN));
            } else {
                throw new SerializationException(node, String.class, "The serialized type key is missing or not an Location key!");
            }
        }

        @Override
        public void serialize(final @NotNull Type type, final @Nullable Location location, final @NotNull ConfigurationNode node) throws SerializationException {
            if (location == null) {
                node.raw(null);
                return;
            }

            node.set(location.serialize());
            node.node(ConfigurationSerialization.SERIALIZED_TYPE_KEY).set(Location.class.getName());
        }
    }
}
