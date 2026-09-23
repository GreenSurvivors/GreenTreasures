package de.greensurvivors.greentreasure.language;

import net.kyori.adventure.audience.Audience;
import net.kyori.adventure.key.KeyPattern;
import net.kyori.adventure.text.Component;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.slf4j.Logger;

import java.nio.file.Path;
import java.time.Duration;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/// manages all translatable and placeholders used by this plugin.
public class MessageManager extends de.greensurvivors.corelib.language.MessageManager {
    // please note: since minutes and months both are identified by m, it is intentional for this pattern to NOT be
    // case-insensitive!
    protected static final @NotNull Pattern DURATION_PATTERN = Pattern.compile("(?<amount>-?\\d+)(?<unit>[tTsSmhHdDwWMyY])");

    public MessageManager(final @NotNull @KeyPattern.Namespace String namespace,
                          final @NotNull Logger logger,
                          final @NotNull Path dataFolder,
                          final @NotNull @KeyPattern.Value String bundleName) throws IllegalStateException {
        super(namespace, logger, dataFolder, bundleName, MessageManager.class);
    }

    /**
     * @return the given duration formated as YYyMMMWWwDDdHHhmmmSSsTTt
     * Y = amount of years, if any
     * W = amount of weeks, if any
     * D = amount of days, if any
     * H = amount of hours, if any
     * m = amount of minutes, if any
     * S = amount of seconds, if any
     * T = amount of ticks, if any
     */
    public static @NotNull Component formatDuration(final @NotNull Duration duration) { // todo better format
        final @NotNull StringBuilder timeStr = new StringBuilder();

        final long days = duration.toDaysPart();
        if (days != 0) { // todo represent more then days
            timeStr.append(days).append("d");
        }

        final int hours = duration.toHoursPart();
        if (hours != 0) {
            timeStr.append(hours).append("h");
        }

        final int minutes = duration.toMinutesPart();
        if (minutes != 0) {
            timeStr.append(minutes).append("m");
        }

        final int seconds = duration.toSecondsPart();
        if (seconds != 0) {
            timeStr.append(seconds).append("s");
        }

        // the tick rate isn't constant anymore, but can get adjusted ingame.
        // with a standard tick rate of 20 t/s / 1000 ms/s we would have duration.toMillisPart() / 50.
        final float ticksPerSecond = Bukkit.getServerTickManager().getTickRate();
        final int ticks = (int) (duration.toMillisPart() * ticksPerSecond / 1000);
        if (ticks != 0) {
            timeStr.append(ticks).append("t");
        }
        return Component.text(timeStr.toString());
    }

    /**
     * Try to get a time period of a string.
     * First try ISO-8601 duration, and afterward our own implementation
     * using the same time unit more than once is permitted.
     * Ticks are counted as standard 20 t/s, not doing anything special with the tick manager
     *
     * @return the duration, or null if not possible
     */
    public @Nullable Duration parseDuration(@NotNull String period) {
        try { //try Iso
            return Duration.parse(period);
        } catch (DateTimeParseException e) {
            logger.debug("Couldn't get time period \"{}\" as duration. Trying to parse manual next.", period, e);
        }

        Matcher matcher = DURATION_PATTERN.matcher(period);
        Duration duration = Duration.ZERO;

        while (matcher.find()) {
            try {
                long num = Long.parseLong(matcher.group("amount"));
                String typ = matcher.group("unit");
                duration = switch (typ) { // from periodPattern
                    case "t", "T" -> duration.plusMillis(50L * num); // ticks
                    case "s", "S" -> duration.plusSeconds(num);
                    case "m" -> duration.plusMinutes(num);
                    case "h", "H" -> duration.plusHours(num);
                    case "d", "D" -> duration.plusDays(num);
                    case "w", "W" -> duration.plusDays(Math.multiplyExact(num, 7));
                    case "M" -> duration.plus(ChronoUnit.MONTHS.getDuration().multipliedBy(num));
                    case "y", "Y" -> duration.plus(ChronoUnit.YEARS.getDuration().multipliedBy(num));
                    default -> duration;
                };

            } catch (NumberFormatException e) {
                logger.warn("Couldn't get time period for {}", period, e);
            }
        }
        return duration == Duration.ZERO ? null : duration;
    }

    /**
     * formats a location to a Component
     *
     * @return the formatted string or "-" if the location was null
     */
    public static @NotNull Component formatLocation(final @Nullable Location loc) {
        if (loc != null) {
            return LangKey.FORMAT_LOCATION.create(
                PlaceHolder.WORLD.string(loc.getWorld().getName()),
                PlaceHolder.X.numeric(loc.getBlockX()),
                PlaceHolder.Y.numeric(loc.getBlockY()),
                PlaceHolder.Z.numeric(loc.getBlockZ())
            );
        }
        return Component.text("-");
    }

    /// prepend the message with the plugins prefix before sending it to the audience.
    public void sendPrefixed(final @NotNull Audience audience, final @NotNull Component message) {
        audience.sendMessage(Component.text()
            .append(LangKey.PLUGIN_PREFIX.create())
            .appendSpace()
            .append(message));
    }

    /// send a translatable component to the audience, prefixed with this plugins prefix.
    public void sendPrefixed(final @NotNull Audience audience, final @NotNull LangKey path) {
        sendPrefixed(audience, path.create());
    }
}
