package de.greensurvivors.greentreasure.language;

import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.LoadingCache;
import net.kyori.adventure.audience.Audience;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.format.Style;
import net.kyori.adventure.text.format.TextDecoration;
import net.kyori.adventure.text.minimessage.MiniMessage;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
import net.kyori.adventure.text.minimessage.tag.resolver.TagResolver;
import net.kyori.adventure.util.UTF8ResourceBundleControl;
import org.apache.commons.io.FileUtils;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.plugin.Plugin;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.*;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.security.CodeSource;
import java.time.Duration;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * manages all translatable and placeholders used by this plugin.
 */
public class MessageManager {
    // please note: since minutes and months both are identified by m, it is intentional for this pattern to NOT be
    // case-insensitive!
    private static final @NotNull Pattern DURATION_PATTERN = Pattern.compile("(?<amount>-?\\d+)(?<unit>[tTsSmhHdDwWMyY])");
    public static final @NotNull Style DEFAULT_STYLE = Style.style(). // todo there has to be a better way to force reset to the default style!
        color(NamedTextColor.WHITE).
        clickEvent(null).
        decorations(Set.of(TextDecoration.values()), false).
        build();
    private final String BUNDLE_NAME = "lang";
    final @NotNull Pattern BUNDLE_FILE_NAME_PATTERN = Pattern.compile(BUNDLE_NAME + "(?:_.*)?.properties");
    private final Plugin plugin;
    private ResourceBundle lang;
    /**
     * caches every component without placeholder for faster access in future and loads missing values automatically
     */
    private final LoadingCache<LangPath, Component> langCache = Caffeine.newBuilder().build(
        path -> MiniMessage.miniMessage().deserialize(getStringFromLang(path)));

    public MessageManager(Plugin plugin) {
        this.plugin = plugin;
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
    public static @NotNull Component formatTime(final @NotNull Duration duration) {
        StringBuilder timeStr = new StringBuilder();

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
     * formats a location to a Component
     *
     * @return the formatted string or "-" if the location was null
     */
    public @NotNull Component formatLocation(final @Nullable Location loc) {
        if (loc != null) {
            return getLang(LangPath.FORMAT_LOCATION,
                Placeholder.unparsed(PlaceHolderKey.WORLD.getKey(), loc.getWorld().getName()),
                Placeholder.unparsed(PlaceHolderKey.X.getKey(), Integer.toString(loc.getBlockX())),
                Placeholder.unparsed(PlaceHolderKey.Y.getKey(), Integer.toString(loc.getBlockY())),
                Placeholder.unparsed(PlaceHolderKey.Z.getKey(), Integer.toString(loc.getBlockZ()))
            );
        }
        return Component.text("-");
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
            plugin.getComponentLogger().debug("Couldn't get time period \"{}\" as duration. Trying to parse manual next.", period, e);
        }

        Matcher matcher = DURATION_PATTERN.matcher(period);
        Duration duration = Duration.ZERO;

        while (matcher.find()) {
            try {
                long num = Long.parseLong(matcher.group("amount"));
                String typ = matcher.group("unit");
                duration = switch (typ) { // from periodPattern
                    case "t", "T" -> duration.plusMillis(50L * num); // ticks
                    case "ms", "MS", "mS", "Ms" -> duration.plusMillis(num);
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
                plugin.getComponentLogger().warn("Couldn't get time period for {}", period, e);
            }
        }
        return duration == Duration.ZERO ? null : duration;
    }

    private @NotNull String getStringFromLang(@NotNull LangPath path) {
        try {
            return lang.getString(path.getPath());
        } catch (MissingResourceException | ClassCastException e) {
            plugin.getLogger().log(Level.WARNING, "couldn't find path: \"" + path.getPath() + "\" in lang files using fallback.", e);
            return path.getDefaultValue();
        }
    }

    /**
     * reload language file.
     */
    public void reload(final @NotNull Locale locale) {
        lang = null; // reset last bundle

        // save all missing keys
        initLangFiles();

        plugin.getLogger().info("Locale set to language: " + locale.toLanguageTag());
        File langDictionary = new File(plugin.getDataFolder(), BUNDLE_NAME);

        URL[] urls;
        try {
            urls = new URL[]{langDictionary.toURI().toURL()};
            lang = ResourceBundle.getBundle(BUNDLE_NAME, locale, new URLClassLoader(urls), UTF8ResourceBundleControl.get());

        } catch (SecurityException | MalformedURLException e) {
            plugin.getLogger().log(Level.WARNING, "Exception while reading lang bundle. Using internal", e);
        } catch (MissingResourceException ignored) { // how? missing write access?
            plugin.getLogger().log(Level.WARNING, "No translation file for " + UTF8ResourceBundleControl.get().toBundleName(BUNDLE_NAME, locale) + " found on disc. Using internal");
        }

        if (lang == null) { // fallback, since we are always trying to save defaults this never should happen
            try {
                lang = PropertyResourceBundle.getBundle(BUNDLE_NAME, locale, plugin.getClass().getClassLoader(), new UTF8ResourceBundleControl());
            } catch (MissingResourceException e) {
                plugin.getLogger().log(Level.SEVERE, "Couldn't get Ressource bundle \"lang\" for locale \"" + locale.toLanguageTag() + "\". Messages WILL be broken!", e);
            }
        }

        // clear component cache
        langCache.invalidateAll();
        langCache.cleanUp();
        langCache.asMap().clear();
    }

    private @NotNull String saveConvert(final @NotNull String theString, final boolean escapeSpace) {
        int len = theString.length();
        int bufLen = len * 2;
        if (bufLen < 0) {
            bufLen = Integer.MAX_VALUE;
        }
        StringBuilder convertedStrBuilder = new StringBuilder(bufLen);

        for (int i = 0; i < theString.length(); i++) {
            char aChar = theString.charAt(i);
            // Handle common case first
            if ((aChar > 61) && (aChar < 127)) {
                if (aChar == '\\') {
                    if (i + 1 < theString.length()) {
                        final char bChar = theString.charAt(i + 1);
                        if (bChar == ' ' || bChar == 't' || bChar == 'n' || bChar == 'r' ||
                            bChar == 'f' || bChar == '\\' || bChar == 'u' || bChar == '=' ||
                            bChar == ':' || bChar == '#' || bChar == '!') {
                            // don't double escape already escaped chars
                            convertedStrBuilder.append(aChar);
                            convertedStrBuilder.append(bChar);
                            i++;
                            continue;
                        } else {
                            // any other char following
                            convertedStrBuilder.append('\\');
                        }
                    } else {
                        // last char was a backslash. escape!
                        convertedStrBuilder.append('\\');
                    }
                }
                convertedStrBuilder.append(aChar);
                continue;
            }

            // escape non escaped chars that have to get escaped
            switch (aChar) {
                case ' ' -> {
                    if (escapeSpace) {
                        convertedStrBuilder.append('\\');
                    }
                    convertedStrBuilder.append(' ');
                }
                case '\t' -> convertedStrBuilder.append("\\t");
                case '\n' -> convertedStrBuilder.append("\\n");
                case '\r' -> convertedStrBuilder.append("\\r");
                case '\f' -> convertedStrBuilder.append("\\f");
                case '=', ':', '#', '!' -> {
                    convertedStrBuilder.append('\\');
                    convertedStrBuilder.append(aChar);
                }
                default -> convertedStrBuilder.append(aChar);
            }
        }

        return convertedStrBuilder.toString();
    }

    /**
     * saves all missing lang files from resources to the plugins datafolder
     */
    private void initLangFiles() {
        CodeSource src = this.getClass().getProtectionDomain().getCodeSource();
        if (src != null) {
            URL jarUrl = src.getLocation();
            try (ZipInputStream zipStream = new ZipInputStream(jarUrl.openStream())) {
                ZipEntry zipEntry;
                while ((zipEntry = zipStream.getNextEntry()) != null) {
                    if (zipEntry.isDirectory()) {
                        continue;
                    }

                    String entryName = zipEntry.getName();

                    if (BUNDLE_FILE_NAME_PATTERN.matcher(entryName).matches()) {
                        File langFile = new File(new File(plugin.getDataFolder(), BUNDLE_NAME), entryName);
                        if (!langFile.exists()) { // don't overwrite existing files
                            FileUtils.copyToFile(zipStream, langFile);
                        } else { // add defaults to file to expand in case there are key-value pairs missing
                            Properties defaults = new Properties();
                            // don't close reader, since we need the stream to be still open for the next entry!
                            defaults.load(new InputStreamReader(zipStream, StandardCharsets.UTF_8));

                            Properties current = new Properties();
                            try (InputStreamReader reader = new InputStreamReader(new FileInputStream(langFile), StandardCharsets.UTF_8)) {
                                current.load(reader);
                            } catch (Exception e) {
                                plugin.getLogger().log(Level.WARNING, "couldn't get current properties file for " + entryName + "!", e);
                                continue;
                            }

                            try (FileWriter fw = new FileWriter(langFile, StandardCharsets.UTF_8, true);
                                 // we are NOT using Properties#store since it gets rid of comments and doesn't guarantee ordering
                                 BufferedWriter bw = new BufferedWriter(fw)) {
                                boolean updated = false; // only write comment once
                                for (Map.Entry<Object, Object> translationPair : defaults.entrySet()) {
                                    if (current.get(translationPair.getKey()) == null) {
                                        if (!updated) {
                                            bw.write("# New Values where added. Is everything else up to date? Time of update: " + new Date());
                                            bw.newLine();

                                            plugin.getLogger().fine("Updated langfile \"" + entryName + "\". Might want to check the new translation strings out!");

                                            updated = true;
                                        }

                                        String key = saveConvert((String) translationPair.getKey(), true);
                                        /* No need to escape embedded and trailing spaces for value, hence
                                         * pass false to flag.
                                         */
                                        String val = saveConvert((String) translationPair.getValue(), false);
                                        bw.write((key + "=" + val));
                                        bw.newLine();
                                    } // current already knows the key
                                } // end of for
                            } // end of try
                        } // end of else (file exists)
                    } // doesn't match
                } // end of elements
            } catch (IOException e) {
                plugin.getLogger().log(Level.WARNING, "Couldn't save lang files", e);
            }
        } else {
            plugin.getLogger().warning("Couldn't save lang files: no CodeSource!");
        }
    }

    /**
     * prepend the message with the plugins prefix before sending it to the audience.
     */
    public void sendMessage(final @NotNull Audience audience, final @NotNull Component messages) {
        audience.sendMessage(langCache.get(LangPath.PLUGIN_PREFIX).appendSpace().append(messages.applyFallbackStyle(DEFAULT_STYLE)));
    }

    /**
     * get a component from lang file and apply the given tag resolver.
     * Note: might be slightly slower than {@link #getLang(LangPath)} since this can not use cache.
     */
    public @NotNull Component getLang(@NotNull LangPath path, @NotNull TagResolver... resolver) {
        return MiniMessage.miniMessage().deserialize(getStringFromLang(path), resolver);
    }

    /**
     * get a component from lang file
     */
    public @NotNull Component getLang(@NotNull LangPath path) {
        return langCache.get(path);
    }

    /**
     * send a component from the lang file to the audience, prefixed with this plugins prefix.
     */
    public void sendLang(@NotNull Audience audience, @NotNull LangPath path) {
        sendMessage(audience, getLang(path));
    }

    /**
     * send a component from the lang file to the audience, prefixed with this plugins prefix and applying the given tag resolver.
     * Note: might be slightly slower than {@link #sendLang(Audience, LangPath)} since this can not use cache.
     */
    public void sendLang(@NotNull Audience audience, @NotNull LangPath path, @NotNull TagResolver... resolver) {
        sendMessage(audience, getLang(path, resolver));
    }
}
