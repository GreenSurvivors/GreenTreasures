package de.greensurvivors.greentreasure.language;

import net.kyori.adventure.audience.Audience;
import net.kyori.adventure.key.Key;
import net.kyori.adventure.key.KeyPattern;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.translation.MiniMessageTranslator;
import net.kyori.adventure.translation.GlobalTranslator;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.slf4j.Logger;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.security.CodeSource;
import java.time.Duration;
import java.time.Instant;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/// manages all translatable and placeholders used by this plugin.
public class MessageManager extends MiniMessageTranslator {
    // please note: since minutes and months both are identified by m, it is intentional for this pattern to NOT be
    // case-insensitive!
    protected static final @NotNull Pattern DURATION_PATTERN = Pattern.compile("(?<amount>-?\\d+)(?<unit>[tTsSmhHdDwWMyY])");
    protected static final @NotNull String BUNDLE_NAME = "lang";
    protected static final @NotNull Pattern BUNDLE_FILE_NAME_PATTERN = Pattern.compile(BUNDLE_NAME + "(?:_(?<langTag>.*))?.properties$");
    protected final @NotNull Key key;
    protected final @NotNull Logger logger;
    protected final @NotNull Path dataFolder;
    protected final @NotNull URL jarUrl;
    protected final @NotNull Map<@NotNull Locale, @NotNull ResourceBundle> translations = new ConcurrentHashMap<>();

    /// used when instancing from the same jar file and class loader
    public MessageManager(final @NotNull @KeyPattern.Namespace String namespace,
                          final @NotNull Logger logger,
                          final @NotNull Path dataFolder) throws IllegalStateException {
        this(namespace, logger, dataFolder, MessageManager.class);
    }

    /// used when instancing from another jar file / another class loader
    public MessageManager(final @NotNull @KeyPattern.Namespace String namespace,
                          final @NotNull Logger logger,
                          final @NotNull Path dataFolder,
                          final @NotNull Class<?> aClass) throws IllegalStateException {
        this.key = Key.key(namespace, BUNDLE_NAME);
        this.logger = logger;
        this.dataFolder = dataFolder;

        final @Nullable CodeSource src = aClass.getProtectionDomain().getCodeSource(); // I'm sure there is a better way to get the resources
        if (src == null) {
            throw new IllegalStateException("Can't find code source!");
        }
        jarUrl = src.getLocation(); // todo there is something here with the Module, just like ResourceBundle does it

        GlobalTranslator.translator().addSource(this);
    }

    @Override
    protected @Nullable String getMiniMessageString(@NotNull String translationKey, final @NotNull Locale locale) {
        // not (re)loaded yet
        if (translations.isEmpty()) {
            logger.debug("requested translation for key {} and locale {}, before anything was loaded!", translationKey, locale.toLanguageTag());
            return null;
        }

        // the translation keys are not namespaced, so we add our namespace artificially, which in turn we need to remove now
        if (!translationKey.startsWith(this.key.namespace())) {
            return null;
        }
        // + 1 for the dot
        translationKey = translationKey.substring(key.namespace().length() + 1);

        try {
            return findBundle(locale).getString(translationKey);
        } catch (final @NotNull MissingResourceException | @NotNull ClassCastException e) {
            logger.debug("couldn't find path: \"{}\" in lang files using fallback.", translationKey, e);
            return null;
        }
    }

    @Override
    public @NotNull Key name() {
        return key;
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

    /// reload language file, best done async or at start up
    public void reload() {
        translations.clear();

        // save all missing keys
        initLangFiles();

        // todo somehow make this process lazy,
        //  without having misses with complicated lang tags (lang_en loads first, but then lang_en_us gets requested, but finds the fallback in the map)
        //  or checking if the resource bunde exists on disk every miss
        // load ALL available translations
        final @NotNull Path langDictionary = dataFolder.resolve(BUNDLE_NAME);
        try {
            final @NotNull URL @NotNull [] urls = new URL[]{langDictionary.toUri().toURL()};
            final @NotNull URLClassLoader urlClassLoader = new URLClassLoader(urls);

            try (final @NotNull DirectoryStream<@NotNull Path> stream = Files.newDirectoryStream(langDictionary, Files::isRegularFile)) {
                for (final @NotNull Path filePath : stream) {
                    final @NotNull Matcher matcher = BUNDLE_FILE_NAME_PATTERN.matcher(filePath.getFileName().toString());

                    if (matcher.matches()) {
                        final @Nullable String langTag = matcher.group("langTag");
                        final @NotNull Locale locale;

                        if (langTag == null) {
                            locale = Locale.ROOT;
                        } else {
                            locale = Locale.forLanguageTag(langTag.replace('_', '-'));
                        }

                        try {
                            translations.put(locale, ResourceBundle.getBundle(BUNDLE_NAME, locale, urlClassLoader));
                        } catch (MissingResourceException _) { // how? missing write access?
                            logger.warn("No translation file for lang {} found on disc.", locale.toLanguageTag());
                        }
                    }
                }
            } catch (final @NotNull IOException e) {
                logger.error("Could not find any ResourceBundles to load!", e);
            }
        } catch (final @NotNull SecurityException | @NotNull MalformedURLException e) {
            logger.warn("Exception while reading lang bundle. Using internal", e);
        }
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

    protected @NotNull ResourceBundle findBundle(final @NotNull Locale locale) {
        @Nullable ResourceBundle bundle = translations.get(locale);
        if (bundle == null) {
            // nothing but a simple shortcut because fetching it everytime we need is just too lengthy
            final @NotNull ResourceBundle.Control control = ResourceBundle.Control.getControl(
                ResourceBundle.Control.FORMAT_PROPERTIES);

            for (final @Nullable Locale candidate : control.getCandidateLocales(BUNDLE_NAME, locale)) {
                bundle = translations.get(candidate);

                if (bundle != null) {
                    return bundle;
                }
            }

            bundle = translations.get(control.getFallbackLocale(BUNDLE_NAME, locale)); // try default locale
            if (bundle == null) {
                bundle = translations.get(Locale.ROOT); // should never be null unless
            }
        }

        return bundle;
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

    /// saves all missing lang files from resources to the plugins datafolder
    private void initLangFiles() { // note: we can't easily merge this with the bundle loading in reload, since the disk may have more translation files than the jar. todo this however, still makes us do double the file lookup and matches for everything that gets shipped.
        try (final @NotNull ZipInputStream zipStream = new ZipInputStream(jarUrl.openStream())) {
            @Nullable ZipEntry zipEntry;
            while ((zipEntry = zipStream.getNextEntry()) != null) {
                if (zipEntry.isDirectory()) {
                    continue;
                }

                final @NotNull String entryName = zipEntry.getName();

                if (BUNDLE_FILE_NAME_PATTERN.matcher(entryName).matches()) {
                    final @NotNull Path langFile = dataFolder.resolve(BUNDLE_NAME, entryName);
                    if (!Files.exists(langFile)) { // don't overwrite existing files
                        Files.copy(zipStream, langFile);
                    } else { // add defaults to file to expand in case there are key-value pairs missing
                        final @NotNull Properties defaults = new Properties();
                        // don't close reader, since we need the stream to be still open for the next entry!
                        defaults.load(new InputStreamReader(zipStream, StandardCharsets.UTF_8));

                        final @NotNull Properties current = new Properties();
                        try (BufferedReader reader = Files.newBufferedReader(langFile, StandardCharsets.UTF_8)) {
                            current.load(reader);
                        } catch (Exception e) {
                            logger.warn("couldn't get current properties file for " + entryName + "!", e);
                            continue;
                        }

                        // we are NOT using Properties#store since it gets rid of comments and doesn't guarantee ordering
                        try (final @NotNull BufferedWriter bw = Files.newBufferedWriter(langFile, StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND)) {
                            boolean updated = false; // only write comment once
                            for (final @NotNull Map.Entry<@NotNull Object, @NotNull Object> translationPair : defaults.entrySet()) {
                                if (current.get(translationPair.getKey()) == null) {
                                    if (!updated) {
                                        bw.write("# New Values where added. Is everything else up to date? Time of update: " + Instant.now().toString());
                                        bw.newLine();

                                        logger.trace("Updated langfile \"{}\". Might want to check the new translation strings out!", entryName);

                                        updated = true;
                                    }

                                    final @NotNull String key = saveConvert((String) translationPair.getKey(), true);
                                    /* No need to escape embedded and trailing spaces for value, hence
                                     * pass false to flag.
                                     */
                                    final @NotNull String val = saveConvert((String) translationPair.getValue(), false);
                                    bw.write((key + "=" + val));
                                    bw.newLine();
                                } // current already knows the key
                            } // end of for
                        } // end of try
                    } // end of else (file exists)
                } // doesn't match
            } // end of elements
        } catch (final @NotNull IOException e) {
            logger.warn("Couldn't save lang files", e);
        }
    }
}
