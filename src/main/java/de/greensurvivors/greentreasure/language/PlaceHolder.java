package de.greensurvivors.greentreasure.language;

import net.kyori.adventure.text.ComponentLike;
import net.kyori.adventure.text.minimessage.tag.Tag;
import net.kyori.adventure.text.minimessage.tag.TagPattern;
import net.kyori.adventure.text.minimessage.tag.resolver.Formatter;
import net.kyori.adventure.text.minimessage.translation.Argument;
import org.jetbrains.annotations.NotNull;

import java.time.temporal.TemporalAccessor;

/// placeholder strings used. will be surrounded in Minimassage typical format of <>
public enum PlaceHolder {
    CMD("cmd"),
    LAST_PAGE("last_page"),
    LOCATION("location"),
    NUMBER("number"),
    PLAYER("player"),
    SHARED("shared"),
    TEXT("text"),
    TIME("time"),
    START("start"),
    TREASURE_ID("treasure_id"),
    UNLIMITED("unlimited"),
    WORLD("world"),
    X("x"),
    Y("y"),
    Z("z"),
    PERCENT("percent"),
    MAX("max");

    private final @NotNull @TagPattern String key;

    PlaceHolder(final @NotNull @TagPattern String key) {
        this.key = key;
    }

    /**
     * Since this will be used in Mini-messages placeholder only the pattern "[!?#]?[a-z0-9_-]*" is valid.
     * if used inside an unparsed text you have to add surrounding <> yourself.
     */
    @Deprecated // don't use it, if you don't have to!
    public @TagPattern @NotNull String getKey() {
        return key;
    }

    /**
     * Creates a named numeric argument.
     *
     * @param value the value
     * @return the named argument
     */
    public @NotNull ComponentLike numeric(final @NotNull Number value) {
        return Argument.tagResolver(Formatter.number(key, value));
    }

    /**
     * Creates a named string argument.
     *
     * @param value the value
     * @return the named argument
     */
    public ComponentLike string(final @NotNull String value) {
        return Argument.string(key, value);
    }

    /**
     * Creates a named component argument.
     *
     * @param value the value
     * @return the named argument
     */
    public @NotNull ComponentLike component(final @NotNull ComponentLike value) {
        return Argument.component(key, value);
    }

    /**
     * Creates a named tag argument.
     *
     * @param tag the tag
     * @return the named argument
     */
    public @NotNull ComponentLike tag(final @NotNull Tag tag) {
        return Argument.tag(key, tag);
    }

    /**
     * Creates a named temporal argument.
     *
     * @param value the time instant
     * @return the named argument
     */
    public @NotNull ComponentLike temporal(final @NotNull TemporalAccessor value) {
        return Argument.tagResolver(Formatter.date(key, value));
    }

    /**
     * Creates a named boolean choice formatted argument.
     *
     * @param value the value
     * @return the named argument
     */
    public @NotNull ComponentLike boolChoice(final boolean value) {
        return Argument.tagResolver(Formatter.booleanChoice(key, value));
    }

    /**
     * Creates a named number choice formatted argument.
     *
     * @param value the value
     * @return the named argument
     */
    public @NotNull ComponentLike numberChoice(final @NotNull Number value) {
        return Argument.tagResolver(Formatter.choice(key, value));
    }
}
