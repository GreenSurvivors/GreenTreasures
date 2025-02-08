package de.greensurvivors.greentreasure.language;

import org.intellij.lang.annotations.Subst;
import org.jetbrains.annotations.NotNull;

/**
 * placeholder strings used. will be surrounded in Minimassage typical format of <>
 */
public enum PlaceHolderKey {
    CMD("cmd"),
    LAST_PAGE("last_page"),
    LOCATION("location"),
    NUMBER("number"),
    PLAYER("player"),
    SHARED("shared"),
    TEXT("text"),
    TIME("time"),
    TREASURE_ID("treasure_id"),
    UNLIMITED("unlimited"),
    WORLD("world"),
    X("x"),
    Y("y"),
    Z("z");

    private final @NotNull String key;

    PlaceHolderKey(@NotNull String key) {
        this.key = key;
    }

    /**
     * Since this will be used in Mini-messages placeholder only the pattern "[!?#]?[a-z0-9_-]*" is valid.
     * if used inside an unparsed text you have to add surrounding <> yourself.
     */
    @Subst("treasure_id") // substitution; will be inserted if the IDE/compiler tests if input is valid.
    public @NotNull String getKey() {
        return key;
    }
}
