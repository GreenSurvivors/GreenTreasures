package de.greensurvivors.greentreasure.language;

import de.greensurvivors.corelib.language.CorePlaceHolder;
import net.kyori.adventure.text.minimessage.tag.TagPattern;
import org.jetbrains.annotations.NotNull;


/// placeholder strings used. will be surrounded in Minimassage typical format of <>
public class PlaceHolder extends CorePlaceHolder {
    public static final @NotNull PlaceHolder CMD = new PlaceHolder("cmd");
    public static final @NotNull PlaceHolder LAST_PAGE = new PlaceHolder("last_page");
    public static final @NotNull PlaceHolder SHARED = new PlaceHolder("shared");
    public static final @NotNull PlaceHolder TIME = new PlaceHolder("time");
    public static final @NotNull PlaceHolder START = new PlaceHolder("start");
    public static final @NotNull PlaceHolder TREASURE_ID = new PlaceHolder("treasure_id");
    public static final @NotNull PlaceHolder UNLIMITED = new PlaceHolder("unlimited");
    public static final @NotNull PlaceHolder PERCENT = new PlaceHolder("percent");
    public static final @NotNull PlaceHolder MAX = new PlaceHolder("max");

    protected PlaceHolder(final @NotNull @TagPattern String key) {
        super(key);
    }
}
