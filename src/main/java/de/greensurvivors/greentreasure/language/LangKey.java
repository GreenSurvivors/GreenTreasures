package de.greensurvivors.greentreasure.language;

import de.greensurvivors.greentreasure.GreenTreasure;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.ComponentLike;
import net.kyori.adventure.text.TranslatableComponent;
import net.kyori.adventure.text.format.Style;
import net.kyori.adventure.text.format.StyleBuilderApplicable;
import net.kyori.adventure.translation.Translatable;
import org.bukkit.plugin.java.JavaPlugin;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.PropertyKey;

/// Paths of all translatable
public enum LangKey implements Translatable {
    //actions
    ACTION_REMOVE_DENIED("action.remove.denied"),
    ACTION_FIND_ALREADY_LOOTED("action.find.looted"),
    ACTION_FIND_LIMITED("action.find.limited"),
    ACTION_FIND_LOCKED("action.find.locked"),
    ACTION_FIND_UNLIMITED("action.find.unlimited"),
    ACTION_TREASURE_EDITED("action.treasure-edit.success"),

    // user cmd args errors
    ARG_NOT_A_BOOL("cmd.error.arg.not-a-bool"),
    ARG_NOT_A_NUMBER("cmd.error.arg.not-a-number"),
    ARG_NOT_PLAYER("cmd.error.arg.no-such-player"),
    ARG_NOT_TIME("cmd.error.arg.not-time"),
    ARG_UNKNOWN("cmd.error.arg.unknown"),

    // commands
    CMD_ABOUT_PLUGIN_DESCRIPTION("cmd.about.description"),
    CMD_ABOUT_PLUGIN_HEADER("cmd.about.header"),
    CMD_ABOUT_PLUGIN_VERSION("cmd.about.version"),
    CMD_CREATE_ERROR_ALREADY_TREASURE("cmd.create.error.already-treasure"),
    CMD_CREATE_ERROR_INVALID_CONTAINER("cmd.create.error.invalid-container"),
    CMD_CREATE_SUCCESS("cmd.create.success"),
    CMD_FORGET_ALL_SUCCESS("cmd.forget-all.success"),
    CMD_FORGET_SHARED_SUCCESS("cmd.forget.shared.success"),
    CMD_FORGET_USER_SUCCESS("cmd.forget.user.success"),
    CMD_HELP_WIKI("cmd.help.wiki"),
    CMD_LIST_FOOTER_BACK("cmd.list.footer.back"),
    CMD_LIST_FOOTER_INNER("cmd.list.footer.inner"),
    CMD_LIST_FOOTER_NEXT("cmd.list.footer.next"),
    CMD_LIST_FOOTER_NONE("cmd.list.footer.none"),
    CMD_LIST_FOOTER_OUTER("cmd.list.footer.outer"),
    CMD_LIST_PLAYER_BODY("cmd.list.player.body"),
    CMD_LIST_PLAYER_EMPTY("cmd.list.player.empty"),
    CMD_LIST_PLAYER_HEADER("cmd.list.player.header"),
    CMD_LIST_PLAYER_NEVER("cmd.list.player.never"),
    CMD_LIST_TREASURES_EMPTY("cmd.list.treasures.empty"),
    CMD_LIST_TREASURES_HEADER("cmd.list.treasures.header"),
    CMD_LIST_TREASURES_BODY("cmd.list.treasures.body"),
    CMD_LIST_NEARBY_TREASURES_EMPTY("cmd.list.nearby-treasures.empty"),
    CMD_LIST_NEARBY_TREASURES_HEADER("cmd.list.nearby-treasures.header"),
    CMD_LIST_NEARBY_TREASURES_BODY("cmd.list.nearby-treasures.body"),
    CMD_LIST_WHO_BODY("cmd.list.who.body"),
    CMD_LIST_WHO_EMPTY("cmd.list.who.empty"),
    CMD_LIST_WHO_HEADER("cmd.list.who.header"),
    CMD_LIST_WHO_SHARED("cmd.list.who.shared"),
    CMD_PEEK_GENERATE_PLAYER("cmd.peek.generate.player"),
    CMD_PEEK_GENERATE_SHARED("cmd.peek.generate.shared"),
    CMD_PEEK_UNLIMITED("cmd.peek.unlimited"),
    CMD_PEEK_WARNING("cmd.peek.warning"),
    CMD_RELOAD_SUCCESS("cmd.reload.success"),
    CMD_SET_FIND_FRESH_MESSAGE_OVERRIDE_SUCCESS("cmd.set.find-fresh-message-override.success"),
    CMD_SET_FIND_FRESH_MESSAGE_OVERRIDE_REMOVED("cmd.set.find-fresh-message-override.removed"),
    CMD_SET_FIND_LOOTED_MESSAGE_OVERRIDE_SUCCESS("cmd.set.find-looted-message-override.success"),
    CMD_SET_FIND_LOOTED_MESSAGE_OVERRIDE_REMOVED("cmd.set.find-looted-message-override.removed"),
    CMD_SET_FORGET_REMOVE_DURATION("cmd.set.forget.remove-duration"),
    CMD_SET_RANDOM_SUCCESS("cmd.set.random.success"),
    CMD_SET_SHARED_SUCCESS("cmd.set.shared.success"),
    CMD_SET_UNLIMITED_SUCCESS("cmd.set.unlimited.success"),
    CMD_SET_FORGET_DURATION_SUCCESS("cmd.set.forget.duration.success"),
    CMD_SET_FORGET_DURATION_INSTANT_SUCCESS("cmd.set.forget.duration-instant.success"),
    CMD_CANCEL_SUCCESS_OTHER("cmd.cancel.success.other"),
    CMD_CANCEL_SUCCESS_SELF("cmd.cancel.success.self"),
    CMD_CANCEL_SUCCESS_IMPORT("cmd.cancel.success.import"),
    CMD_CANCEL_ERROR_NO_RUNNING("cmd.cancel.error.notRunning"),

    //
    CMD_USAGE("cmd.usage"),
    CMD_ERROR_NOT_ENOUGH_ARGS("cmd.error.not-enough-args"),
    ERROR_NOT_LOOKING_AT_CONTAINER("cmd.error.not-looking-at-container"),
    ERROR_NOT_LOOKING_AT_TREASURE("cmd.error.not-looking-at-treasure"),
    ERROR_SENDER_NOT_PLAYER("cmd.error.sender-not-player"),
    ERROR_UNKNOWN("error.unknown"),

    REMOVE_GLOBAL_SUCCESS("remove.global.success"),
    REMOVE_LOCAL_SUCCESS("remove.local.success"),
    REMOVE_ERROR("remove.error"),

    //
    FORMAT_LOCATION("format.location"),
    NO_PERMISSION("no-permission"),
    PLUGIN_PREFIX("prefix", "<gold>[GreenTreasure]</gold>"),
    CHUNK_PROCESS_WORK("chunk-process.work"),

    //
    TREASURE_TITLE_EDIT("treasure.title.edit"),
    TREASURE_TITLE_PEEK_SHARED("treasure.title.peek.shared"),
    TREASURE_TITLE_PEEK_PLAYER("treasure.title.peek.player"),

    REFRESH_MSG_INFO_PERIODIC("refresh.msg.info.periodic"),
    REFRESH_MSG_INFO_PERIODIC_INSTANT("refresh.msg.info.periodic_instant"),
    REFRESH_MSG_INFO_NEVER("refresh.msg.info.never"),
    REFRESH_MSG_INFO_INSTANT_UNLOCK("refresh.msg.info.instant_unlock");

    private final @NotNull @PropertyKey(resourceBundle = "lang") String translationKey;
    private final @Nullable String fallback;

    LangKey(final @NotNull String translationKey) {
        this(translationKey, null); // minecraft will use the key, if no translation was found. No need to set it ourselves
    }

    LangKey(final @NotNull String translationKey, final @Nullable String fallback) {
        this.translationKey = JavaPlugin.getPlugin(GreenTreasure.class).namespace() + "." + translationKey;
        this.fallback = fallback;
    }

    @Override
    public @NotNull String translationKey() {
        return translationKey;
    }

    public @NotNull TranslatableComponent create() {
        return Component.translatable(translationKey, fallback);
    }

    public @NotNull TranslatableComponent create(final @NotNull StyleBuilderApplicable @NotNull ... styles) {
        return Component.translatable(translationKey, fallback, styles);
    }

    public @NotNull TranslatableComponent create(final @NotNull ComponentLike @NotNull ... args) {
        return Component.translatable(translationKey, fallback, args);
    }

    public @NotNull TranslatableComponent create(final @NotNull Style style, final @NotNull ComponentLike @NotNull ... args) {
        return Component.translatable(translationKey, fallback, style, args);
    }
}
