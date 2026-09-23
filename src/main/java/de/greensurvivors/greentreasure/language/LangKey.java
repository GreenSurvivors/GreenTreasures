package de.greensurvivors.greentreasure.language;

import de.greensurvivors.corelib.language.CoreLangKey;
import de.greensurvivors.greentreasure.GreenTreasure;
import net.kyori.adventure.key.Key;
import net.kyori.adventure.key.KeyPattern;
import org.bukkit.plugin.java.JavaPlugin;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.PropertyKey;

/// Paths of all translatable
public class LangKey extends CoreLangKey {
    public static final @NotNull String BUNDLE_NAME = "lang";

    //actions
    public static final @NotNull LangKey ACTION_REMOVE_DENIED = new LangKey("action.remove.denied");
    public static final @NotNull LangKey ACTION_FIND_ALREADY_LOOTED = new LangKey("action.find.looted");
    public static final @NotNull LangKey ACTION_FIND_LIMITED = new LangKey("action.find.limited");
    public static final @NotNull LangKey ACTION_FIND_LOCKED = new LangKey("action.find.locked");
    public static final @NotNull LangKey ACTION_FIND_UNLIMITED = new LangKey("action.find.unlimited");
    public static final @NotNull LangKey ACTION_TREASURE_EDITED = new LangKey("action.treasure-edit.success");

    // user cmd args errors
    public static final @NotNull LangKey ARG_NOT_A_BOOL = new LangKey("cmd.error.arg.not-a-bool");
    public static final @NotNull LangKey ARG_NOT_A_NUMBER = new LangKey("cmd.error.arg.not-a-number");
    public static final @NotNull LangKey ARG_NOT_PLAYER = new LangKey("cmd.error.arg.no-such-player");
    public static final @NotNull LangKey ARG_NOT_TIME = new LangKey("cmd.error.arg.not-time");
    public static final @NotNull LangKey ARG_UNKNOWN = new LangKey("cmd.error.arg.unknown");

    // commands
    public static final @NotNull LangKey CMD_ABOUT_PLUGIN_DESCRIPTION = new LangKey("cmd.about.description");
    public static final @NotNull LangKey  CMD_ABOUT_PLUGIN_HEADER = new LangKey("cmd.about.header");
    public static final @NotNull LangKey CMD_ABOUT_PLUGIN_VERSION = new LangKey("cmd.about.version");
    public static final @NotNull LangKey CMD_CREATE_ERROR_ALREADY_TREASURE = new LangKey("cmd.create.error.already-treasure");
    public static final @NotNull LangKey CMD_CREATE_ERROR_INVALID_CONTAINER = new LangKey("cmd.create.error.invalid-container");
    public static final @NotNull LangKey CMD_CREATE_SUCCESS = new LangKey("cmd.create.success");
    public static final @NotNull LangKey CMD_FORGET_ALL_SUCCESS = new LangKey("cmd.forget-all.success");
    public static final @NotNull LangKey CMD_FORGET_SHARED_SUCCESS = new LangKey("cmd.forget.shared.success");
    public static final @NotNull LangKey CMD_FORGET_USER_SUCCESS = new LangKey("cmd.forget.user.success");
    public static final @NotNull LangKey CMD_HELP_WIKI = new LangKey("cmd.help.wiki");
    public static final @NotNull LangKey CMD_LIST_FOOTER_BACK = new LangKey("cmd.list.footer.back");
    public static final @NotNull LangKey CMD_LIST_FOOTER_INNER = new LangKey("cmd.list.footer.inner");
    public static final @NotNull LangKey CMD_LIST_FOOTER_NEXT = new LangKey("cmd.list.footer.next");
    public static final @NotNull LangKey CMD_LIST_FOOTER_NONE = new LangKey("cmd.list.footer.none");
    public static final @NotNull LangKey CMD_LIST_FOOTER_OUTER = new LangKey("cmd.list.footer.outer");
    public static final @NotNull LangKey CMD_LIST_PLAYER_BODY = new LangKey("cmd.list.player.body");
    public static final @NotNull LangKey CMD_LIST_PLAYER_EMPTY = new LangKey("cmd.list.player.empty");
    public static final @NotNull LangKey CMD_LIST_PLAYER_HEADER = new LangKey("cmd.list.player.header");
    public static final @NotNull LangKey CMD_LIST_PLAYER_NEVER = new LangKey("cmd.list.player.never");
    public static final @NotNull LangKey CMD_LIST_TREASURES_EMPTY = new LangKey("cmd.list.treasures.empty");
    public static final @NotNull LangKey CMD_LIST_TREASURES_HEADER = new LangKey("cmd.list.treasures.header");
    public static final @NotNull LangKey CMD_LIST_TREASURES_BODY = new LangKey("cmd.list.treasures.body");
    public static final @NotNull LangKey CMD_LIST_NEARBY_TREASURES_EMPTY = new LangKey("cmd.list.nearby-treasures.empty");
    public static final @NotNull LangKey CMD_LIST_NEARBY_TREASURES_HEADER = new LangKey("cmd.list.nearby-treasures.header");
    public static final @NotNull LangKey CMD_LIST_NEARBY_TREASURES_BODY = new LangKey("cmd.list.nearby-treasures.body");
    public static final @NotNull LangKey CMD_LIST_WHO_BODY = new LangKey("cmd.list.who.body");
    public static final @NotNull LangKey CMD_LIST_WHO_EMPTY = new LangKey("cmd.list.who.empty");
    public static final @NotNull LangKey CMD_LIST_WHO_HEADER = new LangKey("cmd.list.who.header");
    public static final @NotNull LangKey CMD_LIST_WHO_SHARED = new LangKey("cmd.list.who.shared");
    public static final @NotNull LangKey CMD_PEEK_GENERATE_PLAYER = new LangKey("cmd.peek.generate.player");
    public static final @NotNull LangKey CMD_PEEK_GENERATE_SHARED = new LangKey("cmd.peek.generate.shared");
    public static final @NotNull LangKey CMD_PEEK_UNLIMITED = new LangKey("cmd.peek.unlimited");
    public static final @NotNull LangKey CMD_PEEK_WARNING = new LangKey("cmd.peek.warning");
    public static final @NotNull LangKey CMD_RELOAD_SUCCESS = new LangKey("cmd.reload.success");
    public static final @NotNull LangKey CMD_SET_FIND_FRESH_MESSAGE_OVERRIDE_SUCCESS = new LangKey("cmd.set.find-fresh-message-override.success");
    public static final @NotNull LangKey CMD_SET_FIND_FRESH_MESSAGE_OVERRIDE_REMOVED = new LangKey("cmd.set.find-fresh-message-override.removed");
    public static final @NotNull LangKey CMD_SET_FIND_LOOTED_MESSAGE_OVERRIDE_SUCCESS = new LangKey("cmd.set.find-looted-message-override.success");
    public static final @NotNull LangKey CMD_SET_FIND_LOOTED_MESSAGE_OVERRIDE_REMOVED = new LangKey("cmd.set.find-looted-message-override.removed");
    public static final @NotNull LangKey CMD_SET_FORGET_REMOVE_DURATION = new LangKey("cmd.set.forget.remove-duration");
    public static final @NotNull LangKey CMD_SET_RANDOM_SUCCESS = new LangKey("cmd.set.random.success");
    public static final @NotNull LangKey CMD_SET_SHARED_SUCCESS = new LangKey("cmd.set.shared.success");
    public static final @NotNull LangKey CMD_SET_UNLIMITED_SUCCESS = new LangKey("cmd.set.unlimited.success");
    public static final @NotNull LangKey CMD_SET_FORGET_DURATION_SUCCESS = new LangKey("cmd.set.forget.duration.success");
    public static final @NotNull LangKey CMD_SET_FORGET_DURATION_INSTANT_SUCCESS = new LangKey("cmd.set.forget.duration-instant.success");
    public static final @NotNull LangKey CMD_CANCEL_SUCCESS_OTHER = new LangKey("cmd.cancel.success.other");
    public static final @NotNull LangKey CMD_CANCEL_SUCCESS_SELF = new LangKey("cmd.cancel.success.self");
    public static final @NotNull LangKey CMD_CANCEL_SUCCESS_IMPORT = new LangKey("cmd.cancel.success.import");
    public static final @NotNull LangKey CMD_CANCEL_ERROR_NO_RUNNING = new LangKey("cmd.cancel.error.notRunning");

    //
    public static final @NotNull LangKey CMD_USAGE = new LangKey("cmd.usage");
    public static final @NotNull LangKey CMD_ERROR_NOT_ENOUGH_ARGS = new LangKey("cmd.error.not-enough-args");
    public static final @NotNull LangKey ERROR_NOT_LOOKING_AT_CONTAINER = new LangKey("cmd.error.not-looking-at-container");
    public static final @NotNull LangKey ERROR_NOT_LOOKING_AT_TREASURE = new LangKey("cmd.error.not-looking-at-treasure");
    public static final @NotNull LangKey ERROR_SENDER_NOT_PLAYER = new LangKey("cmd.error.sender-not-player");
    public static final @NotNull LangKey ERROR_UNKNOWN = new LangKey("error.unknown");

    public static final @NotNull LangKey REMOVE_GLOBAL_SUCCESS = new LangKey("remove.global.success");
    public static final @NotNull LangKey REMOVE_LOCAL_SUCCESS = new LangKey("remove.local.success");
    public static final @NotNull LangKey REMOVE_ERROR = new LangKey("remove.error");

    //
    public static final @NotNull LangKey FORMAT_LOCATION = new LangKey("format.location");
    public static final @NotNull LangKey NO_PERMISSION = new LangKey("no-permission");
    public static final @NotNull LangKey PLUGIN_PREFIX = new LangKey("prefix", "<gold>[GreenTreasure]</gold>");
    public static final @NotNull LangKey CHUNK_PROCESS_WORK = new LangKey("chunk-process.work");

    //
    public static final @NotNull LangKey TREASURE_TITLE_EDIT = new LangKey("treasure.title.edit");
    public static final @NotNull LangKey TREASURE_TITLE_PEEK_SHARED = new LangKey("treasure.title.peek.shared");
    public static final @NotNull LangKey TREASURE_TITLE_PEEK_PLAYER = new LangKey("treasure.title.peek.player");

    public static final @NotNull LangKey REFRESH_MSG_INFO_PERIODIC = new LangKey("refresh.msg.info.periodic");
    public static final @NotNull LangKey REFRESH_MSG_INFO_PERIODIC_INSTANT = new LangKey("refresh.msg.info.periodic_instant");
    public static final @NotNull LangKey REFRESH_MSG_INFO_NEVER = new LangKey("refresh.msg.info.never");
    public static final @NotNull LangKey REFRESH_MSG_INFO_INSTANT_UNLOCK = new LangKey("refresh.msg.info.instant_unlock");
    
    protected LangKey(final @NotNull @KeyPattern.Value @PropertyKey(resourceBundle = BUNDLE_NAME) String translationKey) {
        this(translationKey, null); // minecraft will use the key, if no translation was found. No need to set it ourselves
    }

    protected LangKey(final @NotNull @KeyPattern.Value @PropertyKey(resourceBundle = BUNDLE_NAME) String translationKey, final @Nullable String fallback) {
        super(Key.key(JavaPlugin.getPlugin(GreenTreasure.class).namespace(), translationKey), fallback);
    }
}
