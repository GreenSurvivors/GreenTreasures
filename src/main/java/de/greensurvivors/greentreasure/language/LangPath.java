package de.greensurvivors.greentreasure.language;

/// Paths of all translatable
public enum LangPath {
    //actions
    ACTION_BREAK_CONTAINER_SUCCESS("action.break.container.success"),
    ACTION_BREAK_CONTAINER_ERROR("action.break.container.error"),
    ACTION_BREAK_CONTAINER_DENIED("action.break.container.denied"),
    ACTION_FIND_ALREADY_LOOTED("action.find.looted"),
    ACTION_FIND_LIMITED("action.find.limited"),
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
    CMD_DELETE_SUCCESS("cmd.delete.success"),
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
    CMD_LIST_TREASURE_FORGETPERIOD("cmd.list.treasures.forget-period"),
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

    //
    CMD_USAGE("cmd.usage"),
    ERROR_NOT_ENOUGH_ARGS("cmd.error.not-enough-args"),
    ERROR_NOT_LOOKING_AT_CONTAINER("cmd.error.not-looking-at-container"),
    ERROR_NOT_LOOKING_AT_TREASURE("cmd.error.not-looking-at-treasure"),
    ERROR_SENDER_NOT_PLAYER("cmd.error.sender-not-player"),
    ERROR_UNKNOWN("error.unknown"),

    //
    FORMAT_LOCATION("format.location"),
    NO_PERMISSION("no-permission"),
    PLUGIN_PREFIX("prefix", "<gold>[GreenTreasure]</gold>"),
    SET_FORGET_DURATION("cmd.set.forget.duration"),

    //
    TREASURE_TITLE_EDIT("treasure.title.edit"),
    TREASURE_TITLE_PEEK_SHARED("treasure.title.peek.shared"),
    TREASURE_TITLE_PEEK_PLAYER("treasure.title.peek.player");

    private final String path;
    private final String defaultValue;

    LangPath(String path) {
        this.path = path;
        this.defaultValue = path; // we don't need to define a default value, but if something couldn't get loaded we have to return at least helpful information
    }

    LangPath(String path, String defaultValue) {
        this.path = path;
        this.defaultValue = defaultValue;
    }

    public String getPath() {
        return path;
    }

    public String getDefaultValue() {
        return defaultValue;
    }
}
