package de.greensurvivors.greentreasure;

import org.bukkit.Bukkit;
import org.bukkit.permissions.Permission;
import org.bukkit.permissions.PermissionDefault;
import org.jetbrains.annotations.NotNull;

import java.util.Map;

public enum PermmissionManager {
    //right to open a treasure
    TREASURE_OPEN(new Permission("greentreasure.open")),

    //right to use /gt about
    TREASURE_ABOUT(new Permission("greentreasure.about")),
    //right to reload the plugin /gt reload
    TREASURE_RELOAD(new Permission("greentreasure.reload")),

    //right to create a new treasure /gt create
    TREASURE_CREATE(new Permission("greentreasure.create")),
    //right to delete a treasure /gt delete
    TREASURE_DELETE(new Permission("greentreasure.delete")),
    //right to edit the contents of a treasure inventory /gt edit
    TREASURE_EDIT_CONTENT(new Permission("greentreasure.editcontent")),
    //right to peek the inventory (of another player)s treasure /gt peek
    TREASURE_PEEK(new Permission("greentreasure.peek")),
    //right to reset the opening status of a or all players /gt forget and /gt forget-all
    TREASURE_FORGET(new Permission("greentreasure.forget")),

    //right to set the random slot chance /gt random <percentage>
    TREASURE_SET_RANDOM(new Permission("greentreasure.set.random")),
    //right to set a treasure unlimited times lootable /gt unlimited <true/false>
    TREASURE_SET_UNLIMITED(new Permission("greentreasure.set.unlimited")),
    //right to set the inventory of a treasure shared between all players
    TREASURE_SET_SHARED(new Permission("greentreasure.set.shared")),
    //right to set the forget-period of a treasure ready to be looted again
    TREASURE_SET_FORGET(new Permission("greentreasure.set.forget")),

    //right to list what treasures a player has already looted
    TREASURE_LIST_PLAYERS(new Permission("greentreasure.list.player")),
    //right to list all treasures
    TREASURE_LIST_TREASURES(new Permission("greentreasure.list.treasures")),
    //right to list all treasures
    TREASURE_LIST_WHO(new Permission("greentreasure.list.who")),

    //all rights
    TREASURE_ADMIN(new Permission("greentreasure.*", PermissionDefault.OP,
        Map.ofEntries(
            Map.entry(TREASURE_OPEN.permission.getName(), Boolean.TRUE),
            Map.entry(TREASURE_ABOUT.permission.getName(), Boolean.TRUE),
            Map.entry(TREASURE_RELOAD.permission.getName(), Boolean.TRUE),
            Map.entry(TREASURE_CREATE.permission.getName(), Boolean.TRUE),
            Map.entry(TREASURE_DELETE.permission.getName(), Boolean.TRUE),
            Map.entry(TREASURE_EDIT_CONTENT.permission.getName(), Boolean.TRUE),
            Map.entry(TREASURE_PEEK.permission.getName(), Boolean.TRUE),
            Map.entry(TREASURE_FORGET.permission.getName(), Boolean.TRUE),
            Map.entry(TREASURE_SET_RANDOM.permission.getName(), Boolean.TRUE),
            Map.entry(TREASURE_SET_UNLIMITED.permission.getName(), Boolean.TRUE),
            Map.entry(TREASURE_SET_SHARED.permission.getName(), Boolean.TRUE),
            Map.entry(TREASURE_SET_FORGET.permission.getName(), Boolean.TRUE),
            Map.entry(TREASURE_LIST_PLAYERS.permission.getName(), Boolean.TRUE),
            Map.entry(TREASURE_LIST_TREASURES.permission.getName(), Boolean.TRUE),
            Map.entry(TREASURE_LIST_WHO.permission.getName(), Boolean.TRUE)
        )
    ));

    private final @NotNull Permission permission;

    PermmissionManager(final @NotNull Permission permission) {
        this.permission = permission;

        Bukkit.getPluginManager().addPermission(permission);
    }

    public Permission get() {
        return permission;
    }
}
