package de.greensurvivors.green_treasure.permission;

import org.bukkit.command.CommandSender;

public enum Perm {
	//all rights
	TREASURE_ADMIN	("greentreasure.admin"),

	//right to open a treasure
	TREASURE_OPEN("greentreasure.open"),

	//right to use /gt about
	TREASURE_ABOUT("greentreasure.about"),
	//right to reload the plugin /gt reload
	TREASURE_RELOAD	("greentreasure.reload"),
	//right to use /gt help
	TREASURE_HELP ("greentreasure.help"),

	//right to create a new treasure /gt create
	TREASURE_CREATE ("greentreasure.create"),
	//right to delete a treasure /gt delete
	TREASURE_DELETE ("greentreasure.delete"),
	//right to edit the contents of a treasure inventory /gt edit
	TREASURE_EDIT ("greentreasure.edit"),
	//right to peek the inventory (of another player)s treasure /gt peek
	TREASURE_PEEK ("greentreasure.peek"),
	//right to reset the opening status of a or all players /gt forget and /gt forget-all
	TREASURE_FORGET ("greentreasure.forget"),

	//right to set the random slot chance /gt random <percentage>
	TREASURE_SET_RANDOM ("greentreasure.set.random"),
	//right to set a treasure unlimited times lootable /gt unlimited <true/false>
	TREASURE_SET_UNLIMITED ("greentreasure.set.unlimited"),
	//right to set the inventory of a treasure shared between all players
	TREASURE_SET_GLOBAL ("greentreasure.set.global"),
	//right to set the forget-period of a treasure ready to be looted again
	TREASURE_SET_FORGET ("greentreasure.set.forget"),

	//right to list what treasures a player has already looted
	TREASURE_LIST_PLAYERS("greentreasure.list.player"),
	//right to list all treasures
	TREASURE_LIST_TREASURES("greentreasure.list.treasures");

	private final String value;

	Perm(String value) {
		this.value = value;
	}

	public String get() {
		return value;
	}

	/**
	 * Check if CommandSender has any of the given permissions.
	 * @param cs CommandSender to check for
	 * @param permission permissions to check
	 * @return true if cs has at least one of the given permissions.
	 */
	public static boolean hasPermission(CommandSender cs, Perm...permission) {
		for (Perm p : permission) {
			if (cs.hasPermission(p.get()))
				return true;
		}
		return false;
	}
}
