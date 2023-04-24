package de.greensurvivors.greentreasure.language;

import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.comands.TreasureCommands;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.JoinConfiguration;
import net.kyori.adventure.text.TextComponent;
import net.kyori.adventure.text.event.ClickEvent;
import net.kyori.adventure.text.format.TextColor;
import net.kyori.adventure.text.format.TextDecoration;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;
import org.bukkit.Location;
import org.bukkit.inventory.ItemStack;

public enum Lang implements Cons {
	NO_PLAYER ("&cYou have to be a player."),
	NO_NUMBER (String.format("&6'&e%s&6' &cis not a valid number.", VALUE)),
	NO_BOOL(String.format("&6'&e%s&6' &cis not a valid boolean.", VALUE)),
	NO_PERMISSION_COMMAND("&cYou have no permission to perform this command."),
	NO_PERMISSION_SOMETHING("&cYou have no permission to do that."),
	NO_SUCH_PLAYER (String.format("&cCould not get a valid player from %s", VALUE)),
	UNKNOWN_ARGUMENT (String.format("&cUnknown or wrong argument '%s'.", VALUE)),
	UNKNOWN_ERROR ("&cUnknown Error. What happened?"),
	NOT_ENOUGH_ARGS ("&cNot enough arguments."),

	FORMAT_LOCATION (String.format("&f%s&6, &f%s&e, &f%s&e, &f%s", WORLD, X, Y, Z)),

	HELP_WIKI ("&7See more detailed information on the wiki page."),

	PLUGIN_HEADER ("&a-<(&6GreenTreasure&a)>-"),
	PLUGIN_DESCRIPTION(String.format("&aDescription&6: &e%s", VALUE)),
	PLUGIN_VERSION (String.format("&aVersion&6: &e%s", VALUE)),

	RELOAD ("&aReloaded."),

	TREASURE_ALREADYLOOTED ("&7You already looted this treasure."),
	TREASURE_FOUND_LIMITED("&aYou found treasure!"),
	TREASURE_FOUND_UNLIMITED("&aYou found treasure! Take as much as you want!"),

	TREASURE_CREATE_START("&7Starting create process. Please do not interact with the treasure, until the process is complete."),
	TREASURE_CREATE_END(String.format("&a%s was created as treasure.", TYPE)),
	TREASURE_DELETE_START("&7Starting delete process. Please do not interact with the treasure, until the process is complete."),
	TREASURE_DELETE_END(String.format("&a%s was removed as treasure.", TYPE)),

	TREASURE_COMMAND_CREATE (String.format("/%s %s", TreasureCommands.CMD, TreasureCommands.CREATE)),
	TREASURE_COMMAND_DELETE (String.format("/%s %s", TreasureCommands.CMD, TreasureCommands.DELETE)),

	NO_TREASURE (String.format("&cYou are not looking at a treasure. Use &6'&e%s&6' first.", TREASURE_COMMAND_CREATE.get())),

	FORGET_PLAYER (String.format("&aThe treasure forgot &6'&e%s&6'&a has ever found it.", UUID)),
	FORGET_ALL_START("&7startet forgetting all. Please do not interact with the treasure, until the process is complete."),
	FORGET_ALL_END("&aThe Treasure forgot anybody has ever found it."),

	SET_RANDOM_START("&7Starting set process. Please do not interact with the treasure, until the process is complete."),
	SET_RANDOM_END(String.format("&aTreasure was set with a slot chance of &e%s %%&a.", VALUE)),
	SET_FORGET_START("&7Starting set process. Please do not interact with the treasure, until the process is complete."),
	SET_FORGET_END(String.format("&aTreasure forgets every %s.", VALUE)),
	REMOVE_FORGET_END("&aThe treasure never forgets."),
	SET_UNLIMITED_START("&7Starting set process. Please do not interact with the treasure, until the process is complete."),
	SET_UNLIMITED_TRUE("&aThe treasure is now lootable a unlimited amount of times."),
	SET_UNLIMITED_FALSE("&aThe treasure is now lootable only one time."),
	SET_GLOBAL_START("&7Starting set process. Please do not interact with the treasure, until the process is complete."),
	SET_GLOBAL_END("&aThe treasure inventory is now shared by every player."),

	NOT_LOOKINGAT_CONTAINER ("&cYou are not looking at a container."),

	DONT_BREAK_CONTAINER_USER ("&cYou are not allowed to break this Treasure."),
	DONT_BREAK_CONTAINER_ADMIN (String.format("&cThis is a Treasure. If you want to remove it, use &6'&e%s&6'&c first", TREASURE_COMMAND_DELETE.get())),

	LIST_PLAYER_HEADER(String.format("&2-------<(&6Treasures of &e- &6%s&a &6%s&e/&6%s &2)>-------", NAME, VALUE, VALUE2)),
	LIST_PLAYER_BODY(String.format("%s&7 was looted last: %s.", LOCATION, VALUE)),
	LIST_PLAYER_EMPTY("&cNo Treasure was created, yet. Try to create a new one with &e/" + TreasureCommands.CMD + " " + TreasureCommands.CREATE),
	LIST_PLAYER_NEVER("never"),
	LIST_TREASURES_HEADER(String.format("&2-------<(&6Treasures &e- &6%s&e/&6%s &2)>-------", VALUE, VALUE2)),
	LIST_TREASURE_BODY(String.format("%s&7 with slot chance of &e%s%%&7 is global&6: &e%s&7 is unlimited&6: &e%s&7", LOCATION, VALUE, GLOBAL, UNLIMITED)),
	LIST_TREASURES_EMPTY("&cNo Treasure was created, yet. Try to create a new one with &e/" + TreasureCommands.CMD + " " + TreasureCommands.CREATE),
	LIST_TREASURE_FORGETPERIOD(String.format(" resetting every &e%s&7", VALUE)),
	TRUE("true"),
	FALSE("false"),

	LIST_FOOTER_OUTER("&2----"),
	LIST_FOOTER_INNER("&2-----<*>-----"),
	LIST_FOOTER_BACK(String.format("&6<<( &e%s&6 ) ", VALUE)),
	LIST_FOOTER_NEXT(String.format("&6 ( &e%s&6 )>>", VALUE)),
	LIST_FOOTER_NONE("-------"),

	TREASURE_EDITED_START("&7Starting editing process. Please do not interact with the treasure, until the process is complete."),
	TREASURE_EDITED_END("&aTreasure saved with new content."),

	PEEK_PLAYER (String.format("&7Treasure of %s", VALUE)),
	PEEK_GLOBAL ("Global treasure"),
	PEEK_WARNING ("&6Warning: This inventory is not sync. If the Player (or a player in case of global) has the treasure open at the same time, problems may occur."),
	PEEK_GENERATE("&e%s&7 never opened this treasure. Generating new Inventory."),
	PEEK_UNLIMITED("&cThis treasure is unlimited and personalized unlimited treasure is not an unsupported feature.");



	private String value;

	Lang(String value) {
		this.value = value;
	}

	/**
	 * uncolored
	 * @return value
	 */
	public String get() {
		return value;
	}

	/**
	 * Set value.
	 * @param value to set
	 */
	public void set(String value) {
		this.value = value;
	}


	/**
	 * Builds a Component from given text.
	 * @param args to build from
	 * @return Component
	 */
	public static Component build(String args) {
		return build(args, null, null, null, null);
	}

	/**
	 * Build a Component from given text with properties.
	 * There can only be one click action.
	 * @param text to build from
	 * @param command to execute on click
	 * @param hover to show when mouse hovers over the text
	 * @param suggestion to show on click
	 * @return Component
	 */
	public static Component build(String text, String command, Object hover, String suggestion) {
		return build(text, command, hover, suggestion, null);
	}

	/**
	 * Build a Component from given text with properties.
	 * There can only be one click action.
	 * @param text to build from
	 * @param command to execute on click
	 * @param hover to show when mouse hovers over the text
	 * @param suggestion to show on click
	 * @param link to open on click
	 * @return Component
	 */
	public static Component build(String text, String command, Object hover, String suggestion, String link) {
		if (text != null) {
			TextComponent tc = rgb(Component.text(text));
			// command suggestion
			if (suggestion != null && !suggestion.isBlank()) {
				tc = tc.clickEvent(ClickEvent.suggestCommand(suggestion));
			}
			// run command
			if (command != null && !command.isBlank()) {
				tc = tc.clickEvent(ClickEvent.runCommand(command));
			}
			// open link
			if (link != null && !link.isBlank()) {
				tc = tc.clickEvent(ClickEvent.openUrl(link));
			}
			// hover effect
			if (hover != null) {
				if (hover instanceof Component component)
					tc = tc.hoverEvent(component);
				if (hover instanceof ItemStack item)
					tc = tc.hoverEvent(item.asHoverEvent());
				if (hover instanceof String s)
					tc = tc.hoverEvent(Component.text(s));
			}
			return tc;
		}
		return null;
	}

	/**
	 * Join Components together with new lines as separator.
	 * @param components to join together
	 * @return Component
	 */
	public static Component join(Iterable<? extends Component> components) {
		return Component.join(JoinConfiguration.separator(Component.newline()), components);
	}

	/**
	 * Get a Component colored with rgb colors.
	 * @param tc to convert colors
	 * @return Component with all color codes converted
	 */
	private static TextComponent rgb (TextComponent tc) {
		String text = tc.content();
		if (text.contains("&#")) {
			// find first hexColor
			int i = text.indexOf("&#");
			// substring first part (old color)
			tc = LegacyComponentSerializer.legacyAmpersand().deserialize(text.substring(0, i));
			//alternative: tc = LegacyComponentSerializer.legacy('&').deserialize(text.substring(0, i));

			//explicitly deny unset decorations. If not done the server trys to set them after reload
			//and items with this component as lore can't be stacked.
			for (TextDecoration t : TextDecoration.values()){
				if (tc.decorations().containsKey(t))
					tc = tc.decoration(t, false);
			}

			// substring last part (new color)
			tc = tc.append(rgb(Component.text(text.substring(i + 8), TextColor.fromHexString(text.substring(i + 1, i + 8)))));
		} else {
			// text with legacy ChatColor
			tc = LegacyComponentSerializer.legacyAmpersand().deserialize(text);
		}
		return tc;
	}

	/**
	 * formats a period of time into its components
	 * @param period time period in milliseconds
	 * @return a formatted String containing the period
	 */
	public static String formatTimePeriod(long period){
		String result = "";

		long ticks;

		if (period >= Utils.YEAR_IN_MILLISECONDS){
			long years = period / Utils.YEAR_IN_MILLISECONDS;

			result = String.valueOf(years) + " y ";
			period -= years * Utils.YEAR_IN_MILLISECONDS;
		}

		if (period >= Utils.WEAK_IN_MILLISECONDS){
			long weeks = period / Utils.WEAK_IN_MILLISECONDS;

			result = result + String.valueOf(weeks) + " w ";
			period -= weeks * Utils.WEAK_IN_MILLISECONDS;
		}

		if (period >= Utils.DAY_IN_MILLISECONDS){
			long days = period / Utils.DAY_IN_MILLISECONDS;

			result = result + String.valueOf(days) + " d ";
			period -= days * Utils.DAY_IN_MILLISECONDS;
		}

		if (period >= Utils.HOUR_IN_MILLISECONDS){
			long hours = period / Utils.HOUR_IN_MILLISECONDS;

			result = result + String.valueOf(hours) + " h ";
			period -= hours * Utils.HOUR_IN_MILLISECONDS;
		}

		if (period >= Utils.MINUTE_IN_MILLISECONDS){
			long minutes = period / Utils.MINUTE_IN_MILLISECONDS;

			result = result + String.valueOf(minutes) + " m ";
			period -= minutes * Utils.MINUTE_IN_MILLISECONDS;
		}

		if (period >= Utils.SECONDS_IN_MILLISECONDS){
			long seconds = period / Utils.SECONDS_IN_MILLISECONDS;

			result = result + String.valueOf(seconds) + " s ";
			period -= seconds * Utils.SECONDS_IN_MILLISECONDS;
		}

		 ticks = period / Utils.TICKS_IN_MILLISECONDS;

		return result + String.valueOf(ticks) + " t ";
	}

	/**
	 * formats a location to a string
	 * @param loc location to format
	 * @return the formatted string or "-" if the locaion was null
	 */
	public static String locationToString(Location loc) {
		if (loc != null) {
			return FORMAT_LOCATION.get()
					.replace(Lang.WORLD, loc.getWorld().getName())
					.replace(Lang.X, Integer.toString(loc.getBlockX()))
					.replace(Lang.Y, Integer.toString(loc.getBlockY()))
					.replace(Lang.Z, Integer.toString(loc.getBlockZ()));
		}
		return "-";
	}
}
