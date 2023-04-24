package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.dataobjects.list_cmd_helper.ListCmdPlayerDetailHelper;
import de.greensurvivors.greentreasure.dataobjects.list_cmd_helper.ListCmdTreasureHelper;
import de.greensurvivors.greentreasure.language.Lang;
import de.greensurvivors.greentreasure.listener.TreasureListener;
import de.greensurvivors.greentreasure.permission.Perm;
import net.kyori.adventure.text.Component;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.OfflinePlayer;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.UUID;
import java.util.stream.IntStream;

import static de.greensurvivors.greentreasure.comands.TreasureCommands.LIST;

public class ListSubCommand {
    private static final byte ENTRIES_PER_PAGE = 5;

    private static ListSubCommand instance;

    public static ListSubCommand inst() {
        if (instance == null)
            instance = new ListSubCommand();
        return instance;
    }

    /**
     * list all treasures on this server,
     * or if a player was given lists all looted treasures
     * /gt list <num> (list all treasures)
     * /gt list playerName <num>
     * /gt list uuid <num>
     *
     * @param commandSender sender of this command
     */
    protected void handleList(CommandSender commandSender, String[] args) {
        UUID uuidToGetListOf;
        if (args.length >= 2 && !Utils.isInt(args[1])) {
            if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_LIST_PLAYERS)) {

                OfflinePlayer offlinePlayer = Bukkit.getOfflinePlayer(args[1]);
                if (offlinePlayer.hasPlayedBefore()) {
                    uuidToGetListOf = offlinePlayer.getUniqueId();
                } else {
                    try {
                        uuidToGetListOf = UUID.fromString(args[1]);
                    } catch (IllegalArgumentException ignored) {
                        commandSender.sendMessage(Lang.build(Lang.NO_SUCH_PLAYER.get().replace(Lang.VALUE, args[1])));
                        return;
                    }
                }

                final ArrayList<Location> treasureLocations = new ArrayList<>(TreasureListener.inst().getTreasureLocations());
                final int numOfTreasures = treasureLocations.size();

                if (numOfTreasures > 0) {
                    final int numPages = (int) Math.ceil((double) numOfTreasures / (double) ENTRIES_PER_PAGE);

                    final int pageNow; //please note: we are start counting with page 1, not 0 for convenience of users of this plugin
                    if (args.length >= 3) {
                        if (Utils.isInt(args[2])) {
                            //limit page to how many exits
                            pageNow = Math.max(1, Math.min(numPages, Integer.parseInt(args[2])));
                        } else {
                            commandSender.sendMessage(Lang.build(Lang.NO_NUMBER.get().replace(Lang.VALUE, args[2])));
                            return;
                        }
                    } else {
                        pageNow = 1;
                    }

                    //maximum of treasures out of all this page can display
                    final int MAX_TREASURES_THIS_PAGE = Math.min(numOfTreasures, pageNow * ENTRIES_PER_PAGE);
                    //maximum of entries this page can display
                    final int NUM_ENTRIES = MAX_TREASURES_THIS_PAGE - (pageNow - 1) * ENTRIES_PER_PAGE;

                    final ListCmdPlayerDetailHelper helper = new ListCmdPlayerDetailHelper(commandSender, pageNow, numPages, NUM_ENTRIES, uuidToGetListOf);

                    //add the players detail for the page
                    for (int num = (pageNow - 1) * ENTRIES_PER_PAGE; num < MAX_TREASURES_THIS_PAGE; num++) {
                        helper.addEntry(treasureLocations.get(num));
                    }
                } else {
                    commandSender.sendMessage(Lang.build(Lang.LIST_PLAYER_EMPTY.get()));
                }
            } else {
                commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
            }

        } else {
            if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_LIST_TREASURES)) {
                ArrayList<Location> locations = new ArrayList<>(TreasureListener.inst().getTreasureLocations());
                final int numOfTreasures = locations.size();

                if (numOfTreasures > 0) {
                    // collect all messages to send at once
                    final ArrayList<Component> listResult = new ArrayList<>();
                    final int numPages = (int) Math.ceil((double) numOfTreasures / (double) ENTRIES_PER_PAGE);

                    final int pageNow; //please note: we are start counting with page 1, not 0 for convenience of users of this plugin
                    if (args.length >= 2) {
                        if (Utils.isInt(args[1])) {
                            //limit page to how many exits
                            pageNow = Math.max(1, Math.min(numPages, Integer.parseInt(args[1])));
                        } else {
                            commandSender.sendMessage(Lang.build(Lang.NO_NUMBER.get().replace(Lang.VALUE, args[1])));
                            return;
                        }
                    } else {
                        pageNow = 1;
                    }

                    //maximum of treasures out of all this page can display
                    final int MAX_TREASURES_THIS_PAGE = Math.min(numOfTreasures, pageNow * ENTRIES_PER_PAGE);
                    //maximum of entries this page can display
                    final int NUM_ENTRIES = MAX_TREASURES_THIS_PAGE - (pageNow - 1) * ENTRIES_PER_PAGE;

                    final ListCmdTreasureHelper helper = new ListCmdTreasureHelper(commandSender, pageNow, numPages, NUM_ENTRIES);

                    //add the treasure info for the page
                    for (int num = (pageNow - 1) * ENTRIES_PER_PAGE; num < MAX_TREASURES_THIS_PAGE; num++) {
                        TreasureInfo treasureInfo = TreasureListener.inst().getTreasure(locations.get(num));

                        helper.addEntry(treasureInfo, locations.get(num));
                    }

                } else {
                    commandSender.sendMessage(Lang.build(Lang.LIST_TREASURES_EMPTY.get()));
                }

            } else {
                commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
            }
        }
    }

    /**
     * @param args The arguments passed to the command, including final
     *             partial argument to be completed
     * @return suggestion of arguments, filtert by what's already written
     */
    protected List<String> handleTabCompleate(@NotNull String[] args) {
        switch (args.length) {
            case 1 -> {
                return List.of(LIST);
            }
            case 2 -> {
                if (args[0].equalsIgnoreCase(LIST)) {
                    List<String> result = new ArrayList<>();

                    Collection<? extends Player> onlinePlayers = Bukkit.getOnlinePlayers();
                    if (!onlinePlayers.isEmpty()) {
                        result.addAll(onlinePlayers.stream().map(Player::getName).toList());
                    }

                    final int numOfTreasures = TreasureListener.inst().getTreasureLocations().size();
                    if (numOfTreasures > 0) {
                        result.addAll(IntStream.rangeClosed(1, (int) Math.ceil((double) numOfTreasures / (double) ENTRIES_PER_PAGE))
                                .boxed()
                                .map(String::valueOf)
                                .toList());
                    }

                    return result;
                }
            }
            case 3 -> {
                final int numOfTreasures = TreasureListener.inst().getTreasureLocations().size();
                if (numOfTreasures > 0) {
                    return IntStream.rangeClosed(1, (int) Math.ceil((double) numOfTreasures / (double) ENTRIES_PER_PAGE))
                            .boxed()
                            .map(String::valueOf)
                            .toList();
                }
            }
        }


        return new ArrayList<>();
    }
}
