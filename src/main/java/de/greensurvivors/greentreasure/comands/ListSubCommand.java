package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.config.TreasureConfig;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.dataobjects.list_cmd_helper.ListCmdPlayerDetailHelper;
import de.greensurvivors.greentreasure.dataobjects.list_cmd_helper.ListCmdTreasureHelper;
import de.greensurvivors.greentreasure.dataobjects.list_cmd_helper.ListCmdWholHelper;
import de.greensurvivors.greentreasure.language.Lang;
import de.greensurvivors.greentreasure.listener.TreasureListener;
import de.greensurvivors.greentreasure.permission.Perm;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.OfflinePlayer;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static de.greensurvivors.greentreasure.comands.TreasureCommands.LIST;

public class ListSubCommand {
    public enum LIST_TYPE {
        TREASURES ("treasures"),
        PLAYER_DETAIL("player"),
        WHO("who");

        private final String subCommand;

        LIST_TYPE(String subCommand){
            this.subCommand = subCommand;
        }

        public final String getSubCommand(){
            return subCommand;
        }

        public static @Nullable LIST_TYPE fromDSub(String subCommand) {
            return Stream.of(LIST_TYPE.values()).filter(myEnum -> myEnum.getSubCommand().equalsIgnoreCase(subCommand)).findFirst().orElse(null);
        }
    }


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
     * /gt list(0) treasures(1) [num - optional](2) - list all treasures
     * /gt list(0) player(1) playerName(2) [num- optional](3) - detail of all treasures of a player
     * /gt list(0) player(1) uuid(2) [num - optional](3) - detail of all treasures of a player
     * /gt list(0) who(1) [num - optional](2) (while looking at a container) - detail of all players of a treasure
     *
     * @param commandSender sender of this command
     */
    protected void handleList(CommandSender commandSender, String[] args) {
        if (args.length >= 2){
            LIST_TYPE type = LIST_TYPE.fromDSub(args[1]);

            if (type != null){
                switch (type){
                    case TREASURES -> {
                        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_LIST_TREASURES)) {
                            ArrayList<Location> locations = new ArrayList<>(TreasureListener.inst().getTreasureLocations());
                            final int numOfTreasures = locations.size();

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
                    case PLAYER_DETAIL -> {
                        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_LIST_PLAYERS)) {
                            if (args.length >= 3){
                                UUID uuidToGetListOf;

                                OfflinePlayer offlinePlayer = Bukkit.getOfflinePlayer(args[2]);
                                if (offlinePlayer.hasPlayedBefore()) {
                                    uuidToGetListOf = offlinePlayer.getUniqueId();
                                } else {
                                    try {
                                        uuidToGetListOf = UUID.fromString(args[2]);
                                    } catch (IllegalArgumentException ignored) {
                                        commandSender.sendMessage(Lang.build(Lang.NO_SUCH_PLAYER.get().replace(Lang.VALUE, args[2])));
                                        return;
                                    }
                                }

                                final ArrayList<Location> treasureLocations = new ArrayList<>(TreasureListener.inst().getTreasureLocations());
                                final int numOfTreasures = treasureLocations.size();

                                if (numOfTreasures > 0) {
                                    final int numPages = (int) Math.ceil((double) numOfTreasures / (double) ENTRIES_PER_PAGE);

                                    final int pageNow; //please note: we are start counting with page 1, not 0 for convenience of users of this plugin
                                    if (args.length >= 4) {
                                        if (Utils.isInt(args[3])) {
                                            //limit page to how many exits
                                            pageNow = Math.max(1, Math.min(numPages, Integer.parseInt(args[3])));
                                        } else {
                                            commandSender.sendMessage(Lang.build(Lang.NO_NUMBER.get().replace(Lang.VALUE, args[3])));
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
                                commandSender.sendMessage(Lang.build(Lang.NOT_ENOUGH_ARGS.get()));
                            }
                        } else {
                            commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
                        }
                    }
                    case WHO -> {
                        Container container = TreasureCommands.getContainer(commandSender);

                        if (container != null) {
                            TreasureConfig.inst().getAllPlayerDetailAsync(container.getBlock().getLocation(), playerLootDetailMap -> {
                                final int numOfPlayers = playerLootDetailMap.size();

                                if (numOfPlayers > 0) {
                                    final int numPages = (int) Math.ceil((double) numOfPlayers / (double) ENTRIES_PER_PAGE);

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

                                    //maximum of players out of all this page can display
                                    final int MAX_PLAYERS_THIS_PAGE = Math.min(numOfPlayers, pageNow * ENTRIES_PER_PAGE);
                                    //maximum of entries this page can display
                                    final int NUM_ENTRIES = MAX_PLAYERS_THIS_PAGE - (pageNow - 1) * ENTRIES_PER_PAGE;

                                    final ListCmdWholHelper helper = new ListCmdWholHelper(commandSender, pageNow, numPages, NUM_ENTRIES, container.getBlock().getLocation());

                                    final List<UUID> uuids = new ArrayList<>(playerLootDetailMap.keySet());
                                    //add the player info for the page
                                    for (int num = (pageNow - 1) * ENTRIES_PER_PAGE; num < MAX_PLAYERS_THIS_PAGE; num++) {
                                        helper.addEntry(uuids.get(num), playerLootDetailMap.get(uuids.get(num)));
                                    }

                                } else {
                                    commandSender.sendMessage(Lang.build(Lang.LIST_WHO_EMPTY.get()));
                                }
                            });

                        } else {
                            commandSender.sendMessage(Lang.build(Lang.NOT_LOOKINGAT_CONTAINER.get()));
                        }
                    }
                }
            } else {
                commandSender.sendMessage(Lang.build(Lang.UNKNOWN_ARGUMENT.get()));
                return;
            }
        }
    }

    /**
     * @param args The arguments passed to the command, including final
     *             partial argument to be completed
     * /gt list(0) treasures(1) [num - optional](2) - list all treasures
     * /gt list(0) player(1) playerName(2) [num- optional](3) - detail of all treasures of a player
     * /gt list(0) player(1) uuid(2) [num - optional](3) - detail of all treasures of a player
     * /gt list(0) who(1) [num - optional](2) (while looking at a container) - detail of all players of a treasure
     * @return suggestion of arguments, filtert by what's already written
     */
    protected List<String> handleTabComplete(@NotNull String[] args) {
        switch (args.length) {
            case 1 -> {
                return List.of(LIST);
            }
            case 2 -> {
                if (args[0].equalsIgnoreCase(LIST)) {
                    return Arrays.stream(LIST_TYPE.values()).map(LIST_TYPE::getSubCommand).toList();
                }
            }
            case 3 -> {
                if (args[0].equalsIgnoreCase(LIST)) {
                    List<String> result = new ArrayList<>();

                    if (args[1].equalsIgnoreCase(LIST_TYPE.PLAYER_DETAIL.getSubCommand())){
                        Collection<? extends Player> onlinePlayers = Bukkit.getOnlinePlayers();
                        if (!onlinePlayers.isEmpty()) {
                            result.addAll(onlinePlayers.stream().map(Player::getName).toList());
                        }
                    } else if (args[1].equalsIgnoreCase(LIST_TYPE.TREASURES.getSubCommand())){
                        final int numOfTreasures = TreasureListener.inst().getTreasureLocations().size();
                        if (numOfTreasures > 0) {
                            result.addAll(IntStream.rangeClosed(1, (int) Math.ceil((double) numOfTreasures / (double) ENTRIES_PER_PAGE))
                                    .boxed()
                                    .map(String::valueOf)
                                    .toList());
                        }
                    } else if (args[1].equalsIgnoreCase(LIST_TYPE.WHO.getSubCommand())) {
                        //nothing, since loading how many pages there are would hold back all the other completions
                    }

                    return result;
                }
            }
            case 4 -> {
                if (args[0].equalsIgnoreCase(LIST)) {
                    if (args[1].equalsIgnoreCase(LIST_TYPE.PLAYER_DETAIL.getSubCommand())){
                        final int numOfTreasures = TreasureListener.inst().getTreasureLocations().size();
                        if (numOfTreasures > 0) {
                            return IntStream.rangeClosed(1, (int) Math.ceil((double) numOfTreasures / (double) ENTRIES_PER_PAGE))
                                    .boxed()
                                    .map(String::valueOf)
                                    .toList();
                        }
                    }
                }
            }
        }

        return new ArrayList<>();
    }
}
