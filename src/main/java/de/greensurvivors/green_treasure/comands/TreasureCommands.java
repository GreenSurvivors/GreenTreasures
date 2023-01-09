package de.greensurvivors.green_treasure.comands;

import de.greensurvivors.green_treasure.GreenTreasure;
import de.greensurvivors.green_treasure.Utils;
import de.greensurvivors.green_treasure.config.TreasureConfig;
import de.greensurvivors.green_treasure.data_objects.PeekedTreasure;
import de.greensurvivors.green_treasure.data_objects.PlayerLootDetail;
import de.greensurvivors.green_treasure.data_objects.TreasureInfo;
import de.greensurvivors.green_treasure.language.Lang;
import de.greensurvivors.green_treasure.listener.CommandInventoriesListener;
import de.greensurvivors.green_treasure.listener.TreasureListener;
import de.greensurvivors.green_treasure.permission.Perm;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.serializer.plain.PlainTextComponentSerializer;
import org.apache.commons.lang.BooleanUtils;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.OfflinePlayer;
import org.bukkit.block.Block;
import org.bukkit.block.Container;
import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;
import org.bukkit.command.TabCompleter;
import org.bukkit.entity.LivingEntity;
import org.bukkit.entity.Player;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.text.DateFormat;
import java.util.*;
import java.util.stream.Collectors;

//create, change, forget forget-all, random,

public class TreasureCommands implements CommandExecutor, TabCompleter {
    public static final String
            CMD = "greentresure",
            CREATE = "create",
            DELETE = "delete", REMOVE = "remove",
            ABOUT = "about",
            HELP_SHORT = "?", HELP_LONG = "help",
            RELOAD = "reload",
            FORGET = "forget",
            FORGET_ALL = "forget-all",
            EDIT = "edit",
            LIST = "list",
            PEEK = "peek",
            SET = "set",
            RANDOM = "random",
            UNLIMITED = "unlimited",
            GLOBAL = "global";

    // time suffixes
    private static final String
            SUFFIX_YEAR = "y",
            SUFFIX_WEEK = "w",
            SUFFIX_DAY = "d",
            SUFFIX_HOUR = "h",
            SUFFIX_MINUTE = "m",
            SUFFIX_SECOND = "s",
            SUFFIX_TICK = "t",
            SUFFIX_MILLISECOND = "ms";

    private static TreasureCommands instance;

    public static TreasureCommands inst() {
        if (instance == null)
            instance = new TreasureCommands();
        return instance;
    }

    /**
     *
     * @param commandSender Source of the command
     * @param command Command which was executed
     * @param label Alias of the command which was used
     * @param args Passed command arguments
     * @return always true, except if the subcommand was unknown, because most subcommands have their own handling of errors
     */
    @Override
    public boolean onCommand(@NotNull CommandSender commandSender, @NotNull Command command, @NotNull String label, @NotNull String[] args) {
        if (args.length > 0) {
            switch (args[0].toLowerCase()) {
                case CREATE -> handleCreate(commandSender);
                case DELETE, REMOVE -> handleDelete(commandSender);
                case EDIT -> handleEdit(commandSender);
                case LIST -> handleList(commandSender, args);
                case PEEK -> handlePeek(commandSender, args);
                case FORGET -> handleForget(commandSender, args);
                case FORGET_ALL -> handleForgetAll(commandSender);
                case SET -> {
                    if (args.length > 1){
                        switch (args[1].toLowerCase()) {
                            case RANDOM -> handleSetRandom(commandSender, args);
                            case UNLIMITED -> handleSetUnlimited(commandSender, args);
                            case GLOBAL -> handleSetGlobal(commandSender, args);
                            case FORGET -> handleSetForget(commandSender, args);
                            default -> {
                                return false;
                            }
                        }
                    } else {
                        commandSender.sendMessage(Lang.build(Lang.NOT_ENOUGH_ARGS.get()));
                    }
                }
                case ABOUT -> handleAbout(commandSender);
                case RELOAD -> handleReload(commandSender);
                case HELP_SHORT, HELP_LONG -> handleHelp(commandSender, args);
                default -> {
                    return false;
                }
            }
        } else {
            // plugin
            handleAbout(commandSender);
        }

        return true;
    }

    /**
     * @param commandSender Source of the command. For players tab-completing a
     *     command inside a command block, this will be the player, not
     *     the command block.
     * @param command Command which was executed
     * @param label Alias of the command which was used
     * @param args The arguments passed to the command, including final
     *     partial argument to be completed
     * @return suggestion of arguments, filtert by what's already written
     */
    @Override
    public @Nullable List<String> onTabComplete(@NotNull CommandSender commandSender, @NotNull Command command, @NotNull String label, @NotNull String[] args) {

        switch (args.length){
            case 1 -> {
                final ArrayList<String> subCommands = new ArrayList<>();

                //check permissions and add subCommands to be suggested for the commandSender
                if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_CREATE)){
                    subCommands.add(CREATE);
                }
                if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_DELETE)){
                    subCommands.add(DELETE);
                    subCommands.add(REMOVE);
                }
                if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_EDIT)){
                    subCommands.add(EDIT);
                }
                if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_LIST_PLAYERS, Perm.TREASURE_LIST_TREASURES)){
                    subCommands.add(LIST);
                }
                if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_PEEK)){
                    subCommands.add(PEEK);
                }
                if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_FORGET)){
                    subCommands.add(FORGET);
                    subCommands.add(FORGET_ALL);
                }
                if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_FORGET, Perm.TREASURE_SET_UNLIMITED, Perm.TREASURE_SET_RANDOM, Perm.TREASURE_SET_GLOBAL)){
                    subCommands.add(SET);
                }

                if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_ABOUT)){
                    subCommands.add(ABOUT);
                }
                if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_RELOAD)){
                    subCommands.add(RELOAD);
                }
                if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_HELP)){
                    subCommands.add(HELP_LONG);
                    subCommands.add(HELP_SHORT);
                }

                return subCommands.stream().filter(s -> s.startsWith(args[0].toLowerCase())).collect(Collectors.toList());
            }
            case 2 -> {
                final ArrayList<String> arguments = new ArrayList<>();

                switch (args[1]){
                    case CREATE, DELETE, REMOVE, EDIT, FORGET_ALL, ABOUT, RELOAD -> {
                        return null;
                    }
                    case LIST -> {
                        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_LIST_PLAYERS)){

                            Collection<? extends Player> onlinePlayers =  Bukkit.getOnlinePlayers();
                            if (!onlinePlayers.isEmpty()){
                                arguments.addAll(onlinePlayers.stream().map(Player::getName).toList());
                            }
                        }
                    }
                    case PEEK -> {
                        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_PEEK)){

                            Collection<? extends Player> onlinePlayers =  Bukkit.getOnlinePlayers();
                            if (!onlinePlayers.isEmpty()){
                                arguments.addAll(onlinePlayers.stream().map(Player::getName).toList());
                            }
                        }
                    }
                    case FORGET -> {
                        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_FORGET)){

                            Collection<? extends Player> onlinePlayers =  Bukkit.getOnlinePlayers();
                            if (!onlinePlayers.isEmpty()){
                                arguments.addAll(onlinePlayers.stream().map(Player::getName).toList());
                            }
                        }
                    }
                    case SET -> {
                        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_RANDOM)){
                            arguments.add(RANDOM);
                        }
                        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_UNLIMITED)){
                            arguments.add(UNLIMITED);
                        }
                        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_GLOBAL)){
                            arguments.add(GLOBAL);
                        }
                        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_FORGET)){
                            arguments.add(FORGET);
                        }
                    }
                    case HELP_SHORT, HELP_LONG -> {
                        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_CREATE)){
                            arguments.add(CREATE);
                        }
                        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_DELETE)){
                            arguments.add(DELETE);
                            arguments.add(REMOVE);
                        }
                        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_EDIT)){
                            arguments.add(EDIT);
                        }
                        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_LIST_PLAYERS, Perm.TREASURE_LIST_TREASURES)){
                            arguments.add(LIST);
                        }
                        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_PEEK)){
                            arguments.add(PEEK);
                        }
                        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_FORGET)){
                            arguments.add(FORGET);
                            arguments.add(FORGET_ALL);
                        }
                        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_FORGET, Perm.TREASURE_SET_UNLIMITED, Perm.TREASURE_SET_RANDOM, Perm.TREASURE_SET_GLOBAL)){
                            arguments.add(SET);
                        }
                        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_RANDOM)){
                            arguments.add(RANDOM);
                        }
                        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_UNLIMITED)){
                            arguments.add(UNLIMITED);
                        }
                        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_GLOBAL)){
                            arguments.add(GLOBAL);
                        }
                        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_FORGET)){
                            arguments.add(FORGET);
                        }
                        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_ABOUT)){
                            arguments.add(ABOUT);
                        }
                        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_RELOAD)){
                            arguments.add(RELOAD);
                        }
                        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_HELP)){
                            arguments.add(HELP_LONG);
                        }
                    }
                }

                return arguments.stream().filter(s -> s.startsWith(args[1].toLowerCase())).collect(Collectors.toList());
            }

            case 3 -> { //gt set(0) forget(1) <1y 2w 3d 4h 5m 6s 7t 8ms>(2..)
                //don't get confused with help subcommands
                if (args[1].equalsIgnoreCase(SET)){
                    final ArrayList<String> arguments = new ArrayList<>();
                    switch (args[2]){
                        case RANDOM -> {
                            arguments.add("100.0");
                            arguments.add("75.0");
                            arguments.add("50.0");
                            arguments.add("25.0");
                            arguments.add("10.0");
                            arguments.add("5.0");
                            arguments.add("2.0");
                        }
                        case UNLIMITED, GLOBAL -> {
                            arguments.add(Boolean.TRUE.toString());
                            arguments.add(Boolean.FALSE.toString());
                        }
                        case FORGET -> {
                            arguments.add(SUFFIX_YEAR);
                            arguments.add(SUFFIX_WEEK);
                            arguments.add(SUFFIX_DAY);
                            arguments.add(SUFFIX_HOUR);
                            arguments.add(SUFFIX_MINUTE);
                            arguments.add(SUFFIX_SECOND);
                            arguments.add(SUFFIX_TICK);
                            arguments.add(SUFFIX_MILLISECOND);
                        }
                    }

                    return arguments.stream().filter(s -> s.startsWith(args[2].toLowerCase())).collect(Collectors.toList());
                }
            }

            default -> {
                if (args[1].equalsIgnoreCase(SET) && args[2].equalsIgnoreCase(FORGET)){
                    final ArrayList<String> arguments = new ArrayList<>();
                    arguments.add(SUFFIX_YEAR);
                    arguments.add(SUFFIX_WEEK);
                    arguments.add(SUFFIX_DAY);
                    arguments.add(SUFFIX_HOUR);
                    arguments.add(SUFFIX_MINUTE);
                    arguments.add(SUFFIX_SECOND);
                    arguments.add(SUFFIX_TICK);
                    arguments.add(SUFFIX_MILLISECOND);

                    return arguments.stream().filter(s -> s.startsWith(args[args.length-1].toLowerCase())).collect(Collectors.toList());
                }
            }
        }

        return null;
    }

    /**
     * Get container a living entity is looking at
     * @param commandSender sender of this command looking at a container
     * @return container or null if the living entity is not looking at a container
     */
    private static Container getContainer(CommandSender commandSender) {
        if (commandSender instanceof LivingEntity livingEntity) {
            // is the player looking at a container?
            Block block = livingEntity.getTargetBlockExact(5);

            if (block.getState() instanceof Container container) {
                return container;
            } else {
                commandSender.sendMessage(Lang.build(Lang.NOT_LOOKINGAT_CONTAINER.get()));
                return null;
            }
        } else {
            commandSender.sendMessage(Lang.build(Lang.NO_PLAYER.get()));
            return null;
        }
    }

    /**
     * creates a new treasure
     * /gt create
     * @param commandSender sender of this command
     */
    private void handleCreate(CommandSender commandSender) {
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_CREATE)) {
            Container container = getContainer(commandSender);

            if (container != null){
                Location location = container.getBlock().getLocation();
                List<ItemStack> itemStacks = Arrays.stream(container.getInventory().getContents()).toList();

                TreasureConfig.inst().saveTreasureAsync(location, itemStacks, container.getBlock().getType().name());

                commandSender.sendMessage(Lang.build(Lang.TREASURE_CREATE.get().replace(Lang.TYPE,
                        container.customName() == null ? container.getBlock().getType().name() :
                        PlainTextComponentSerializer.plainText().serialize(container.customName()))));
            } else {
                commandSender.sendMessage(Lang.build(Lang.NOT_LOOKINGAT_CONTAINER.get()));
            }
        } else {
            commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
        }
    }

    /**
     * deletes a treasure AND all the UNLOOTED stuff for every player
     * /gt delete
     * @param commandSender sender of this command
     */
    private void handleDelete(CommandSender commandSender) {
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_DELETE)) {
            Container container = getContainer(commandSender);

            if (container != null){
                TreasureConfig.inst().saveTreasureAsync(container.getBlock().getLocation(), null, null);
                TreasureConfig.inst().forgetAllAsync(container.getBlock().getLocation());

                commandSender.sendMessage(Lang.build(Lang.TREASURE_DELETE.get().replace(Lang.TYPE,
                        container.customName() == null ? container.getBlock().getType().name() :
                                PlainTextComponentSerializer.plainText().serialize(container.customName()))));
            } else {
                commandSender.sendMessage(Lang.build(Lang.NOT_LOOKINGAT_CONTAINER.get()));
            }
        } else {
            commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
        }
    }

    /**
     * lets the commandSender edit a treasure inventory
     * note: two 2 players editing the same inventory at the same time is NOT supported right now
     * /gt edit
     * @param commandSender sender of this command
     */
    private void handleEdit(CommandSender commandSender){
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_EDIT)) {
            Container container = getContainer(commandSender);

            if (container != null){
                if (commandSender instanceof Player player){
                    Component eTitle = Lang.build("EDIT TREASURE");
                    final Location location = Utils.cleanLocation(container.getLocation());
                    TreasureInfo treasureInfo = TreasureListener.inst().getTreasure(location);

                    if (treasureInfo != null) {
                        Inventory nowEditing = Bukkit.createInventory(null, container.getInventory().getType(), eTitle);
                        // clone every item stack and put it into the new inventory
                        nowEditing.setContents(treasureInfo.itemLoot().stream().map(s -> s == null ? null : s.clone()).toArray(ItemStack[]::new));

                        CommandInventoriesListener.inst().addEditingTreasure(player.openInventory(nowEditing), location);
                    } else {
                        commandSender.sendMessage(Lang.build(Lang.NO_TREASURE.get()));
                    }
                } else {
                    commandSender.sendMessage(Lang.build(Lang.NO_PLAYER.get()));
                }
            } else {
                commandSender.sendMessage(Lang.build(Lang.NOT_LOOKINGAT_CONTAINER.get()));
            }
        } else {
            commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
        }
    }

    /**
     * list all treasures on this server,
     * or if a player was given lists all looted treasures
     * /gt list (list all treasures)
     * /gt list playerName
     * /gt list uuid
     * @param commandSender sender of this command
     */
    private void handleList(CommandSender commandSender, String[] args){
        UUID uuidToGetListOf;
        if (args.length >= 2){
            if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_LIST_PLAYERS)) {

                OfflinePlayer offlinePlayer = Bukkit.getOfflinePlayer(args[1]);
                if (offlinePlayer.hasPlayedBefore()){
                    uuidToGetListOf = offlinePlayer.getUniqueId();
                } else {
                    try{
                        uuidToGetListOf = UUID.fromString(args[1]);
                    }catch (IllegalArgumentException ignored){
                        commandSender.sendMessage(Lang.build(Lang.NO_SUCH_PLAYER.get().replace(Lang.VALUE, args[1])));
                        return;
                    }
                }

                Set<Location> treasureLocations = TreasureListener.inst().getTreasureLocations();
                final ArrayList<PlayerLootDetail> playerLootDetails = new ArrayList<>();

                for(Iterator<Location> locationIterator = treasureLocations.iterator(); locationIterator.hasNext();) {
                    final Location treasureLocation = locationIterator.next();
                    final boolean lastEntry = !locationIterator.hasNext();

                    TreasureConfig.inst().getPlayerLootDetailAsync(uuidToGetListOf, treasureLocation, playerLootDetail_result -> {
                        playerLootDetails.add(playerLootDetail_result);

                        if (lastEntry){
                            // collect all messages to send at once
                            List<Component> components = new ArrayList<>();
                            // header
                            components.add(Lang.build(Lang.LIST_HEADER_PLAYER.get().replace(Lang.VALUE, uuidToGetListOf.toString())));

                            //build treasureInfo
                            for (PlayerLootDetail playerLootDetail: playerLootDetails){
                                components.add(Lang.build(Lang.LIST_PLAYER.get().
                                        replace(Lang.LOCATION, Lang.locationToString(treasureLocation)).
                                        replace(Lang.VALUE, playerLootDetail.unLootedStuff() == null ? Lang.LIST_NEVER.get() : DateFormat.getDateTimeInstance().format(new Date(playerLootDetail.lastLootedTimeStamp())))));
                            }

                            // send components
                            commandSender.sendMessage(Lang.join(components));
                        }
                    });
                }
            } else {
                commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
            }

        } else {
            if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_LIST_TREASURES)) {
                // collect all messages to send at once
                List<Component> components = new ArrayList<>();
                // header
                components.add(Lang.build(Lang.LIST_HEADER_TREASURES.get()));

                //build list
                for (Location treasureLocation : TreasureListener.inst().getTreasureLocations()){
                    TreasureInfo treasureInfo = TreasureListener.inst().getTreasure(treasureLocation);

                    String treasureInfoStr = Lang.LIST_TREASURE_BODY.get().
                            replace(Lang.LOCATION, Lang.locationToString(treasureLocation)).
                            replace(Lang.VALUE, String.valueOf(((double)treasureInfo.slotChance()) / 100.0d)).
                            replace(Lang.GLOBAL, treasureInfo.isGlobal() ? Lang.TRUE.get() : Lang.FALSE.get()).
                            replace(Lang.UNLIMITED, treasureInfo.isUnlimited() ? Lang.TRUE.get() : Lang.FALSE.get());

                    if (treasureInfo.timeUntilForget() > 0){
                        treasureInfoStr = treasureInfoStr + Lang.LIST_TREASURE_FORGETPERIOD.get().replace(Lang.VALUE, Lang.formatTimePeriod(treasureInfo.timeUntilForget()));
                    }

                    components.add(Lang.build(treasureInfoStr));
                }

                // send components
                commandSender.sendMessage(Lang.join(components));
            } else {
                commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
            }
        }
    }

    /**
     * let's the command sender open the treasure-inventory for another player
     * /gt peek (takes the commandSender self)
     * /gt peek playerName
     * /gt peek uuid
     * @param commandSender sender of this command
     * @param args given arguments
     */
    private void handlePeek(CommandSender commandSender, String[] args){
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_PEEK)) {
            if (commandSender instanceof Player player){
                Container container = getContainer(commandSender);

                if (container != null){
                    Location location = Utils.cleanLocation(container.getBlock().getLocation());
                    TreasureInfo treasureInfo = TreasureListener.inst().getTreasure(location);

                    if (treasureInfo != null){
                        if (treasureInfo.isGlobal()){
                            //load global treasure async
                            TreasureConfig.inst().getPlayerLootDetailAsync(null, location, playerLootDetail -> {
                                Inventory nowPeeking;
                                long timeStamp= playerLootDetail.lastLootedTimeStamp();

                                if ((playerLootDetail.unLootedStuff() == null || playerLootDetail.unLootedStuff().isEmpty())){
                                    nowPeeking = Bukkit.createInventory(null, container.getInventory().getType(), Lang.build(Lang.PEEK_PLAYER.get()));

                                    // clone every item stack and put it into the new inventory
                                    nowPeeking.setContents(TreasureListener.inst().getTreasure(location).itemLoot().stream().map(s -> s == null ? null : s.clone()).toArray(ItemStack[]::new));

                                    player.sendMessage(Lang.build(Lang.PEEK_WARNING.get()));
                                } else {
                                    nowPeeking = Bukkit.createInventory(null, container.getInventory().getType(), Lang.build(Lang.PEEK_PLAYER.get()));
                                    // get items left there last time
                                    nowPeeking.setContents(playerLootDetail.unLootedStuff().toArray(new ItemStack[0]));

                                    player.sendMessage(Lang.build(Lang.PEEK_WARNING.get()));
                                }

                                CommandInventoriesListener.inst().addPeekingTreasure(player.openInventory(nowPeeking), new PeekedTreasure(null, location, timeStamp));
                            });
                        return;
                        }

                        UUID uuidToPeek;
                        if (args.length >= 2){
                            OfflinePlayer offlinePlayer = Bukkit.getOfflinePlayer(args[1]);
                            if (offlinePlayer.hasPlayedBefore()){
                                uuidToPeek = offlinePlayer.getUniqueId();
                            } else {
                                try{
                                    uuidToPeek = UUID.fromString(args[1]);
                                }catch (IllegalArgumentException ignored){
                                    commandSender.sendMessage(Lang.build(Lang.NO_SUCH_PLAYER.get().replace(Lang.VALUE, args[1])));
                                    return;
                                }
                            }
                        } else {
                            uuidToPeek = player.getUniqueId();
                        }

                        TreasureConfig.inst().getPlayerLootDetailAsync(uuidToPeek, location, playerLootData -> {
                            Inventory nowEditing = Bukkit.createInventory(null, container.getInventory().getType(), Lang.build(Lang.PEEK_PLAYER.get().replace(Lang.VALUE, uuidToPeek.toString())));
                            // clone every item stack and put it into the new inventory
                            nowEditing.setContents(treasureInfo.itemLoot().stream().map(s -> s == null ? null : s.clone()).toArray(ItemStack[]::new));

                            CommandInventoriesListener.inst().addPeekingTreasure(player.openInventory(nowEditing), new PeekedTreasure(uuidToPeek, location, playerLootData.lastLootedTimeStamp()));
                            commandSender.sendMessage(Lang.build(Lang.PEEK_WARNING.get()));
                        });
                    } else {
                        commandSender.sendMessage(Lang.build(Lang.NO_TREASURE.get()));
                    }
                } else {
                    commandSender.sendMessage(Lang.build(Lang.NOT_LOOKINGAT_CONTAINER.get()));
                }
            } else {
                commandSender.sendMessage(Lang.build(Lang.NO_PLAYER.get()));
            }
        } else {
            commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
        }

    }

    /**
     * the treasure forgets a given somebody has ever looted it
     * /gt forget (takes the commandSender self)
     * /gt forget uuid
     * /gt forget playerName
     * @param commandSender sender of this command
     * @param args given arguments
     */
    private void handleForget(CommandSender commandSender, String[] args){
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_FORGET)) {
            Container container = getContainer(commandSender);

            if (container != null){
                TreasureInfo treasureInfo = TreasureListener.inst().getTreasure(container.getLocation());

                if (treasureInfo != null && treasureInfo.isGlobal()){
                    TreasureConfig.inst().forgetPlayerAsync(null, container.getBlock().getLocation());

                    commandSender.sendMessage(Lang.build(Lang.FORGET_ALL.get()));
                    return;
                }

                UUID uuidToForget;
                if (args.length >= 2){
                    OfflinePlayer offlinePlayer = Bukkit.getOfflinePlayer(args[1]);
                    if (offlinePlayer.hasPlayedBefore()){
                        uuidToForget = offlinePlayer.getUniqueId();
                    } else {
                        try{
                            uuidToForget = UUID.fromString(args[1]);
                        }catch (IllegalArgumentException ignored){
                            commandSender.sendMessage(Lang.build(Lang.NO_SUCH_PLAYER.get().replace(Lang.VALUE, args[1])));
                            return;
                        }
                    }
                } else {
                    if (commandSender instanceof LivingEntity livingEntity){
                        uuidToForget = livingEntity.getUniqueId();
                    } else {
                        commandSender.sendMessage(Lang.build(Lang.NO_PLAYER.get()));
                        return;
                    }
                }

                TreasureConfig.inst().forgetPlayerAsync(uuidToForget, container.getBlock().getLocation());
                commandSender.sendMessage(Lang.build(Lang.FORGET_PLAYER.get().replace(Lang.UUID, uuidToForget.toString())));
            } else {
                commandSender.sendMessage(Lang.build(Lang.NOT_LOOKINGAT_CONTAINER.get()));
            }
        } else {
            commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
        }
    }

    /**
     * the treasure forgets anybody has ever looted it.
     * /gt forget-all
     * @param commandSender sender of this command
     */
    private void handleForgetAll(CommandSender commandSender){
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_FORGET)) {
            Container container = getContainer(commandSender);

            if (container != null){
                TreasureConfig.inst().forgetAllAsync(container.getBlock().getLocation());

                commandSender.sendMessage(Lang.build(Lang.FORGET_ALL.get()));
            } else {
                commandSender.sendMessage(Lang.build(Lang.NOT_LOOKINGAT_CONTAINER.get()));
            }
        } else {
            commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
        }
    }

    /**
     * sets how probable it is for an inventory slot to be lootable
     * /gt set(0) random(1) <slotChance>(2)
     * @param commandSender sender of this command
     * @param args given arguments
     */
    private void handleSetRandom(CommandSender commandSender, String[] args) {
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_RANDOM)) {
            Container container = getContainer(commandSender);

            if (container != null){
                if (TreasureListener.inst().getTreasure(Utils.cleanLocation(container.getLocation())) != null){
                    if (args.length > 2){
                        if (Utils.isDouble(args[2])){
                            int slotPercentage = (int)(Double.parseDouble(args[2]) * 100);

                            slotPercentage = Math.max(Math.min(slotPercentage, 10000), 0);

                            TreasureConfig.inst().setRandomAsync(container.getLocation(), slotPercentage);

                            commandSender.sendMessage(Lang.build(Lang.SET_RANDOM.get().replace(Lang.VALUE, String.valueOf(((float)slotPercentage) / 100.0f))));
                        } else {
                            commandSender.sendMessage(Lang.build(Lang.NO_NUMBER.get().replace(Lang.VALUE, args[2])));
                        }
                    } else {
                        commandSender.sendMessage(Lang.build(Lang.NOT_ENOUGH_ARGS.get()));
                    }
                } else {
                    commandSender.sendMessage(Lang.build(Lang.NO_TREASURE.get()));
                }
            } else {
                commandSender.sendMessage(Lang.build(Lang.NOT_LOOKINGAT_CONTAINER.get()));
            }
        } else {
            commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
        }
    }

    /**
     * set's if the treasure is lootable an unlimited amount of times
     * /gt set(0) unlimited(1) <true/false>(2)
     * @param commandSender sender of this command
     * @param args given arguments
     */
    private void handleSetUnlimited(CommandSender commandSender, String[] args) {
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_UNLIMITED)) {
            Container container = getContainer(commandSender);

            if (container != null){
                if (TreasureListener.inst().getTreasure(Utils.cleanLocation(container.getLocation())) != null){
                    if (args.length > 2){
                        Boolean isUnLimited = BooleanUtils.toBooleanObject(args[2]);

                        if (isUnLimited != null){
                            TreasureConfig.inst().setUnlimitedAsync(container.getLocation(), isUnLimited);

                            commandSender.sendMessage(Lang.build(Lang.SET_UNLIMITED.get().replace(Lang.VALUE, String.valueOf(isUnLimited))));
                        } else {
                            commandSender.sendMessage(Lang.build(Lang.NO_BOOL.get().replace(Lang.VALUE, args[2])));
                        }
                    } else {
                        commandSender.sendMessage(Lang.build(Lang.NOT_ENOUGH_ARGS.get()));
                    }
                } else {
                    commandSender.sendMessage(Lang.build(Lang.NO_TREASURE.get()));
                }
            } else {
                commandSender.sendMessage(Lang.build(Lang.NOT_LOOKINGAT_CONTAINER.get()));
            }
        } else {
            commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
        }
    }

    /**
     * set's if all player share the same loot inventory or not
     * /gt set(0) global(1) <true/false>(2)
     * @param commandSender sender of this command
     * @param args given arguments
     */
    private void handleSetGlobal(CommandSender commandSender, String[] args) {
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_GLOBAL)) {
            Container container = getContainer(commandSender);

            if (container != null){
                if (TreasureListener.inst().getTreasure(Utils.cleanLocation(container.getLocation())) != null){
                    if (args.length > 2){
                        Boolean isUnLimited = BooleanUtils.toBooleanObject(args[2]);

                        if (isUnLimited != null){
                            TreasureConfig.inst().setGlobalAsync(container.getLocation(), isUnLimited);

                            commandSender.sendMessage(Lang.build(Lang.SET_GLOBAL.get().replace(Lang.VALUE, String.valueOf(isUnLimited))));
                        } else {
                            commandSender.sendMessage(Lang.build(Lang.NO_BOOL.get().replace(Lang.VALUE, args[2])));
                        }
                    } else {
                        commandSender.sendMessage(Lang.build(Lang.NOT_ENOUGH_ARGS.get()));
                    }
                } else {
                    commandSender.sendMessage(Lang.build(Lang.NO_TREASURE.get()));
                }
            } else {
                commandSender.sendMessage(Lang.build(Lang.NOT_LOOKINGAT_CONTAINER.get()));
            }
        } else {
            commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
        }
    }

    /**
     * set's the time period a treasure forgets it was ever opened
     * /gt set(0) forget(1) <1y 2w 3d 4h 5m 6s 7t 8ms>(2..)
     * @param commandSender sender of this command
     * @param args given arguments
     */
    private void handleSetForget(CommandSender commandSender, String[] args) {
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_SET_FORGET)) {
        Container container = getContainer(commandSender);

            if (container != null){
                if (TreasureListener.inst().getTreasure(Utils.cleanLocation(container.getLocation())) != null){
                    if (args.length > 2){
                        long forgetPeriod = -1L;

                        for (int i = 2; i < args.length; i++){
                            if (args[i].endsWith(SUFFIX_YEAR)){
                                String periodWithoutSuffix = args[i].replace(SUFFIX_YEAR, "");

                                if (Utils.isInt(periodWithoutSuffix)){
                                    forgetPeriod += Long.parseLong(periodWithoutSuffix) * Utils.YEAR_IN_MILLISECONDS;
                                } else {
                                    commandSender.sendMessage(Lang.build(Lang.NO_NUMBER.get().replace(Lang.VALUE, args[i])));
                                    return;
                                }
                            } else if (args[i].endsWith(SUFFIX_WEEK)) {
                                String periodWithoutSuffix = args[i].replace(SUFFIX_WEEK, "");

                                if (Utils.isInt(periodWithoutSuffix)){
                                    forgetPeriod += Long.parseLong(periodWithoutSuffix) * Utils.WEAK_IN_MILLISECONDS;
                                } else {
                                    commandSender.sendMessage(Lang.build(Lang.NO_NUMBER.get().replace(Lang.VALUE, args[i])));
                                    return;
                                }
                            } else if (args[i].endsWith(SUFFIX_DAY)) {
                                String periodWithoutSuffix = args[i].replace(SUFFIX_DAY, "");

                                if (Utils.isInt(periodWithoutSuffix)){
                                    forgetPeriod += Long.parseLong(periodWithoutSuffix) * Utils.DAY_IN_MILLISECONDS;
                                } else {
                                    commandSender.sendMessage(Lang.build(Lang.NO_NUMBER.get().replace(Lang.VALUE, args[i])));
                                    return;
                                }
                            } else if (args[i].endsWith(SUFFIX_HOUR)) {
                                String periodWithoutSuffix = args[i].replace(SUFFIX_HOUR, "");

                                if (Utils.isInt(periodWithoutSuffix)){
                                    forgetPeriod += Long.parseLong(periodWithoutSuffix) * Utils.HOUR_IN_MILLISECONDS;
                                } else {
                                    commandSender.sendMessage(Lang.build(Lang.NO_NUMBER.get().replace(Lang.VALUE, args[i])));
                                    return;
                                }
                            } else if (args[i].endsWith(SUFFIX_MINUTE)) {
                                String periodWithoutSuffix = args[i].replace(SUFFIX_MINUTE, "");

                                if (Utils.isInt(periodWithoutSuffix)){
                                    forgetPeriod += Long.parseLong(periodWithoutSuffix) * Utils.MINUTE_IN_MILLISECONDS;
                                } else {
                                    commandSender.sendMessage(Lang.build(Lang.NO_NUMBER.get().replace(Lang.VALUE, args[i])));
                                    return;
                                }
                            } else if (args[i].endsWith(SUFFIX_SECOND)) {
                                String periodWithoutSuffix = args[i].replace(SUFFIX_SECOND, "");

                                if (Utils.isInt(periodWithoutSuffix)){
                                    forgetPeriod += Long.parseLong(periodWithoutSuffix) * Utils.SECONDS_IN_MILLISECONDS;
                                } else {
                                    commandSender.sendMessage(Lang.build(Lang.NO_NUMBER.get().replace(Lang.VALUE, args[i])));
                                    return;
                                }
                            } else if (args[i].endsWith(SUFFIX_TICK)) {
                                String periodWithoutSuffix = args[i].replace(SUFFIX_TICK, "");

                                if (Utils.isInt(periodWithoutSuffix)){
                                    forgetPeriod += Long.parseLong(periodWithoutSuffix) * Utils.TICKS_IN_MILLISECONDS;
                                } else {
                                    commandSender.sendMessage(Lang.build(Lang.NO_NUMBER.get().replace(Lang.VALUE, args[i])));
                                    return;
                                }
                            } else if (args[i].endsWith(SUFFIX_MILLISECOND)) {
                                String periodWithoutSuffix = args[i].replace(SUFFIX_MILLISECOND, "");

                                if (Utils.isInt(periodWithoutSuffix)){
                                    forgetPeriod += Long.parseLong(periodWithoutSuffix);
                                } else {
                                    commandSender.sendMessage(Lang.build(Lang.NO_NUMBER.get().replace(Lang.VALUE, args[i])));
                                    return;
                                }
                            } else {
                                commandSender.sendMessage(Lang.build(Lang.UNKNOWN_ARGUMENT.get().replace(Lang.VALUE, args[i])));
                                return;
                            }
                        }

                        TreasureConfig.inst().setForgetAsync(container.getLocation(), forgetPeriod);

                        //negative values turn forget off
                        if (forgetPeriod < 0){
                            commandSender.sendMessage(Lang.build(Lang.REMOVE_FORGET.get()));
                        } else {
                            commandSender.sendMessage(Lang.build(Lang.SET_FORGET.get().replace(Lang.VALUE, Lang.formatTimePeriod(forgetPeriod))));
                        }
                    } else {
                        commandSender.sendMessage(Lang.build(Lang.NOT_ENOUGH_ARGS.get()));
                    }
                } else {
                    commandSender.sendMessage(Lang.build(Lang.NO_TREASURE.get()));
                }
            } else {
                commandSender.sendMessage(Lang.build(Lang.NOT_LOOKINGAT_CONTAINER.get()));
            }
        } else {
            commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
        }
    }

    /**
     * reloads the plugin
     * /gt reload
     * @param commandSender sender of this command
     */
    private void handleReload(CommandSender commandSender) {
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_RELOAD)) {
            TreasureConfig.inst().reloadMain();

            commandSender.sendMessage(Lang.build(Lang.RELOAD.get()));
        } else {
            commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
        }
    }

    /**
     * get basic informations about this plugin like its version
     * /gt about
     * @param commandSender sender of this command
     */
    private void handleAbout(CommandSender commandSender) {
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_ABOUT)) {
            // collect all messages to send at once
            List<Component> components = new ArrayList<>();
            // header
            components.add(Lang.build(Lang.PLUGIN_HEADER.get()));
            // version (update ?)
            components.add(Lang.build(Lang.PLUGIN_VERSION.get().replace(Lang.VALUE, GreenTreasure.inst().getDescription().getVersion())));

            //description
            components.add(Lang.build(Lang.PLUGIN_VERSION.get().replace(Lang.VALUE, GreenTreasure.inst().getDescription().getDescription())));

            // send components
            commandSender.sendMessage(Lang.join(components));
        } else {
            commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
        }
    }

    /**
     * get a help-text about this plugin and it's commands
     * @param commandSender sender of this command
     * @param args given arguments
     */
    private void handleHelp(CommandSender commandSender, String[] args) {
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_HELP)) {
            // collect all messages to send at once
            List<Component> components = new ArrayList<>();
            //wiki
            components.add(Lang.build(Lang.HELP_WIKI.get()));

            //todo explain commands

            // send components
            commandSender.sendMessage(Lang.join(components));
        } else {
            commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
        }
    }
}
