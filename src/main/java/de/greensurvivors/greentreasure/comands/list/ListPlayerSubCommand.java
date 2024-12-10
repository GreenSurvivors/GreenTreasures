package de.greensurvivors.greentreasure.comands.list;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.comands.ASubCommand;
import de.greensurvivors.greentreasure.comands.ListSubCommand;
import de.greensurvivors.greentreasure.comands.TreasureCommands;
import de.greensurvivors.greentreasure.dataobjects.AListCmdHelper;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import de.greensurvivors.greentreasure.permission.PermmissionManager;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.event.ClickEvent;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;

import java.text.DateFormat;
import java.util.*;
import java.util.stream.IntStream;

public class ListPlayerSubCommand extends ASubCommand {

    public ListPlayerSubCommand(@NotNull GreenTreasure plugin) {
        super(plugin);
    }

    @Override
    protected boolean checkPermission(@NotNull Permissible permissible) {
        return permissible.hasPermission(PermmissionManager.TREASURE_LIST_PLAYERS.get());
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("player");
    }

    @Override
    @NotNull
    public Component getHelpText() {
        return null;
    }

    @Override
    public boolean onCommand(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        if (checkPermission(sender)) {
            if (args.length >= 3) {
                final @NotNull UUID uuidToGetListOf;

                OfflinePlayer offlinePlayer = Bukkit.getOfflinePlayer(args[2]);
                if (offlinePlayer.hasPlayedBefore()) {
                    uuidToGetListOf = offlinePlayer.getUniqueId();
                } else {
                    try {
                        uuidToGetListOf = UUID.fromString(args[2]);
                    } catch (IllegalArgumentException ignored) {
                        plugin.getMessageManager().sendLang(sender, LangPath.ARG_NOT_PLAYER,
                            Placeholder.unparsed(PlaceHolderKey.TEXT.getKey(), args[2]));
                        return false;
                    }
                }

                final @NotNull List<@NotNull String> treasureIds = plugin.getTreasureListener().getTreasureLocations();
                final int numOfTreasures = treasureIds.size();

                if (numOfTreasures > 0) {
                    final int numPages = (int) Math.ceil((double) numOfTreasures / (double) ListSubCommand.ENTRIES_PER_PAGE);

                    final int pageNow; //please note: we are start counting with page 1, not 0 for convenience of users of this plugin
                    if (args.length >= 4) {
                        if (Utils.isInt(args[3])) {
                            //limit page to how many exits
                            pageNow = Math.max(1, Math.min(numPages, Integer.parseInt(args[3])));
                        } else {
                            plugin.getMessageManager().sendLang(sender, LangPath.ARG_NOT_A_NUMBER,
                                Placeholder.unparsed(PlaceHolderKey.TEXT.getKey(), args[3]));
                            return false;
                        }
                    } else {
                        pageNow = 1;
                    }

                    //maximum of treasures out of all this page can display
                    final int MAX_TREASURES_THIS_PAGE = Math.min(numOfTreasures, pageNow * ListSubCommand.ENTRIES_PER_PAGE);
                    //maximum of entries this page can display
                    final int NUM_ENTRIES = MAX_TREASURES_THIS_PAGE - (pageNow - 1) * ListSubCommand.ENTRIES_PER_PAGE;

                    final ListCmdPlayerDetailHelper helper = new ListCmdPlayerDetailHelper(plugin, sender, pageNow, numPages, NUM_ENTRIES, uuidToGetListOf);

                    //add the players detail for the page
                    for (int num = (pageNow - 1) * ListSubCommand.ENTRIES_PER_PAGE; num < MAX_TREASURES_THIS_PAGE; num++) {
                        helper.addEntry(treasureIds.get(num));
                    }
                } else {
                    final @NotNull String cmd = TreasureCommands.CMD + " " + plugin.getTreasureCommands().getCreateSubCmd().getAliases().iterator().next();

                    plugin.getMessageManager().sendLang(sender, LangPath.CMD_LIST_PLAYER_EMPTY,
                        Placeholder.component(PlaceHolderKey.CMD.getKey(), Component.text(cmd).clickEvent(ClickEvent.runCommand(cmd))));
                }
            } else {
                plugin.getMessageManager().sendLang(sender, LangPath.ERROR_NOT_ENOUGH_ARGS);
                return false;
            }
        } else {
            plugin.getMessageManager().sendLang(sender, LangPath.NO_PERMISSION);
        }

        return true;
    }

    @Override
    public @NotNull List<@NotNull String> onTabComplete(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        if (args.length == 3) {
            List<String> result = new ArrayList<>();

            Collection<? extends Player> onlinePlayers = Bukkit.getOnlinePlayers();
            if (!onlinePlayers.isEmpty()) {
                result.addAll(onlinePlayers.stream().map(Player::getName).toList());
            }

            return result;
        } else if (args.length == 4) {
            final int numOfTreasures = plugin.getTreasureListener().getTreasureLocations().size();
            if (numOfTreasures > 0) {
                return IntStream.rangeClosed(1, (int) Math.ceil((double) numOfTreasures / (double) ListSubCommand.ENTRIES_PER_PAGE))
                    .boxed()
                    .map(String::valueOf)
                    .toList();
            }
        }

        return new ArrayList<>();
    }

    private class ListCmdPlayerDetailHelper extends AListCmdHelper {
        private final @NotNull UUID uuidToGetListOf;

        public ListCmdPlayerDetailHelper(final @NotNull GreenTreasure plugin, final CommandSender commandSender, final int pageNow, final int lastPage, final int numEntries, final @NotNull UUID uuidToGetListOf) {
            super(plugin, commandSender, pageNow, lastPage, numEntries,
                //page will be added by super
                TreasureCommands.CMD + " " + plugin.getTreasureCommands().getListSubCmd().getAliases().iterator().next() + " " + getAliases().iterator().next() + " " + uuidToGetListOf + " ");

            this.uuidToGetListOf = uuidToGetListOf;

            final @NotNull Component playerDisplay;
            final OfflinePlayer offlinePlayer = Bukkit.getOfflinePlayer(uuidToGetListOf);
            if (offlinePlayer.isOnline()) {
                playerDisplay = offlinePlayer.getPlayer().displayName();
            } else if (offlinePlayer.getName() != null) {
                playerDisplay = Component.text(offlinePlayer.getName());
            } else {
                playerDisplay = Component.text(uuidToGetListOf.toString());
            }

            // header
            super.componentResult.add(plugin.getMessageManager().getLang(LangPath.CMD_LIST_PLAYER_HEADER,
                Placeholder.component(PlaceHolderKey.PLAYER.getKey(), playerDisplay),
                Placeholder.unparsed(PlaceHolderKey.NUMBER.getKey(), String.valueOf(pageNow)),
                Placeholder.unparsed(PlaceHolderKey.LAST_PAGE.getKey(), String.valueOf(lastPage))));
        }

        public void addEntry(final String treasureId) {
            plugin.getConfigHandler().getPlayerLootDetail(Bukkit.getOfflinePlayer(uuidToGetListOf), treasureId).thenAccept(playerLootDetail_result -> {
                synchronized (super.MUTEX) {
                    super.numOfEntriesStillToDo--;

                    //build treasureInfo
                    super.componentResult.add(plugin.getMessageManager().getLang(LangPath.CMD_LIST_PLAYER_BODY,
                        Placeholder.unparsed(PlaceHolderKey.TREASURE_ID.getKey(), treasureId),
                        Placeholder.component(PlaceHolderKey.TIME.getKey(),
                            (playerLootDetail_result == null || playerLootDetail_result.unLootedStuff() == null) ?
                                plugin.getMessageManager().getLang(LangPath.CMD_LIST_PLAYER_NEVER) :
                                Component.text(DateFormat.getDateTimeInstance().format(new Date(playerLootDetail_result.lastChangedTimeStamp()))))));

                    if (super.numOfEntriesStillToDo <= 0) {
                        sendMessage();
                    }
                }
            });
        }
    }
}
