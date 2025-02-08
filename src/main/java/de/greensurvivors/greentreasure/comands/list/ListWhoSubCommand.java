package de.greensurvivors.greentreasure.comands.list;

import com.github.f4b6a3.ulid.Ulid;
import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermissionManager;
import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.comands.ASubCommand;
import de.greensurvivors.greentreasure.comands.ListSubCommand;
import de.greensurvivors.greentreasure.comands.MainCommand;
import de.greensurvivors.greentreasure.dataobjects.AListCmdHelper;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.resolver.Formatter;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.text.DateFormat;
import java.util.*;

public class ListWhoSubCommand extends ASubCommand {

    public ListWhoSubCommand(@NotNull GreenTreasure plugin) {
        super(plugin);
    }

    @Override
    protected boolean checkPermission(@NotNull Permissible permissible) {
        return permissible.hasPermission(PermissionManager.TREASURE_LIST_WHO.get());
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("who");
    }

    @Override
    @NotNull
    public Component getHelpText() {
        return null;
    }

    // todo alternative to looking at a chest accept a ulid
    @Override
    public boolean onCommand(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        if (checkPermission(sender)) {
            Container container = plugin.getMainCommand().getContainer(sender);

            if (container != null) {
                final @Nullable TreasureInfo treasureInfo = plugin.getTreasureManager().getTreasureInfo(container);

                if (treasureInfo != null) {
                    plugin.getDatabaseManager().getAllPlayerData(treasureInfo.treasureId()).thenAccept(playerLootDetailMap -> {
                        final int numOfPlayers = playerLootDetailMap.size();

                        if (numOfPlayers > 0) {
                            final int numPages = (int) Math.ceil((double) numOfPlayers / (double) ListSubCommand.ENTRIES_PER_PAGE);

                            final int pageNow; //please note: we are start counting with page 1, not 0 for convenience of users of this plugin
                            if (args.length >= 3) {
                                if (Utils.isInt(args[2])) {
                                    //limit page to how many exits
                                    pageNow = Math.max(1, Math.min(numPages, Integer.parseInt(args[2])));
                                } else {
                                    plugin.getMessageManager().sendLang(sender, LangPath.ARG_NOT_A_NUMBER,
                                        Placeholder.unparsed(PlaceHolderKey.TEXT.getKey(), args[2]));
                                    return;
                                }
                            } else {
                                pageNow = 1;
                            }

                            //maximum of players out of all this page can display
                            final int MAX_PLAYERS_THIS_PAGE = Math.min(numOfPlayers, pageNow * ListSubCommand.ENTRIES_PER_PAGE);
                            //maximum of entries this page can display
                            final int NUM_ENTRIES = MAX_PLAYERS_THIS_PAGE - (pageNow - 1) * ListSubCommand.ENTRIES_PER_PAGE;

                            final ListCmdWhoHelper helper = new ListCmdWhoHelper(plugin, sender, pageNow, numPages, NUM_ENTRIES, treasureInfo.treasureId());

                            final List<UUID> uuids = new ArrayList<>(playerLootDetailMap.keySet());
                            //add the player info for the page
                            for (int num = (pageNow - 1) * ListSubCommand.ENTRIES_PER_PAGE; num < MAX_PLAYERS_THIS_PAGE; num++) {
                                helper.addEntry(uuids.get(num), playerLootDetailMap.get(uuids.get(num)));
                            }

                        } else {
                            plugin.getMessageManager().sendLang(sender, LangPath.CMD_LIST_WHO_EMPTY);
                        }
                    });
                } else {
                    plugin.getMessageManager().sendLang(sender, LangPath.ERROR_NOT_LOOKING_AT_TREASURE);
                }
            } else {
                plugin.getMessageManager().sendLang(sender, LangPath.ERROR_NOT_LOOKING_AT_CONTAINER);
            }
        } else {
            plugin.getMessageManager().sendLang(sender, LangPath.NO_PERMISSION);
        }

        return true;
    }

    @Override
    public @NotNull List<@NotNull String> onTabComplete(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        return List.of(); //nothing, since loading how many pages there are would hold back all the other completions
    }

    public class ListCmdWhoHelper extends AListCmdHelper {
        public ListCmdWhoHelper(final @NotNull GreenTreasure plugin, final @NotNull CommandSender commandSender,
                                final int pageNow, final int lastPage, final int numEntries,
                                final @NotNull Ulid treasureId) {
            super(plugin, commandSender, pageNow, lastPage, numEntries,
                //page will be added by super
                MainCommand.CMD + " " + plugin.getMainCommand().getListSubCmd().getAliases().iterator().next() + " " + getAliases().iterator().next() + " ");

            // header
            super.componentResult.add(plugin.getMessageManager().getLang(LangPath.CMD_LIST_WHO_HEADER,
                Placeholder.unparsed(PlaceHolderKey.TREASURE_ID.getKey(), treasureId.toString()),
                Formatter.number(PlaceHolderKey.NUMBER.getKey(),pageNow),
                Formatter.number(PlaceHolderKey.LAST_PAGE.getKey(), lastPage)
            ));
        }

        public void addEntry(final @NotNull UUID uuid, final @NotNull PlayerLootDetail playerLootDetail) {
            super.numOfEntriesStillToDo--;

            @Nullable Component who = null;
            final @Nullable Player onlinePlayer = Bukkit.getPlayer(uuid);
            if (onlinePlayer != null) {
                who = onlinePlayer.displayName();
            } else {
                final @NotNull OfflinePlayer offlinePlayer = Bukkit.getOfflinePlayer(uuid);
                final @Nullable String name = offlinePlayer.getName();

                if (name != null) {
                    who = Component.text(name);
                }
            }

            super.componentResult.add(plugin.getMessageManager().getLang(LangPath.CMD_LIST_WHO_BODY,
                Placeholder.component(PlaceHolderKey.PLAYER.getKey(), who == null ? plugin.getMessageManager().getLang(LangPath.CMD_LIST_WHO_SHARED) : who),
                Placeholder.component(PlaceHolderKey.TIME.getKey(),
                    (playerLootDetail.unLootedStuff() == null) ?
                        plugin.getMessageManager().getLang(LangPath.CMD_LIST_PLAYER_NEVER) :
                        Component.text(DateFormat.getDateTimeInstance().format(new Date(playerLootDetail.lastChangedTimeStamp())))
                )
            ));

            if (super.numOfEntriesStillToDo <= 0) {
                sendMessage();
            }
        }
    }
}
