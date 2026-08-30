package de.greensurvivors.greentreasure.comands.list;

import com.github.f4b6a3.ulid.Ulid;
import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermissionManager;
import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.comands.ASubCommand;
import de.greensurvivors.greentreasure.comands.ListSubCommand;
import de.greensurvivors.greentreasure.comands.MainCommand;
import de.greensurvivors.greentreasure.dataobjects.AListCmdHelper;
import de.greensurvivors.greentreasure.dataobjects.DynamicPlayerAudience;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import de.greensurvivors.greentreasure.language.LangKey;
import de.greensurvivors.greentreasure.language.PlaceHolder;
import net.kyori.adventure.audience.Audience;
import net.kyori.adventure.text.Component;
import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.UUID;

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
    public @NotNull Component getHelpText() {
        return null;
    }

    // todo alternative to looking at a chest accept a ulid
    @Override
    public boolean onCommand(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        if (checkPermission(sender)) {
            Container container = plugin.getMainCommand().getContainer(sender);

            if (container != null) {
                final @NotNull Audience audience = DynamicPlayerAudience.fromAudience(sender);

                plugin.getTreasureManager().getTreasureInfo(container).thenAccept(treasureInfo -> {
                    if (treasureInfo != null) {
                        plugin.getDatabaseManager().getAllPlayerData(treasureInfo.treasureId()).thenAccept(playerLootDetailMap -> {
                            final int numOfPlayers = playerLootDetailMap.size();

                            if (numOfPlayers > 0) {
                                final int numPages = (int) Math.ceil((double) numOfPlayers / (double) ListSubCommand.ENTRIES_PER_PAGE);

                                final int pageNow; //please note: we are start counting with page 1, not 0 for convenience of users of this plugin
                                if (args.length >= 3) {
                                    if (Utils.isInt(args[2])) {
                                        //limit page to how many exits
                                        pageNow = Math.clamp(Integer.parseInt(args[2]), 1, numPages);
                                    } else {
                                        plugin.getMessageManager().sendPrefixed(audience, LangKey.ARG_NOT_A_NUMBER.create(
                                            PlaceHolder.TEXT.string(args[2])));
                                        return;
                                    }
                                } else {
                                    pageNow = 1;
                                }

                                //maximum of players out of all this page can display
                                final int MAX_PLAYERS_THIS_PAGE = Math.min(numOfPlayers, pageNow * ListSubCommand.ENTRIES_PER_PAGE);
                                //maximum of entries this page can display
                                final int NUM_ENTRIES = MAX_PLAYERS_THIS_PAGE - (pageNow - 1) * ListSubCommand.ENTRIES_PER_PAGE;

                                final ListCmdWhoHelper helper = new ListCmdWhoHelper(plugin, audience, pageNow, numPages, NUM_ENTRIES, treasureInfo.treasureId());

                                final List<UUID> uuids = new ArrayList<>(playerLootDetailMap.keySet());
                                //add the player info for the page
                                for (int num = (pageNow - 1) * ListSubCommand.ENTRIES_PER_PAGE; num < MAX_PLAYERS_THIS_PAGE; num++) {
                                    helper.addEntry(uuids.get(num), playerLootDetailMap.get(uuids.get(num)));
                                }

                            } else {
                                plugin.getMessageManager().sendPrefixed(audience, LangKey.CMD_LIST_WHO_EMPTY);
                            }
                        });
                    } else {
                        plugin.getMessageManager().sendPrefixed(audience, LangKey.ERROR_NOT_LOOKING_AT_TREASURE);
                    }
                });
            } else {
                plugin.getMessageManager().sendPrefixed(sender, LangKey.ERROR_NOT_LOOKING_AT_CONTAINER);
            }
        } else {
            plugin.getMessageManager().sendPrefixed(sender, LangKey.NO_PERMISSION);
        }

        return true;
    }

    @Override
    public @NotNull List<@NotNull String> onTabComplete(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        return List.of(); //nothing, since loading how many pages there are would hold back all the other completions
    }

    public class ListCmdWhoHelper extends AListCmdHelper {
        public ListCmdWhoHelper(final @NotNull GreenTreasure plugin, final @NotNull Audience audience,
                                final int pageNow, final int lastPage, final int numEntries,
                                final @NotNull Ulid treasureId) {
            super(plugin, audience, pageNow, lastPage, numEntries,
                //page will be added by super
                MainCommand.CMD + " " + plugin.getMainCommand().getListSubCmd().getAliases().iterator().next() + " " + getAliases().iterator().next() + " ");

            // header
            componentResult.add(LangKey.CMD_LIST_WHO_HEADER.create(
                PlaceHolder.TREASURE_ID.string(treasureId.toString()),
                PlaceHolder.NUMBER.numeric(pageNow),
                PlaceHolder.LAST_PAGE.numeric(lastPage)
            ));
        }

        public void addEntry(final @NotNull UUID uuid, final @NotNull PlayerLootDetail playerLootDetail) {
            numOfEntriesStillToDo--;

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

            componentResult.add(LangKey.CMD_LIST_WHO_BODY.create(
                PlaceHolder.PLAYER.component(who == null ? LangKey.CMD_LIST_WHO_SHARED.create() : who),
                (playerLootDetail.unLootedStuff() == null) ?
                    PlaceHolder.TIME.component(LangKey.CMD_LIST_PLAYER_NEVER.create()) :
                    PlaceHolder.TIME.temporal(playerLootDetail.lastChangedInstant())
            ));

            if (numOfEntriesStillToDo <= 0) {
                sendMessage();
            }
        }
    }
}
