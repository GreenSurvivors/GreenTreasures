package de.greensurvivors.greentreasure.comands.list;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermissionManager;
import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.comands.ASubCommand;
import de.greensurvivors.greentreasure.comands.ListSubCommand;
import de.greensurvivors.greentreasure.comands.MainCommand;
import de.greensurvivors.greentreasure.dataobjects.AListCmdHelper;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.MessageManager;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.JoinConfiguration;
import net.kyori.adventure.text.event.ClickEvent;
import net.kyori.adventure.text.minimessage.tag.resolver.Formatter;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
import org.bukkit.Location;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Entity;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;

import java.util.*;

public class ListNearbyCommand extends ASubCommand {
    public ListNearbyCommand(@NotNull GreenTreasure plugin) {
        super(plugin);
    }

    @Override
    protected boolean checkPermission(@NotNull Permissible permissible) {
        return permissible.hasPermission(PermissionManager.TREASURE_LIST_NEAR.get());
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("near", "nearby");
    }

    @Override
    public @NotNull Component getHelpText() {
        return null;
    }

    @Override
    public boolean onCommand(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        if (checkPermission(sender)) {
            if (sender instanceof Entity entity) {
                if (args.length >= 3) {
                    if (Utils.isInt(args[2])) {

                        if (args.length >= 4 && !Utils.isInt(args[3])) {
                            plugin.getMessageManager().sendLang(sender, LangPath.ARG_NOT_A_NUMBER,
                                Placeholder.unparsed(PlaceHolderKey.TEXT.getKey(), args[3]));
                            return false;
                        }

                        plugin.getTreasureManager().getNearTreasures(entity.getLocation(), Integer.parseInt(args[2])).
                            thenAccept(nearTreasures -> {
                                final int numOfTreasures = nearTreasures.size();

                                if (numOfTreasures > 0) {
                                    final int numPages = (int) Math.ceil((double) numOfTreasures / (double) ListSubCommand.ENTRIES_PER_PAGE);

                                    final int pageNow; //please note: we are start counting with page 1, not 0 for convenience of users of this plugin
                                    if (args.length >= 4) {
                                        if (Utils.isInt(args[3])) {
                                            //limit page to how many exits
                                            pageNow = Math.max(1, Math.min(numPages, Integer.parseInt(args[3])));
                                        } else {
                                            // this case will never happen, as we have sorted it out above
                                            return;
                                        }
                                    } else {
                                        pageNow = 1;
                                    }

                                    //maximum of treasures out of all this page can display
                                    final int MAX_TREASURES_THIS_PAGE = Math.min(numOfTreasures, pageNow * ListSubCommand.ENTRIES_PER_PAGE);
                                    //maximum of entries this page can display
                                    final int NUM_ENTRIES = MAX_TREASURES_THIS_PAGE - (pageNow - 1) * ListSubCommand.ENTRIES_PER_PAGE;

                                    final ListCmdNearTreasuresHelper helper = new ListCmdNearTreasuresHelper(plugin, sender, pageNow, numPages, NUM_ENTRIES);

                                    //add the treasure info for the page
                                    int num = (pageNow - 1) * ListSubCommand.ENTRIES_PER_PAGE;
                                    Iterator<Map.Entry<@NotNull TreasureInfo, @NotNull SortedSet<@NotNull Location>>> iterator = nearTreasures.sequencedEntrySet().iterator();

                                    // skip entries before this index
                                    if (num > 0) {
                                        for (int i = 0; i < num && iterator.hasNext(); i++) {
                                            iterator.next();
                                        }
                                    }

                                    while (num < MAX_TREASURES_THIS_PAGE && iterator.hasNext()) {
                                        helper.addEntry(iterator.next());
                                        num++;
                                    }
                                } else {
                                    final @NotNull String cmd = MainCommand.CMD + " " + plugin.getMainCommand().getCreateSubCmd().getAliases().iterator().next();

                                    plugin.getMessageManager().sendLang(sender, LangPath.CMD_LIST_NEARBY_TREASURES_EMPTY,
                                        Placeholder.component(PlaceHolderKey.CMD.getKey(), Component.text(cmd).clickEvent(ClickEvent.runCommand(cmd))));
                                }
                            });
                    } else {
                        plugin.getMessageManager().sendLang(sender, LangPath.ARG_NOT_A_NUMBER,
                            Placeholder.unparsed(PlaceHolderKey.TEXT.getKey(), args[2]));
                        return false;
                    }
                } else {
                    plugin.getMessageManager().sendLang(sender, LangPath.ERROR_NOT_ENOUGH_ARGS);
                    return false;
                }
            } else {
                plugin.getMessageManager().sendLang(sender, LangPath.ERROR_SENDER_NOT_PLAYER);
            }
        }

        return true;
    }

    @Override
    public @NotNull List<@NotNull String> onTabComplete(@NotNull CommandSender sender, @NotNull String @NotNull [] args) { // todo suggest a distance
        return List.of();
    }

    private class ListCmdNearTreasuresHelper extends AListCmdHelper {

        public ListCmdNearTreasuresHelper(final @NotNull GreenTreasure plugin, final CommandSender commandSender, final int pageNow, final int lastPage, final int numEntries) {
            super(plugin, commandSender, pageNow, lastPage, numEntries,
                //page will be added by super
                MainCommand.CMD + " " + plugin.getMainCommand().getListSubCmd().getAliases().iterator().next() + " " + getAliases().iterator().next() + " ");

            // header
            super.componentResult.add(plugin.getMessageManager().getLang(LangPath.CMD_LIST_NEARBY_TREASURES_HEADER,
                Formatter.number(PlaceHolderKey.NUMBER.getKey(), pageNow),
                Formatter.number(PlaceHolderKey.LAST_PAGE.getKey(), lastPage)));
        }

        public void addEntry(final Map.Entry<TreasureInfo, SortedSet<Location>> entry) {
            synchronized (super.MUTEX) {
                super.numOfEntriesStillToDo--;

                //build treasureInfo
                @NotNull Component treasureInfoComponent = plugin.getMessageManager().getLang(LangPath.CMD_LIST_NEARBY_TREASURES_BODY,
                    Placeholder.unparsed(PlaceHolderKey.TREASURE_ID.getKey(), entry.getKey().treasureId().toString()),
                    Formatter.number(PlaceHolderKey.NUMBER.getKey(), ((double) entry.getKey().slotChance()) / 100.0d),
                    Formatter.booleanChoice(PlaceHolderKey.SHARED.getKey(), entry.getKey().isShared()),
                    Formatter.booleanChoice(PlaceHolderKey.UNLIMITED.getKey(), entry.getKey().isUnlimited()),
                    Placeholder.component(PlaceHolderKey.LOCATION.getKey(),
                        Component.join(JoinConfiguration.commas(true),
                            entry.getValue().stream().map(location ->
                                plugin.getMessageManager().formatLocation(location).
                                    clickEvent(ClickEvent.suggestCommand("/tp " + location.getX() + " " + location.getY() + " " + location.getZ()))
                            ).toList()))
                );

                if (entry.getKey().timeUntilForget().isPositive()) {
                    treasureInfoComponent = treasureInfoComponent.appendSpace().append(plugin.getMessageManager().getLang(LangPath.CMD_LIST_TREASURE_FORGETPERIOD,
                        Placeholder.component(PlaceHolderKey.TIME.getKey(), MessageManager.formatTime(entry.getKey().timeUntilForget()))));
                }

                super.componentResult.add(treasureInfoComponent);

                if (super.numOfEntriesStillToDo <= 0) {
                    sendMessage();
                }
            }
        }
    }
}
