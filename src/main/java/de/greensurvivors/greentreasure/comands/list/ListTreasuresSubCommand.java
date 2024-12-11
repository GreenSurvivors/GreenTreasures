package de.greensurvivors.greentreasure.comands.list;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermmissionManager;
import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.comands.ASubCommand;
import de.greensurvivors.greentreasure.comands.ListSubCommand;
import de.greensurvivors.greentreasure.comands.TreasureCommands;
import de.greensurvivors.greentreasure.dataobjects.AListCmdHelper;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.MessageManager;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.event.ClickEvent;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
import org.bukkit.command.CommandSender;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class ListTreasuresSubCommand extends ASubCommand {

    public ListTreasuresSubCommand(@NotNull GreenTreasure plugin) {
        super(plugin);
    }

    @Override
    protected boolean checkPermission(@NotNull Permissible permissible) {
        return permissible.hasPermission(PermmissionManager.TREASURE_LIST_TREASURES.get());
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("treasures");
    }

    @Override
    @NotNull
    public Component getHelpText() {
        return null;
    }

    @Override
    public boolean onCommand(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        if (checkPermission(sender)) {
            plugin.getDatabaseManager().getTreasureIds().thenAccept(treasureIds -> {
                final int numOfTreasures = treasureIds.size();

                if (numOfTreasures > 0) {
                    final int numPages = (int) Math.ceil((double) numOfTreasures / (double) ListSubCommand.ENTRIES_PER_PAGE);

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

                    //maximum of treasures out of all this page can display
                    final int MAX_TREASURES_THIS_PAGE = Math.min(numOfTreasures, pageNow * ListSubCommand.ENTRIES_PER_PAGE);
                    //maximum of entries this page can display
                    final int NUM_ENTRIES = MAX_TREASURES_THIS_PAGE - (pageNow - 1) * ListSubCommand.ENTRIES_PER_PAGE;

                    final ListCmdTreasureHelper helper = new ListCmdTreasureHelper(plugin, sender, pageNow, numPages, NUM_ENTRIES);

                    //add the treasure info for the page
                    for (int num = (pageNow - 1) * ListSubCommand.ENTRIES_PER_PAGE; num < MAX_TREASURES_THIS_PAGE; num++) {
                        TreasureInfo treasureInfo = plugin.getTreasureManager().getTreasure(treasureIds.get(num));

                        helper.addEntry(treasureInfo, treasureIds.get(num));
                    }

                } else {
                    final @NotNull String cmd = TreasureCommands.CMD + " " + plugin.getTreasureCommands().getCreateSubCmd().getAliases().iterator().next();

                    plugin.getMessageManager().sendLang(sender, LangPath.CMD_LIST_TREASURES_EMPTY,
                        Placeholder.component(PlaceHolderKey.CMD.getKey(), Component.text(cmd).clickEvent(ClickEvent.runCommand(cmd))));
                }
            });
        } else {
            plugin.getMessageManager().sendLang(sender, LangPath.NO_PERMISSION);
        }

        return true;
    }

    @Override
    public @NotNull List<@NotNull String> onTabComplete(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        return new ArrayList<>();
    }

    public class ListCmdTreasureHelper extends AListCmdHelper {
        public ListCmdTreasureHelper(final @NotNull GreenTreasure plugin, final @NotNull CommandSender commandSender, final int pageNow, final int lastPage, final int numEntries) {
            super(plugin, commandSender, pageNow, lastPage, numEntries,
                TreasureCommands.CMD + " " + plugin.getTreasureCommands().getListSubCmd().getAliases().iterator().next() + " " + getAliases().iterator().next() + " "); //page will be added by super

            // header
            super.componentResult.add(plugin.getMessageManager().getLang(LangPath.CMD_LIST_TREASURES_HEADER,
                Placeholder.unparsed(PlaceHolderKey.NUMBER.getKey(), String.valueOf(pageNow)),
                Placeholder.unparsed(PlaceHolderKey.LAST_PAGE.getKey(), String.valueOf(lastPage))));
        }

        public void addEntry(final @NotNull TreasureInfo treasureInfo, final @NotNull String treasureId) {
            super.numOfEntriesStillToDo--;

            //build treasureInfo
            @NotNull Component treasureInfoComponent = plugin.getMessageManager().getLang(LangPath.CMD_LIST_TREASURE_BODY,
                Placeholder.unparsed(PlaceHolderKey.TREASURE_ID.getKey(), treasureId),
                Placeholder.unparsed(PlaceHolderKey.NUMBER.getKey(), String.valueOf(((double) treasureInfo.slotChance()) / 100.0d)),
                Placeholder.component(PlaceHolderKey.SHARED.getKey(), plugin.getMessageManager().getLang(treasureInfo.isShared() ? LangPath.BOOLEAN_TRUE : LangPath.BOOLEAN_FALSE)),
                Placeholder.component(PlaceHolderKey.UNLIMITED.getKey(), plugin.getMessageManager().getLang(treasureInfo.isUnlimited() ? LangPath.BOOLEAN_TRUE : LangPath.BOOLEAN_FALSE))
            );

            if (treasureInfo.timeUntilForget().isPositive()) {
                treasureInfoComponent = treasureInfoComponent.appendSpace().append(plugin.getMessageManager().getLang(LangPath.CMD_LIST_TREASURE_FORGETPERIOD,
                    Placeholder.component(PlaceHolderKey.TIME.getKey(), MessageManager.formatTime(treasureInfo.timeUntilForget()))));
            }

            super.componentResult.add(treasureInfoComponent);

            if (super.numOfEntriesStillToDo <= 0) {
                sendMessage();
            }
        }
    }
}
