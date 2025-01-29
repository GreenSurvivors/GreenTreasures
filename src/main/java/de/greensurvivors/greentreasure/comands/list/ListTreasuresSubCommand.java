package de.greensurvivors.greentreasure.comands.list;

import com.github.f4b6a3.ulid.Ulid;
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
import net.kyori.adventure.text.event.ClickEvent;
import net.kyori.adventure.text.minimessage.tag.resolver.Formatter;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
import org.bukkit.command.CommandSender;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class ListTreasuresSubCommand extends ASubCommand {

    public ListTreasuresSubCommand(@NotNull GreenTreasure plugin) {
        super(plugin);
    }

    @Override
    protected boolean checkPermission(@NotNull Permissible permissible) {
        return permissible.hasPermission(PermissionManager.TREASURE_LIST_TREASURES.get());
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
                        final @Nullable TreasureInfo treasureInfo = plugin.getTreasureManager().getTreasureInfo(treasureIds.get(num));

                        if (treasureInfo == null) {
                            plugin.getComponentLogger().debug("skipped listing treasure with id {}, because it was unknown. Probably removed by another thread.", treasureIds.get(num));
                            continue;
                        }

                        helper.addEntry(treasureInfo, treasureIds.get(num));
                    }

                } else {
                    final @NotNull String cmd = MainCommand.CMD + " " + plugin.getMainCommand().getCreateSubCmd().getAliases().iterator().next();

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
                MainCommand.CMD + " " + plugin.getMainCommand().getListSubCmd().getAliases().iterator().next() + " " + getAliases().iterator().next() + " "); //page will be added by super

            // header
            super.componentResult.add(plugin.getMessageManager().getLang(LangPath.CMD_LIST_TREASURES_HEADER,
                Formatter.number(PlaceHolderKey.NUMBER.getKey(), pageNow),
                Formatter.number(PlaceHolderKey.LAST_PAGE.getKey(), lastPage)));
        }

        public void addEntry(final @NotNull TreasureInfo treasureInfo, final @NotNull Ulid treasureId) {
            super.numOfEntriesStillToDo--;

            //build treasureInfo
            @NotNull Component treasureInfoComponent = plugin.getMessageManager().getLang(LangPath.CMD_LIST_TREASURES_BODY,
                Placeholder.unparsed(PlaceHolderKey.TREASURE_ID.getKey(), treasureId.toString()),
                Formatter.number(PlaceHolderKey.NUMBER.getKey(), ((double) treasureInfo.slotChance()) / 100.0d),
                Formatter.booleanChoice(PlaceHolderKey.SHARED.getKey(), treasureInfo.isShared()),
                Formatter.booleanChoice(PlaceHolderKey.UNLIMITED.getKey(), treasureInfo.isUnlimited())
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
