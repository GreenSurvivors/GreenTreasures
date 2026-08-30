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
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.language.LangKey;
import de.greensurvivors.greentreasure.language.PlaceHolder;
import net.kyori.adventure.audience.Audience;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.TextComponent;
import net.kyori.adventure.text.event.ClickEvent;
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
        return permissible.hasPermission(PermissionManager.TREASURE_LIST_TREASURES.get());
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("treasures");
    }

    @Override
    public @NotNull Component getHelpText() {
        return null;
    }

    @Override
    public boolean onCommand(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        if (checkPermission(sender)) {
            final Audience audience = DynamicPlayerAudience.fromAudience(sender);
            plugin.getDatabaseManager().getTreasureIds().thenAccept(treasureIds -> {
                final int numOfTreasures = treasureIds.size();

                if (numOfTreasures > 0) {
                    final int numPages = (int) Math.ceil((double) numOfTreasures / (double) ListSubCommand.ENTRIES_PER_PAGE);

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

                    //maximum of treasures out of all this page can display
                    final int MAX_TREASURES_THIS_PAGE = Math.min(numOfTreasures, pageNow * ListSubCommand.ENTRIES_PER_PAGE);
                    //maximum of entries this page can display
                    final int NUM_ENTRIES = MAX_TREASURES_THIS_PAGE - (pageNow - 1) * ListSubCommand.ENTRIES_PER_PAGE;

                    final ListCmdTreasureHelper helper = new ListCmdTreasureHelper(plugin, audience, pageNow, numPages, NUM_ENTRIES);

                    //add the treasure info for the page
                    for (int num = (pageNow - 1) * ListSubCommand.ENTRIES_PER_PAGE; num < MAX_TREASURES_THIS_PAGE; num++) {
                        final int finalNum = num;

                        plugin.getTreasureManager().getTreasureInfo(treasureIds.get(num)).thenAccept(treasureInfo -> {
                            if (treasureInfo == null) {
                                plugin.getComponentLogger().debug("skipped listing treasure with id {}, because it was unknown. Probably removed by another thread.", treasureIds.get(finalNum));
                                return;
                            }

                            helper.addEntry(treasureInfo, treasureIds.get(finalNum));
                        });
                    }

                } else {
                    final @NotNull String cmd = MainCommand.CMD + " " + plugin.getMainCommand().getCreateSubCmd().getAliases().iterator().next();

                    plugin.getMessageManager().sendPrefixed(audience, LangKey.CMD_LIST_TREASURES_EMPTY.create(
                        PlaceHolder.CMD.component(Component.text(cmd).clickEvent(ClickEvent.runCommand(cmd)))));
                }
            });
        } else {
            plugin.getMessageManager().sendPrefixed(sender, LangKey.NO_PERMISSION);
        }

        return true;
    }

    @Override
    public @NotNull List<@NotNull String> onTabComplete(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        return new ArrayList<>();
    }

    public class ListCmdTreasureHelper extends AListCmdHelper {
        public ListCmdTreasureHelper(final @NotNull GreenTreasure plugin, final @NotNull Audience audience, final int pageNow, final int lastPage, final int numEntries) {
            super(plugin, audience, pageNow, lastPage, numEntries,
                MainCommand.CMD + " " + plugin.getMainCommand().getListSubCmd().getAliases().iterator().next() + " " + getAliases().iterator().next() + " "); //page will be added by super

            // header
            componentResult.add(LangKey.CMD_LIST_TREASURES_HEADER.create(
                PlaceHolder.NUMBER.numeric(pageNow),
                PlaceHolder.LAST_PAGE.numeric(lastPage)));
        }

        public void addEntry(final @NotNull TreasureInfo treasureInfo, final @NotNull Ulid treasureId) {
            numOfEntriesStillToDo--;

            //build treasureInfo
            final @NotNull TextComponent.Builder treasureInfoComponentBuilder = Component.text();

            treasureInfoComponentBuilder.append(LangKey.CMD_LIST_TREASURES_BODY.create(
                PlaceHolder.TREASURE_ID.string(treasureId.toString()),
                PlaceHolder.NUMBER.numeric(((double) treasureInfo.nonEmptyPermyriad()) / 100.0d),
                PlaceHolder.SHARED.boolChoice(treasureInfo.isShared()),
                PlaceHolder.UNLIMITED.boolChoice(treasureInfo.isUnlimited())
            ));

            if (treasureInfo.doesForget() || !treasureInfo.isUnlocked()) {
                treasureInfoComponentBuilder.appendSpace().append(treasureInfo.getRefreshInfo().infoMessage());
            }

            componentResult.add(treasureInfoComponentBuilder);

            if (numOfEntriesStillToDo <= 0) {
                sendMessage();
            }
        }
    }
}
