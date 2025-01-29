package de.greensurvivors.greentreasure.dataobjects;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.JoinConfiguration;
import net.kyori.adventure.text.TextComponent;
import net.kyori.adventure.text.event.ClickEvent;
import net.kyori.adventure.text.minimessage.tag.resolver.Formatter;
import org.bukkit.command.CommandSender;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;

public abstract class AListCmdHelper {
    protected final @NotNull Object MUTEX = new Object();
    protected final @NotNull ArrayList<@NotNull Component> componentResult = new ArrayList<>();
    protected final @NotNull GreenTreasure plugin;
    protected final @NotNull String command;
    private final int pageNow, lastPage;
    private final @NotNull CommandSender commandSender;
    protected int numOfEntriesStillToDo;

    public AListCmdHelper(final @NotNull GreenTreasure plugin, final @NotNull CommandSender commandSender,
                          int pageNow, int lastPage, int numEntries,
                          final @NotNull String command) {
        this.plugin = plugin;
        this.commandSender = commandSender;
        this.lastPage = lastPage;
        this.pageNow = pageNow;
        this.numOfEntriesStillToDo = numEntries;
        this.command = command;
    }

    protected void sendMessage() {
        TextComponent.Builder footerBuilder = Component.text();

        footerBuilder.append(plugin.getMessageManager().getLang(LangPath.CMD_LIST_FOOTER_OUTER));

        if (pageNow > 1) {
            footerBuilder.append(plugin.getMessageManager().getLang(LangPath.CMD_LIST_FOOTER_BACK,
                    Formatter.number(PlaceHolderKey.NUMBER.getKey(), pageNow - 1)).
                clickEvent(ClickEvent.runCommand(command + (pageNow - 1))));
        } else {
            footerBuilder.append(plugin.getMessageManager().getLang(LangPath.CMD_LIST_FOOTER_NONE));
        }

        footerBuilder.append(plugin.getMessageManager().getLang(LangPath.CMD_LIST_FOOTER_INNER));

        if (pageNow < lastPage) {
            footerBuilder.append(plugin.getMessageManager().getLang(LangPath.CMD_LIST_FOOTER_NEXT,
                    Formatter.number(PlaceHolderKey.NUMBER.getKey(), pageNow + 1)).
                clickEvent(ClickEvent.runCommand(command + (pageNow + 1))));
        } else {
            footerBuilder.append(plugin.getMessageManager().getLang(LangPath.CMD_LIST_FOOTER_NONE));
        }

        footerBuilder.append(plugin.getMessageManager().getLang(LangPath.CMD_LIST_FOOTER_OUTER));

        componentResult.add(footerBuilder.build());

        // send components
        commandSender.sendMessage(Component.join(JoinConfiguration.newlines(), componentResult));
    }
}
