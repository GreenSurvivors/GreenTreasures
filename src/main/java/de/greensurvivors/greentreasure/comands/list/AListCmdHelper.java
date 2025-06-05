package de.greensurvivors.greentreasure.comands.list;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.language.LangKey;
import de.greensurvivors.greentreasure.language.PlaceHolder;
import net.kyori.adventure.audience.Audience;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.ComponentLike;
import net.kyori.adventure.text.JoinConfiguration;
import net.kyori.adventure.text.TextComponent;
import net.kyori.adventure.text.event.ClickEvent;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

public abstract class AListCmdHelper {
    protected final @NotNull Object MUTEX = new Object();
    protected final @NotNull List<@NotNull ComponentLike> componentResult = new ArrayList<>();
    protected final @NotNull GreenTreasure plugin;
    protected final @NotNull String command;
    private final int pageNow, lastPage;
    private final @NotNull Audience audience;
    protected int numOfEntriesStillToDo;

    public AListCmdHelper(final @NotNull GreenTreasure plugin, final @NotNull Audience audience,
                          int pageNow, int lastPage, int numEntries,
                          final @NotNull String command) {
        this.plugin = plugin;
        this.audience = audience;
        this.lastPage = lastPage;
        this.pageNow = pageNow;
        this.numOfEntriesStillToDo = numEntries;
        this.command = command;
    }

    protected void sendMessage() {
        final @NotNull TextComponent.Builder footerBuilder = Component.text();

        footerBuilder.append(LangKey.CMD_LIST_FOOTER_OUTER.create());

        if (pageNow > 1) {
            footerBuilder.append(LangKey.CMD_LIST_FOOTER_BACK.create(
                    PlaceHolder.NUMBER.numeric(pageNow - 1)).
                clickEvent(ClickEvent.runCommand("/" + command + (pageNow - 1))));
        } else {
            footerBuilder.append(LangKey.CMD_LIST_FOOTER_NONE.create());
        }

        footerBuilder.append(LangKey.CMD_LIST_FOOTER_INNER.create());

        if (pageNow < lastPage) {
            footerBuilder.append(LangKey.CMD_LIST_FOOTER_NEXT.create(
                    PlaceHolder.NUMBER.numeric(pageNow + 1)).
                clickEvent(ClickEvent.runCommand("/" + command + (pageNow + 1))));
        } else {
            footerBuilder.append(LangKey.CMD_LIST_FOOTER_NONE.create());
        }

        footerBuilder.append(LangKey.CMD_LIST_FOOTER_OUTER.create());

        componentResult.add(footerBuilder.build());

        // send components
        audience.sendMessage(Component.join(JoinConfiguration.newlines(), componentResult));
    }
}
