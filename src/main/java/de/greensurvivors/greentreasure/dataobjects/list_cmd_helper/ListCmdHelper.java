package de.greensurvivors.greentreasure.dataobjects.list_cmd_helper;

import de.greensurvivors.greentreasure.language.Lang;
import net.kyori.adventure.text.Component;
import org.bukkit.command.CommandSender;

import java.util.ArrayList;

public abstract class ListCmdHelper {
    protected final Object MUTEX = new Object();
    protected final ArrayList<Component> componentResult = new ArrayList<>();
    private final int PAGE_NOW, LAST_PAGE;
    private final CommandSender COMMAND_SENDER;
    protected int NumOfPagesStillToDo;
    protected String command;

    public ListCmdHelper(CommandSender commandSender, int pageNow, int lastPage, int numEntries) {
        this.COMMAND_SENDER = commandSender;
        this.LAST_PAGE = lastPage;
        this.PAGE_NOW = pageNow;
        this.NumOfPagesStillToDo = numEntries;
    }

    protected void sendMessage() {
        Component footer = Lang.build(Lang.LIST_FOOTER_OUTER.get());

        if (PAGE_NOW > 1) {
            footer = footer.append(Lang.build(Lang.LIST_FOOTER_BACK.get().replace(Lang.VALUE, String.valueOf(PAGE_NOW - 1)),
                    null, null,
                    command + (PAGE_NOW - 1),
                    null));
        } else {
            footer = footer.append(Lang.build(Lang.LIST_FOOTER_NONE.get()));
        }

        footer = footer.append(Lang.build(Lang.LIST_FOOTER_INNER.get()));

        if (PAGE_NOW < LAST_PAGE) {
            footer = footer.append(Lang.build(Lang.LIST_FOOTER_NEXT.get().replace(Lang.VALUE, String.valueOf(PAGE_NOW + 1)),
                    null, null,
                    command + (PAGE_NOW + 1),
                    null));
        } else {
            footer = footer.append(Lang.build(Lang.LIST_FOOTER_NONE.get()));
        }

        footer = footer.append(Lang.build(Lang.LIST_FOOTER_OUTER.get()));

        componentResult.add(footer);

        // send components
        COMMAND_SENDER.sendMessage(Lang.join(componentResult));
    }
}
