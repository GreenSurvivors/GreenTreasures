package de.greensurvivors.greentreasure.comands.set;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermissionManager;
import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.comands.ASubCommand;
import de.greensurvivors.greentreasure.language.LangKey;
import de.greensurvivors.greentreasure.language.PlaceHolder;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.MiniMessage;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

public class SetFindFreshMessageOverrideSubCommand extends ASubCommand {

    public SetFindFreshMessageOverrideSubCommand(@NotNull GreenTreasure plugin) {
        super(plugin);
    }

    @Override
    protected boolean checkPermission(@NotNull Permissible permissible) {
        return permissible.hasPermission(PermissionManager.TREASURE_SET_FIND_FRESH_MSG_OVERRIDE.get());
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("findfreshmessageoverride", "findfresh", "findfreshmsg");
    }

    @Override
    public @NotNull Component getHelpText() {
        return null;
    }

    /**
     * set's the message the user gets, if they open the treasure with new generated loot
     * /gt set(0) findfreshmsg(1) <new message>(2)
     *
     * @param sender sender of this command
     * @param args   given arguments
     */
    public boolean onCommand(final @NotNull CommandSender sender, final @NotNull String @NotNull [] args) {
        if (checkPermission(sender)) {
            final @Nullable Container container = plugin.getMainCommand().getContainer(sender);

            if (container != null) {
                plugin.getTreasureManager().getTreasureInfo(container).thenAccept(treasureInfo -> {
                    if (treasureInfo != null) {
                        final @NotNull Component name = Utils.getDisplayName(container);

                        if (args.length > 2) {
                            final @NotNull String newMessage = String.join(" ", Arrays.copyOfRange(args, 2, args.length));

                            plugin.getDataAccessor().setFindFreshMessageOverride(treasureInfo.treasureId(), newMessage).thenRun(() ->
                                plugin.getMessageManager().sendPrefixed(sender, LangKey.CMD_SET_FIND_FRESH_MESSAGE_OVERRIDE_SUCCESS.create(
                                    PlaceHolder.TREASURE_ID.component(name),
                                    PlaceHolder.TEXT.component(MiniMessage.miniMessage().deserialize(newMessage))
                                )));

                        } else {
                            plugin.getDataAccessor().setFindFreshMessageOverride(treasureInfo.treasureId(), null).thenRun(() ->
                                plugin.getMessageManager().sendPrefixed(sender, LangKey.CMD_SET_FIND_FRESH_MESSAGE_OVERRIDE_REMOVED.create(
                                    PlaceHolder.TREASURE_ID.component(name))));
                        }
                    } else {
                        plugin.getMessageManager().sendPrefixed(sender, LangKey.ERROR_NOT_LOOKING_AT_TREASURE);
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

    public @NotNull List<@NotNull String> onTabComplete(@NotNull CommandSender sender, final @NotNull String @NotNull [] args) {
        return new ArrayList<>();
    }
}
