package de.greensurvivors.greentreasure.comands.set;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermissionManager;
import de.greensurvivors.greentreasure.comands.ASubCommand;
import de.greensurvivors.greentreasure.language.LangKey;
import de.greensurvivors.greentreasure.language.PlaceHolder;
import net.kyori.adventure.text.Component;
import org.apache.commons.lang3.BooleanUtils;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class SetUnlimitedSubCommand extends ASubCommand {

    public SetUnlimitedSubCommand(@NotNull GreenTreasure plugin) {
        super(plugin);
    }

    @Override
    protected boolean checkPermission(@NotNull Permissible permissible) {
        return permissible.hasPermission(PermissionManager.TREASURE_SET_UNLIMITED.get());
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("unlimited");
    }

    @Override
    public @NotNull Component getHelpText() {
        return null;
    }

    /**
     * set's if the treasure is lootable an unlimited amount of times
     * /gt set(0) unlimited(1) <true/false>(2)
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
                        if (args.length > 2) {
                            Boolean isUnLimited = BooleanUtils.toBooleanObject(args[2]);

                            if (isUnLimited != null) {
                                plugin.getDataAccessor().setUnlimited(treasureInfo.treasureId(), isUnLimited).thenRun(() ->
                                    plugin.getMessageManager().sendPrefixed(sender, LangKey.CMD_SET_UNLIMITED_SUCCESS.create(
                                        PlaceHolder.UNLIMITED.boolChoice(isUnLimited)
                                    )));

                            } else {
                                plugin.getMessageManager().sendPrefixed(sender, LangKey.ARG_NOT_A_BOOL.create(
                                    PlaceHolder.TEXT.string(args[2])));
                            }
                        } else {
                            plugin.getMessageManager().sendPrefixed(sender, LangKey.CMD_ERROR_NOT_ENOUGH_ARGS);
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
        if (args.length == 3) {
            return List.of(Boolean.TRUE.toString(), Boolean.FALSE.toString());
        }

        return new ArrayList<>();
    }
}
