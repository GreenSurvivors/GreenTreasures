package de.greensurvivors.greentreasure.comands.set;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermmissionManager;
import de.greensurvivors.greentreasure.comands.ASubCommand;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
import org.apache.commons.lang.BooleanUtils;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class SetSharedSubCommand extends ASubCommand {

    public SetSharedSubCommand(@NotNull GreenTreasure plugin) {
        super(plugin);
    }

    @Override
    protected boolean checkPermission(@NotNull Permissible permissible) {
        return permissible.hasPermission(PermmissionManager.TREASURE_SET_SHARED.get());
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("global", "shared");
    }

    @Override
    @NotNull
    public Component getHelpText() {
        return null;
    }

    /**
     * set's if all player share the same loot inventory or not
     * /gt set(0) shared(1) <true/false>(2)
     *
     * @param sender sender of this command
     * @param args   given arguments
     */
    public boolean onCommand(final @NotNull CommandSender sender, final @NotNull String @NotNull [] args) {
        if (checkPermission(sender)) {
            final @Nullable Container container = plugin.getTreasureCommands().getContainer(sender);

            if (container != null) {
                final @Nullable TreasureInfo treasureInfo = plugin.getTreasureManager().getTreasureInfo(container);

                if (treasureInfo != null) {
                    if (args.length > 2) {
                        Boolean isUnLimited = BooleanUtils.toBooleanObject(args[2]);

                        if (isUnLimited != null) {
                            plugin.getDatabaseManager().setShared(treasureInfo.treasureId(), isUnLimited).thenRun(() ->
                                plugin.getMessageManager().sendLang(sender, LangPath.CMD_SET_SHARED_SUCCESS,
                                    Placeholder.component(PlaceHolderKey.BOOL.getKey(), plugin.getMessageManager().getLang(isUnLimited ? LangPath.BOOLEAN_TRUE : LangPath.BOOLEAN_FALSE))
                                ));
                        } else {
                            plugin.getMessageManager().sendLang(sender, LangPath.ARG_NOT_A_BOOL,
                                Placeholder.unparsed(PlaceHolderKey.TEXT.getKey(), args[2]));

                            return false;
                        }
                    } else {
                        plugin.getMessageManager().sendLang(sender, LangPath.ERROR_NOT_ENOUGH_ARGS);
                        return false;
                    }
                } else {
                    plugin.getMessageManager().sendLang(sender, LangPath.ERROR_NOT_LOOKING_AT_TREASURE);
                }
            } else {
                plugin.getMessageManager().sendLang(sender, LangPath.ERROR_NOT_LOOKING_AT_CONTAINER);
            }
        } else {
            plugin.getMessageManager().sendLang(sender, LangPath.NO_PERMISSION);
        }

        return true;
    }

    public @NotNull List<@NotNull String> onTabComplete(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        if (args.length == 3) {
            return List.of(Boolean.TRUE.toString(), Boolean.FALSE.toString());
        }

        return new ArrayList<>();
    }
}
