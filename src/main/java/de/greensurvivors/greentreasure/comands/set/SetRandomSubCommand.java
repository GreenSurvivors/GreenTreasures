package de.greensurvivors.greentreasure.comands.set;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermissionManager;
import de.greensurvivors.greentreasure.comands.ASubCommand;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.resolver.Formatter;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
import org.apache.commons.lang3.math.NumberUtils;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.Range;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class SetRandomSubCommand extends ASubCommand {

    public SetRandomSubCommand(@NotNull GreenTreasure plugin) {
        super(plugin);
    }

    @Override
    protected boolean checkPermission(@NotNull Permissible permissible) {
        return permissible.hasPermission(PermissionManager.TREASURE_SET_RANDOM.get());
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("random", "nonemptypermyriad");
    }

    @Override
    @NotNull
    public Component getHelpText() {
        return null;
    }

    /**
     * sets how many permyriad of the non-empty items should be in contained in a freshly generated treasure
     * /gt set(0) random(1) <nonEmptyPermyriad>(2)
     *
     * @param sender sender of this command
     * @param args   given arguments
     */
    public boolean onCommand(final @NotNull CommandSender sender, final @NotNull String @NotNull [] args) {
        if (checkPermission(sender)) {
            final @Nullable Container container = plugin.getMainCommand().getContainer(sender);

            if (container != null) {
                final @Nullable TreasureInfo treasureInfo = plugin.getTreasureManager().getTreasureInfo(container);

                if (treasureInfo != null) {
                    if (args.length > 2) {
                        final @Nullable Number parsedNumber = NumberUtils.createNumber(args[2]);

                        if (parsedNumber != null) {
                            @Range(from = 0, to = 10000) short nonEmptyPermyriad = (short) (parsedNumber.doubleValue() * 100);

                            nonEmptyPermyriad = (short) Math.max(Math.min(nonEmptyPermyriad, 10000), 0);

                            int finalSlotPercentage = nonEmptyPermyriad;
                            plugin.getDatabaseManager().setRandom(treasureInfo.treasureId(), nonEmptyPermyriad).thenRun(() ->
                                plugin.getMessageManager().sendLang(sender, LangPath.CMD_SET_RANDOM_SUCCESS,
                                    Formatter.number(PlaceHolderKey.NUMBER.getKey(), ((float) finalSlotPercentage) / 100.0f)));
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

    public @NotNull List<@NotNull String> onTabComplete(@NotNull CommandSender sender, final @NotNull String @NotNull [] args) {
        if (args.length == 3) {
            return List.of("100.0", "75.0", "50.0", "25.0", "10.0", "5.0", "2.0");
        }

        return new ArrayList<>();
    }
}
