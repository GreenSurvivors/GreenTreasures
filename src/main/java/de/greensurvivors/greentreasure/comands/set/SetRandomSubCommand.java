package de.greensurvivors.greentreasure.comands.set;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.comands.ASubCommand;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import de.greensurvivors.greentreasure.permission.PermmissionManager;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
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
        return permissible.hasPermission(PermmissionManager.TREASURE_SET_RANDOM.get());
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("random", "slotchance");
    }

    @Override
    @NotNull
    public Component getHelpText() {
        return null;
    }

    /**
     * sets how probable it is for an inventory slot to be lootable
     * /gt set(0) random(1) <slotChance>(2)
     *
     * @param sender sender of this command
     * @param args   given arguments
     */
    public boolean onCommand(final @NotNull CommandSender sender, final @NotNull String @NotNull [] args) {
        if (checkPermission(sender)) {
            final @Nullable Container container = plugin.getTreasureCommands().getContainer(sender);

            if (container != null) {
                final @Nullable String treasureId = plugin.getTreasureListener().getTreasureId(container);

                if (treasureId != null && plugin.getTreasureListener().getTreasure(treasureId) != null) {
                    if (args.length > 2) {
                        if (Utils.isDouble(args[2])) {
                            @Range(from = 0, to = 10000) short slotPercentage = (short) (Double.parseDouble(args[2]) * 100);

                            slotPercentage = (short) Math.max(Math.min(slotPercentage, 10000), 0);

                            int finalSlotPercentage = slotPercentage;
                            plugin.getDatabaseManager().setRandom(treasureId, slotPercentage).thenRun(() ->
                                plugin.getMessageManager().sendLang(sender, LangPath.CMD_SET_RANDOM_SUCCESS,
                                    Placeholder.unparsed(PlaceHolderKey.NUMBER.getKey(), String.valueOf(((float) finalSlotPercentage) / 100.0f))));
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

    /**
     * @param sender
     * @param args   The arguments passed to the command, including final
     *               partial argument to be completed
     * @return suggestion of arguments
     */
    public @NotNull List<@NotNull String> onTabComplete(@NotNull CommandSender sender, final @NotNull String @NotNull [] args) {
        if (args.length == 3) {
            return List.of("100.0", "75.0", "50.0", "25.0", "10.0", "5.0", "2.0");
        }

        return new ArrayList<>();
    }
}
