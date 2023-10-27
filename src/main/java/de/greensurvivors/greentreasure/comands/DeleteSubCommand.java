package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.config.TreasureConfig;
import de.greensurvivors.greentreasure.language.Lang;
import de.greensurvivors.greentreasure.permission.Perm;
import net.kyori.adventure.text.serializer.plain.PlainTextComponentSerializer;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static de.greensurvivors.greentreasure.comands.TreasureCommands.DELETE;
import static de.greensurvivors.greentreasure.comands.TreasureCommands.REMOVE;

public class DeleteSubCommand {
    private static DeleteSubCommand instance;

    public static DeleteSubCommand inst() {
        if (instance == null)
            instance = new DeleteSubCommand();
        return instance;
    }

    /**
     * deletes a treasure AND all the UNLOOTED stuff for every player
     * /gt delete
     *
     * @param commandSender sender of this command
     */
    protected void handleDelete(CommandSender commandSender) {
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_DELETE)) {
            Container container = TreasureCommands.getContainer(commandSender);

            if (container != null) {
                commandSender.sendMessage(Lang.build(Lang.TREASURE_DELETE_START.get()));

                TreasureConfig.inst().deleteTreasureAsync(container.getBlock().getLocation(), () -> {
                });
                TreasureConfig.inst().forgetAllAsync(container.getBlock().getLocation(), () ->
                        commandSender.sendMessage(Lang.build(Lang.TREASURE_DELETE_END.get().replace(Lang.TYPE,
                                container.customName() == null ? container.getBlock().getType().name() :
                                        PlainTextComponentSerializer.plainText().serialize(container.customName())))));


            } else {
                commandSender.sendMessage(Lang.build(Lang.NOT_LOOKINGAT_CONTAINER.get()));
            }
        } else {
            commandSender.sendMessage(Lang.build(Lang.NO_PERMISSION_COMMAND.get()));
        }
    }

    /**
     * @param args The arguments passed to the command, including final
     *             partial argument to be completed
     * @return suggestion of arguments
     */
    public Collection<String> handleTabComplete(@NotNull String[] args) {
        if (args.length == 1) {
            return List.of(DELETE, REMOVE);
        } else {
            return new ArrayList<>();
        }
    }
}
