package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.config.TreasureConfig;
import de.greensurvivors.greentreasure.language.Lang;
import de.greensurvivors.greentreasure.permission.Perm;
import net.kyori.adventure.text.serializer.plain.PlainTextComponentSerializer;
import org.bukkit.Location;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;

import java.util.*;

import static de.greensurvivors.greentreasure.comands.TreasureCommands.CREATE;

public class CreateSubCommand {
    private static CreateSubCommand instance;

    public static CreateSubCommand inst() {
        if (instance == null)
            instance = new CreateSubCommand();
        return instance;
    }

    /**
     * creates a new treasure
     * /gt create
     *
     * @param commandSender sender of this command
     */
    protected void handleCreate(CommandSender commandSender) {
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_CREATE)) {
            final Container container = TreasureCommands.getContainer(commandSender);

            if (container != null) {
                Location location = container.getBlock().getLocation();
                List<ItemStack> itemStacks = Arrays.stream(container.getInventory().getContents()).toList();

                commandSender.sendMessage(Lang.build(Lang.TREASURE_CREATE_START.get()));

                TreasureConfig.inst().saveTreasureAsync(location, itemStacks, container.getBlock().getType().name(), () ->
                        commandSender.sendMessage(Lang.build(Lang.TREASURE_CREATE_END.get().replace(Lang.TYPE,
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
    public Collection<String> handleTabCompleate(@NotNull String[] args) {
        if (args.length == 1) {
            return Collections.singletonList(CREATE);
        } else {
            return new ArrayList<>();
        }
    }
}
