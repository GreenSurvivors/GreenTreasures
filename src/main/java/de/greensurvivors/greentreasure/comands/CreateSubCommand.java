package de.greensurvivors.greentreasure.comands;

import com.github.f4b6a3.ulid.Ulid;
import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermissionManager;
import de.greensurvivors.greentreasure.language.LangKey;
import de.greensurvivors.greentreasure.language.PlaceHolder;
import net.kyori.adventure.text.Component;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.bukkit.inventory.ItemStack;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public class CreateSubCommand extends ASubCommand {

    public CreateSubCommand(@NotNull GreenTreasure plugin) {
        super(plugin);
    }

    @Override
    protected boolean checkPermission(@NotNull Permissible permissible) {
        return permissible.hasPermission(PermissionManager.TREASURE_CREATE.get());
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("create");
    }

    @Override
    public @NotNull Component getHelpText() {
        return null;
    }

    /**
     * creates a new treasure
     * /gt create
     *
     * @param sender sender of this command
     */
    public boolean onCommand(final @NotNull CommandSender sender, final @NotNull String @NotNull [] args) {
        if (checkPermission(sender)) {
            final @Nullable Container container = plugin.getMainCommand().getContainer(sender);

            if (container != null) {
                if (container.getInventory().getType().isCreatable()) {
                    plugin.getTreasureManager().getTreasureInfo(container).thenAccept(treasureInfo -> {
                        if (treasureInfo == null) {
                            final @NotNull Ulid newTreasureId = plugin.getTreasureManager().createNewMonotonicUlid();
                            plugin.getTreasureManager().setTreasureId(container, newTreasureId);

                            List<ItemStack> itemStacks = Arrays.stream(container.getInventory().getContents()).toList();
                            container.update(true, false);

                            plugin.getDatabaseManager().setTreasureContents(newTreasureId, itemStacks).thenRun(() ->
                                plugin.getMessageManager().sendPrefixed(sender, LangKey.CMD_CREATE_SUCCESS.create(
                                    PlaceHolder.TREASURE_ID.component(
                                        Objects.requireNonNullElseGet(
                                            container.customName(),
                                            () -> Component.translatable(container.getBlock().translationKey())
                                    )))));
                        } else {
                            plugin.getMessageManager().sendPrefixed(sender, LangKey.CMD_CREATE_ERROR_ALREADY_TREASURE);
                        }
                    });
                } else {
                    plugin.getMessageManager().sendPrefixed(sender, LangKey.CMD_CREATE_ERROR_INVALID_CONTAINER);
                }
            } else {
                plugin.getMessageManager().sendPrefixed(sender, LangKey.ERROR_NOT_LOOKING_AT_CONTAINER);
            }
        } else {
            plugin.getMessageManager().sendPrefixed(sender, LangKey.NO_PERMISSION);
        }

        return true;
    }

    public @NotNull List<@NotNull String> onTabComplete(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        if (args.length == 1) {
            return List.copyOf(getAliases());
        } else {
            return new ArrayList<>();
        }
    }
}
