package de.greensurvivors.greentreasure.comands;

import com.github.f4b6a3.ulid.Ulid;
import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermissionManager;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.bukkit.inventory.ItemStack;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

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
                    if (plugin.getTreasureManager().getTreasureInfo(container) == null) {
                        final @NotNull Ulid newTreasureId = plugin.getTreasureManager().createNewMonotonicUlid();
                        plugin.getTreasureManager().setTreasureId(container, newTreasureId);

                        List<ItemStack> itemStacks = Arrays.stream(container.getInventory().getContents()).toList();

                        plugin.getDatabaseManager().setTreasureContents(newTreasureId, itemStacks).thenRun(() ->
                            plugin.getMessageManager().sendLang(sender, LangPath.CMD_CREATE_SUCCESS,
                                Placeholder.component(PlaceHolderKey.NAME.getKey(),
                                    container.customName() == null ?
                                        Component.translatable(container.getBlock().getType().getBlockTranslationKey()) :
                                        container.customName()
                                )));
                    } else {
                        plugin.getMessageManager().sendLang(sender, LangPath.CMD_CREATE_ERROR_ALREADY_TREASURE);
                    }
                } else {
                    plugin.getMessageManager().sendLang(sender, LangPath.CMD_CREATE_ERROR_INVALID_CONTAINER);
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
        if (args.length == 1) {
            return List.copyOf(getAliases());
        } else {
            return new ArrayList<>();
        }
    }
}
