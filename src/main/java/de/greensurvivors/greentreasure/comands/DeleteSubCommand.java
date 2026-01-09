package de.greensurvivors.greentreasure.comands;

import com.github.f4b6a3.ulid.Ulid;
import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermissionManager;
import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.event.ClickEvent;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class DeleteSubCommand extends ASubCommand {
    protected static final @NotNull String GLOBAL = "global", LOCAL = "local";

    public DeleteSubCommand(@NotNull GreenTreasure plugin) {
        super(plugin);
    }

    @Override
    protected boolean checkPermission(@NotNull Permissible permissible) {
        return permissible.hasPermission(PermissionManager.TREASURE_DELETE.get());
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("delete", "remove");
    }

    @Override
    public @NotNull Component getHelpText() {
        return null;
    }

    /**
     * deletes a treasure AND all the UNLOOTED stuff for every player
     * /gt delete
     *
     * @param sender sender of this command
     */
    public boolean onCommand(final @NotNull CommandSender sender, final @NotNull String @NotNull [] args) {
        if (checkPermission(sender)) {
            final @Nullable Container container = plugin.getMainCommand().getContainer(sender);

            if (container != null) {
                final @Nullable Ulid treasureId = plugin.getTreasureManager().getTreasureId(container);

                if (treasureId != null) {
                    boolean isGlobal = true;

                    if (args.length > 1) {
                        if (args[1].equalsIgnoreCase(LOCAL)) {
                            isGlobal = false;
                        } else if (!args[1].equalsIgnoreCase(GLOBAL)) {
                            plugin.getMessageManager().sendLang(sender, LangPath.ARG_UNKNOWN,
                                Placeholder.unparsed(PlaceHolderKey.TEXT.getKey(), args[1]));
                            return true;
                        }
                    }

                    final @NotNull String command = "/" + MainCommand.CMD + " " + getAliases().iterator().next();
                    if (isGlobal) {
                        plugin.getTreasureManager().deleteTreasure(container).thenAccept(success -> {
                            if (success) {
                                plugin.getMessageManager().sendLang(sender, LangPath.REMOVE_GLOBAL_SUCCESS,
                                    Placeholder.component(PlaceHolderKey.TREASURE_ID.getKey(), Utils.getDisplayName(container)));
                            } else {
                                plugin.getMessageManager().sendLang(sender, LangPath.REMOVE_ERROR,
                                    Placeholder.component(PlaceHolderKey.TREASURE_ID.getKey(),
                                        Utils.getDisplayName(container)),
                                    Placeholder.component(PlaceHolderKey.CMD.getKey(),
                                        Component.text().
                                            content(command).
                                            clickEvent(ClickEvent.suggestCommand(command))
                                    )
                                );
                            }
                        });
                    } else {
                        if (plugin.getTreasureManager().deleteTreasureLocal(container)){
                            plugin.getMessageManager().sendLang(sender, LangPath.REMOVE_LOCAL_SUCCESS,
                                Placeholder.component(PlaceHolderKey.TREASURE_ID.getKey(), Utils.getDisplayName(container)));
                        } else {
                            plugin.getMessageManager().sendLang(sender, LangPath.REMOVE_ERROR,
                                Placeholder.component(PlaceHolderKey.TREASURE_ID.getKey(),
                                    Utils.getDisplayName(container)),
                                Placeholder.component(PlaceHolderKey.CMD.getKey(),
                                    Component.text().
                                        content(command).
                                        clickEvent(ClickEvent.suggestCommand(command))
                                )
                            );
                        }
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
        if (args.length == 1) {
            return List.copyOf(getAliases());
        } if (args.length == 2) {
            return List.of(GLOBAL, LOCAL);
        } else {
            return new ArrayList<>();
        }
    }
}
