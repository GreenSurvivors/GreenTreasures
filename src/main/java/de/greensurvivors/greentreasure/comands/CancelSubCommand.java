package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermissionManager;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.UUID;

public class CancelSubCommand extends ASubCommand{
    protected final static @NotNull String IMPORT1 = "import";
    protected final static @NotNull String IMPORT2 = "import_legacy";

    public CancelSubCommand(@NotNull GreenTreasure plugin) {
        super(plugin);
    }

    @Override
    protected boolean checkPermission(@NotNull Permissible permissible) {
        return permissible.hasPermission(PermissionManager.TREASURE_LIST_NEAR.get());
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("cancel_lookup");
    }

    @Override
    public @NotNull Component getHelpText() {
        return null;
    }

    @Override
    public boolean onCommand(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        if (checkPermission(sender)) {
            if (args.length >= 2) {
                if (args[1].equals(IMPORT1) || args[1].equals(IMPORT2)) {
                    if (plugin.getLegacyDataImporter().cancelImport()) {
                        plugin.getMessageManager().sendLang(sender, LangPath.CMD_CANCEL_SUCCESS_IMPORT);
                    } else {
                        plugin.getMessageManager().sendLang(sender, LangPath.CMD_CANCEL_ERROR_NO_RUNNING);
                    }
                } else {
                    final @Nullable OfflinePlayer playerToCancelAllProcessesFrom;
                    OfflinePlayer offlinePlayer = Bukkit.getOfflinePlayer(args[1]);
                    if (offlinePlayer.hasPlayedBefore()) {
                        playerToCancelAllProcessesFrom = offlinePlayer;
                    } else {
                        try {
                            playerToCancelAllProcessesFrom = Bukkit.getOfflinePlayer(UUID.fromString(args[1]));
                        } catch (final @NotNull IllegalArgumentException ignored) {
                            plugin.getMessageManager().sendLang(sender, LangPath.ARG_NOT_PLAYER,
                                Placeholder.unparsed(PlaceHolderKey.TEXT.getKey(), args[1]));

                            return true;
                        }
                    }
                    if (playerToCancelAllProcessesFrom.hasPlayedBefore()) {
                        if (plugin.getChunkParser().cancelProcessForUUID(playerToCancelAllProcessesFrom.getUniqueId())) {
                            plugin.getMessageManager().sendLang(sender, LangPath.CMD_CANCEL_SUCCESS_OTHER,
                                Placeholder.unparsed(PlaceHolderKey.PLAYER.getKey(), playerToCancelAllProcessesFrom.getName()));
                        } else {
                            plugin.getMessageManager().sendLang(sender, LangPath.CMD_CANCEL_ERROR_NO_RUNNING);
                        }
                    } else {
                        plugin.getMessageManager().sendLang(sender, LangPath.ARG_NOT_PLAYER,
                            Placeholder.unparsed(PlaceHolderKey.TEXT.getKey(), args[1]));
                    }
                }
            } else {
                if (sender instanceof final @NotNull Player player) {
                    if (plugin.getChunkParser().cancelProcessForUUID(player.getUniqueId())) {
                        plugin.getMessageManager().sendLang(sender, LangPath.CMD_CANCEL_SUCCESS_SELF);
                    } else {
                        plugin.getMessageManager().sendLang(sender, LangPath.CMD_CANCEL_ERROR_NO_RUNNING);
                    }
                } else {
                    plugin.getMessageManager().sendLang(sender, LangPath.ERROR_SENDER_NOT_PLAYER);
                }
            }
        } else {
            plugin.getMessageManager().sendLang(sender, LangPath.NO_PERMISSION);
        }

        return true;
    }

    @Override
    public @NotNull List<@NotNull String> onTabComplete(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        if (args.length == 1) {
            return List.copyOf(getAliases());
        } else if (args.length == 2) {
            final @NotNull List<@NotNull String> result = new ArrayList<>();
            result.add(IMPORT1);
            result.add(IMPORT2);

            for (Player player : plugin.getServer().getOnlinePlayers()) {
                result.add(player.getName());
                result.add(player.getUniqueId().toString());
            }

            return result;
        } else {
            return new ArrayList<>();
        }
    }
}
