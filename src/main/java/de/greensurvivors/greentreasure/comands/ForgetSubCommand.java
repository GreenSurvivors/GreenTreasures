package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import de.greensurvivors.greentreasure.permission.PermmissionManager;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public class ForgetSubCommand extends ASubCommand {

    public ForgetSubCommand(final @NotNull GreenTreasure plugin) {
        super(plugin);
    }

    @Override
    protected boolean checkPermission(@NotNull Permissible permissible) {
        return permissible.hasPermission(PermmissionManager.TREASURE_FORGET.get());
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("forget");
    }

    @Override
    public @NotNull Component getHelpText() {
        return null;
    }

    /**
     * the treasure forgets a given somebody has ever looted it
     * /gt forget (takes the commandSender self)
     * /gt forget uuid
     * /gt forget playerName
     *
     * @param sender sender of this command
     * @param args   given arguments
     */
    public boolean onCommand(final @NotNull CommandSender sender, final @NotNull String @NotNull [] args) {
        if (checkPermission(sender)) {
            Container container = plugin.getTreasureCommands().getContainer(sender);

            if (container != null) {
                final @Nullable String treasureId = plugin.getTreasureListener().getTreasureId(container);

                if (treasureId != null) {
                    final @Nullable TreasureInfo treasureInfo = plugin.getTreasureListener().getTreasure(treasureId);

                    if (treasureInfo != null) {
                        if (treasureInfo.isShared()) {
                            plugin.getDatabaseManager().forgetPlayer(null, treasureId).thenRun(() ->
                                plugin.getMessageManager().sendLang(sender, LangPath.CMD_FORGET_SHARED_SUCCESS,
                                    Placeholder.component(PlaceHolderKey.NAME.getKey(), Utils.getDisplayName(container))));
                        }

                        final OfflinePlayer playerToForget;
                        if (args.length >= 2) {
                            OfflinePlayer offlinePlayer = Bukkit.getOfflinePlayer(args[1]);
                            if (offlinePlayer.hasPlayedBefore()) {
                                playerToForget = offlinePlayer;
                            } else {
                                try {
                                    playerToForget = Bukkit.getOfflinePlayer(UUID.fromString(args[1]));
                                } catch (IllegalArgumentException ignored) {
                                    plugin.getMessageManager().sendLang(sender, LangPath.ARG_NOT_PLAYER,
                                        Placeholder.unparsed(PlaceHolderKey.TEXT.getKey(), args[1]));
                                    return false;
                                }
                            }
                        } else {
                            if (sender instanceof OfflinePlayer offlinePlayer) {
                                playerToForget = offlinePlayer;
                            } else {
                                plugin.getMessageManager().sendLang(sender, LangPath.ERROR_SENDER_NOT_PLAYER);
                                return true;
                            }
                        }

                        if (playerToForget.hasPlayedBefore()) {
                            plugin.getDatabaseManager().forgetPlayer(playerToForget, treasureId).thenRun(() -> {
                                final @NotNull Component playerName;
                                final @Nullable Player player = playerToForget.getPlayer();
                                if (player != null) {
                                    playerName = player.displayName();
                                } else {
                                    if (playerToForget.getName() != null) {
                                        playerName = Component.text(playerToForget.getName());
                                    } else {
                                        playerName = Component.text(playerToForget.getUniqueId().toString());
                                    }
                                }

                                plugin.getMessageManager().sendLang(sender, LangPath.CMD_FORGET_USER_SUCCESS,
                                    Placeholder.component(PlaceHolderKey.NAME.getKey(), Utils.getDisplayName(container)),
                                    Placeholder.component(PlaceHolderKey.PLAYER.getKey(), playerName));
                            });
                        } else {
                            plugin.getMessageManager().sendLang(sender, LangPath.ARG_NOT_PLAYER,
                                Placeholder.unparsed(PlaceHolderKey.TEXT.getKey(), args[1]));
                            return false;
                        }
                    } else {
                        plugin.getMessageManager().sendLang(sender, LangPath.ERROR_NOT_LOOKING_AT_TREASURE);
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
        switch (args.length) {
            case 1 -> {
                return List.copyOf(getAliases());
            }
            case 2 -> {
                Collection<? extends Player> onlinePlayers = Bukkit.getOnlinePlayers();
                if (!onlinePlayers.isEmpty()) {
                    return onlinePlayers.stream().map(Player::getName).toList();
                }
            }
        }

        return new ArrayList<>();
    }
}
