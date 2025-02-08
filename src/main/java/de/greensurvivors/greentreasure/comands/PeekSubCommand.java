package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermissionManager;
import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.dataobjects.InventoryHolderWrapper;
import de.greensurvivors.greentreasure.dataobjects.PeekedTreasure;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.language.LangPath;
import de.greensurvivors.greentreasure.language.PlaceHolderKey;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.resolver.Placeholder;
import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.inventory.Inventory;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.UUID;

public class PeekSubCommand extends ASubCommand {

    public PeekSubCommand(final @NotNull GreenTreasure plugin) {
        super(plugin);
    }

    @Override
    protected boolean checkPermission(@NotNull Permissible permissible) {
        return permissible.hasPermission(PermissionManager.TREASURE_PEEK.get());
    }

    @Override
    public @NotNull Set<@NotNull String> getAliases() {
        return Set.of("peek");
    }

    @Override
    @NotNull
    public Component getHelpText() {
        return null;
    }

    /**
     * let's the command sender open the treasure-inventory for another player
     * /gt peek (takes the commandSender self)
     * /gt peek playerName
     * /gt peek uuid
     *
     * @param sender sender of this command
     * @param args   given arguments
     */
    public boolean onCommand(final @NotNull CommandSender sender, final @NotNull String @NotNull [] args) {
        if (checkPermission(sender)) {
            if (sender instanceof Player player) {
                Container container = plugin.getMainCommand().getContainer(sender);

                if (container != null) {
                    final @Nullable TreasureInfo treasureInfo = plugin.getTreasureManager().getTreasureInfo(container);

                    if (treasureInfo != null) {
                        // unlimited treasure
                        if (treasureInfo.isUnlimited()) {
                            plugin.getMessageManager().sendLang(sender, LangPath.CMD_PEEK_UNLIMITED);
                        }

                        if (treasureInfo.isShared()) {
                            //load shared treasure
                            plugin.getDatabaseManager().getPlayerData(null, treasureInfo.treasureId()).thenAccept(playerLootDetail -> {
                                final @NotNull Inventory nowPeeking;
                                long timeStamp = playerLootDetail.lastChangedTimeStamp();

                                if ((playerLootDetail.unLootedStuff() == null || playerLootDetail.unLootedStuff().isEmpty())) {
                                    plugin.getMessageManager().sendLang(sender, LangPath.CMD_PEEK_GENERATE_SHARED,
                                        Placeholder.component(PlaceHolderKey.TREASURE_ID.getKey(), plugin.getMessageManager().getLang(LangPath.CMD_LIST_WHO_SHARED)));

                                    final @NotNull Component title = plugin.getMessageManager().getLang(LangPath.TREASURE_TITLE_PEEK_SHARED,
                                        Placeholder.component(PlaceHolderKey.TREASURE_ID.getKey(), Utils.getDisplayName(container)));
                                    nowPeeking = Bukkit.createInventory(container, container.getInventory().getType(), title);

                                    Utils.setContents(nowPeeking, treasureInfo.itemLoot(), treasureInfo.slotChance());

                                    plugin.getMessageManager().sendLang(sender, LangPath.CMD_PEEK_WARNING);
                                } else {
                                    final @NotNull Component title = plugin.getMessageManager().getLang(LangPath.TREASURE_TITLE_PEEK_SHARED,
                                        Placeholder.component(PlaceHolderKey.TREASURE_ID.getKey(), Utils.getDisplayName(container)));

                                    nowPeeking = Bukkit.createInventory(container, container.getInventory().getType(), title);

                                    // get items left there last time
                                    Utils.setContents(nowPeeking, playerLootDetail.unLootedStuff());
                                    plugin.getMessageManager().sendLang(sender, LangPath.CMD_PEEK_WARNING);
                                }

                                plugin.getCommandInventoriesListener().addPeekingTreasure(player.openInventory(nowPeeking),
                                    new PeekedTreasure(null, treasureInfo.treasureId(), timeStamp));
                            });
                        } else {
                            final @NotNull OfflinePlayer playerToPeek;
                            if (args.length >= 2) {
                                OfflinePlayer offlinePlayer = Bukkit.getOfflinePlayer(args[1]);
                                if (offlinePlayer.hasPlayedBefore()) {
                                    playerToPeek = offlinePlayer;
                                } else {
                                    try {
                                        playerToPeek = Bukkit.getOfflinePlayer(UUID.fromString(args[1]));

                                        if (!playerToPeek.hasPlayedBefore()) {
                                            plugin.getMessageManager().sendLang(sender, LangPath.ARG_NOT_PLAYER,
                                                Placeholder.unparsed(PlaceHolderKey.TEXT.getKey(), args[1]));
                                            return false;
                                        }
                                    } catch (IllegalArgumentException ignored) {
                                        plugin.getMessageManager().sendLang(sender, LangPath.ARG_NOT_PLAYER,
                                            Placeholder.unparsed(PlaceHolderKey.TEXT.getKey(), args[1]));
                                        return false;
                                    }
                                }
                            } else {
                                playerToPeek = player;
                            }

                            plugin.getDatabaseManager().getPlayerData(playerToPeek, treasureInfo.treasureId()).thenAccept(playerLootDetail -> {
                                final @NotNull Component name;
                                if (playerToPeek.isOnline()) {
                                    name = playerToPeek.getPlayer().displayName();
                                } else if (playerToPeek.getName() != null) {
                                    name = Component.text(playerToPeek.getName());
                                } else {
                                    name = Component.text(playerToPeek.getUniqueId().toString());
                                }

                                final @NotNull Component title = plugin.getMessageManager().getLang(LangPath.TREASURE_TITLE_PEEK_PLAYER,
                                    Placeholder.component(PlaceHolderKey.PLAYER.getKey(), name),
                                    Placeholder.component(PlaceHolderKey.TREASURE_ID.getKey(), Utils.getDisplayName(container)));
                                final Inventory nowPeeking = Bukkit.createInventory(new InventoryHolderWrapper<>(container, true), container.getInventory().getType(), title);

                                if ((playerLootDetail == null || playerLootDetail.unLootedStuff() == null || playerLootDetail.unLootedStuff().isEmpty())) {
                                    plugin.getMessageManager().sendLang(sender, LangPath.CMD_PEEK_GENERATE_PLAYER,
                                        Placeholder.component(PlaceHolderKey.PLAYER.getKey(), name));

                                    Utils.setContents(nowPeeking, treasureInfo.itemLoot(), treasureInfo.slotChance());
                                } else {
                                    Utils.setContents(nowPeeking, playerLootDetail.unLootedStuff());
                                }

                                plugin.getCommandInventoriesListener().addPeekingTreasure(player.openInventory(nowPeeking), new PeekedTreasure(playerToPeek.getUniqueId(), treasureInfo.treasureId(), playerLootDetail.lastChangedTimeStamp()));
                                plugin.getMessageManager().sendLang(sender, LangPath.CMD_PEEK_WARNING);
                            });
                        }
                    } else {
                        plugin.getMessageManager().sendLang(sender, LangPath.ERROR_NOT_LOOKING_AT_TREASURE);
                    }
                } else {
                    plugin.getMessageManager().sendLang(sender, LangPath.ERROR_NOT_LOOKING_AT_CONTAINER);
                }
            } else {
                plugin.getMessageManager().sendLang(sender, LangPath.ERROR_SENDER_NOT_PLAYER);
            }
        } else {
            plugin.getMessageManager().sendLang(sender, LangPath.NO_PERMISSION);
        }

        return true;
    }

    /**
     * @param args The arguments passed to the command, including final
     *             partial argument to be completed
     * @return suggestion of arguments
     */
    public @NotNull List<@NotNull String> onTabComplete(@NotNull CommandSender sender, @NotNull String @NotNull [] args) {
        if (args.length == 2) {
            return Bukkit.getOnlinePlayers().stream().map(Player::getName).toList();
        }

        return new ArrayList<>();
    }
}
