package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.PermissionManager;
import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.dataobjects.InventoryHolderWrapper;
import de.greensurvivors.greentreasure.dataobjects.PeekedTreasure;
import de.greensurvivors.greentreasure.language.LangKey;
import de.greensurvivors.greentreasure.language.PlaceHolder;
import net.kyori.adventure.text.Component;
import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.InventoryType;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryHolder;
import org.bukkit.permissions.Permissible;
import org.bukkit.persistence.PersistentDataHolder;
import org.jetbrains.annotations.NotNull;

import java.time.Instant;
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
    public @NotNull Component getHelpText() {
        return null;
    }

    /**
     * lets the command sender open the treasure-inventory for another player
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
                    plugin.getTreasureManager().getTreasureInfo(container).thenAccept(treasureInfo -> {
                        if (treasureInfo != null) {
                            // unlimited treasure
                            if (treasureInfo.isUnlimited()) {
                                plugin.getMessageManager().sendPrefixed(sender, LangKey.CMD_PEEK_UNLIMITED);
                                return;
                            }

                            if (treasureInfo.isShared()) {
                                //load shared treasure
                                plugin.getDatabaseManager().getPlayerData(null, treasureInfo.treasureId()).thenAccept(playerLootDetail -> {
                                    final @NotNull Inventory nowPeeking;
                                    final @NotNull InventoryHolderWrapper<?> wrapper = new InventoryHolderWrapper<>((InventoryHolder & PersistentDataHolder) Utils.getTreasureHolder(container), true);

                                    if (playerLootDetail == null || playerLootDetail.isEmpty()) {
                                        plugin.getMessageManager().sendPrefixed(sender, LangKey.CMD_PEEK_GENERATE_SHARED.create(
                                            PlaceHolder.TREASURE_ID.component(LangKey.CMD_LIST_WHO_SHARED.create())));

                                        final @NotNull Component title = LangKey.TREASURE_TITLE_PEEK_SHARED.create(
                                            PlaceHolder.TREASURE_ID.component(Utils.getDisplayName(container)));

                                        if (container.getInventory().getType() == InventoryType.CHEST) {
                                            nowPeeking = Bukkit.createInventory(wrapper, container.getInventory().getSize(), title);
                                        } else {
                                            nowPeeking = Bukkit.createInventory(wrapper, container.getInventory().getType(), title);
                                        }

                                        Utils.setContents(nowPeeking, treasureInfo.itemLoot(), treasureInfo.nonEmptyPermyriad());

                                        plugin.getMessageManager().sendPrefixed(sender, LangKey.CMD_PEEK_WARNING);
                                    } else {
                                        final @NotNull Component title = LangKey.TREASURE_TITLE_PEEK_SHARED.create(
                                            PlaceHolder.TREASURE_ID.component(Utils.getDisplayName(container)));

                                        if (container.getInventory().getType() == InventoryType.CHEST) {
                                            nowPeeking = Bukkit.createInventory(wrapper, container.getInventory().getSize(), title);
                                        } else {
                                            nowPeeking = Bukkit.createInventory(wrapper, container.getInventory().getType(), title);
                                        }

                                        // get items left there last time
                                        Utils.setContents(nowPeeking, playerLootDetail.unLootedStuff());
                                        plugin.getMessageManager().sendPrefixed(sender, LangKey.CMD_PEEK_WARNING);
                                    }

                                    plugin.getCommandInventoriesListener().addPeekingTreasure(player.openInventory(nowPeeking),
                                        new PeekedTreasure(null, treasureInfo.treasureId(), Instant.now(), Instant.now()));
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
                                                plugin.getMessageManager().sendPrefixed(sender, LangKey.ARG_NOT_PLAYER.create(
                                                    PlaceHolder.TEXT.string(args[1])));
                                                return;
                                            }
                                        } catch (IllegalArgumentException ignored) {
                                            plugin.getMessageManager().sendPrefixed(sender, LangKey.ARG_NOT_PLAYER.create(
                                                PlaceHolder.TEXT.string(args[1])));
                                            return;
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

                                    final @NotNull Component title = LangKey.TREASURE_TITLE_PEEK_PLAYER.create(
                                        PlaceHolder.PLAYER.component(name),
                                        PlaceHolder.TREASURE_ID.component(Utils.getDisplayName(container)));
                                    final @NotNull Inventory nowPeeking;

                                    if (container.getInventory().getType() == InventoryType.CHEST) {
                                        nowPeeking = Bukkit.createInventory(
                                            new InventoryHolderWrapper<>(
                                                container,
                                                true,
                                                playerLootDetail == null ? null : playerLootDetail.firstLootedInstant()
                                            ),
                                            container.getInventory().getSize(),
                                            title
                                        );
                                    } else {
                                        nowPeeking = Bukkit.createInventory(
                                            new InventoryHolderWrapper<>(
                                                container,
                                                true,
                                                playerLootDetail == null ? null : playerLootDetail.firstLootedInstant()
                                            ),
                                            container.getInventory().getType(),
                                            title
                                        );
                                    }

                                    if ((playerLootDetail == null || playerLootDetail.isEmpty())) {
                                        plugin.getMessageManager().sendPrefixed(sender, LangKey.CMD_PEEK_GENERATE_PLAYER.create(
                                            PlaceHolder.PLAYER.component(name)));

                                        Utils.setContents(nowPeeking, treasureInfo.itemLoot(), treasureInfo.nonEmptyPermyriad());
                                    } else {
                                        Utils.setContents(nowPeeking, playerLootDetail.unLootedStuff());
                                    }

                                    plugin.getCommandInventoriesListener().addPeekingTreasure(player.openInventory(nowPeeking),
                                        new PeekedTreasure(
                                            playerToPeek.getUniqueId(),
                                            treasureInfo.treasureId(),
                                            playerLootDetail == null ? Instant.now() : playerLootDetail.firstLootedInstant(),
                                            playerLootDetail == null ? Instant.now() : playerLootDetail.lastChangedInstant()
                                        )
                                    );
                                    plugin.getMessageManager().sendPrefixed(sender, LangKey.CMD_PEEK_WARNING);
                                });
                            }
                        } else {
                            plugin.getMessageManager().sendPrefixed(sender, LangKey.ERROR_NOT_LOOKING_AT_TREASURE);
                        }
                    });
                } else {
                    plugin.getMessageManager().sendPrefixed(sender, LangKey.ERROR_NOT_LOOKING_AT_CONTAINER);
                }
            } else {
                plugin.getMessageManager().sendPrefixed(sender, LangKey.ERROR_SENDER_NOT_PLAYER);
            }
        } else {
            plugin.getMessageManager().sendPrefixed(sender, LangKey.NO_PERMISSION);
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
