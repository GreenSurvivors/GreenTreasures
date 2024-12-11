package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.Utils;
import de.greensurvivors.greentreasure.dataobjects.PeekedTreasure;
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
import org.bukkit.inventory.InventoryView;
import org.bukkit.inventory.ItemStack;
import org.bukkit.permissions.Permissible;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public class PeekSubCommand extends ASubCommand {

    public PeekSubCommand(final @NotNull GreenTreasure plugin) {
        super(plugin);
    }

    @Override
    protected boolean checkPermission(@NotNull Permissible permissible) {
        return permissible.hasPermission(PermmissionManager.TREASURE_PEEK.get());
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
                Container container = plugin.getTreasureCommands().getContainer(sender);

                if (container != null) {
                    final @Nullable String treasureId = plugin.getTreasureListener().getTreasureId(container);

                    if (treasureId != null) {
                        final @Nullable TreasureInfo treasureInfo = plugin.getTreasureListener().getTreasure(treasureId);

                        if (treasureInfo != null) {
                            // unlimited treasure
                            if (treasureInfo.isUnlimited()) {
                                plugin.getMessageManager().sendLang(sender, LangPath.CMD_PEEK_UNLIMITED);
                            }

                            if (treasureInfo.isShared()) {
                                //load shared treasure
                                plugin.getDatabaseManager().getPlayerData(null, treasureId).thenAccept(playerLootDetail -> {
                                    final InventoryView nowPeeking;
                                    long timeStamp = playerLootDetail.lastChangedTimeStamp();

                                    if ((playerLootDetail.unLootedStuff() == null || playerLootDetail.unLootedStuff().isEmpty())) {
                                        plugin.getMessageManager().sendLang(sender, LangPath.CMD_PEEK_GENERATE_SHARED,
                                            Placeholder.component(PlaceHolderKey.NAME.getKey(), plugin.getMessageManager().getLang(LangPath.CMD_LIST_WHO_SHARED)));

                                        nowPeeking = Utils.openInventory(player, container, container.getInventory().getType());

                                        if (nowPeeking == null) {
                                            plugin.getMessageManager().sendLang(sender, LangPath.ERROR_UNKNOWN);

                                            return;
                                        }

                                        int slotChance = treasureInfo.slotChance();
                                        Random random = new Random();

                                        // clone every item stack and put it into the new inventory
                                        nowPeeking.getTopInventory().setContents(treasureInfo.itemLoot().stream().map(s -> s == null || s.isEmpty() ? ItemStack.empty() : random.nextInt(0, 1000) > slotChance ? null : s.clone()).toArray(ItemStack[]::new));

                                        plugin.getMessageManager().sendLang(sender, LangPath.CMD_PEEK_WARNING);
                                    } else {
                                        nowPeeking = Utils.openInventory(player, container, container.getInventory().getType()); //Bukkit.createInventory(null, container.getInventory().getType(), Lang.build(Lang.PEEK_SHARED.get()));

                                        if (nowPeeking == null) {
                                            plugin.getMessageManager().sendLang(sender, LangPath.ERROR_UNKNOWN);

                                            return;
                                        }
                                        // get items left there last time
                                        nowPeeking.getTopInventory().setContents(playerLootDetail.unLootedStuff().toArray(new ItemStack[0]));

                                        plugin.getMessageManager().sendLang(sender, LangPath.CMD_PEEK_WARNING);
                                    }

                                    plugin.getCommandInventoriesListener().addPeekingTreasure(nowPeeking, new PeekedTreasure(null, treasureId, timeStamp));
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

                                plugin.getDatabaseManager().getPlayerData(playerToPeek, treasureId).thenAccept(playerLootDetail -> {
                                    final InventoryView nowPeeking;

                                    if ((playerLootDetail.unLootedStuff() == null || playerLootDetail.unLootedStuff().isEmpty())) {
                                        final @NotNull Component name;
                                        if (playerToPeek.isOnline()) {
                                            name = playerToPeek.getPlayer().displayName();
                                        } else if (playerToPeek.getName() != null) {
                                            name = Component.text(playerToPeek.getName());
                                        } else {
                                            name = Component.text(playerToPeek.getUniqueId().toString());
                                        }

                                        plugin.getMessageManager().sendLang(sender, LangPath.CMD_PEEK_GENERATE_PLAYER,
                                            Placeholder.component(PlaceHolderKey.NAME.getKey(), name));

                                        nowPeeking = Utils.openInventory(player, container, container.getInventory().getType()); //Bukkit.createInventory(null, container.getInventory().getType(), Lang.build(Lang.PEEK_PLAYER.get().replace(Lang.VALUE, playerToPeek.toString())));

                                        if (nowPeeking == null) {
                                            plugin.getMessageManager().sendLang(sender, LangPath.ERROR_UNKNOWN);

                                            return;
                                        }

                                        int slotChance = treasureInfo.slotChance();
                                        Random random = new Random();

                                        // clone every item stack and put it into the new inventory
                                        nowPeeking.getTopInventory().setContents(treasureInfo.itemLoot().stream().map(s -> s == null || s.isEmpty() ? ItemStack.empty() : random.nextInt(0, 1000) > slotChance ? null : s.clone()).toArray(ItemStack[]::new));
                                    } else {
                                        nowPeeking = Utils.openInventory(player, container, container.getInventory().getType()); //Bukkit.createInventory(null, container.getInventory().getType(), Lang.build(Lang.PEEK_PLAYER.get().replace(Lang.VALUE, playerToPeek.toString())));

                                        if (nowPeeking == null) {
                                            plugin.getMessageManager().sendLang(sender, LangPath.ERROR_UNKNOWN);

                                            return;
                                        }

                                        // get items left there last time
                                        nowPeeking.getTopInventory().setContents(playerLootDetail.unLootedStuff().toArray(new ItemStack[0]));
                                    }

                                    plugin.getCommandInventoriesListener().addPeekingTreasure(nowPeeking, new PeekedTreasure(playerToPeek.getUniqueId(), treasureId, playerLootDetail.lastChangedTimeStamp()));
                                    plugin.getMessageManager().sendLang(sender, LangPath.CMD_PEEK_WARNING);
                                });
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
