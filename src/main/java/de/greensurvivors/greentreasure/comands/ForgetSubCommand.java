package de.greensurvivors.greentreasure.comands;

import de.greensurvivors.greentreasure.config.TreasureConfig;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.language.Lang;
import de.greensurvivors.greentreasure.listener.TreasureListener;
import de.greensurvivors.greentreasure.permission.Perm;
import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.block.Container;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.LivingEntity;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;

import java.util.*;

import static de.greensurvivors.greentreasure.comands.TreasureCommands.FORGET;

public class ForgetSubCommand {
    private static ForgetSubCommand instance;

    public static ForgetSubCommand inst() {
        if (instance == null)
            instance = new ForgetSubCommand();
        return instance;
    }

    /**
     * the treasure forgets a given somebody has ever looted it
     * /gt forget (takes the commandSender self)
     * /gt forget uuid
     * /gt forget playerName
     *
     * @param commandSender sender of this command
     * @param args          given arguments
     */
    protected void handleForget(CommandSender commandSender, String[] args) {
        if (Perm.hasPermission(commandSender, Perm.TREASURE_ADMIN, Perm.TREASURE_FORGET)) {
            Container container = TreasureCommands.getContainer(commandSender);

            if (container != null) {
                TreasureInfo treasureInfo = TreasureListener.inst().getTreasure(container.getLocation());

                if (treasureInfo != null && treasureInfo.isGlobal()) {
                    commandSender.sendMessage(Lang.build(Lang.FORGET_PLAYER_START.get()));
                    TreasureConfig.inst().forgetPlayerAsync(null, container.getBlock().getLocation(), () ->
                            commandSender.sendMessage(Lang.build(Lang.FORGET_ALL_END.get())));

                    return;
                }

                UUID uuidToForget;
                if (args.length >= 2) {
                    OfflinePlayer offlinePlayer = Bukkit.getOfflinePlayer(args[1]);
                    if (offlinePlayer.hasPlayedBefore()) {
                        uuidToForget = offlinePlayer.getUniqueId();
                    } else {
                        try {
                            uuidToForget = UUID.fromString(args[1]);
                        } catch (IllegalArgumentException ignored) {
                            commandSender.sendMessage(Lang.build(Lang.NO_SUCH_PLAYER.get().replace(Lang.VALUE, args[1])));
                            return;
                        }
                    }
                } else {
                    if (commandSender instanceof LivingEntity livingEntity) {
                        uuidToForget = livingEntity.getUniqueId();
                    } else {
                        commandSender.sendMessage(Lang.build(Lang.NO_PLAYER.get()));
                        return;
                    }
                }

                commandSender.sendMessage(Lang.build(Lang.FORGET_PLAYER_START.get()));
                TreasureConfig.inst().forgetPlayerAsync(uuidToForget, container.getBlock().getLocation(), () ->
                        commandSender.sendMessage(Lang.build(Lang.FORGET_PLAYER_END.get().replace(Lang.UUID, uuidToForget.toString()))));
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
    protected List<String> handleTabCompleate(@NotNull String[] args) {
        switch (args.length) {
            case 1 -> {
                return Collections.singletonList(FORGET);
            }
            case 2 -> {
                if (args[0].equalsIgnoreCase(FORGET)) {
                    Collection<? extends Player> onlinePlayers = Bukkit.getOnlinePlayers();
                    if (!onlinePlayers.isEmpty()) {
                        return onlinePlayers.stream().map(Player::getName).toList();
                    }
                }
            }
        }

        return new ArrayList<>();
    }
}
