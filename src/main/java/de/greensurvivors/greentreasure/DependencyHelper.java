package de.greensurvivors.greentreasure;

import com.sk89q.worldedit.bukkit.BukkitAdapter;
import com.sk89q.worldguard.WorldGuard;
import com.sk89q.worldguard.bukkit.event.block.UseBlockEvent;
import com.sk89q.worldguard.protection.flags.Flag;
import com.sk89q.worldguard.protection.flags.StateFlag;
import com.sk89q.worldguard.protection.flags.registry.FlagConflictException;
import com.sk89q.worldguard.protection.flags.registry.FlagRegistry;
import com.sk89q.worldguard.protection.regions.RegionContainer;
import com.sk89q.worldguard.protection.regions.RegionQuery;
import org.bukkit.Bukkit;
import org.bukkit.block.Block;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.inventory.InventoryOpenEvent;
import org.bukkit.event.player.PlayerInteractEvent;
import org.bukkit.persistence.PersistentDataHolder;
import org.bukkit.plugin.Plugin;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class DependencyHelper {
    private final @NotNull GreenTreasure plugin;
    private final boolean worldGuardFound;
    private boolean worldGuardEnabled;
    private final @Nullable ClassAbstraction abstraction;

    public DependencyHelper(@NotNull GreenTreasure plugin) {
        this.plugin = plugin;

        worldGuardFound = Bukkit.getPluginManager().getPlugin("WorldGuard") != null;
        checkWorldGuardEnabled();

        if (worldGuardFound && !worldGuardEnabled) {
            abstraction = new ClassAbstraction();
        } else {
            abstraction = null;
        }
    }

    private void checkWorldGuardEnabled() {
        if (worldGuardFound) {
            final @Nullable Plugin worldGuardPlugin = Bukkit.getPluginManager().getPlugin("WorldGuard");

            if (worldGuardPlugin != null && worldGuardPlugin.isEnabled()) {
                worldGuardEnabled = true;
                return;
            }
        }

        worldGuardEnabled = false;
    }

    // this clas only gets instanced once it's safe to use WorldGuard classes, else wise we will run into ClassNotFoundException
    private class ClassAbstraction implements Listener {
        private @Nullable StateFlag overrideProtection = null;

        // worldguard internal event, NOT part of API.
        // worldguard fires this event, so it goes
        // original event start --> worldguard handling the event and then fires the UseBlockEvent -->
        // we listen as early as possible to the UseBlockEvent and allowing the original event to pass -->
        // no worldguard listener should cancel the original event -->
        // original event gets handled by every plugin between -->
        // if not canceled we finally handle the original event ourselves below
        // of course this does not just include the PlayerInteractEvent,
        // but also InventoryOpenEvent, BlockDamageEvent (for cakes), EntityInteractEvent, PlayerBedEnterEvent,
        // PlayerTakeLecternBookEvent, CauldronLevelChangeEvent, BlockDispenseEvent (for some reason) and PlayerOpenSignEvent
        // (worldguard version 7.0.9, 2024)
        // we only allow the use of blocks when we do handle the original event,
        // but it might be a good inspiration to support more block types
        @EventHandler(priority = EventPriority.LOWEST, ignoreCancelled = true)
        private void onWorldGuardUseBlockEvent(final @NotNull UseBlockEvent event) {
            if (event.getOriginalEvent() instanceof PlayerInteractEvent || event.getOriginalEvent() instanceof InventoryOpenEvent) {
                for (final @NotNull Block block : event.getBlocks()) { // this means WE have to check every interacted block at least twice! I recommend to turn on caching if using in combination with world guard
                    if (block.getState() instanceof PersistentDataHolder persistentDataHolder) {
                        final @NotNull RegionContainer container = WorldGuard.getInstance().getPlatform().getRegionContainer();
                        final @NotNull RegionQuery query = container.createQuery();

                        if (!query.testState(BukkitAdapter.adapt(block.getLocation()), null, overrideProtection)) {
                            if (plugin.getTreasureManager().getTreasureInfo(persistentDataHolder) != null) {
                                // allow the interaction of this block
                                event.setAllowed(true);
                            }
                        }
                    }
                }
            }
        }

        private void registerFlag() {
            Bukkit.getPluginManager().registerEvents(this, plugin);

            final @NotNull FlagRegistry registry = WorldGuard.getInstance().getFlagRegistry();

            try {
                // create a flag with the name "treasures-override-protection", defaulting to false
                final @NotNull StateFlag flag = new StateFlag("treasures-override-protection", false);
                registry.register(flag);
                overrideProtection = flag; // only set our field if there was no error
            } catch (FlagConflictException e) {
                // some other plugin registered a flag by the same name already.
                // you can use the existing flag, but this may cause conflicts - be sure to check type
                final @Nullable Flag<?> existing = registry.get("treasures-override-protection");
                if (existing instanceof StateFlag) {
                    overrideProtection = (StateFlag) existing;
                } else {
                    // types don't match - this is bad news! some other plugin conflicts
                    // hopefully this never actually happens

                    plugin.getComponentLogger().error("Could not register overriding protection worldguard flag!", e);
                }
            }
        }
    }
}
