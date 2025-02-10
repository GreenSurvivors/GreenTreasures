package de.greensurvivors.greentreasure;

import net.kyori.adventure.text.Component;
import org.bukkit.block.Container;
import org.bukkit.block.DoubleChest;
import org.bukkit.inventory.DoubleChestInventory;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryHolder;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.Range;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.*;
import java.util.function.Function;
import java.util.random.RandomGenerator;
import java.util.random.RandomGeneratorFactory;
import java.util.regex.Pattern;

public class Utils {
    private static final Pattern INT_PATTERN = Pattern.compile("^[+-]?\\d+$");
    // Since paperclip doesn't allow access to the classloader of the app,
    // we may or may not have access to RandomGeneratorFactory.getDefault().
    // that's why we are filter all available our own.
    private static final @NotNull BigInteger MAX_PERIOD = new BigDecimal("1E70").toBigInteger();
    public static final @NotNull RandomGenerator RANDOM_GENERATOR = RandomGeneratorFactory.all().
        filter(fact -> fact.stateBits() >= 128).
        // don't go overboard, we don't need the big ones
            filter(fact -> fact.stateBits() < 384).
        filter(fact -> fact.period().compareTo(MAX_PERIOD) < 1).
        // use legacy (< java 17) last since they are slow and insecure, except SecureRandom, witch is extra slow, only if we really have to
            min(new BooleanComparator<RandomGeneratorFactory<RandomGenerator>>(fact -> fact.group().equalsIgnoreCase("Legacy")).
            // prefer hardware accelerated RNGs
                thenComparing(new BooleanComparator<>(RandomGeneratorFactory::isHardware)).
            // note: the period comparing is inverse, the biggest one will get sorted first!
                thenComparing((f, g) -> g.period().compareTo(f.period())).
            // if everything else is satisfied, we may as well prefer a stochastic one
                thenComparing(new BooleanComparator<>(RandomGeneratorFactory::isStochastic)).
            // last we compare by name, to be consistent between startups, since default ordering isn't guaranteed
                thenComparing(RandomGeneratorFactory::name)).
        // fallback if everything fails
            orElse(RandomGeneratorFactory.of("Random")).
        create();

    private Utils() {
        throw new RuntimeException("Do not try to instantiate this");
    }

    @SuppressWarnings("DataFlowIssue") // since it is a block, it has a block translation key.
    public static @NotNull Component getDisplayName(final @NotNull Container container) {
        return Objects.requireNonNullElse(container.customName(), Component.translatable(container.getBlock().getType().getBlockTranslationKey()));
    }

    /// clone every item stack and put it into the new inventory
    public static void setContents(final @NotNull Inventory inventory, @NotNull List<@Nullable ItemStack> items) {
        final int inventorySize = inventory.getSize();
        final @NotNull ItemStack[] newContents = new ItemStack[inventorySize];

        for (int i = 0, itemsSize = items.size(); i < itemsSize && i < inventorySize; i++) {
            @Nullable ItemStack itemStack = items.get(i);

            if (itemStack == null || itemStack.isEmpty()) {
                newContents[i] = ItemStack.empty();
            } else {
                newContents[i] = itemStack.clone();
            }
        }

        // fill missing slots, this is technically obsolete, since the Inventory should replace missing item slots with empty ones.
        // but just to be extra safe we do it beforehand.
        for (int i = items.size(); i < inventorySize; i++) {
            newContents[i] = ItemStack.empty();
        }

        inventory.setContents(newContents);
    }


    /// clone every item stack and put it into the new inventory
    public static void setContents(final @NotNull Inventory inventory, @NotNull List<@Nullable ItemStack> items, final @Range(from = 0, to = 10000) int nonEmptyPermyriad) {
        final int inventorySize = inventory.getSize();
        final @Nullable ItemStack @NotNull [] newContents = new ItemStack[inventorySize];

        final @NotNull List<@NotNull ItemToSlot> itemsToSlot = new ArrayList<>();

        final int maxSize = Math.min(items.size(), inventorySize);
        if (nonEmptyPermyriad > 0 && nonEmptyPermyriad < 10000) {

            // get all non-empty items, shuffle them and add the needed amount back at the right slot
            for (int i1 = 0; i1 < maxSize; i1++) {
                final @Nullable ItemStack itemStack = items.get(i1);
                if (itemStack != null && !itemStack.isEmpty()) {
                    itemsToSlot.add(new ItemToSlot(itemStack, i1));
                }
            }

            Collections.shuffle(itemsToSlot, RANDOM_GENERATOR);

            final int amountOfItemsNeeded = Math.round(nonEmptyPermyriad * itemsToSlot.size() / 10000f);
            for (int i = 0; i < amountOfItemsNeeded ; i++) {
                final @NotNull ItemToSlot itemToSlot = itemsToSlot.get(i);

                newContents[itemToSlot.slot] = itemToSlot.itemStack.clone();
            }
        } else {
            for (int i = 0 ; i < maxSize; i++) {
                final @Nullable ItemStack itemStack = items.get(i);

                newContents[i] = itemStack == null ? ItemStack.empty() : itemStack.clone();
            }
        }

        // fill missing slots, this is technically obsolete, since the Inventory should replace missing item slots with empty ones.
        // but just to be extra safe we do it beforehand.
        for (int i = 0; i < inventorySize; i++) {
            if (newContents[i] == null) {
                newContents[i] = ItemStack.empty();
            }
        }

        inventory.setContents(newContents);
    }

    /// double chests are wierd.
    @Contract("null -> null; !null -> !null")
    public static @Nullable InventoryHolder getTreasureHolder (final @Nullable InventoryHolder holder) {
        if (holder == null) {
            return null;
        } else if (holder.getInventory() instanceof DoubleChestInventory doubleChestInventory &&
            ((DoubleChest)doubleChestInventory.getHolder(false)).getLeftSide(false) instanceof InventoryHolder leftSide) {
            return leftSide;
        } else {
            return holder;
        }
    }

    /**
     * Test if a String can safely convert into an int
     *
     * @param toTest String input
     * @return true if the pattern matches and is neither null nor empty
     */
    public static boolean isInt(@Nullable String toTest) {
        if (toTest == null) {
            return false;
        }

        if (toTest.isBlank()) {
            return false;
        }

        return INT_PATTERN.matcher(toTest).matches();
    }

    private record BooleanComparator<T>(@NotNull Function<T, Boolean> function) implements Comparator<T> {
        @Override
        public int compare(T o1, T o2) {
            if (function.apply(o1)) {
                return function.apply(o2) ? 0 : -1;
            } else {
                return function.apply(o2) ? 1 : 0;
            }
        }
    }

    private record ItemToSlot(@NotNull ItemStack itemStack, int slot){
    }
}
