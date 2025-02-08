package de.greensurvivors.greentreasure;

import net.kyori.adventure.text.Component;
import org.bukkit.block.Container;
import org.bukkit.block.DoubleChest;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryHolder;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.Range;

import java.util.List;
import java.util.Objects;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;
import java.util.regex.Pattern;

public class Utils {
    private static final Pattern INT_PATTERN = Pattern.compile("^[+-]?\\d+$");

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
    public static void setContents(final @NotNull Inventory inventory, @NotNull List<@Nullable ItemStack> items, final @Range(from = 0, to = 10000) int slotChance) {
        final int inventorySize = inventory.getSize();
        final @NotNull ItemStack[] newContents = new ItemStack[inventorySize];
        final @NotNull Random random = ThreadLocalRandom.current();

        for (int i = 0, itemsSize = items.size(); i < itemsSize && i < inventorySize; i++) {
            @Nullable ItemStack itemStack = items.get(i);

            if (itemStack == null || itemStack.isEmpty() ||
                random.nextInt(0, 10000) > slotChance) {
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

    /// double chests are wierd.
    @Contract("null -> null; !null -> !null")
    public static @Nullable InventoryHolder getTreasureHolder (final @Nullable InventoryHolder holder) {
        if (holder instanceof DoubleChest doubleChest && doubleChest.getLeftSide() != null) {
            return doubleChest.getLeftSide();
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
}
