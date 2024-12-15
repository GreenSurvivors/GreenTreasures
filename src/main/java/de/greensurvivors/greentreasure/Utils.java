package de.greensurvivors.greentreasure;

import net.kyori.adventure.text.Component;
import org.bukkit.block.Container;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.Range;

import java.util.List;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;
import java.util.regex.Pattern;

public class Utils {
    final static String Digits = "(\\p{Digit}+)";
    final static String HexDigits = "(\\p{XDigit}+)";
    // an exponent is 'e' or 'E' followed by an optionally
    // signed decimal integer.
    final static String Exp = "[eE][+-]?" + Digits;
    final static String fpRegex =
        ("[\\x00-\\x20]*" + // Optional leading "whitespace"
            "[+-]?(" +         // Optional sign character
            //"NaN|" +           // "NaN" string
            //"Infinity|" +      // "Infinity" string

            // A decimal floating-point string representing a finite positive
            // number without a leading sign has at most five basic pieces:
            // Digits . Digits ExponentPart FloatTypeSuffix
            //
            // Since this method allows integer-only strings as input
            // in addition to strings of floating-point literals, the
            // two sub-patterns below are simplifications of the grammar
            // productions from the Java Language Specification, 2nd
            // edition, section 3.10.2.

            // Digits ._opt Digits_opt ExponentPart_opt FloatTypeSuffix_opt
            "(((" + Digits + "(\\.)?(" + Digits + "?)(" + Exp + ")?)|" +

            // . Digits ExponentPart_opt FloatTypeSuffix_opt
            "(\\.(" + Digits + ")(" + Exp + ")?)|" +

            // Hexadecimal strings
            "((" +
            // 0[xX] HexDigits ._opt BinaryExponent FloatTypeSuffix_opt
            "(0[xX]" + HexDigits + "(\\.)?)|" +

            // 0[xX] HexDigits_opt . HexDigits BinaryExponent FloatTypeSuffix_opt
            "(0[xX]" + HexDigits + "?(\\.)" + HexDigits + ")" +

            ")[pP][+-]?" + Digits + "))" +
            "[fFdD]?))" +
            "[\\x00-\\x20]*");// Optional trailing "whitespace"
    private static final Pattern FLOAT_PATTERN = Pattern.compile(fpRegex);
    private static final Pattern INT_PATTERN = Pattern.compile("^[+-]?\\d+$");

    private Utils () {
        throw new RuntimeException("Do not try to instantiate this");
    }

    public static @NotNull Component getDisplayName(final @NotNull Container container) {
        return container.customName() == null ?
            Component.translatable(container.getBlock().getType().getBlockTranslationKey()) :
            container.customName();
    }

    /// clone every item stack and put it into the new inventory
    public static void setContents(final @NotNull Inventory inventory, @NotNull List<@Nullable ItemStack> items){
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
    public static void setContents(final @NotNull Inventory inventory, @NotNull List<@Nullable ItemStack> items, final @Range(from = 0, to = 10000) int slotChance){
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

        return INT_PATTERN.matcher(toTest).find();
    }

    /**
     * Test if a String can safely convert into a double
     *
     * @param toTest String input
     * @return true if the pattern matches and is neither null nor empty
     */
    public static boolean isDouble(String toTest) {
        if (toTest == null) {
            return false;
        }

        if (toTest.isBlank()) {
            return false;
        }

        return FLOAT_PATTERN.matcher(toTest).find();
    }
}
