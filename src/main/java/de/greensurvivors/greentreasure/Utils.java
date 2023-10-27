package de.greensurvivors.greentreasure;

import org.bukkit.Location;
import org.jetbrains.annotations.Nullable;

import java.util.regex.Pattern;

public class Utils {
    // conversion constants for a period of time into milliseconds
    public static final long
            YEAR_IN_MILLISECONDS = 31557600000L,
            WEAK_IN_MILLISECONDS = 604800000L,
            DAY_IN_MILLISECONDS = 86400000L,
            HOUR_IN_MILLISECONDS = 3600000L,
            MINUTE_IN_MILLISECONDS = 60000L,
            SECONDS_IN_MILLISECONDS = 1000L,
            TICKS_IN_MILLISECONDS = 50L;
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

        if (toTest.isEmpty() || toTest.isBlank()) { //empty
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

        if (toTest.isEmpty() || toTest.isBlank()) { //empty
            return false;
        }

        return FLOAT_PATTERN.matcher(toTest).find();
    }

    /**
     * clears a location of its pitch, yaw and turns it to bock locations
     *
     * @param locationIn location to be cleaned
     * @return simplified location or null if the input location was also null
     */
    public static @Nullable Location cleanLocation(@Nullable Location locationIn) {
        if (locationIn == null) {
            return null;
        }

        Location locationOut = locationIn.toBlockLocation();
        locationOut.setPitch(0.0f);
        locationOut.setYaw(0.0f);

        return locationOut;
    }
}
