package de.greensurvivors.greentreasure.data.dao;

import com.github.f4b6a3.ulid.Ulid;
import com.zaxxer.hikari.HikariDataSource;
import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import de.greensurvivors.greentreasure.dataobjects.refreshInfo.*;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.Range;
import org.jetbrains.annotations.Unmodifiable;

import java.sql.*;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class TreasureData {
    private static final long DEFAULT_FORGET_DURATION_MILLIS = -1L;
    private static final short DEFAULT_SLOT_CHANCE = 10000;
    private static final boolean DEFAULT_IS_UNLIMITED = false, DEFAULT_IS_SHARED = false;

    // database keys
    private final static @NotNull String TREASURE_TABLE = "treasure";
    private static final String
        /// treasure identifier
        TREASURE_ID_KEY = "treasureid",
        TREASURE_CONTENT_KEY = "content",
        TREASURE_REFRESH_DURATION_LEGACY_KEY = "forgetduration",
        TREASURE_REFRESH_DURATION_KEY = "refreshduration",
        TREASURE_REFRESH_START_TIME_KEY = "refreshstart",
        TREASURE_NON_EMPTY_PERMYRIAD_KEY = "nonemptypermyriad",
        TREASURE_UNLIMITED_KEY = "unlimited",
        TREASURE_SHARED_KEY = "shared",
        TREASURE_FIND_FRESH_MESSAGE_OVERRIDE_KEY = "findfreshmessageoverride",
        TREASURE_FIND_LOOTED_MESSAGE_OVERRIDE_KEY = "findlootedmessageoverride";

    private final @NotNull GreenTreasure plugin;
    private final @NotNull HikariDataSource dataSource;

    public TreasureData(final @NotNull GreenTreasure plugin, final @NotNull HikariDataSource dataSource) {
        this.plugin = plugin;
        this.dataSource = dataSource;
    }

    public @Nullable @Unmodifiable TreasureInfo loadTreasureUrgently(final @NotNull Ulid treasureId) throws SQLException {
        final @NotNull String statementStr = "SELECT " +
            TREASURE_CONTENT_KEY + ", " +
            TREASURE_REFRESH_DURATION_KEY + ", " +
            TREASURE_REFRESH_START_TIME_KEY + "," +
            TREASURE_NON_EMPTY_PERMYRIAD_KEY + ", " +
            TREASURE_UNLIMITED_KEY + ", " +
            TREASURE_SHARED_KEY + ", " +
            TREASURE_FIND_FRESH_MESSAGE_OVERRIDE_KEY + ", " +
            TREASURE_FIND_LOOTED_MESSAGE_OVERRIDE_KEY +
            " FROM " + TREASURE_TABLE +
            " WHERE " + TREASURE_ID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
            preparedStatement.setBytes(1, treasureId.toBytes());

            try (final ResultSet resultSet = preparedStatement.executeQuery()) {

                if (resultSet.next()) {
                    final @NotNull Blob blob = resultSet.getBlob(TREASURE_CONTENT_KEY);
                    final long forgetDurationMillis = resultSet.getLong(TREASURE_REFRESH_DURATION_KEY);
                    final @Nullable Long forgetStartMillis = resultSet.getObject(TREASURE_REFRESH_START_TIME_KEY, Long.TYPE);
                    final short nonEmptyPermyriad = resultSet.getShort(TREASURE_NON_EMPTY_PERMYRIAD_KEY);
                    final boolean isUnlimited = resultSet.getBoolean(TREASURE_UNLIMITED_KEY);
                    final boolean isShared = resultSet.getBoolean(TREASURE_SHARED_KEY);
                    final @Nullable String findFreshMessageOverride = resultSet.getString(TREASURE_FIND_FRESH_MESSAGE_OVERRIDE_KEY);
                    final @Nullable String findLootedMessageOverride = resultSet.getString(TREASURE_FIND_LOOTED_MESSAGE_OVERRIDE_KEY);

                    if (blob.length() <= 1) {
                        plugin.getComponentLogger().warn("No or malformed item list found for treasure id {}. Skipping. On thread {}", treasureId, Thread.currentThread().getName());
                        return null;
                    }

                    final @NotNull List<ItemStack> items = List.of(ItemStack.deserializeItemsFromBytes(blob.getBytes(1, (int) blob.length())));
                    blob.free();

                    final @NotNull ARefreshInfo forgetContainer;
                    if (forgetDurationMillis <= 0) {
                        if (forgetStartMillis == null) {
                            forgetContainer = new NoForget(plugin);
                        } else {
                            forgetContainer = new InstantUnlock(plugin, Instant.ofEpochMilli(forgetStartMillis));
                        }
                    } else {
                        if (forgetStartMillis == null) {
                            forgetContainer = new PeriodicForget(plugin, Duration.ofMillis(forgetDurationMillis));
                        } else {
                            forgetContainer = new PeriodicInstantForget(plugin, Instant.ofEpochMilli(forgetStartMillis), Duration.ofMillis(forgetDurationMillis));
                        }
                    }

                    return new TreasureInfo(treasureId, items, forgetContainer, nonEmptyPermyriad, isUnlimited, isShared, findFreshMessageOverride, findLootedMessageOverride);
                } else { // this treasure was deleted / never created
                    plugin.getComponentLogger().debug("got no answer for get request for treasure info {}, on thread {}", treasureId, Thread.currentThread().getName());
                    return null;
                }
            }
        }
    }

    public void setTreasureContents(final @NotNull Ulid treasureId, final @NotNull List<@NotNull ItemStack> contents) throws SQLException {
        final @NotNull String statementStr = "INSERT INTO " + TREASURE_TABLE + "(" +
            TREASURE_ID_KEY + ", " +
            TREASURE_CONTENT_KEY + ") " +
            "VALUES (?, ?) ON DUPLICATE KEY UPDATE " +
            TREASURE_CONTENT_KEY + " = VALUES(" + TREASURE_CONTENT_KEY + ")";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            preparedStatement.setBytes(1, treasureId.toBytes());

            final @NotNull Blob blob = connection.createBlob();
            blob.setBytes(1, ItemStack.serializeItemsAsBytes(contents.toArray(new ItemStack[0])));
            preparedStatement.setBlob(2, blob);

            int rowsAffected = preparedStatement.executeUpdate();
            blob.free();

            plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished set request for treasure {}, on thread {}", rowsAffected, treasureId, Thread.currentThread().getName());
        }
    }

    public @Nullable @Unmodifiable List<ItemStack> getTreasureContents(final @NotNull Ulid treasureId) throws SQLException {
        final @NotNull String statementStr =
            "SELECT " + TREASURE_CONTENT_KEY +
                " FROM " + TREASURE_TABLE +
                " WHERE " + TREASURE_ID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
            preparedStatement.setString(1, treasureId.toString());

            try (final ResultSet resultSet = preparedStatement.executeQuery()) {
                if (resultSet.next()) {
                    //get list from blob
                    final @NotNull Blob blob = resultSet.getBlob(TREASURE_CONTENT_KEY);
                    final @NotNull List<ItemStack> items = List.of(ItemStack.deserializeItemsFromBytes(blob.getBytes(1, (int) blob.length())));
                    blob.free();

                    plugin.getComponentLogger().debug("successfully got treasure contents for get request for treasure id {}, on thread {}", treasureId, Thread.currentThread().getName());
                    return items;
                } else { // this treasure was deleted / never created
                    plugin.getComponentLogger().debug("got no answer for get request for treasure contents {}, on thread {}", treasureId, Thread.currentThread().getName());
                    return null;
                }
            }
        }
    }

    // note: yes it has to return a list and not a set, because its entries have to be addressable by index
    /// guarantees no duplicate Ulids in the returned List
    public @NotNull List<@NotNull Ulid> getTreasureIds() throws SQLException {
        final @NotNull String statementStr = "SELECT " + TREASURE_ID_KEY + " FROM " + TREASURE_TABLE;

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {

            try (final ResultSet resultSet = preparedStatement.executeQuery()) {
                final @NotNull Set<@NotNull Ulid> resultFilter = new HashSet<>();

                while (resultSet.next()) {
                    resultFilter.add(Ulid.from(resultSet.getBytes(TREASURE_ID_KEY)));
                }

                return new ArrayList<>(resultFilter);
            }
        }
    }

    public int deleteTreasure(final @NotNull Ulid treasureId) throws SQLException {
        final @NotNull String treasureStatementStr = "DELETE FROM " + TREASURE_TABLE + " WHERE " + TREASURE_ID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement treasurePreparedStatement = connection.prepareStatement(treasureStatementStr)) {
            treasurePreparedStatement.setBytes(1, treasureId.toBytes());

            return treasurePreparedStatement.executeUpdate();
        }
    }

    public void setRandom(final @NotNull Ulid treasureId, final @Range(from = 0, to = 10000) Short nonEmptyPermyriad) throws SQLException {
        final @NotNull String statementStr = "UPDATE " + TREASURE_TABLE +
            " SET " + TREASURE_NON_EMPTY_PERMYRIAD_KEY + " = ? WHERE " + TREASURE_ID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            preparedStatement.setShort(1, nonEmptyPermyriad);
            preparedStatement.setBytes(2, treasureId.toBytes());

            int rowsAffected = preparedStatement.executeUpdate();

            plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished set slot chance request for treasure {} to {}, on thread {}", rowsAffected, treasureId, nonEmptyPermyriad / 100.0D, Thread.currentThread().getName());
        }
    }

    public void setShared(final @NotNull Ulid treasureId, final boolean isShared) throws SQLException {
        final @NotNull String statementStr = "UPDATE " + TREASURE_TABLE +
            " SET " + TREASURE_SHARED_KEY + " = ? WHERE " + TREASURE_ID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            preparedStatement.setBoolean(1, isShared);
            preparedStatement.setBytes(2, treasureId.toBytes());

            int rowsAffected = preparedStatement.executeUpdate();

            plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished set shared request for treasure {} to {}, on thread {}", rowsAffected, treasureId, isShared, Thread.currentThread().getName());
        }
    }

    public void setUnlimited(final @NotNull Ulid treasureId, final boolean isUnLimited) throws SQLException {
        final @NotNull String statementStr = "UPDATE " + TREASURE_TABLE +
            " SET " + TREASURE_UNLIMITED_KEY + " = ? WHERE " + TREASURE_ID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            preparedStatement.setBoolean(1, isUnLimited);
            preparedStatement.setBytes(2, treasureId.toBytes());

            int rowsAffected = preparedStatement.executeUpdate();

            plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished set unlimited request for treasure {} to {}, on thread {}", rowsAffected, treasureId, isUnLimited, Thread.currentThread().getName());
        }
    }

    public void setForgetDuration(final @NotNull Ulid treasureId, final @Nullable Instant forgetStart, final @Nullable Duration forgettingDuration) throws SQLException {
        final @NotNull String statementStr = "UPDATE " + TREASURE_TABLE + " SET " +
            TREASURE_REFRESH_DURATION_KEY + " = ?, " +
            TREASURE_REFRESH_START_TIME_KEY + " = ? " +
            " WHERE " + TREASURE_ID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            preparedStatement.setLong(1, forgettingDuration == null ? DEFAULT_FORGET_DURATION_MILLIS : forgettingDuration.toMillis());
            preparedStatement.setObject(2, forgetStart == null ? null : forgetStart.toEpochMilli());
            preparedStatement.setBytes(3, treasureId.toBytes());

            int rowsAffected = preparedStatement.executeUpdate();

            plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished set forget duration request for treasure {} to {}, on thread {}", rowsAffected, treasureId, forgettingDuration, Thread.currentThread().getName());
        }
    }

    public void setFindFreshMessageOverride(final @NotNull Ulid treasureId, final @Nullable String findFreshMessageOverride) throws SQLException {
        final @NotNull String statementStr = "UPDATE " + TREASURE_TABLE +
            " SET " + TREASURE_FIND_FRESH_MESSAGE_OVERRIDE_KEY + " = ? WHERE " + TREASURE_ID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            preparedStatement.setString(1, findFreshMessageOverride);
            preparedStatement.setBytes(2, treasureId.toBytes());

            int rowsAffected = preparedStatement.executeUpdate();
            plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished set findFreshMessageOverride request for treasure {} to \"{}\", on thread {}", rowsAffected, treasureId, findFreshMessageOverride, Thread.currentThread().getName());
        }
    }

    public void setFindLootedMessageOverride(final @NotNull Ulid treasureId, final @Nullable String findLootedMessageOverride) throws SQLException {
        final @NotNull String statementStr = "UPDATE " + TREASURE_TABLE +
            " SET " + TREASURE_FIND_LOOTED_MESSAGE_OVERRIDE_KEY + " = ? WHERE " + TREASURE_ID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            preparedStatement.setString(1, findLootedMessageOverride);
            preparedStatement.setBytes(2, treasureId.toBytes());

            int rowsAffected = preparedStatement.executeUpdate();
            plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished set findLootedMessageOverride request for treasure {} to {}, on thread {}", rowsAffected, treasureId, findLootedMessageOverride, Thread.currentThread().getName());
        }
    }

    /**
     * creates missing treasure table
     */
    public void createTableTreasure() {
        final String statementStr = "CREATE TABLE IF NOT EXISTS " + TREASURE_TABLE + " (" +
            TREASURE_ID_KEY + " BINARY(16) PRIMARY KEY, " +
            TREASURE_CONTENT_KEY + " MEDIUMBLOB NOT NULL, " +
            TREASURE_REFRESH_DURATION_KEY + " BIGINT NOT NULL DEFAULT " + DEFAULT_FORGET_DURATION_MILLIS + ", " + // < 0 means no forgetting
            TREASURE_REFRESH_START_TIME_KEY + " BIGINT NULL DEFAULT NULL, " + // even though it is unlikely, using any time instant before Epoch is valid, so we have to use NULL as "not set"
            TREASURE_NON_EMPTY_PERMYRIAD_KEY + " SMALLINT UNSIGNED NOT NULL DEFAULT " + DEFAULT_SLOT_CHANCE + ", " +
            TREASURE_UNLIMITED_KEY + " BOOLEAN NOT NULL DEFAULT " + DEFAULT_IS_UNLIMITED + ", " +
            TREASURE_SHARED_KEY + " BOOLEAN NOT NULL DEFAULT " + DEFAULT_IS_SHARED + ", " +
            TREASURE_FIND_FRESH_MESSAGE_OVERRIDE_KEY + " TEXT, " + // text is nullable with the default being null
            TREASURE_FIND_LOOTED_MESSAGE_OVERRIDE_KEY + " TEXT)";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            preparedStatement.executeUpdate();
        } catch (SQLException e) {
            plugin.getComponentLogger().error("Could not create treasure data table.", e);
        }
    }

    public void updateTableTreasure() {
        try (final @NotNull Connection connection = dataSource.getConnection()) {
            @NotNull String statementStr = "ALTER TABLE " + TREASURE_TABLE + " ";
            boolean changesToMake = false;

            // todo someone smart probably can just call getColumns with a null key for the column key and check the resultset. I didn't understand the JavaDocs nor the result itself...
            try (final @NotNull ResultSet columnsSet = connection.getMetaData().getColumns(connection.getCatalog(), null, TREASURE_TABLE, TREASURE_REFRESH_DURATION_LEGACY_KEY)) {
                if (columnsSet.next()) {
                    statementStr += "RENAME COLUMN " + TREASURE_REFRESH_DURATION_LEGACY_KEY + " TO " + TREASURE_REFRESH_DURATION_KEY;
                    changesToMake = true;
                }
            }

            try (final @NotNull ResultSet columnsSet = connection.getMetaData().getColumns(connection.getCatalog(), null, TREASURE_TABLE, TREASURE_REFRESH_START_TIME_KEY)) {
                if (!columnsSet.next()) {
                    if (changesToMake) {
                        statementStr += ", ";
                    }

                    statementStr += "ADD " + TREASURE_REFRESH_START_TIME_KEY + " BIGINT NULL DEFAULT NULL";
                    changesToMake = true;
                }
            }

            if (changesToMake) {
                try (final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
                    preparedStatement.executeUpdate();

                    plugin.getComponentLogger().info("Amended table " + TREASURE_TABLE);
                }
            } else {
                plugin.getComponentLogger().debug("Table " + TREASURE_TABLE + " is up to date!");
            }
        } catch (final @NotNull SQLException e) {
            plugin.getComponentLogger().error("Could not amend treasure data table.", e);
        }
    }
}
