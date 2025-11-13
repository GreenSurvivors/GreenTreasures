package de.greensurvivors.greentreasure.data.dao;

import com.github.f4b6a3.ulid.Ulid;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import org.apache.commons.collections4.list.SetUniqueList;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.Range;

import javax.sql.DataSource;
import java.sql.*;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;

public class TreasureDao extends BaseDao {
    public final static @NotNull String
            TREASURE_TABLE = "treasure",
            /// treasure identifier
            TREASURE_ID_KEY = "treasureid",
    /// the items of this treasure, the player data table as well as the treasure table have the same column name
            TREASURE_CONTENT_KEY = "content",
            TREASURE_SHARED_KEY = "shared",
            TREASURE_UNLIMITED_KEY = "unlimited",
            TREASURE_FORGET_DURATION_KEY = "forgetduration",
            TREASURE_FIND_FRESH_MESSAGE_OVERRIDE_KEY = "findfreshmessageoverride",
            TREASURE_FIND_LOOTED_MESSAGE_OVERRIDE_KEY = "findlootedmessageoverride",
            TREASURE_NON_EMPTY_PERMYRIAD_KEY = "nonemptypermyriad";

    private static final short DEFAULT_SLOT_CHANCE = 10000;
    private static final boolean DEFAULT_IS_UNLIMITED = false, DEFAULT_IS_SHARED = false;
    private static final long DEFAULT_FORGET_DURATION_MILLIS = -1L;

    public TreasureDao(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    public static class MalformedItemListException extends Exception {}

    @Override
    public void createTable() throws SQLException {
        String treasureFields = TREASURE_ID_KEY + " BINARY(16) PRIMARY KEY, " +
                TREASURE_CONTENT_KEY + " MEDIUMBLOB NOT NULL, " +
                TREASURE_FORGET_DURATION_KEY + " BIGINT NOT NULL DEFAULT " + DEFAULT_FORGET_DURATION_MILLIS + ", " + // < 0 means no forgetting
                TREASURE_NON_EMPTY_PERMYRIAD_KEY + " SMALLINT UNSIGNED NOT NULL DEFAULT " + DEFAULT_SLOT_CHANCE + ", " +
                TREASURE_UNLIMITED_KEY + " BOOLEAN NOT NULL DEFAULT " + DEFAULT_IS_UNLIMITED + ", " +
                TREASURE_SHARED_KEY + " BOOLEAN NOT NULL DEFAULT " + DEFAULT_IS_SHARED + ", " +
                TREASURE_FIND_FRESH_MESSAGE_OVERRIDE_KEY + " TEXT, " + // test is nullable with the default being null
                TREASURE_FIND_LOOTED_MESSAGE_OVERRIDE_KEY + " TEXT";
        createTable(TREASURE_TABLE, treasureFields);
    }

    public int setTreasureContents(final @NotNull Ulid treasureId, final @NotNull List<@NotNull ItemStack> contents) throws SQLException {
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
            return rowsAffected;
        }
    }

    public @Nullable List<ItemStack> getTreasureContents(final @NotNull String treasureId) throws SQLException {
        final @NotNull String statementStr =
                "SELECT " + TREASURE_CONTENT_KEY +
                        " FROM " + TREASURE_TABLE +
                        " WHERE " + TREASURE_ID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
            preparedStatement.setString(1, treasureId);

            try (final ResultSet resultSet = preparedStatement.executeQuery()) {
                if (resultSet.next()) {
                    //get list from blob
                    final @NotNull Blob blob = resultSet.getBlob(TREASURE_CONTENT_KEY);
                    final @NotNull List<ItemStack> items = new ArrayList<>(List.of(ItemStack.deserializeItemsFromBytes(blob.getBytes(1, (int) blob.length()))));
                    blob.free();
                    return items;
                }
            }
        }
        return null;
    }

    public int deleteTreasure(final @NotNull Ulid treasureId) throws SQLException{
        final @NotNull String treasureStatementStr = "DELETE FROM " + TREASURE_TABLE + " WHERE " + TREASURE_ID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement treasurePreparedStatement = connection.prepareStatement(treasureStatementStr)) {
            treasurePreparedStatement.setBytes(1, treasureId.toBytes());

            return treasurePreparedStatement.executeUpdate();
        }
    }

    public int setRandom(final @NotNull Ulid treasureId, final @Range(from = 0, to = 10000) Short nonEmptyPermyriad) throws SQLException{
        final @NotNull String statementStr = "UPDATE " + TREASURE_TABLE +
                " SET " + TREASURE_NON_EMPTY_PERMYRIAD_KEY + " = ? WHERE " + TREASURE_ID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            preparedStatement.setShort(1, nonEmptyPermyriad);
            preparedStatement.setBytes(2, treasureId.toBytes());

            return preparedStatement.executeUpdate();
        }
    }

    public int setShared(final @NotNull Ulid treasureId, final boolean isShared) throws SQLException {
        final @NotNull String statementStr = "UPDATE " + TREASURE_TABLE +
                " SET " + TREASURE_SHARED_KEY + " = ? WHERE " + TREASURE_ID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            preparedStatement.setBoolean(1, isShared);
            preparedStatement.setBytes(2, treasureId.toBytes());

            return preparedStatement.executeUpdate();
        }
    }

    public int setUnlimited(final @NotNull Ulid treasureId, final boolean isUnLimited) throws SQLException{
        final @NotNull String statementStr = "UPDATE " + TREASURE_TABLE +
                " SET " + TREASURE_UNLIMITED_KEY + " = ? WHERE " + TREASURE_ID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            preparedStatement.setBoolean(1, isUnLimited);
            preparedStatement.setBytes(2, treasureId.toBytes());

            return preparedStatement.executeUpdate();
        }
    }

    public int setForgetDuration(final @NotNull Ulid treasureId, final @Nullable Duration forgettingDuration) throws SQLException {
        final @NotNull String statementStr = "UPDATE " + TREASURE_TABLE +
                " SET " + TREASURE_FORGET_DURATION_KEY + " = ? WHERE " + TREASURE_ID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            preparedStatement.setLong(1, forgettingDuration == null ? DEFAULT_FORGET_DURATION_MILLIS : forgettingDuration.toMillis());
            preparedStatement.setBytes(2, treasureId.toBytes());

            return preparedStatement.executeUpdate();
        }
    }

    public int setFindFreshMessageOverride(final @NotNull Ulid treasureId, final @Nullable String findFreshMessageOverride) throws SQLException {
        final @NotNull String statementStr = "UPDATE " + TREASURE_TABLE +
                " SET " + TREASURE_FIND_FRESH_MESSAGE_OVERRIDE_KEY + " = ? WHERE " + TREASURE_ID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            preparedStatement.setString(1, findFreshMessageOverride);
            preparedStatement.setBytes(2, treasureId.toBytes());

            return preparedStatement.executeUpdate();
        }
    }

    public int setFindLootedMessageOverride(final @NotNull Ulid treasureId, final @Nullable String findLootedMessageOverride) throws SQLException {
        final @NotNull String statementStr = "UPDATE " + TREASURE_TABLE +
                " SET " + TREASURE_FIND_LOOTED_MESSAGE_OVERRIDE_KEY + " = ? WHERE " + TREASURE_ID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            preparedStatement.setString(1, findLootedMessageOverride);
            preparedStatement.setBytes(2, treasureId.toBytes());

            return preparedStatement.executeUpdate();
        }
    }

    public @Nullable TreasureInfo loadTreasureUrgently(final @NotNull Ulid treasureId) throws SQLException, MalformedItemListException{
        final @NotNull String statementStr = "SELECT " +
                TREASURE_CONTENT_KEY + ", " +
                TREASURE_FORGET_DURATION_KEY + ", " +
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
                    final long forgetDurationMillis = resultSet.getLong(TREASURE_FORGET_DURATION_KEY);
                    final short nonEmptyPermyriad = resultSet.getShort(TREASURE_NON_EMPTY_PERMYRIAD_KEY);
                    final boolean isUnlimited = resultSet.getBoolean(TREASURE_UNLIMITED_KEY);
                    final boolean isShared = resultSet.getBoolean(TREASURE_SHARED_KEY);
                    final @Nullable String findFreshMessageOverride = resultSet.getString(TREASURE_FIND_FRESH_MESSAGE_OVERRIDE_KEY);
                    final @Nullable String findLootedMessageOverride = resultSet.getString(TREASURE_FIND_LOOTED_MESSAGE_OVERRIDE_KEY);

                    if (blob.length() <= 1) {
                        throw new MalformedItemListException();
                    }

                    final @NotNull List<ItemStack> items = new ArrayList<>(List.of(ItemStack.deserializeItemsFromBytes(blob.getBytes(1, (int) blob.length()))));
                    blob.free();

                    return new TreasureInfo(treasureId, items, Duration.ofMillis(forgetDurationMillis), nonEmptyPermyriad, isUnlimited, isShared, findFreshMessageOverride, findLootedMessageOverride);
                } else { // this treasure was deleted / never created
                    return null;
                }
            }
        }
    }

    public @NotNull SetUniqueList<@NotNull Ulid> getTreasureIds() throws SQLException {
        final @NotNull String statementStr = "SELECT " + TREASURE_ID_KEY + " FROM " + TREASURE_TABLE;

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {

            try (final ResultSet resultSet = preparedStatement.executeQuery()) {
                final @NotNull SetUniqueList<@NotNull Ulid> resultList = SetUniqueList.setUniqueList(new ArrayList<>());

                while (resultSet.next()) {
                    resultList.add(Ulid.from(resultSet.getBytes(TREASURE_ID_KEY)));
                }
                return resultList;
            }
        }
    }


}
