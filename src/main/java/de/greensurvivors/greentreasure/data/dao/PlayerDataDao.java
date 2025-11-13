package de.greensurvivors.greentreasure.data.dao;

import com.github.f4b6a3.ulid.Ulid;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.sql.DataSource;
import java.sql.*;
import java.util.*;

import static de.greensurvivors.greentreasure.data.dao.TreasureDao.TREASURE_ID_KEY;
import static de.greensurvivors.greentreasure.data.dao.TreasureDao.TREASURE_TABLE;

public class PlayerDataDao extends BaseDao{
    private final static @NotNull String
            USER_TABLE = "user",
            /// player identifier
            PID_KEY = "pid",
            /// uuid of player
            UUID_KEY = "uuid",
            TIMES_LOOTED = "times_looted", // unused for now
            /// last time a player had changed the treasure
            TREASURE_LAST_TIMESTAMP_KEY = "timestamplast",
            TREASURE_FIRST_TIMESTAMP_KEY = "timestampfirst",
            /// the items of this treasure, the player data table as well as the treasure table have the same column name
            TREASURE_CONTENT_KEY = "content",
            PLAYERDATA_TABLE = "playerdata";

    public PlayerDataDao(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    @Override
    public void createTable() throws SQLException {
        String treasureFields = PID_KEY + " INT UNSIGNED NOT NULL, " +
                // is BINARY instead of UUID since SQL instances can not be trusted shifting UUIDs around in order to "optimizing" them;
                // and not be string / char array since byte array is shorter bitwise and therefor faster
                TREASURE_ID_KEY + " BINARY(16) NOT NULL, " +
                TREASURE_FIRST_TIMESTAMP_KEY + " BIGINT UNSIGNED, " +
                TREASURE_LAST_TIMESTAMP_KEY + " BIGINT UNSIGNED, " +
                TREASURE_CONTENT_KEY + " MEDIUMBLOB NOT NULL, " +
                TIMES_LOOTED + " INT UNSIGNED DEFAULT 0, " +
                // important: don't make TREASURE_ID UNIQUE on its own, only one player could have an entry otherwise
                // but also don't let TREASURE_ID without constrains, else wise a player can infinit entries of the same treasure, not updating them
                "PRIMARY KEY (" + PID_KEY + ", " + TREASURE_ID_KEY + "), " +
                "FOREIGN KEY (" + PID_KEY + ") REFERENCES " + USER_TABLE + "(" + PID_KEY + "), " +
                "FOREIGN KEY (" + TREASURE_ID_KEY + ") REFERENCES " + TREASURE_TABLE + "(" + TREASURE_ID_KEY + ")";
        createTable(PLAYERDATA_TABLE, treasureFields);
    }

    public int deletePlayerData(final @NotNull Ulid treasureId) throws SQLException {
        final @NotNull String playerDataStatementStr = "DELETE FROM " + PLAYERDATA_TABLE + " WHERE " + TREASURE_ID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement playerDataPreparedStatement = connection.prepareStatement(playerDataStatementStr);) {
             playerDataPreparedStatement.setBytes(1, treasureId.toBytes());

            return playerDataPreparedStatement.executeUpdate();
        }
    }

    public int setPlayerData(final @NotNull UUID uuid, final @NotNull Ulid treasureId, final @NotNull PlayerLootDetail lootDetail) throws SQLException{
        final @NotNull String statementStr =
                "INSERT INTO " + PLAYERDATA_TABLE + " (" +
                        TREASURE_ID_KEY + ", " +
                        PID_KEY + ", " +
                        TREASURE_FIRST_TIMESTAMP_KEY + ", " +
                        TREASURE_LAST_TIMESTAMP_KEY + ", " +
                        TREASURE_CONTENT_KEY + ") " +
                        "VALUES (" +
                        "?, " +
                        "(SELECT " + PID_KEY + " FROM " + USER_TABLE + " WHERE " + UUID_KEY + " = ?), " +
                        "?, " +
                        "?, " +
                        "?) ON DUPLICATE KEY UPDATE " +
                        TREASURE_FIRST_TIMESTAMP_KEY + " = VALUES(" + TREASURE_FIRST_TIMESTAMP_KEY + "), " +
                        TREASURE_LAST_TIMESTAMP_KEY + " = VALUES(" + TREASURE_LAST_TIMESTAMP_KEY + "), " +
                        TREASURE_CONTENT_KEY + " = VALUES(" + TREASURE_CONTENT_KEY + ")";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            preparedStatement.setBytes(1, treasureId.toBytes());
            preparedStatement.setString(2, uuid.toString());
            preparedStatement.setLong(3, lootDetail.firstLootedTimeStamp());
            preparedStatement.setLong(4, lootDetail.lastChangedTimeStamp());

            final @NotNull Blob blob = connection.createBlob();
            if (lootDetail.unLootedStuff() == null) {
                blob.setBytes(1, new byte[0]);
            } else {
                blob.setBytes(1, ItemStack.serializeItemsAsBytes(lootDetail.unLootedStuff()));
            }
            preparedStatement.setBlob(5, blob);

            final int rowsAffected = preparedStatement.executeUpdate();
            blob.free();
            return rowsAffected;
        }
    }

    public @Nullable PlayerLootDetail getPlayerData(final @NotNull UUID uuid, final @NotNull Ulid treasureId) throws SQLException {
        final @NotNull String statementStr =
                "SELECT p." + TREASURE_LAST_TIMESTAMP_KEY + "," +
                        " p." + TREASURE_FIRST_TIMESTAMP_KEY + "," +
                        " COALESCE(p." + TREASURE_CONTENT_KEY + ", t." + TREASURE_CONTENT_KEY + ") AS " + TREASURE_CONTENT_KEY +
                        " FROM " + PLAYERDATA_TABLE + " AS p" +
                        " JOIN " + USER_TABLE + " AS u" +
                        " ON p." + PID_KEY + " = u." + PID_KEY +
                        " LEFT JOIN " + TREASURE_TABLE + " AS t" +
                        " ON p." + TREASURE_ID_KEY + " = t." + TREASURE_ID_KEY +
                        " WHERE p." + TREASURE_ID_KEY + " = ? AND u." + UUID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
            preparedStatement.setBytes(1, treasureId.toBytes());
            preparedStatement.setString(2, uuid.toString());

            try (final ResultSet resultSet = preparedStatement.executeQuery()) {
                if (resultSet.next()) {
                    final long firstTimeStamp = resultSet.getLong(TREASURE_FIRST_TIMESTAMP_KEY);
                    final long lastTimeStamp = resultSet.getLong(TREASURE_LAST_TIMESTAMP_KEY);

                    //get list from string
                    final @Nullable Blob blob = resultSet.getBlob(TREASURE_CONTENT_KEY);
                    final @Nullable List<ItemStack> items;
                    if (blob == null || blob.length() <= 0) {
                        items = null;
                    } else {
                        items = new ArrayList<>(List.of(ItemStack.deserializeItemsFromBytes(blob.getBytes(1, (int) blob.length()))));
                        blob.free();
                    }

                    return new PlayerLootDetail(firstTimeStamp, lastTimeStamp, items);
                } else { //player had never opened this treasure
                    return null;
                }
            }
        }
    }

    public int forgetAll(final @NotNull Ulid treasureId) throws SQLException{
        final @NotNull String statementStr = "DELETE FROM " + PLAYERDATA_TABLE + " WHERE  " + TREASURE_ID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            preparedStatement.setBytes(1, treasureId.toBytes());
            return preparedStatement.executeUpdate();
        }
    }

    public int forgetPlayer(final @NotNull UUID uuid, final @NotNull Ulid treasureId) throws SQLException{
        final String statementStr =
                "DELETE FROM " + PLAYERDATA_TABLE +
                        " WHERE " + TREASURE_ID_KEY + " = ? " +
                        "AND " + PID_KEY + " = (SELECT " + PID_KEY + " FROM " + USER_TABLE + " WHERE " + UUID_KEY + " = ?)";

        try (final Connection connection = dataSource.getConnection();
             final PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {

            preparedStatement.setBytes(1, treasureId.toBytes());
            preparedStatement.setString(2, uuid.toString());
            return preparedStatement.executeUpdate();
        }
    }

    public @NotNull Map<UUID, @NotNull PlayerLootDetail> getAllPlayerData(final @NotNull Ulid treasureId) throws SQLException {
        final String statementStr =
                "SELECT u." + UUID_KEY + "," +
                        " t." + TREASURE_FIRST_TIMESTAMP_KEY + "," +
                        " t." + TREASURE_LAST_TIMESTAMP_KEY + "," +
                        " t." + TREASURE_CONTENT_KEY +
                        " FROM " + PLAYERDATA_TABLE + " AS t" +
                        " JOIN " + USER_TABLE + " AS u ON t." + PID_KEY + " = u." + PID_KEY +
                        " WHERE t." + TREASURE_ID_KEY + " = ?";

        try (final Connection connection = dataSource.getConnection();
             final PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {

            preparedStatement.setBytes(1, treasureId.toBytes());

            try (final ResultSet resultSet = preparedStatement.executeQuery()) {
                final Map<UUID, PlayerLootDetail> result = new HashMap<>();

                while (resultSet.next()) {
                    final UUID playerUUID = UUID.fromString(resultSet.getString(UUID_KEY));
                    final long firstTimeStamp = resultSet.getLong(TREASURE_FIRST_TIMESTAMP_KEY);
                    final long lastTimeStamp = resultSet.getLong(TREASURE_LAST_TIMESTAMP_KEY);

                    //get list from string
                    final @Nullable Blob blob = resultSet.getBlob(TREASURE_CONTENT_KEY);
                    final @Nullable List<ItemStack> items;
                    if (blob == null || blob.length() <= 0) {
                        items = null;
                    } else {
                        items = new ArrayList<>(List.of(ItemStack.deserializeItemsFromBytes(blob.getBytes(1, (int) blob.length()))));
                        blob.free();
                    }

                    result.put(playerUUID, new PlayerLootDetail(firstTimeStamp, lastTimeStamp, items));
                }

                return result;
            }
        }
    }
}
