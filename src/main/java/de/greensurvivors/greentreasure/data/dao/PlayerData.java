package de.greensurvivors.greentreasure.data.dao;

import com.github.f4b6a3.ulid.Ulid;
import com.zaxxer.hikari.HikariDataSource;
import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import org.bukkit.Bukkit;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.sql.*;
import java.time.Instant;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public class PlayerData {
    // database keys
    private static final @NotNull String PLAYERDATA_TABLE = "playerdata";
    private final static @NotNull String
        /// player identifier
        PID_KEY = "pid",
        TIMES_LOOTED = "times_looted", // unused for now
        /// treasure identifier
        TREASURE_ID_KEY = "treasureid",
        /// last time a player had changed the treasure
        TREASURE_LAST_CHANGED_INSTANT_KEY = "timestamplast",
        TREASURE_FIRST_LOOTED_INSTANT_KEY = "timestampfirst",
        TREASURE_CONTENT_KEY = "content";

    private final @NotNull GreenTreasure plugin;
    private final @NotNull HikariDataSource dataSource;

    public PlayerData(final @NotNull GreenTreasure plugin, final @NotNull HikariDataSource dataSource) {
        this.plugin = plugin;
        this.dataSource = dataSource;
    }

    public void setPlayerData(final int playerId, final @NotNull Ulid treasureId, final @NotNull PlayerLootDetail lootDetail) throws SQLException {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        final @NotNull String statementStr =
            "INSERT INTO " + PLAYERDATA_TABLE + " (" +
                TREASURE_ID_KEY + ", " +
                PID_KEY + ", " +
                TREASURE_FIRST_LOOTED_INSTANT_KEY + ", " +
                TREASURE_LAST_CHANGED_INSTANT_KEY + ", " +
                TREASURE_CONTENT_KEY + ") " +
                "VALUES (" +
                "?, " +
                "?, " +
                "?, " +
                "?, " +
                "?) ON DUPLICATE KEY UPDATE " +
                TREASURE_FIRST_LOOTED_INSTANT_KEY + " = VALUES(" + TREASURE_FIRST_LOOTED_INSTANT_KEY + "), " +
                TREASURE_LAST_CHANGED_INSTANT_KEY + " = VALUES(" + TREASURE_LAST_CHANGED_INSTANT_KEY + "), " +
                TREASURE_CONTENT_KEY + " = VALUES(" + TREASURE_CONTENT_KEY + ")";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            preparedStatement.setBytes(1, treasureId.toBytes());
            preparedStatement.setInt(2, playerId);
            preparedStatement.setLong(3, lootDetail.firstLootedInstant().toEpochMilli());
            preparedStatement.setLong(4, lootDetail.lastChangedInstant().toEpochMilli());

            final @NotNull Blob blob = connection.createBlob();
            if (lootDetail.unLootedStuff() == null) {
                blob.setBytes(1, new byte[0]);
            } else {
                blob.setBytes(1, ItemStack.serializeItemsAsBytes(lootDetail.unLootedStuff()));
            }
            preparedStatement.setBlob(5, blob);

            final int rowsAffected = preparedStatement.executeUpdate();
            blob.free();
            Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));

            plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished set request for player id {} at timestamp {}, on thread {}",
                rowsAffected, playerId, lootDetail.lastChangedInstant(), Thread.currentThread().getName());
        }
    }

    /**
     * Returns the known information about a treasure looted by a player
     *
     * @param playerId   player id who we want data of
     * @param treasureId identifier of the treasure
     * @return PlayerLootDetail or null, if getting the data wasn't successfully
     * (like in cases if the player never opened this treasure)
     */
    public @Nullable PlayerLootDetail getPlayerData(final int playerId, final @NotNull Ulid treasureId) throws SQLException {
        final @NotNull String statementStr =
            "SELECT p." + TREASURE_LAST_CHANGED_INSTANT_KEY + "," +
                " p." + TREASURE_FIRST_LOOTED_INSTANT_KEY + "," +
                " p." + TREASURE_CONTENT_KEY +
                " FROM " + PLAYERDATA_TABLE + " AS p" +
                " WHERE p." + TREASURE_ID_KEY + " = ? AND p." + PID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
            preparedStatement.setBytes(1, treasureId.toBytes());
            preparedStatement.setInt(2, playerId);

            try (final ResultSet resultSet = preparedStatement.executeQuery()) {
                if (resultSet.next()) {
                    final @NotNull Instant firstLootedInstant = Instant.ofEpochMilli(resultSet.getLong(TREASURE_FIRST_LOOTED_INSTANT_KEY));
                    final @NotNull Instant lastChangedInstant = Instant.ofEpochMilli(resultSet.getLong(TREASURE_LAST_CHANGED_INSTANT_KEY));

                    //get list from string
                    final @Nullable Blob blob = resultSet.getBlob(TREASURE_CONTENT_KEY);
                    final @Nullable List<ItemStack> items;
                    if (blob == null || blob.length() <= 0) {
                        items = null;
                    } else {
                        items = List.of(ItemStack.deserializeItemsFromBytes(blob.getBytes(1, (int) blob.length())));
                        blob.free();
                    }

                    plugin.getComponentLogger().debug("successfully got data for getPlayerData request for player id {}: {}, on thread {}", playerId, items, Thread.currentThread().getName());
                    return new PlayerLootDetail(firstLootedInstant, lastChangedInstant, items);
                } else { //player had never opened this treasure

                    plugin.getComponentLogger().debug("got no answer for get request for player id {}, and defaulted to null player data, on thread {}", playerId, Thread.currentThread().getName());
                    return null;
                }
            }
        }
    }

    public void forgetPlayer(final int playerId, final @NotNull Ulid treasureId) throws SQLException {
        final String statementStr =
            "DELETE FROM " + PLAYERDATA_TABLE +
                " WHERE " + TREASURE_ID_KEY + " = ?" +
                " AND " + PID_KEY + " = ?";

        try (final Connection connection = dataSource.getConnection();
             final PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {

            preparedStatement.setBytes(1, treasureId.toBytes());
            preparedStatement.setInt(2, playerId);
            int rowsAffected = preparedStatement.executeUpdate();

            plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished forgetPlayer for player id {} and treasure {}, on thread {}",
                rowsAffected, playerId, treasureId, Thread.currentThread().getName());
        }
    }

    public void forgetAll(final @NotNull Ulid treasureId) throws SQLException {
        final @NotNull String statementStr = "DELETE FROM " + PLAYERDATA_TABLE + " WHERE  " + TREASURE_ID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            preparedStatement.setBytes(1, treasureId.toBytes());

            int rowsAffected = preparedStatement.executeUpdate();

            plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished forgetAll request for treasure id {}, on thread {}", rowsAffected, treasureId, Thread.currentThread().getName());
        }
    }

    public int deleteTreasure(final @NotNull Ulid treasureId) throws SQLException {
        // first delete all rows with foreign keys, then the rows itself
        final @NotNull String playerDataStatementStr = "DELETE FROM " + PLAYERDATA_TABLE + " WHERE " + TREASURE_ID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement playerDataPreparedStatement = connection.prepareStatement(playerDataStatementStr)) {
            playerDataPreparedStatement.setBytes(1, treasureId.toBytes());

            return playerDataPreparedStatement.executeUpdate();
        }
    }

    /**
     * Contains all player looted treasures
     */
    public void createTablePlayerData() {
        final String statementStr = "CREATE TABLE IF NOT EXISTS " + PLAYERDATA_TABLE + " (" +
            PID_KEY + " INT UNSIGNED NOT NULL, " +
            // is BINARY instead of UUID since SQL instances can not be trusted shifting UUIDs around in order to "optimizing" them;
            // and not be string / char array since byte array is shorter bitwise and therefor faster
            TREASURE_ID_KEY + " BINARY(16) NOT NULL, " +
            TREASURE_FIRST_LOOTED_INSTANT_KEY + " BIGINT UNSIGNED, " +
            TREASURE_LAST_CHANGED_INSTANT_KEY + " BIGINT UNSIGNED, " +
            TREASURE_CONTENT_KEY + " MEDIUMBLOB NOT NULL, " +
            TIMES_LOOTED + " INT UNSIGNED DEFAULT 0, " +
            // important: don't make TREASURE_ID UNIQUE on its own, only one player could have an entry otherwise
            // but also don't let TREASURE_ID without constrains, else wise a player can infinit entries of the same treasure, not updating them
            "PRIMARY KEY (" + PID_KEY + ", " + TREASURE_ID_KEY + "))";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            preparedStatement.executeUpdate();
        } catch (SQLException e) {
            plugin.getComponentLogger().error("Could not create table {}.", PLAYERDATA_TABLE, e);
        }
    }
}
