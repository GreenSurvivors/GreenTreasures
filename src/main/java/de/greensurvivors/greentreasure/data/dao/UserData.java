package de.greensurvivors.greentreasure.data.dao;

import com.destroystokyo.paper.profile.PlayerProfile;
import com.zaxxer.hikari.HikariDataSource;
import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.data.DataAccessor;
import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.sql.*;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

public class UserData {
    // database keys
    private final static @NotNull String USER_TABLE = "user";
    private final static @NotNull String
        /// player identifier
        PID_KEY = "pid",
        /// uuid of player
        UUID_KEY = "uuid",
        /// name of player
        NAME = "name";
    private static final @NotNull PlayerProfile
        // in case a treasure was shared (@ is not permitted as a valid char and therefor always unique)
        // uuid created by UUID.nameUUIDFromBytes(("OfflinePlayer:@shared").getBytes(StandardCharsets.UTF_8))
        // note: while valid premium names are stricter, at the time of writing any ascii char in range of ' '..\u007F is totally fine.
        SHARED_PROFILE = Bukkit.createProfileExact(UUID.fromString("c1fadf20-80f9-3e87-b5f2-548a5d33c7dc"), "@shared");

    private final @NotNull GreenTreasure plugin;
    private final @NotNull HikariDataSource dataSource;

    public UserData(final @NotNull GreenTreasure plugin, final @NotNull HikariDataSource dataSource) {
        this.plugin = plugin;
        this.dataSource = dataSource;
    }

    public @Nullable Integer getPlayerId(final @Nullable UUID uuid) throws SQLException {
        final String statementStr =
            "SELECT u." + PID_KEY +
                " FROM " + USER_TABLE + " AS u" +
                " WHERE u." + UUID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
            preparedStatement.setString(1, uuid == null ? SHARED_PROFILE.getId().toString() : uuid.toString());

            try (final ResultSet resultSet = preparedStatement.executeQuery()) {
                if (resultSet.next()) {
                    final int playerId = resultSet.getInt(PID_KEY);

                    plugin.getComponentLogger().debug("successfully got data for pid request for player with uuid {}: on thread {}", uuid, Thread.currentThread().getName());
                    return playerId;
                } else {
                    plugin.getComponentLogger().debug("got no answer for get request for player  with uuid {}, and defaulted to null player id, on thread {}",
                        uuid, Thread.currentThread().getName());

                    return null;
                }
            }
        }
    }

    /// does not include the shared id
    public @NotNull Map<UUID, Integer> getAllPlayerIds() throws SQLException {
        final String statementStr =
            "SELECT u." + PID_KEY + ", u." + UUID_KEY +
            " FROM " + USER_TABLE + " AS u ";

        try (final Connection connection = dataSource.getConnection();
             final PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {

            try (final ResultSet resultSet = preparedStatement.executeQuery()) {
                final Map<UUID, Integer> result = new HashMap<>();

                while (resultSet.next()) {
                    result.put(UUID.fromString(resultSet.getString(UUID_KEY)), resultSet.getInt(PID_KEY));
                }

                result.remove(SHARED_PROFILE.getUniqueId());

                return result;
            }
        }
    }

    /**
     * Tries to add a player into the player table
     */
    public void addPlayer(final @Nullable OfflinePlayer player) {
        final String statementStr =
            "INSERT INTO " + USER_TABLE + " " +
                "(" + NAME + ", " + UUID_KEY + ") VALUES (?, ?) " +
                "ON DUPLICATE KEY UPDATE " + NAME + " = VALUES(" + NAME + ")";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            if (player == null) {
                preparedStatement.setString(1, SHARED_PROFILE.getName());
                preparedStatement.setString(2, SHARED_PROFILE.getId().toString());
            } else {
                preparedStatement.setString(1, player.getName());
                preparedStatement.setString(2, player.getUniqueId().toString());
            }
            preparedStatement.executeUpdate();

        } catch (SQLException e) {
            plugin.getComponentLogger().error("Could not add player '{}'.", player == null ? SHARED_PROFILE.getName() : player.getName(), e);

            if (e instanceof SQLSyntaxErrorException && DataAccessor.MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
            }
        }
    }

    /**
     * Create a player table to reference against
     */
    public void createTableUser() {
        final String statementStr = "CREATE TABLE IF NOT EXISTS " + USER_TABLE + " (" +
            PID_KEY + " INT UNSIGNED AUTO_INCREMENT PRIMARY KEY, " +
            NAME + " VARCHAR(16) UNIQUE NOT NULL, " +
            UUID_KEY + " VARCHAR(36) UNIQUE NOT NULL)";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            preparedStatement.executeUpdate();
        } catch (SQLException e) {
            plugin.getComponentLogger().error("Could not create user table.", e);
        }
    }
}
