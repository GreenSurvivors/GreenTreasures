package de.greensurvivors.greentreasure.dataobjects;

import com.zaxxer.hikari.HikariDataSource;
import de.greensurvivors.greentreasure.GreenTreasure;
import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.inventory.ItemStack;
import org.bukkit.plugin.Plugin;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.slf4j.Logger;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicLong;

public class DatabaseManager {
    // database keys
    private final static @NotNull String
        /// player table
        USER_TABLE = "user",
        /// player identifier
        PID_KEY = "pid",
        /// uuid of player
        UUID_KEY = "uuid",
        /// name of player
        NAME = "name",
        /// treasure table
        TREASURES_TABLE = "treasures",
        /// treasure identifier
        TREASURE_ID_KEY = "treasureid",
        /// last time a player had changed the treasure
        TREASURE_TIMESTAMP_KEY = "timestamp",
        /// the items remaining in this treasure
        TREASURE_CONTENT_KEY = "content";
    // config keys
    private final static @NotNull String
        HOST = "host",
        PORT = "port",
        LOGIN_USER_NAME = "user",
        PASSWORD = "password",
        DATABASE = "database";
    private static final String NULL = "NULL";
    private final @NotNull GreenTreasure plugin;
    /// we use this instead of {@link org.bukkit.scheduler.BukkitScheduler#runTaskAsynchronously(Plugin, Runnable)} because the bukkit scheduler waits to the next tick to start a task.
    private final @NotNull Executor asyncExecutor;
    private @Nullable HikariDataSource dataSource = null;
    // connection information
    private volatile @NotNull String host = "localhost", database = "database";
    private volatile @Nullable String loginUserName = null, password = null;
    private volatile int port = 3306;

    public DatabaseManager(final @NotNull GreenTreasure plugin) {
        this.plugin = plugin;

        // we expect a burst of requests and long time nothing.
        // so don't hold any thread in the dry periods, but grow as big as we need to
        // log errors with the plugins logger
        final @NotNull AtomicLong count = new AtomicLong(0L);
        final @NotNull ThreadFactory threadFactory = runnable -> {
            Thread thread = Executors.defaultThreadFactory().newThread(runnable);
            Objects.requireNonNull(thread);

            thread.setName(String.format("GreenTreasure Database thread - %1$d", count.getAndIncrement()));
            thread.setUncaughtExceptionHandler(new UncaughtExceptionHandler(plugin.getComponentLogger()));

            return thread;
        };
        asyncExecutor = new ThreadPoolExecutor(5, Integer.MAX_VALUE, 30L, TimeUnit.SECONDS, new SynchronousQueue<>(), threadFactory);
    }

    public @NotNull Map<@NotNull String, @NotNull Object> serializeDatabaseConnectionConfig() {
        HashMap<String, Object> databaseData = new HashMap<>();

        databaseData.put(HOST, host);
        databaseData.put(PORT, port);
        databaseData.put(LOGIN_USER_NAME, loginUserName);
        databaseData.put(PASSWORD, password);
        databaseData.put(DATABASE, database);

        return databaseData;
    }

    /**
     * loads SQL- login details from a map (config).
     */
    public void reload(final @Nullable Map<@NotNull String, @NotNull Object> databaseData) {
        if (databaseData == null) {
            return;
        }

        boolean shouldConnect = true;

        // host
        if (databaseData.containsKey(HOST) && databaseData.get(HOST) instanceof String hostStr) {
            host = hostStr;
        } else {
            plugin.getComponentLogger().warn("Missing value for  {}", HOST);
            shouldConnect = false;
        }
        // user
        if (databaseData.containsKey(LOGIN_USER_NAME) && databaseData.get(LOGIN_USER_NAME) instanceof String userStr) {
            loginUserName = userStr;
        } else {
            plugin.getComponentLogger().warn("Missing value for {}", LOGIN_USER_NAME);
            shouldConnect = false;
        }
        // password
        if (databaseData.containsKey(PASSWORD) && databaseData.get(PASSWORD) instanceof String passwortStr) {
            password = passwortStr;
        } else {
            plugin.getComponentLogger().warn("Missing value for {}", PASSWORD);
        }
        // database
        if (databaseData.containsKey(DATABASE) && databaseData.get(DATABASE) instanceof String databaseNameStr) {
            database = databaseNameStr;
        } else {
            plugin.getComponentLogger().warn("Missing value for {}", DATABASE);
            shouldConnect = false;
        }
        // port
        if (databaseData.containsKey(PORT) && databaseData.get(PORT) instanceof Number portInt) {
            port = portInt.intValue();
        } else {
            plugin.getComponentLogger().warn("Missing value for {}", PORT);
        }

        if (dataSource != null) {
            closeConnection();
        }

        if (shouldConnect) {
            dataSource = new HikariDataSource();
            dataSource.setJdbcUrl("jdbc:mysql://" + host + ":" + port + "/" + database);
            dataSource.setUsername(loginUserName);
            dataSource.setPassword(password);
            //dataSource.setIdleTimeout(60000); // unused, since maximum pool size == minimum
            dataSource.setMaximumPoolSize(4); // don't keep the default 10 threads alive. we are way too small for that

            // pre start pool - the first time hasConnection() is called would return false otherwise since the pool needs a second to start after it was invoked
            createTablePlayer();
            createTableTreasure();
        }
    }

    /**
     * Returns the known information about a treasure looted by a player
     *
     * @param player     player who we want data of
     * @param treasureId identifier of the treasure
     * @return PlayerLootDetail or null, if getting the data wasn't successfully
     * (like in cases if the player never opened this treasure)
     */
    public @NotNull CompletableFuture<@Nullable PlayerLootDetail> getPlayerData(final @NotNull OfflinePlayer player, final @NotNull String treasureId) {
        final @NotNull CompletableFuture<@Nullable PlayerLootDetail> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));
            }

            createTableTreasure();
            addPlayer(player);

            final @NotNull String statementStr =
                "SELECT q." + TREASURE_TIMESTAMP_KEY + ", q." + TREASURE_CONTENT_KEY +
                    " FROM " + TREASURES_TABLE +
                    " AS q JOIN " + USER_TABLE + " AS p ON q." + PID_KEY + " = p." + PID_KEY +
                    " WHERE q." + TREASURE_ID_KEY + " LIKE ? AND p." + UUID_KEY + " = ?";

            try (final @NotNull Connection connection = dataSource.getConnection();
                 final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
                preparedStatement.setString(1, treasureId);
                preparedStatement.setString(2, player.getUniqueId().toString());

                try (final ResultSet resultSet = preparedStatement.executeQuery()) {
                    if (resultSet.next()) {
                        long timeStamp = resultSet.getLong(1);

                        //get list from string
                        final @Nullable String rawItemStr = resultSet.getString(2);
                        final @Nullable List<ItemStack> items;
                        if (rawItemStr == null || rawItemStr.equalsIgnoreCase(NULL)) {
                            items = null;
                        } else {
                            items = new ArrayList<>(List.of(ItemStack.deserializeItemsFromBytes(Base64.getDecoder().decode(rawItemStr))));
                        }

                        //plugin.getComponentLogger().debug("successfully got amount for get request for player {} to {} amount of times, on thread {}", player.getName(), result, Thread.currentThread().getName());
                        Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(new PlayerLootDetail(timeStamp, items)));
                    } else { //player had never done this quest

                        plugin.getComponentLogger().debug("got no answer for get request for player {}, and defaulted to null player data, on thread {}", player.getName(), Thread.currentThread().getName());
                        Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));
                    }
                }
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));

                plugin.getComponentLogger().warn("Could not get treasure identifier '{}' player loot detail for '{}'", treasureId, player.getName(), e);
            }
        });

        return resultFuture;
    }

    /**
     * Saves the loot detail of a player
     *
     * @param player     player whose data is about to be saved.
     * @param treasureId the treasure id
     * @param lootDetail the new data
     */
    public @NotNull CompletableFuture<Void> setPlayerData(final @NotNull OfflinePlayer player, final @NotNull String treasureId, final @NotNull PlayerLootDetail lootDetail) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                return;
            }

            createTableTreasure();
            addPlayer(player);

            final @NotNull String statementStr =
                "INSERT INTO " + TREASURES_TABLE + " (" + TREASURE_ID_KEY + ", " + PID_KEY + ", " + TREASURE_TIMESTAMP_KEY + ", " + TREASURE_CONTENT_KEY + ") VALUES (" +
                    "?, " +
                    "(SELECT " + PID_KEY + " FROM " + USER_TABLE + " WHERE " + UUID_KEY + " = ?), " +
                    "?, " +
                    "?) ON DUPLICATE KEY UPDATE " +
                    TREASURE_TIMESTAMP_KEY + " = VALUES(" + TREASURE_TIMESTAMP_KEY + "), " +
                    TREASURE_CONTENT_KEY + " = VALUES(" + TREASURE_CONTENT_KEY + ")";

            try (final @NotNull Connection connection = dataSource.getConnection();
                 final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
                preparedStatement.setString(1, treasureId);
                preparedStatement.setString(2, player.getUniqueId().toString());
                preparedStatement.setLong(3, lootDetail.lastChangedTimeStamp());
                preparedStatement.setString(4, lootDetail.unLootedStuff() == null ? NULL : Base64.getEncoder().encodeToString(ItemStack.serializeItemsAsBytes(lootDetail.unLootedStuff())));

                int rowsAffected = preparedStatement.executeUpdate();
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));

                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished set request for player {} at timestamp {}, on thread {}", rowsAffected, player.getName(), lootDetail.lastChangedTimeStamp(), Thread.currentThread().getName());
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));

                plugin.getComponentLogger().warn("Could not set player loot data for '{}' at timestamp '{}' for '{}'",
                    treasureId, lootDetail.lastChangedTimeStamp(), player.getName(), e);
            }
        });

        return resultFuture;
    }

    public @NotNull CompletableFuture<Void> forgetAll(final @NotNull String treasureId) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                return;
            }

            createTableTreasure();

            final @NotNull String statementStr = "DELETE FROM " + TREASURES_TABLE + " WHERE  " + TREASURE_ID_KEY + " = ?";

            try (final @NotNull Connection connection = dataSource.getConnection();
                 final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
                preparedStatement.setString(1, treasureId);

                int rowsAffected = preparedStatement.executeUpdate();
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));

                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished forgetAll request for treasure id {}, on thread {}", rowsAffected, treasureId, Thread.currentThread().getName());
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));

                plugin.getComponentLogger().warn("Could not forgetAll treasure data for '{}'", treasureId, e);
            }
        });

        return resultFuture;
    }

    public @NotNull CompletableFuture<Void> forgetPlayer(final @NotNull OfflinePlayer player, final @NotNull String treasureId) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                return;
            }

            createTableTreasure();
            addPlayer(player);

            final String statementStr =
                "DELETE FROM " + TREASURES_TABLE +
                    " WHERE " + TREASURE_ID_KEY + " = ? " +
                    "AND " + PID_KEY + " = (SELECT " + PID_KEY + " FROM " + USER_TABLE + " WHERE " + UUID_KEY + " = ?)";

            try (final Connection connection = dataSource.getConnection();
                 final PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {

                preparedStatement.setString(1, treasureId);
                preparedStatement.setString(2, player.getUniqueId().toString());
                int rowsAffected = preparedStatement.executeUpdate();
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));

                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished forgetPlayer for {} and treasure {}, on thread {}",
                    rowsAffected, player.getName(), treasureId, Thread.currentThread().getName());
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));

                plugin.getComponentLogger().warn("Could not forget player loot detail for '{}' with treasure '{}'", player.getName(), treasureId, e);
            }
        });

        return resultFuture;
    }

    public @NotNull CompletableFuture<@NotNull Map<UUID, @NotNull PlayerLootDetail>> getAllPlayerData(final @NotNull String treasureId) {
        final CompletableFuture<Map<UUID, PlayerLootDetail>> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(Collections.emptyMap()));
                return;
            }

            createTableTreasure();

            final String statementStr =
                "SELECT u." + UUID_KEY + ", t." + TREASURE_TIMESTAMP_KEY + ", t." + TREASURE_CONTENT_KEY +
                    " FROM " + TREASURES_TABLE + " AS t" +
                    " JOIN " + USER_TABLE + " AS u ON t." + PID_KEY + " = u." + PID_KEY +
                    " WHERE t." + TREASURE_ID_KEY + " = ?";

            try (final Connection connection = dataSource.getConnection();
                 final PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {

                preparedStatement.setString(1, treasureId);

                try (final ResultSet resultSet = preparedStatement.executeQuery()) {
                    final Map<UUID, PlayerLootDetail> result = new HashMap<>();

                    while (resultSet.next()) {
                        final UUID playerUUID = UUID.fromString(resultSet.getString(1));
                        final long timeStamp = resultSet.getLong(2);

                        //get list from string
                        final @Nullable String rawItemStr = resultSet.getString(2);
                        final @Nullable List<ItemStack> items;
                        if (rawItemStr == null || rawItemStr.equalsIgnoreCase(NULL)) {
                            items = null;
                        } else {
                            items = new ArrayList<>(List.of(ItemStack.deserializeItemsFromBytes(Base64.getDecoder().decode(rawItemStr))));
                        }

                        result.put(playerUUID, new PlayerLootDetail(timeStamp, items));
                    }

                    Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(result));
                }
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(Collections.emptyMap()));
                plugin.getComponentLogger().warn("Could not retrieve all player data for treasure '{}'", treasureId, e);
            }
        });

        return resultFuture;
    }

    /**
     * Checks if the connection exists and is valid.
     */
    public boolean hasConnection() {
        return dataSource != null && dataSource.isRunning();
    }

    /**
     * closes the connection if valid and sets the connection to null
     */
    public void closeConnection() {
        if (dataSource != null && !dataSource.isClosed()) {
            dataSource.close();
            plugin.getComponentLogger().debug("Logout");
        }
    }

    /**
     * creates missing treasure table
     */
    private void createTableTreasure() {
        createTablePlayer();

        final String statementStr = "CREATE TABLE IF NOT EXISTS " + TREASURES_TABLE + " (" +
            PID_KEY + " INT UNSIGNED NOT NULL," +
            TREASURE_ID_KEY + " VARCHAR(255) NOT NULL, " +
            TREASURE_TIMESTAMP_KEY + " BIGINT, " +
            TREASURE_CONTENT_KEY + " MEDIUMTEXT, " +
            // important: don't make TREASURE_ID UNIQUE on its own, only one player could have an entry otherwise
            // but also don't let TREASURE_ID without constrains, else wise a player can infinit entries of the same treasure, not updating them
            "PRIMARY KEY (" + PID_KEY + ", " + TREASURE_ID_KEY + "), " +
            "FOREIGN KEY (" + PID_KEY + ") REFERENCES " + USER_TABLE + "(" + PID_KEY + "))";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            preparedStatement.executeUpdate();
        } catch (SQLException e) {
            plugin.getComponentLogger().error("Could not create quests.", e);
        }
    }

    /**
     * Create a player table to reference against
     */
    protected void createTablePlayer() {
        if (dataSource != null) {
            final String statementStr = "CREATE TABLE IF NOT EXISTS " + USER_TABLE + " (" +
                PID_KEY + " INT UNSIGNED AUTO_INCREMENT PRIMARY KEY, " +
                NAME + " VARCHAR(16) UNIQUE NOT NULL, " +
                UUID_KEY + " VARCHAR(36) UNIQUE NOT NULL )";

            try (final @NotNull Connection connection = dataSource.getConnection();
                 final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
                preparedStatement.executeUpdate();
            } catch (SQLException e) {
                plugin.getComponentLogger().error("Could not create players.", e);
            }
        }
    }

    /**
     * Tries to add a player into the player table
     */
    protected void addPlayer(final @NotNull OfflinePlayer player) {
        createTablePlayer();

        final String statementStr =
            "INSERT INTO " + USER_TABLE + " " +
                "(" + NAME + ", " + UUID_KEY + ") VALUES (?, ?) " +
                "ON DUPLICATE KEY UPDATE " + NAME + " = VALUES(" + NAME + ")";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            preparedStatement.setString(1, player.getName());
            preparedStatement.setString(2, player.getUniqueId().toString());
            preparedStatement.executeUpdate();

        } catch (SQLException e) {
            plugin.getComponentLogger().error("Could not add player '{}'.", player.getName(), e);
        }
    }

    public static class UncaughtExceptionHandler implements Thread.UncaughtExceptionHandler {
        private final @NotNull Logger logger;

        public UncaughtExceptionHandler(final @NotNull Logger logger) {
            this.logger = logger;
        }

        @Override
        public void uncaughtException(final @NotNull Thread thread, final @NotNull Throwable throwable) {
            this.logger.error("Caught previously unhandled exception :");
            this.logger.error(thread.getName(), throwable);
        }
    }
}
