package de.greensurvivors.greentreasure;

import com.zaxxer.hikari.HikariDataSource;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import org.apache.commons.collections4.list.SetUniqueList;
import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.inventory.ItemStack;
import org.bukkit.plugin.Plugin;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.Range;
import org.slf4j.Logger;

import java.sql.*;
import java.time.Duration;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicLong;

public class DatabaseManager {
    private final static @NotNull String
        /// player table
        USER_TABLE = "user",
        /// treasure table
        TREASURE_TABLE = "treasure",
        /// player data table
        PLAYERDATA_TABLE = "playerdata";
    // database keys
    private final static @NotNull String
        /// player identifier
        PID_KEY = "pid",
        /// uuid of player
        UUID_KEY = "uuid",
        /// name of player
        NAME = "name",
        /// treasure identifier
        TREASURE_ID_KEY = "treasureid",
        /// last time a player had changed the treasure
        TREASURE_TIMESTAMP_KEY = "timestamp",
        /// the items of this treasure, the player data table as well as the treasure table have the same column name
        TREASURE_CONTENT_KEY = "content",
        TREASURE_FORGET_DURATION_KEY = "forgetduration",
        TREASURE_SLOT_CHANCE_KEY = "slotchance",
        TREASURE_UNLIMITED_KEY = "unlimited",
        TREASURE_SHARED_KEY = "shared";
    // config keys
    private final static @NotNull String
        HOST = "host",
        PORT = "port",
        LOGIN_USER_NAME = "user",
        PASSWORD = "password",
        DATABASE = "database";
    private static final long DEFAULT_FORGET_DURATION_MILLIS = -1L;
    private static final short DEFAULT_SLOT_CHANCE = 10000;
    private static final boolean DEFAULT_IS_UNLIMITED = false, DEFAULT_IS_SHARED = false;
    private static final @NotNull OfflinePlayer
        // in case a treasure was shared (@ is not permitted as a valid char and therefor always unique)
        SHARED_PROFILE = Bukkit.getOfflinePlayer("@SHARED");
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
            createTableUser();
            createTablePlayerData();
            createTableTreasure();
        }
    }

    public @NotNull CompletableFuture<Void> setTreasureContents(final @NotNull String treasureId, final @NotNull List<@NotNull ItemStack> contents) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            createTableTreasure();

            final @NotNull String statementStr = "INSERT INTO " + TREASURE_TABLE + "(" +
                TREASURE_ID_KEY + ", " +
                TREASURE_CONTENT_KEY + ") " +
                "VALUES (?, ?) ON DUPLICATE KEY UPDATE " +
                TREASURE_CONTENT_KEY + " = VALUES(" + TREASURE_CONTENT_KEY + ")";

            try (final @NotNull Connection connection = dataSource.getConnection();
                 final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
                preparedStatement.setString(1, treasureId);

                final @NotNull Blob blob = connection.createBlob();
                blob.setBytes(1, ItemStack.serializeItemsAsBytes(contents.toArray(new ItemStack[0])));
                preparedStatement.setBlob(2, blob);

                int rowsAffected = preparedStatement.executeUpdate();
                blob.free();
                Bukkit.getScheduler().runTask(plugin, () -> {
                    plugin.getTreasureManager().invalidateTreasure(treasureId);
                    resultFuture.complete(null);
                });

                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished set request for treasure {}, on thread {}", rowsAffected, treasureId, Thread.currentThread().getName());
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not set treasure contents for '{}'", treasureId, e);
            }
        });

        return resultFuture;
    }

    public @NotNull CompletableFuture<@Nullable List<ItemStack>> getTreasureContents(final @NotNull String treasureId) {
        final @NotNull CompletableFuture<@Nullable List<ItemStack>> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            createTableTreasure();

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

                        plugin.getComponentLogger().debug("successfully got treasure contents for get request for treasure id {}, on thread {}", treasureId, Thread.currentThread().getName());
                        Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(items));
                    } else { // this treasure was deleted / never created
                        plugin.getComponentLogger().debug("got no answer for get request for treasure contents {}, on thread {}", treasureId, Thread.currentThread().getName());
                        Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));
                    }
                }
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not get treasure data for treasure id '{}'", treasureId, e);
            }
        });

        return resultFuture;
    }

    public @NotNull CompletableFuture<Void> deleteTreasure(final @NotNull String treasureId) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            createTableTreasure();

            final @NotNull String statementStr = "DELETE FROM " + TREASURE_TABLE +
                " WHERE " + TREASURE_ID_KEY + " = ?";

            try (final @NotNull Connection connection = dataSource.getConnection();
                 final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
                preparedStatement.setString(1, treasureId);

                int rowsAffected = preparedStatement.executeUpdate();
                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished delete request for treasure {}, on thread {}", rowsAffected, treasureId, Thread.currentThread().getName());

                Bukkit.getScheduler().runTask(plugin, () -> forgetAll(treasureId).thenRun(() -> {
                    plugin.getTreasureManager().invalidateTreasure(treasureId);
                    resultFuture.complete(null);
                }));
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not delete treasure with id '{}'", treasureId, e);
            }
        });

        return resultFuture;
    }

    /**
     * set the random slot chance
     *
     * @param treasureId information to identify a treasure
     * @param slotChance how probable it is for a slot to be in the freshly opened treasure;
     *                   is multiplied by 100 to allow floating points
     */
    public @NotNull CompletableFuture<Void> setRandom(final @NotNull String treasureId, final @Range(from = 0, to = 10000) Short slotChance) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            createTableTreasure();

            final @NotNull String statementStr = "INSERT INTO " + TREASURE_TABLE + "(" +
                TREASURE_ID_KEY + ", " +
                TREASURE_SLOT_CHANCE_KEY + ") " +
                "VALUES (?, ?) ON DUPLICATE KEY UPDATE " +
                TREASURE_SLOT_CHANCE_KEY + " = VALUES(" + TREASURE_SLOT_CHANCE_KEY + ")";

            try (final @NotNull Connection connection = dataSource.getConnection();
                 final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
                preparedStatement.setString(1, treasureId);
                preparedStatement.setShort(2, slotChance);

                int rowsAffected = preparedStatement.executeUpdate();
                Bukkit.getScheduler().runTask(plugin, () -> {
                    plugin.getTreasureManager().invalidateTreasure(treasureId);
                    resultFuture.complete(null);
                });

                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished set slot chance request for treasure {} to {}, on thread {}", rowsAffected, treasureId, slotChance / 100.0D, Thread.currentThread().getName());
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not set treasure slot chance for '{}' to {}", treasureId, slotChance / 100.0D, e);
            }
        });

        return resultFuture;
    }

    /**
     * set the shared status
     *
     * @param treasureId information to identify a treasure
     * @param isShared   if the Inventory is globally shared across all players
     */
    public @NotNull CompletableFuture<Void> setShared(final @NotNull String treasureId, final boolean isShared) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            createTableTreasure();

            final @NotNull String statementStr = "INSERT INTO " + TREASURE_TABLE + "(" +
                TREASURE_ID_KEY + ", " +
                TREASURE_SHARED_KEY + ") " +
                "VALUES (?, ?) ON DUPLICATE KEY UPDATE " +
                TREASURE_SHARED_KEY + " = VALUES(" + TREASURE_SHARED_KEY + ")";

            try (final @NotNull Connection connection = dataSource.getConnection();
                 final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
                preparedStatement.setString(1, treasureId);
                preparedStatement.setBoolean(2, isShared);

                int rowsAffected = preparedStatement.executeUpdate();
                Bukkit.getScheduler().runTask(plugin, () -> {
                    plugin.getTreasureManager().invalidateTreasure(treasureId);
                    resultFuture.complete(null);
                });

                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished set shared request for treasure {} to {}, on thread {}", rowsAffected, treasureId, isShared, Thread.currentThread().getName());
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not set treasure shared setting for '{}' to {}", treasureId, isShared, e);
            }
        });

        return resultFuture;
    }

    /**
     * set the unlimited status
     *
     * @param treasureId  information to identify a treasure
     * @param isUnLimited if one can loot the treasure as often as one wants
     */
    public @NotNull CompletableFuture<Void> setUnlimited(final @NotNull String treasureId, final boolean isUnLimited) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            createTableTreasure();

            final @NotNull String statementStr = "INSERT INTO " + TREASURE_TABLE + "(" +
                TREASURE_ID_KEY + ", " +
                TREASURE_UNLIMITED_KEY + ") " +
                "VALUES (?, ?) ON DUPLICATE KEY UPDATE " +
                TREASURE_UNLIMITED_KEY + " = VALUES(" + TREASURE_UNLIMITED_KEY + ")";

            try (final @NotNull Connection connection = dataSource.getConnection();
                 final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
                preparedStatement.setString(1, treasureId);
                preparedStatement.setBoolean(2, isUnLimited);

                int rowsAffected = preparedStatement.executeUpdate();
                Bukkit.getScheduler().runTask(plugin, () -> {
                    plugin.getTreasureManager().invalidateTreasure(treasureId);
                    resultFuture.complete(null);
                });

                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished set unlimited request for treasure {} to {}, on thread {}", rowsAffected, treasureId, isUnLimited, Thread.currentThread().getName());
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not set treasure unlimited setting for '{}' to {}", treasureId, isUnLimited, e);
            }
        });

        return resultFuture;
    }

    /**
     * set the forget-period
     *
     * @param treasureId         information to identify a treasure
     * @param forgettingDuration how long a Treasure has to be not looted until it is filled again.
     *                           negative or null values mean the Treasure will never restock.
     */
    public @NotNull CompletableFuture<Void> setForgetDuration(final @NotNull String treasureId, final @Nullable Duration forgettingDuration) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            createTableTreasure();

            final @NotNull String statementStr = "INSERT INTO " + TREASURE_TABLE + "(" +
                TREASURE_ID_KEY + ", " +
                TREASURE_FORGET_DURATION_KEY + ") " +
                "VALUES (?, ?) ON DUPLICATE KEY UPDATE " +
                TREASURE_FORGET_DURATION_KEY + " = VALUES(" + TREASURE_FORGET_DURATION_KEY + ")";

            try (final @NotNull Connection connection = dataSource.getConnection();
                 final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
                preparedStatement.setString(1, treasureId);
                preparedStatement.setLong(2, forgettingDuration == null ? DEFAULT_FORGET_DURATION_MILLIS : forgettingDuration.toMillis());

                int rowsAffected = preparedStatement.executeUpdate();
                Bukkit.getScheduler().runTask(plugin, () -> {
                    plugin.getTreasureManager().invalidateTreasure(treasureId);
                    resultFuture.complete(null);
                });

                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished set forget duration request for treasure {} to {}, on thread {}", rowsAffected, treasureId, forgettingDuration, Thread.currentThread().getName());
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not set treasure forget duration for '{}' to {}", treasureId, forgettingDuration, e);
            }
        });

        return resultFuture;
    }

    /**
     * The reason why this method is NOT async is that certain actions in this plugin can't just wait around for a CompletableFuture to complete.
     * Like to know if an Event should get canceled, the server has to get stalled until we retrieve the data.
     * So just skip the nonsense and cache the data whenever possible.
     */
    public @Nullable TreasureInfo loadTreasure(final @NotNull String treasureId) {
        createTableTreasure();

        final @NotNull String statementStr = "SELECT " +
            TREASURE_CONTENT_KEY + ", " +
            TREASURE_FORGET_DURATION_KEY + ", " +
            TREASURE_SLOT_CHANCE_KEY + ", " +
            TREASURE_UNLIMITED_KEY + ", " +
            TREASURE_SHARED_KEY +
            " FROM " + TREASURE_TABLE +
            " WHERE " + TREASURE_ID_KEY + " = ?";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
            preparedStatement.setString(1, treasureId);

            try (final ResultSet resultSet = preparedStatement.executeQuery()) {

                if (resultSet.next()) {
                    final @NotNull Blob blob = resultSet.getBlob(TREASURE_CONTENT_KEY);
                    final long forgetDurationMillis = resultSet.getLong(TREASURE_FORGET_DURATION_KEY);
                    final short slotChance = resultSet.getShort(TREASURE_SLOT_CHANCE_KEY);
                    final boolean isUnlimited = resultSet.getBoolean(TREASURE_UNLIMITED_KEY);
                    final boolean isShared = resultSet.getBoolean(TREASURE_SHARED_KEY);

                    if (blob.length() <= 1) {
                        plugin.getComponentLogger().warn("No or malformed item list found for treasure id {}. Skipping. On thread {}", treasureId, Thread.currentThread().getName());
                        return null;
                    }

                    final @NotNull List<ItemStack> items = new ArrayList<>(List.of(ItemStack.deserializeItemsFromBytes(blob.getBytes(1, (int) blob.length()))));
                    blob.free();

                    return new TreasureInfo(treasureId, items, Duration.ofMillis(forgetDurationMillis), slotChance, isUnlimited, isShared);
                } else { // this treasure was deleted / never created
                    plugin.getComponentLogger().debug("got no answer for get request for treasure info {}, on thread {}", treasureId, Thread.currentThread().getName());
                    return null;
                }
            }
        } catch (SQLException e) {
            plugin.getComponentLogger().warn("Could not get treasure data for treasure id '{}'", treasureId, e);

            return null;
        }
    }

    public @NotNull CompletableFuture<@NotNull SetUniqueList<@NotNull String>> getTreasureIds() {
        final @NotNull CompletableFuture<@NotNull SetUniqueList<@NotNull String>> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            createTableTreasure();

            final @NotNull String statementStr = "SELECT " + TREASURE_ID_KEY + " FROM " + TREASURE_TABLE;

            try (final @NotNull Connection connection = dataSource.getConnection();
                 final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {

                try (final ResultSet resultSet = preparedStatement.executeQuery()) {
                    final @NotNull SetUniqueList<@NotNull String> resultList = SetUniqueList.setUniqueList(new ArrayList<>());

                    while (resultSet.next()) {
                        resultList.add(resultSet.getString(TREASURE_ID_KEY));
                    }

                    Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(resultList));
                }
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not set treasure ids", e);
            }
        });

        return resultFuture;
    }

    /**
     * Saves the loot detail of a player
     *
     * @param player     player whose data is about to be saved, null for global / shared
     * @param treasureId the treasure id
     * @param lootDetail the new data
     */
    public @NotNull CompletableFuture<Void> setPlayerData(final @Nullable OfflinePlayer player, final @NotNull String treasureId, final @NotNull PlayerLootDetail lootDetail) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            createTablePlayerData();
            addPlayer(player == null ? SHARED_PROFILE : player);

            final @NotNull String statementStr =
                "INSERT INTO " + PLAYERDATA_TABLE + " (" + TREASURE_ID_KEY + ", " + PID_KEY + ", " + TREASURE_TIMESTAMP_KEY + ", " + TREASURE_CONTENT_KEY + ") VALUES (" +
                    "?, " +
                    "(SELECT " + PID_KEY + " FROM " + USER_TABLE + " WHERE " + UUID_KEY + " = ?), " +
                    "?, " +
                    "?) ON DUPLICATE KEY UPDATE " +
                    TREASURE_TIMESTAMP_KEY + " = VALUES(" + TREASURE_TIMESTAMP_KEY + "), " +
                    TREASURE_CONTENT_KEY + " = VALUES(" + TREASURE_CONTENT_KEY + ")";

            try (final @NotNull Connection connection = dataSource.getConnection();
                 final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
                preparedStatement.setString(1, treasureId);
                preparedStatement.setString(2, player == null ? SHARED_PROFILE.getUniqueId().toString() : player.getUniqueId().toString());
                preparedStatement.setLong(3, lootDetail.lastChangedTimeStamp());

                final @NotNull Blob blob = connection.createBlob();
                if (lootDetail.unLootedStuff() == null) {
                    blob.setBytes(1, new byte[0]);
                } else {
                    blob.setBytes(1, ItemStack.serializeItemsAsBytes(lootDetail.unLootedStuff()));
                }
                blob.free();

                final int rowsAffected = preparedStatement.executeUpdate();
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));

                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished set request for player {} at timestamp {}, on thread {}",
                    rowsAffected, player == null ? SHARED_PROFILE.getName() : player.getName(), lootDetail.lastChangedTimeStamp(), Thread.currentThread().getName());
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not set player loot data for '{}' at timestamp '{}' for '{}'",
                    treasureId, lootDetail.lastChangedTimeStamp(), player == null ? SHARED_PROFILE.getName() : player.getName(), e);
            }
        });

        return resultFuture;
    }

    /**
     * Returns the known information about a treasure looted by a player
     *
     * @param player     player who we want data of, null for global / shared
     * @param treasureId identifier of the treasure
     * @return PlayerLootDetail or null, if getting the data wasn't successfully
     * (like in cases if the player never opened this treasure)
     */
    public @NotNull CompletableFuture<@Nullable PlayerLootDetail> getPlayerData(final @Nullable OfflinePlayer player, final @NotNull String treasureId) {
        final @NotNull CompletableFuture<@Nullable PlayerLootDetail> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            createTablePlayerData();
            addPlayer(player == null ? SHARED_PROFILE : player);

            final @NotNull String statementStr =

                "SELECT p." + TREASURE_TIMESTAMP_KEY + ", COALESCE(p." + TREASURE_CONTENT_KEY + ", t." + TREASURE_CONTENT_KEY + ") AS " + TREASURE_CONTENT_KEY +
                    " FROM " + PLAYERDATA_TABLE + " AS p" +
                    " JOIN " + USER_TABLE + " AS u" +
                    " ON p." + PID_KEY + " = u." + PID_KEY +
                    " LEFT JOIN " + TREASURE_TABLE + " AS t" +
                    " ON p." + TREASURE_ID_KEY + " = t." + TREASURE_ID_KEY +
                    " WHERE p." + TREASURE_ID_KEY + " = ? AND u." + UUID_KEY + " = ?";

            try (final @NotNull Connection connection = dataSource.getConnection();
                 final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
                preparedStatement.setString(1, treasureId);
                preparedStatement.setString(2, player == null ? SHARED_PROFILE.getUniqueId().toString() : player.getUniqueId().toString());

                try (final ResultSet resultSet = preparedStatement.executeQuery()) {
                    if (resultSet.next()) {
                        long timeStamp = resultSet.getLong(TREASURE_TIMESTAMP_KEY);

                        //get list from string
                        final @Nullable Blob blob = resultSet.getBlob(TREASURE_CONTENT_KEY);
                        final @Nullable List<ItemStack> items;
                        if (blob == null || blob.length() <= 0) {
                            items = null;
                        } else {
                            items = new ArrayList<>(List.of(ItemStack.deserializeItemsFromBytes(blob.getBytes(1, (int) blob.length()))));
                        }

                        //plugin.getComponentLogger().debug("successfully got amount for get request for player {} to {} amount of times, on thread {}", player.getName(), result, Thread.currentThread().getName());
                        Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(new PlayerLootDetail(timeStamp, items)));
                    } else { //player had never opened this treasure

                        plugin.getComponentLogger().debug("got no answer for get request for player {}, and defaulted to null player data, on thread {}",
                            player == null ? SHARED_PROFILE.getName() : player.getName(), Thread.currentThread().getName());
                        Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));
                    }
                }
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));

                plugin.getComponentLogger().warn("Could not get treasure identifier '{}' player loot detail for '{}'", treasureId, player == null ? SHARED_PROFILE.getName() : player.getName(), e);
            }
        });

        return resultFuture;
    }

    public @NotNull CompletableFuture<Void> forgetAll(final @NotNull String treasureId) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            createTablePlayerData();

            final @NotNull String statementStr = "DELETE FROM " + PLAYERDATA_TABLE + " WHERE  " + TREASURE_ID_KEY + " = ?";

            try (final @NotNull Connection connection = dataSource.getConnection();
                 final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
                preparedStatement.setString(1, treasureId);

                int rowsAffected = preparedStatement.executeUpdate();
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));

                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished forgetAll request for treasure id {}, on thread {}", rowsAffected, treasureId, Thread.currentThread().getName());
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not forgetAll treasure data for '{}'", treasureId, e);
            }
        });

        return resultFuture;
    }

    public @NotNull CompletableFuture<Void> forgetPlayer(final @Nullable OfflinePlayer player, final @NotNull String treasureId) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            createTablePlayerData();
            addPlayer(player == null ? SHARED_PROFILE : player);

            final String statementStr =
                "DELETE FROM " + PLAYERDATA_TABLE +
                    " WHERE " + TREASURE_ID_KEY + " = ? " +
                    "AND " + PID_KEY + " = (SELECT " + PID_KEY + " FROM " + USER_TABLE + " WHERE " + UUID_KEY + " = ?)";

            try (final Connection connection = dataSource.getConnection();
                 final PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {

                preparedStatement.setString(1, treasureId);
                preparedStatement.setString(2, player == null ? SHARED_PROFILE.getUniqueId().toString() : player.getUniqueId().toString());
                int rowsAffected = preparedStatement.executeUpdate();
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));

                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished forgetPlayer for {} and treasure {}, on thread {}",
                    rowsAffected, player == null ? SHARED_PROFILE.getName() : player.getName(), treasureId, Thread.currentThread().getName());
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not forget player loot detail for '{}' with treasure '{}'", player == null ? SHARED_PROFILE.getName() : player.getName(), treasureId, e);
            }
        });

        return resultFuture;
    }

    public @NotNull CompletableFuture<@NotNull Map<UUID, @NotNull PlayerLootDetail>> getAllPlayerData(final @NotNull String treasureId) {
        final CompletableFuture<Map<UUID, PlayerLootDetail>> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            createTablePlayerData();

            final String statementStr =
                "SELECT u." + UUID_KEY + ", t." + TREASURE_TIMESTAMP_KEY + ", t." + TREASURE_CONTENT_KEY +
                    " FROM " + PLAYERDATA_TABLE + " AS t" +
                    " JOIN " + USER_TABLE + " AS u ON t." + PID_KEY + " = u." + PID_KEY +
                    " WHERE t." + TREASURE_ID_KEY + " = ?";

            try (final Connection connection = dataSource.getConnection();
                 final PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {

                preparedStatement.setString(1, treasureId);

                try (final ResultSet resultSet = preparedStatement.executeQuery()) {
                    final Map<UUID, PlayerLootDetail> result = new HashMap<>();

                    while (resultSet.next()) {
                        final UUID playerUUID = UUID.fromString(resultSet.getString(UUID_KEY));
                        final long timeStamp = resultSet.getLong(TREASURE_TIMESTAMP_KEY);

                        //get list from string
                        final @Nullable Blob blob = resultSet.getBlob(TREASURE_CONTENT_KEY);
                        final @Nullable List<ItemStack> items;
                        if (blob == null || blob.length() <= 0) {
                            items = null;
                        } else {
                            items = new ArrayList<>(List.of(ItemStack.deserializeItemsFromBytes(blob.getBytes(1, (int) blob.length()))));
                            blob.free();
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
            plugin.getComponentLogger().debug("Logout database.");
        }
    }

    /**
     * creates missing treasure table
     */
    protected void createTableTreasure() {
        if (dataSource != null) {
            final String statementStr = "CREATE TABLE IF NOT EXISTS " + TREASURE_TABLE + " (" +
                TREASURE_ID_KEY + " VARCHAR(255) PRIMARY KEY, " +
                TREASURE_CONTENT_KEY + " MEDIUMBLOB NOT NULL, " +
                TREASURE_FORGET_DURATION_KEY + " BIGINT NOT NULL DEFAULT " + DEFAULT_FORGET_DURATION_MILLIS + ", " + // < 0 means no forgetting
                TREASURE_SLOT_CHANCE_KEY + " SMALLINT UNSIGNED NOT NULL DEFAULT " + DEFAULT_SLOT_CHANCE + ", " +
                TREASURE_UNLIMITED_KEY + " BOOLEAN NOT NULL DEFAULT " + DEFAULT_IS_UNLIMITED + ", " +
                TREASURE_SHARED_KEY + " BOOLEAN NOT NULL DEFAULT " + DEFAULT_IS_SHARED + ")";

            try (final @NotNull Connection connection = dataSource.getConnection();
                 final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
                preparedStatement.executeUpdate();
            } catch (SQLException e) {
                plugin.getComponentLogger().error("Could not create player data table.", e);
            }
        }
    }

    /**
     * Create a player table to reference against
     */
    protected void createTableUser() {
        if (dataSource != null) {
            final String statementStr = "CREATE TABLE IF NOT EXISTS " + USER_TABLE + " (" +
                PID_KEY + " INT UNSIGNED AUTO_INCREMENT PRIMARY KEY, " +
                NAME + " VARCHAR(16) UNIQUE NOT NULL, " +
                UUID_KEY + " VARCHAR(36) UNIQUE NOT NULL )";

            try (final @NotNull Connection connection = dataSource.getConnection();
                 final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
                preparedStatement.executeUpdate();
            } catch (SQLException e) {
                plugin.getComponentLogger().error("Could not create user table.", e);
            }
        }
    }

    /**
     * Contains all player looted treasures
     */
    private void createTablePlayerData() {
        createTableUser();

        final String statementStr = "CREATE TABLE IF NOT EXISTS " + PLAYERDATA_TABLE + " (" +
            PID_KEY + " INT UNSIGNED NOT NULL," +
            TREASURE_ID_KEY + " VARCHAR(255) NOT NULL, " +
            TREASURE_TIMESTAMP_KEY + " BIGINT UNSIGNED, " +
            TREASURE_CONTENT_KEY + " MEDIUMBLOB NOT NULL, " +
            // important: don't make TREASURE_ID UNIQUE on its own, only one player could have an entry otherwise
            // but also don't let TREASURE_ID without constrains, else wise a player can infinit entries of the same treasure, not updating them
            "PRIMARY KEY (" + PID_KEY + ", " + TREASURE_ID_KEY + "), " +
            "FOREIGN KEY (" + TREASURE_ID_KEY + ") REFERENCES " + TREASURE_TABLE + "(" + TREASURE_ID_KEY + "), " +
            "FOREIGN KEY (" + PID_KEY + ") REFERENCES " + USER_TABLE + "(" + PID_KEY + "))";

        try (final @NotNull Connection connection = dataSource.getConnection();
             final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
            preparedStatement.executeUpdate();
        } catch (SQLException e) {
            plugin.getComponentLogger().error("Could not create quests.", e);
        }
    }

    /**
     * Tries to add a player into the player table
     */
    protected void addPlayer(final @NotNull OfflinePlayer player) {
        createTableUser();

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

    public static class NoConnectionException extends SQLException {
        public NoConnectionException() {
            super();
        }

        public NoConnectionException(final @NotNull String message) {
            super(message);
        }
    }
}
