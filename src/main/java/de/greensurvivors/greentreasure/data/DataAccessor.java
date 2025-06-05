package de.greensurvivors.greentreasure.data;

import com.github.f4b6a3.ulid.Ulid;
import com.zaxxer.hikari.HikariDataSource;
import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.UncaughtExceptionHandler;
import de.greensurvivors.greentreasure.data.dao.PlayerData;
import de.greensurvivors.greentreasure.data.dao.TreasureData;
import de.greensurvivors.greentreasure.data.dao.UserData;
import de.greensurvivors.greentreasure.dataobjects.PlayerLootDetail;
import de.greensurvivors.greentreasure.dataobjects.TreasureInfo;
import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.inventory.ItemStack;
import org.bukkit.plugin.Plugin;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.Range;

import java.sql.SQLException;
import java.sql.SQLSyntaxErrorException;
import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.*;
import java.util.regex.Pattern;

public class DataAccessor {
    // config keys
    private final static @NotNull String
        HOST = "host",
        PORT = "port",
        LOGIN_USER_NAME = "user",
        PASSWORD = "password",
        DATABASE = "database";
    /// there is no specific missing table exception. Our best guess is to use this pattern.
    public static final @NotNull Pattern MISSING_TABLE_PATTERN = Pattern.compile("Table '.*?' doesn't exist$");
    private final @NotNull GreenTreasure plugin;
    /// we use this instead of {@link org.bukkit.scheduler.BukkitScheduler#runTaskAsynchronously(Plugin, Runnable)}, so we don't block a plattform thread while waiting for io.
    private volatile @NotNull ExecutorService asyncExecutor;
    private volatile @MonotonicNonNull HikariDataSource dataSource = null;
    // connection information
    private volatile @NotNull String host = "localhost", database = "database";
    private volatile @Nullable String loginUserName = null, password = null;
    private volatile int port = 3306;
    private @Nullable UserData userDataAccessor = null;
    private @Nullable TreasureData treasureDataAccessor = null;
    private @Nullable PlayerData playerDataAccessor = null;

    public DataAccessor(final @NotNull GreenTreasure plugin) {
        this.plugin = plugin;

        createExecutorService();
    }

    private void createExecutorService() {
        // We expect a burst of requests and long time nothing.
        // So don't hold any thread in the dry periods,
        // don't block any plattform thread while waiting for the database,
        // but grow as big as we need to.
        // Log errors via the plugins logger
        final @NotNull ThreadFactory threadFactory = Thread.ofVirtual()
            .name("GreenTreasure Database thread - ", 0)
            .uncaughtExceptionHandler(new UncaughtExceptionHandler(plugin.getComponentLogger()))
            .factory();

        asyncExecutor = Executors.newThreadPerTaskExecutor(threadFactory);
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
            createExecutorService();

            dataSource = new HikariDataSource();
            dataSource.setJdbcUrl("jdbc:mysql://" + host + ":" + port + "/" + database);
            dataSource.setUsername(loginUserName);
            dataSource.setPassword(password);
            //dataSource.setIdleTimeout(60000); // unused, since maximum pool size == minimum
            dataSource.setMaximumPoolSize(4); // don't keep the default 10 threads alive. we are way too small for that

            userDataAccessor = new UserData(plugin, dataSource);
            treasureDataAccessor = new TreasureData(plugin, dataSource);
            playerDataAccessor = new PlayerData(plugin, dataSource);

            // pre start pool - the first time hasConnection() is called would return false otherwise since the pool needs a second to start after it was invoked
            userDataAccessor.createTableUser();
            treasureDataAccessor.createTableTreasure();
            treasureDataAccessor.updateTableTreasure();
            playerDataAccessor.createTablePlayerData();
        } else {
            userDataAccessor = null;
            treasureDataAccessor = null;
            playerDataAccessor = null;
        }
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
            try {
                if (asyncExecutor != null) {
                    asyncExecutor.shutdown();

                    if (!asyncExecutor.awaitTermination(3, TimeUnit.SECONDS)) {
                        plugin.getComponentLogger().error("Error: the timeout of 3 seconds elapsed and the database executor still hasn't returned! Data loss is imminent!");
                    }
                }
            } catch (InterruptedException e) {
                plugin.getComponentLogger().error("Couldn't gracefully shut down database thread pool. You may encounter data loss!", e);
            }

            dataSource.close();
            plugin.getComponentLogger().debug("Logout database.");
        }
    }

    public @NotNull CompletableFuture<Void> setTreasureContents(final @NotNull Ulid treasureId, final @NotNull List<@NotNull ItemStack> contents) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!plugin.getDataAccessor().hasConnection() || treasureDataAccessor == null) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            try {
                treasureDataAccessor.setTreasureContents(treasureId, contents);
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not set treasure contents for '{}'", treasureId, e);

                if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                    Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                }
            }
        });

        return resultFuture;
    }

    public @Nullable TreasureInfo loadTreasureUrgently(final @NotNull Ulid treasureId) {
        if (!plugin.getDataAccessor().hasConnection() || treasureDataAccessor == null) {
            return null;
        }

        try {
            return treasureDataAccessor.loadTreasureUrgently(treasureId);
        } catch (SQLException e) {
            plugin.getComponentLogger().warn("Could not get treasure data for treasure id '{}'", treasureId, e);

            if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
            }
        }

        return null;
    }

    public @NotNull CompletableFuture<@Nullable TreasureInfo> loadTreasure(final @NotNull Ulid treasureId) {
        final @NotNull CompletableFuture<@Nullable TreasureInfo> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!plugin.getDataAccessor().hasConnection() || treasureDataAccessor == null) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            final @Nullable TreasureInfo result;
            try {
                result = treasureDataAccessor.loadTreasureUrgently(treasureId);
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(result));
            } catch (SQLException e) {
                plugin.getComponentLogger().warn("Could not get treasure data for treasure id '{}'", treasureId, e);

                if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                    Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                }

                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));
            }
        });

        return resultFuture;
    }

    public @NotNull CompletableFuture<Void> deleteTreasure(final @NotNull Ulid treasureId) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!plugin.getDataAccessor().hasConnection() || treasureDataAccessor == null) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            try {
                final int affectedPlayerRows = playerDataAccessor.deleteTreasure(treasureId);
                final int affectedTreasureRows = treasureDataAccessor.deleteTreasure(treasureId);

                int rowsAffected = affectedPlayerRows + affectedTreasureRows;
                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished delete request for treasure {}, on thread {}", rowsAffected, treasureId, Thread.currentThread().getName());

                playerDataAccessor.forgetAll(treasureId);

                Bukkit.getScheduler().runTask(plugin, () -> {
                    plugin.getTreasureManager().invalidateTreasure(treasureId);
                    resultFuture.complete(null);
                });
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not delete treasure with id '{}'", treasureId, e);

                if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                    Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                }
            }
        });

        return resultFuture;
    }

    public @NotNull CompletableFuture<Void> forgetAll(final @NotNull Ulid treasureId) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!plugin.getDataAccessor().hasConnection() || playerDataAccessor == null) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            try {
                playerDataAccessor.forgetAll(treasureId);

                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not forget all treasure with id '{}'", treasureId, e);

                if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                    Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                }
            }
        });

        return resultFuture;
    }

    // note: yes it has to return a list and not a set, because its entries have to be addressable by index
    /// guarantees no duplicate Ulids in the returned List
    public @NotNull CompletableFuture<@NotNull List<@NotNull Ulid>> getTreasureIds() {
        final @NotNull CompletableFuture<@NotNull List<@NotNull Ulid>> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!plugin.getDataAccessor().hasConnection() || treasureDataAccessor == null) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            try {
                final @NotNull List<@NotNull Ulid> resultList = treasureDataAccessor.getTreasureIds();
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(resultList));

            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not set treasure ids", e);

                if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                    Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                }
            }
        });

        return resultFuture;
    }

    /**
     * set the random slot chance
     *
     * @param treasureId        information to identify a treasure
     * @param nonEmptyPermyriad how many permyrid (percent, but with a max value of 10 000, just to not deal with floating point problems)
     *                          should contain an item in the freshly opened treasure;
     */
    public @NotNull CompletableFuture<Void> setRandom(final @NotNull Ulid treasureId, final @Range(from = 0, to = 10000) Short nonEmptyPermyriad) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!plugin.getDataAccessor().hasConnection() || treasureDataAccessor == null) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            try {
                treasureDataAccessor.setRandom(treasureId, nonEmptyPermyriad);
                Bukkit.getScheduler().runTask(plugin, () -> {
                    plugin.getTreasureManager().invalidateTreasure(treasureId);
                    resultFuture.complete(null);
                });
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not set treasure slot chance for '{}' to {}", treasureId, nonEmptyPermyriad / 100.0D, e);

                if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                    Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                }
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
    public @NotNull CompletableFuture<Void> setShared(final @NotNull Ulid treasureId, final boolean isShared) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!plugin.getDataAccessor().hasConnection() || treasureDataAccessor == null) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            try {
                treasureDataAccessor.setShared(treasureId, isShared);

                Bukkit.getScheduler().runTask(plugin, () -> {
                    plugin.getTreasureManager().invalidateTreasure(treasureId);
                    resultFuture.complete(null);
                });

            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not set treasure shared setting for '{}' to {}", treasureId, isShared, e);

                if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                    Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                }
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
    public @NotNull CompletableFuture<Void> setUnlimited(final @NotNull Ulid treasureId, final boolean isUnLimited) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!plugin.getDataAccessor().hasConnection() || treasureDataAccessor == null) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            try {
                treasureDataAccessor.setUnlimited(treasureId, isUnLimited);

                Bukkit.getScheduler().runTask(plugin, () -> {
                    plugin.getTreasureManager().invalidateTreasure(treasureId);
                    resultFuture.complete(null);
                });

            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not set treasure unlimited setting for '{}' to {}", treasureId, isUnLimited, e);

                if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                    Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                }
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
    public @NotNull CompletableFuture<Void> setForgetDuration(final @NotNull Ulid treasureId,
                                                              final @Nullable Instant forgetStart,
                                                              final @Nullable Duration forgettingDuration) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!plugin.getDataAccessor().hasConnection() || treasureDataAccessor == null) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            try {
                treasureDataAccessor.setForgetDuration(treasureId, forgetStart, forgettingDuration);

                Bukkit.getScheduler().runTask(plugin, () -> {
                    plugin.getTreasureManager().invalidateTreasure(treasureId);
                    resultFuture.complete(null);
                });
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not set treasure forget duration for '{}' to {}", treasureId, forgettingDuration, e);

                if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                    Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                }
            }
        });

        return resultFuture;
    }

    /**
     * @param treasureId information to identify a treasure
     */
    public @NotNull CompletableFuture<Void> setFindFreshMessageOverride(final @NotNull Ulid treasureId, final @Nullable String findFreshMessageOverride) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!plugin.getDataAccessor().hasConnection() || treasureDataAccessor == null) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            try {
                treasureDataAccessor.setFindFreshMessageOverride(treasureId, findFreshMessageOverride);

                Bukkit.getScheduler().runTask(plugin, () -> {
                    plugin.getTreasureManager().invalidateTreasure(treasureId);
                    resultFuture.complete(null);
                });
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not set treasure findFreshMessageOverride setting for '{}' to \"{}\"", treasureId, findFreshMessageOverride, e);

                if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                    Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                }
            }
        });

        return resultFuture;
    }

    /**
     * @param treasureId information to identify a treasure
     */
    public @NotNull CompletableFuture<Void> setFindLootedMessageOverride(final @NotNull Ulid treasureId, final @Nullable String findLootedMessageOverride) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!plugin.getDataAccessor().hasConnection() || treasureDataAccessor == null) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            try {
                treasureDataAccessor.setFindLootedMessageOverride(treasureId, findLootedMessageOverride);

                Bukkit.getScheduler().runTask(plugin, () -> {
                    plugin.getTreasureManager().invalidateTreasure(treasureId);
                    resultFuture.complete(null);
                });
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not set treasure findLootedMessageOverride setting for '{}' to {}", treasureId, findLootedMessageOverride, e);

                if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                    Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                }
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
    public @NotNull CompletableFuture<Void> setPlayerData(final @Nullable OfflinePlayer player, final @NotNull Ulid treasureId,
                                                          final @NotNull PlayerLootDetail lootDetail) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!plugin.getDataAccessor().hasConnection() || userDataAccessor == null) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            userDataAccessor.addPlayer(player );
            final @Nullable Integer playerId;
            try {
                playerId = userDataAccessor.getPlayerId(player == null ? null : player.getUniqueId());
            } catch (SQLException e) {
                plugin.getComponentLogger().warn("Could not get player identifier for '{}'", player == null ? "shared" : player.getName(), e);

                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                    Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                }
                return;
            }

            if (playerId != null) {
                try {
                    playerDataAccessor.setPlayerData(playerId, treasureId, lootDetail);

                    plugin.getServer().getScheduler().runTask(plugin, () -> resultFuture.complete(null));
                } catch (SQLException e) {
                    Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                    plugin.getComponentLogger().warn("Could not set player loot data for player id '{}' at timestamp '{}' for '{}'",
                        treasureId, lootDetail.lastChangedInstant(), playerId, e);

                    if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                        Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                    }
                }
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
    public @NotNull CompletableFuture<@Nullable PlayerLootDetail> getPlayerData(final @Nullable OfflinePlayer player, final @NotNull Ulid treasureId) {
        final @NotNull CompletableFuture<@Nullable PlayerLootDetail> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!plugin.getDataAccessor().hasConnection() || userDataAccessor == null) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            userDataAccessor.addPlayer(player );
            final @Nullable Integer playerId;
            try {
                playerId = userDataAccessor.getPlayerId(player == null ? null : player.getUniqueId());
            } catch (SQLException e) {
                plugin.getComponentLogger().warn("Could not get player identifier for '{}'", player == null ? "shared" : player.getName(), e);

                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                    Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                }
                return;
            }

            if (playerId != null) {
                final @Nullable PlayerLootDetail lootDetail;
                try {
                    lootDetail = playerDataAccessor.getPlayerData(playerId, treasureId);
                }  catch (SQLException e) {
                    plugin.getComponentLogger().warn("Could not get treasure identifier '{}' player loot detail for player id '{}'", treasureId, playerId, e);
                    Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                    if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                        Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                    }

                    return;
                }

                if (lootDetail != null && lootDetail.unLootedStuff() == null) {
                    final @NotNull PlayerLootDetail defaultLoot;
                    try {
                        defaultLoot = new PlayerLootDetail(lootDetail.firstLootedInstant(), lootDetail.lastChangedInstant(), treasureDataAccessor.getTreasureContents(treasureId));
                        Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(defaultLoot));
                    }  catch (SQLException e) {
                        plugin.getComponentLogger().warn("Could not get treasure data for treasure id '{}'", treasureId, e);
                        Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                        if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                            Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                        }
                    }
                } else {
                    Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(lootDetail));
                }
            } else {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));
            }
        });

        return resultFuture;
    }

    public @NotNull CompletableFuture<@NotNull Map<UUID, @NotNull PlayerLootDetail>> getAllPlayerData(final @NotNull Ulid treasureId) {
        final CompletableFuture<Map<UUID, PlayerLootDetail>> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!plugin.getDataAccessor().hasConnection() || userDataAccessor == null) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            final @NotNull Map<UUID, Integer> playerUUIDs;
            try {
                playerUUIDs = userDataAccessor.getAllPlayerIds();
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(Collections.emptyMap()));
                plugin.getComponentLogger().warn("Could not retrieve all player data for treasure '{}'", treasureId, e);

                if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                    Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                }

                return;
            }

            final Map<UUID, PlayerLootDetail> result = new HashMap<>();

            for (Map.Entry<UUID, Integer> entry : playerUUIDs.entrySet()) {
                try {
                    final @Nullable PlayerLootDetail lootDetail = playerDataAccessor.getPlayerData(entry.getValue(), treasureId);

                    if (lootDetail != null && lootDetail.unLootedStuff() == null) {
                        final @NotNull PlayerLootDetail defaultLoot = new PlayerLootDetail(
                            lootDetail.firstLootedInstant(),
                            lootDetail.lastChangedInstant(),
                            treasureDataAccessor.getTreasureContents(treasureId)
                        );
                        result.put(entry.getKey(), defaultLoot);
                    } else {
                        result.put(entry.getKey(), lootDetail);
                    }
                } catch (SQLException e) {
                    plugin.getComponentLogger().warn("Could not retrieve player loot detail for treasure '{}' and player with uuid '{}'", treasureId, entry.getKey(), e);

                    if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                        Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));
                        Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                        return;
                    }
                }
            }

            Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(result));
        });

        return resultFuture;
    }

    public @NotNull CompletableFuture<Void> forgetPlayer(final @Nullable OfflinePlayer player, final @NotNull Ulid treasureId) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!plugin.getDataAccessor().hasConnection() || userDataAccessor == null) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            userDataAccessor.addPlayer(player );
            final @Nullable Integer playerId;
            try {
                playerId = userDataAccessor.getPlayerId(player == null ? null : player.getUniqueId());
            } catch (SQLException e) {
                plugin.getComponentLogger().warn("Could not get player identifier for '{}'", player == null ? "shared" : player.getName(), e);

                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                    Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                }
                return;
            }

            if (playerId != null) {
                try {
                    playerDataAccessor.forgetPlayer(playerId, treasureId);
                } catch (SQLException e) {
                    Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                    plugin.getComponentLogger().warn("Could not forget player loot detail for '{}' with treasure '{}'", player == null ? "@shared" : player.getName(), treasureId, e);

                    if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                        Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                    }
                }
            }
        });

        return resultFuture;
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
