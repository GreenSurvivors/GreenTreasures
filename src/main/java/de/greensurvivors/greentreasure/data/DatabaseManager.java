package de.greensurvivors.greentreasure.data;

import com.github.f4b6a3.ulid.Ulid;
import com.zaxxer.hikari.HikariDataSource;
import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.UncaughtExceptionHandler;
import de.greensurvivors.greentreasure.data.dao.PlayerDataDao;
import de.greensurvivors.greentreasure.data.dao.TreasureDao;
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

import java.sql.*;
import java.time.Duration;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Pattern;

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
        NAME = "name";

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
    /// there is no specific missing table exception. Our best guess is to use this pattern.
    private static final @NotNull Pattern MISSING_TABLE_PATTERN = Pattern.compile("Table '.*?' doesn't exist$");
    private final @NotNull GreenTreasure plugin;
    /// we use this instead of {@link org.bukkit.scheduler.BukkitScheduler#runTaskAsynchronously(Plugin, Runnable)} because the bukkit scheduler waits to the next tick to start a task.
    private final @NotNull Executor asyncExecutor;
    private @Nullable HikariDataSource dataSource = null;
    // connection information
    private volatile @NotNull String host = "localhost", database = "database";
    private volatile @Nullable String loginUserName = null, password = null;
    private volatile int port = 3306;

    private TreasureDao treasureDao;
    private PlayerDataDao playerDataDao;

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

            treasureDao = new TreasureDao(dataSource);
            playerDataDao = new PlayerDataDao(dataSource);

            // pre start pool - the first time hasConnection() is called would return false otherwise since the pool needs a second to start after it was invoked
            createTableUser();
            createTableTreasure();
            createTablePlayerData();

            registerDaos();
        }
    }

    private void registerDaos() {
        this.treasureDao = new TreasureDao(dataSource);
        this.playerDataDao = new PlayerDataDao(dataSource);
    }

    public @NotNull CompletableFuture<Void> setTreasureContents(final @NotNull Ulid treasureId, final @NotNull List<@NotNull ItemStack> contents) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            try {
                int rowsAffected = treasureDao.setTreasureContents(treasureId, contents);
                Bukkit.getScheduler().runTask(plugin, () -> {
                    plugin.getTreasureManager().invalidateTreasure(treasureId);
                    resultFuture.complete(null);
                });

                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished set request for treasure {}, on thread {}", rowsAffected, treasureId, Thread.currentThread().getName());
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

    public @NotNull CompletableFuture<@Nullable List<ItemStack>> getTreasureContents(final @NotNull String treasureId) {
        final @NotNull CompletableFuture<@Nullable List<ItemStack>> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }
            try {
                List<ItemStack> items = treasureDao.getTreasureContents(treasureId);
                if (items != null) {
                    plugin.getComponentLogger().debug("successfully got treasure contents for get request for treasure id {}, on thread {}", treasureId, Thread.currentThread().getName());
                    Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(items));
                } else { // this treasure was deleted / never created
                    plugin.getComponentLogger().debug("got no answer for get request for treasure contents {}, on thread {}", treasureId, Thread.currentThread().getName());
                    Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));
                }
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not get treasure data for treasure id '{}'", treasureId, e);

                if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                    Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                }
            }
        });

        return resultFuture;
    }

    public @NotNull CompletableFuture<Void> deleteTreasure(final @NotNull Ulid treasureId) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            try {
                int rowsAffected = playerDataDao.deletePlayerData(treasureId) + treasureDao.deleteTreasure(treasureId);
                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished delete request for treasure {}, on thread {}", rowsAffected, treasureId, Thread.currentThread().getName());

                Bukkit.getScheduler().runTask(plugin, () -> forgetAll(treasureId).thenRun(() -> {
                    plugin.getTreasureManager().invalidateTreasure(treasureId);
                    resultFuture.complete(null);
                }));
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
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            try {
                int rowsAffected = treasureDao.setRandom(treasureId, nonEmptyPermyriad);
                Bukkit.getScheduler().runTask(plugin, () -> {
                    plugin.getTreasureManager().invalidateTreasure(treasureId);
                    resultFuture.complete(null);
                });

                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished set slot chance request for treasure {} to {}, on thread {}", rowsAffected, treasureId, nonEmptyPermyriad / 100.0D, Thread.currentThread().getName());
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
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            try {
                int rowsAffected = treasureDao.setShared(treasureId, isShared);
                Bukkit.getScheduler().runTask(plugin, () -> {
                    plugin.getTreasureManager().invalidateTreasure(treasureId);
                    resultFuture.complete(null);
                });

                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished set shared request for treasure {} to {}, on thread {}", rowsAffected, treasureId, isShared, Thread.currentThread().getName());
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
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            try {

                int rowsAffected = treasureDao.setUnlimited(treasureId, isUnLimited);
                Bukkit.getScheduler().runTask(plugin, () -> {
                    plugin.getTreasureManager().invalidateTreasure(treasureId);
                    resultFuture.complete(null);
                });

                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished set unlimited request for treasure {} to {}, on thread {}", rowsAffected, treasureId, isUnLimited, Thread.currentThread().getName());
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
    public @NotNull CompletableFuture<Void> setForgetDuration(final @NotNull Ulid treasureId, final @Nullable Duration forgettingDuration) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            try {
                int rowsAffected = treasureDao.setForgetDuration(treasureId, forgettingDuration);
                Bukkit.getScheduler().runTask(plugin, () -> {
                    plugin.getTreasureManager().invalidateTreasure(treasureId);
                    resultFuture.complete(null);
                });

                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished set forget duration request for treasure {} to {}, on thread {}", rowsAffected, treasureId, forgettingDuration, Thread.currentThread().getName());
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
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            try {
                int rowsAffected = treasureDao.setFindFreshMessageOverride(treasureId, findFreshMessageOverride);
                Bukkit.getScheduler().runTask(plugin, () -> {
                    plugin.getTreasureManager().invalidateTreasure(treasureId);
                    resultFuture.complete(null);
                });

                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished set findFreshMessageOverride request for treasure {} to \"{}\", on thread {}", rowsAffected, treasureId, findFreshMessageOverride, Thread.currentThread().getName());
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
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            try {
                int rowsAffected = treasureDao.setFindLootedMessageOverride(treasureId, findLootedMessageOverride);
                Bukkit.getScheduler().runTask(plugin, () -> {
                    plugin.getTreasureManager().invalidateTreasure(treasureId);
                    resultFuture.complete(null);
                });

                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished set findLootedMessageOverride request for treasure {} to {}, on thread {}", rowsAffected, treasureId, findLootedMessageOverride, Thread.currentThread().getName());
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

    public @Nullable TreasureInfo loadTreasureUrgently(final @NotNull Ulid treasureId) {
        try {
            TreasureInfo info = treasureDao.loadTreasureUrgently(treasureId);

            if (info == null) {
                plugin.getComponentLogger().debug("got no answer for get request for treasure info {}, on thread {}", treasureId, Thread.currentThread().getName());
                return null;
            } else {
                return info;
            }
        } catch (SQLException e) {
            if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
            }

            plugin.getComponentLogger().warn("Could not get treasure data for treasure id '{}'", treasureId, e);
            return null;
        } catch (TreasureDao.MalformedItemListException e) {
            plugin.getComponentLogger().warn("No or malformed item list found for treasure id {}. Skipping. On thread {}", treasureId, Thread.currentThread().getName());
            return null;
        }
    }

    public @NotNull CompletableFuture<@Nullable TreasureInfo> loadTreasure(final @NotNull Ulid treasureId) {
        final @NotNull CompletableFuture<@Nullable TreasureInfo> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            final @Nullable TreasureInfo result = loadTreasureUrgently(treasureId);
            Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(result));
        });

        return resultFuture;
    }

    public @NotNull CompletableFuture<@NotNull SetUniqueList<@NotNull Ulid>> getTreasureIds() {
        final @NotNull CompletableFuture<@NotNull SetUniqueList<@NotNull Ulid>> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            try {
                final @NotNull SetUniqueList<@NotNull Ulid> resultList = treasureDao.getTreasureIds();
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
     * Saves the loot detail of a player
     *
     * @param player     player whose data is about to be saved, null for global / shared
     * @param treasureId the treasure id
     * @param lootDetail the new data
     */
    public @NotNull CompletableFuture<Void> setPlayerData(final @Nullable OfflinePlayer player, final @NotNull Ulid treasureId, final @NotNull PlayerLootDetail lootDetail) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            addPlayer(player == null ? SHARED_PROFILE : player);

            UUID uuid = player == null ? SHARED_PROFILE.getUniqueId() : player.getUniqueId();
            try {
                final int rowsAffected = playerDataDao.setPlayerData(uuid, treasureId, lootDetail);
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));

                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished set request for player {} at timestamp {}, on thread {}",
                    rowsAffected, player == null ? SHARED_PROFILE.getName() : player.getName(), lootDetail.lastChangedTimeStamp(), Thread.currentThread().getName());
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not set player loot data for '{}' at timestamp '{}' for '{}'",
                    treasureId, lootDetail.lastChangedTimeStamp(), player == null ? SHARED_PROFILE.getName() : player.getName(), e);

                if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                    Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
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
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }
            addPlayer(player == null ? SHARED_PROFILE : player);

            UUID uuid = player == null ? SHARED_PROFILE.getUniqueId() : player.getUniqueId();
            try {
                PlayerLootDetail data = playerDataDao.getPlayerData(uuid, treasureId);
                if (data != null) {
                    plugin.getComponentLogger().debug("successfully got data for getPlayerData request for player {}: {}, on thread {}", player == null ? "!Shared!" : player.getName(), data.unLootedStuff(), Thread.currentThread().getName());
                    Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(data));
                } else { //player had never opened this treasure
                    plugin.getComponentLogger().debug("got no answer for get request for player {}, and defaulted to null player data, on thread {}",
                            player == null ? SHARED_PROFILE.getName() : player.getName(), Thread.currentThread().getName());
                    Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));
                }
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));

                plugin.getComponentLogger().warn("Could not get treasure identifier '{}' player loot detail for '{}'", treasureId, player == null ? SHARED_PROFILE.getName() : player.getName(), e);

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
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            try {
                int rowsAffected = playerDataDao.forgetAll(treasureId);
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));
                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished forgetAll request for treasure id {}, on thread {}", rowsAffected, treasureId, Thread.currentThread().getName());
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not forgetAll treasure data for '{}'", treasureId, e);

                if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                    Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                }
            }
        });

        return resultFuture;
    }

    public @NotNull CompletableFuture<Void> forgetPlayer(final @Nullable OfflinePlayer player, final @NotNull Ulid treasureId) {
        final @NotNull CompletableFuture<Void> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            OfflinePlayer offlinePlayer = player == null ? SHARED_PROFILE : player;
            addPlayer(offlinePlayer);

            try {
                int rowsAffected = playerDataDao.forgetPlayer(offlinePlayer.getUniqueId(), treasureId);
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(null));

                plugin.getComponentLogger().debug("Rows affected: {} -> successfully finished forgetPlayer for {} and treasure {}, on thread {}",
                    rowsAffected, offlinePlayer.getName(), treasureId, Thread.currentThread().getName());
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(e));

                plugin.getComponentLogger().warn("Could not forget player loot detail for '{}' with treasure '{}'", offlinePlayer.getName(), treasureId, e);

                if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                    Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                }
            }
        });

        return resultFuture;
    }

    public @NotNull CompletableFuture<@NotNull Map<UUID, @NotNull PlayerLootDetail>> getAllPlayerData(final @NotNull Ulid treasureId) {
        final CompletableFuture<Map<UUID, PlayerLootDetail>> resultFuture = new CompletableFuture<>();

        asyncExecutor.execute(() -> {
            if (!hasConnection()) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.completeExceptionally(new NoConnectionException()));
                return;
            }

            try {
                Map<UUID, PlayerLootDetail> result = playerDataDao.getAllPlayerData(treasureId);
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(result));
            } catch (SQLException e) {
                Bukkit.getScheduler().runTask(plugin, () -> resultFuture.complete(Collections.emptyMap()));

                plugin.getComponentLogger().warn("Could not retrieve all player data for treasure '{}'", treasureId, e);

                if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                    Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
                }
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
            try {
                treasureDao.createTable();
            } catch (SQLException e) {
                plugin.getComponentLogger().error("Could not create treasure data table.", e);
            }
        }
    }

    /**
     * Create a player table to reference against, only table without own dao
     */
    protected void createTableUser() {
        if (dataSource != null) {
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

    /**
     * Contains all player looted treasures
     */
    private void createTablePlayerData() {
        try {
            playerDataDao.createTable();
        } catch (SQLException e) {
            if (e instanceof SQLSyntaxErrorException && MISSING_TABLE_PATTERN.matcher(e.getMessage()).matches()) {
                Bukkit.getScheduler().runTask(plugin, plugin::shutdownForcefully);
            }

            plugin.getComponentLogger().error("Could not create table {}.", PLAYERDATA_TABLE, e);
        }
    }

    /**
     * Tries to add a player into the player table, only table without own dao
     */
    protected void addPlayer(final @NotNull OfflinePlayer player) {
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

    public static class NoConnectionException extends SQLException {
        public NoConnectionException() {
            super();
        }

        public NoConnectionException(final @NotNull String message) {
            super(message);
        }
    }
}
