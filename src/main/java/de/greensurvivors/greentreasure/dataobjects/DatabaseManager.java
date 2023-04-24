package de.greensurvivors.greentreasure.dataobjects;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.ToNumberPolicy;
import com.google.gson.reflect.TypeToken;
import de.greensurvivors.greentreasure.GreenTreasure;
import de.greensurvivors.greentreasure.TreasureLogger;
import de.greensurvivors.greentreasure.config.TreasureConfig;
import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.inventory.ItemStack;
import org.bukkit.scheduler.BukkitTask;
import org.jetbrains.annotations.NotNull;

import javax.annotation.Nullable;
import java.io.Serial;
import java.io.Serializable;
import java.lang.reflect.Type;
import java.sql.*;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

/**
 * Exception if sql connection is currently not available.
 */
class NoSQLConnectionException extends Exception implements Serializable {
    /**
     * default serialVersionUID
     */
    @Serial
    private static final long serialVersionUID = 1L;

    public NoSQLConnectionException() {
        super("No SQL Connection");
    }
}


public class DatabaseManager {
    private final String
            PLAYER_KEY = "players",
            /**
             * player identifier
             */
            PID_KEY = "pid",
            /**
             * uuid of player
             */
            UUID_KEY = "uuid",
            /**
             * name of player
             */
            NAME_KEY = "name",
            /**
             * last time a player had looted the treasure
             */
            TREASURE_TIMESTAMP_KEY = "tstamp",
            /**
             * the items remaining in this treasure
             */
            TREASURE_LOOT_KEY = "loot";

    private final String
            HOST = "host",
            PORT = "port",
            USER = "user",
            PASSWORD = "password",
            DATABASE = "database";
    private final GreenTreasure plugin;
    //don't lose precision on longs
    private final Gson GSON = new GsonBuilder().setObjectToNumberStrategy(ToNumberPolicy.LONG_OR_DOUBLE).create();
    private final Type MAP_TYPE = new TypeToken<Map>() {}.getType();
    private final Type LIST_TYPE = new TypeToken<List>() {}.getType();
    private Connection conn;
    private String host, user, password, database;
    private Integer port;
    private BukkitTask thread = null;
    private int reconnected_Tries = 0;
    private boolean shouldConnect = true;

    public DatabaseManager(GreenTreasure plugin, HashMap<String, Object> databaseData) {
        this.plugin = plugin;

        loadData(databaseData);

        if (shouldConnect) {
            reconnect();
        }
    }

    public HashMap<String, Object> getDatabaseData() {
        HashMap<String, Object> databaseData = new HashMap<>();

        databaseData.put(HOST, host == null ? HOST : host);
        databaseData.put(PORT, port == null ? PORT : port);
        databaseData.put(USER, user == null ? USER : user);
        databaseData.put(PASSWORD, password == null ? PASSWORD : password);
        databaseData.put(DATABASE, database == null ? DATABASE : database);

        return databaseData;
    }

    /**
     * Returns the known information about a treasure looted by a player
     *
     * @param player         player who we want data of
     * @param lootIdentifier identifier of the treasure
     * @return PlayerLootDetail or null, if getting the data wasn't successfully
     * (like in cases if the player never opened this treasure)
     */
    public @Nullable PlayerLootDetail getPlayerData(@NotNull OfflinePlayer player, @NotNull String lootIdentifier) {
        if (hasConnection()) {
            PreparedStatement st = null;
            ResultSet rs = null;

            try {
                st = conn.prepareStatement(String.format("SELECT EXISTS ( SELECT TABLE_NAME FROM " +
                                "information_schema.TABLES " +
                                "WHERE TABLE_NAME = '%s' )", lootIdentifier),
                        ResultSet.TYPE_FORWARD_ONLY,
                        ResultSet.CONCUR_READ_ONLY);
                rs = st.executeQuery();

                if (rs.next() && rs.getInt(1) > 0) {
                    addPlayer(player);

                    st = conn.prepareStatement(String.format(
                                    "SELECT `%s`, `%s` " +
                                            "FROM `%s` " +
                                            "AS c JOIN %s AS p ON c.%s = p.%s WHERE %s LIKE ?",
                                    TREASURE_TIMESTAMP_KEY, TREASURE_LOOT_KEY,
                                    lootIdentifier,
                                    PLAYER_KEY, PID_KEY, PID_KEY, UUID_KEY),
                            ResultSet.TYPE_FORWARD_ONLY,
                            ResultSet.CONCUR_READ_ONLY);
                    st.setString(1, player.getUniqueId().toString());
                    rs = st.executeQuery();

                    if (rs.next()) {
                        long timeStamp = rs.getLong(1);

                        //get list from string
                        List<?> itemList = GSON.fromJson(rs.getString(2), LIST_TYPE);
                        List<ItemStack> items = TreasureConfig.deserializeItemList(itemList);

                        return new PlayerLootDetail(timeStamp, items);
                    } else { //player had never opened this treasure
                        TreasureLogger.log(Level.INFO, "null treasure");
                        return null;
                    }
                } else {
                    TreasureLogger.log(Level.FINE, String.format("Unknown loot identifier '%s' for '%s'", lootIdentifier, player.getName()));
                }
            } catch (SQLException e) {
                TreasureLogger.log(Level.WARNING, String.format("Could not get lootIdentifier '%s' loot detail for '%s'", lootIdentifier, player.getName()));
                e.printStackTrace();
            } finally {
                closeResources(st, rs);
            }
        } else {
            noConnection();
        }
        return null;
    }

    /**
     * Saves the loot detail of a player
     *
     * @param player         player whose data is about to be saved.
     * @param lootIdentifier the treasure id
     * @param lootDetail     the new data
     */
    public void setPlayerData(@NotNull OfflinePlayer player, @NotNull String lootIdentifier, @NotNull PlayerLootDetail lootDetail) {
        if (hasConnection()) {
            PreparedStatement st = null;
            try {
                createTreasureTable(lootIdentifier);
                addPlayer(player);

                st = conn.prepareStatement(String.format(
                        "INSERT INTO `%s` " +
                                "(%s, %s, %s) " +
                                "VALUES ((SELECT %s FROM %s WHERE %s LIKE ?), ?, ?) " +
                                "ON DUPLICATE KEY UPDATE %s = VALUES(%s)",
                        lootIdentifier,
                        PID_KEY,  TREASURE_TIMESTAMP_KEY, TREASURE_LOOT_KEY,
                        PID_KEY, PLAYER_KEY, UUID_KEY,
                        TREASURE_LOOT_KEY, TREASURE_LOOT_KEY));

                st.setString(1, player.getUniqueId().toString());
                st.setLong(2, lootDetail.lastLootedTimeStamp());
                st.setString(3, GSON.toJson(TreasureConfig.serializeItemList(lootDetail.unLootedStuff()), LIST_TYPE));
                st.executeUpdate();
            } catch (SQLException e) {
                TreasureLogger.log(Level.WARNING, String.format("Could not set player loot detail '%s' for '%s' to '%s'", lootIdentifier, player.getName(), GSON.toJson(lootDetail.serialize(), MAP_TYPE)));
                e.printStackTrace();
            } finally {
                closeResources(st, null);
            }
        } else {
            noConnection();
        }
    }

    /**
     * @param lootIdentifier
     */
    public void forgetAll(@NotNull String lootIdentifier) {
        deleteTreasureTable(lootIdentifier);
    }

    /**
     * creates a new table for a treasure
     *
     * @param identifier unique id of this treasure
     */
    private void createTreasureTable(String identifier) {
        // try to connect to treasure tables
        if (hasConnection()) {
            PreparedStatement st = null;
            try {
                st = conn.prepareStatement(String.format(
                        "CREATE TABLE IF NOT EXISTS `%s` (" +
                                "%s INT UNSIGNED PRIMARY KEY, " +
                                "%s BIGINT, " +
                                "%s MEDIUMTEXT, " +
                                "FOREIGN KEY (%s) REFERENCES %s(%s))",
                        identifier,
                        PID_KEY,
                        TREASURE_TIMESTAMP_KEY,
                        TREASURE_LOOT_KEY,
                        PID_KEY, PLAYER_KEY, PID_KEY));
                st.executeUpdate();
            } catch (SQLException e) {
                TreasureLogger.log(Level.WARNING, "Could not create loot detail table for " + identifier);
                e.printStackTrace();
            } finally {
                closeResources(st, null);
            }
        } else {
            TreasureLogger.log(Level.WARNING, "Could not create loot detail table, no connection.");
        }
    }

    /**
     * deletes the table of a treasure and therefore effectively forgets any player ever looted it
     * @param identifier
     */
    private void deleteTreasureTable(String identifier) {
        // try to connect to treasure tables
        if (hasConnection()) {
            PreparedStatement st = null;
            try {
                st = conn.prepareStatement(String.format(
                        "DROP TABLE IF EXISTS `%s`", identifier));
                st.executeUpdate();
            } catch (SQLException e) {
                TreasureLogger.log(Level.WARNING, "Could not delete loot detail Table for " + identifier);
                e.printStackTrace();
            } finally {
                closeResources(st, null);
            }
        } else {
            TreasureLogger.log(Level.WARNING, "Could not create loot detail table, no connection.");
        }
    }

    /**
     * Tests and validates the connection
     *
     * @return true, if the connection is valid
     */
    public Boolean hasConnection() {
        try {
            return (conn != null) && conn.isValid(1);
        } catch (SQLException ex) {
            return false;
        }
    }

    /**
     * loads MySQL-data from config map.
     */
    private void loadData(HashMap<String, Object> databaseData) {
        if (databaseData == null) {
            return;
        }

        // host
        if (databaseData.containsKey(HOST) && databaseData.get(HOST) instanceof String hostStr) {
            host = hostStr;
        } else {
            TreasureLogger.log(Level.WARNING, "Missing sql value for " + HOST);
            shouldConnect = false;
        }
        // user
        if (databaseData.containsKey(USER) && databaseData.get(USER) instanceof String userStr) {
            user = userStr;
        } else {
            TreasureLogger.log(Level.WARNING, "Missing sql value for " + USER);
            shouldConnect = false;
        }
        // password
        if (databaseData.containsKey(PASSWORD) && databaseData.get(PASSWORD) instanceof String passwortStr) {
            password = passwortStr;
        } else {
            TreasureLogger.log(Level.WARNING, "Missing sql value for " + PASSWORD);
        }
        // database
        if (databaseData.containsKey(DATABASE) && databaseData.get(DATABASE) instanceof String databaseNameStr) {
            database = databaseNameStr;
        } else {
            TreasureLogger.log(Level.WARNING, "Missing sql value for " + DATABASE);
            shouldConnect = false;
        }
        // port
        if (databaseData.containsKey(PORT) && databaseData.get(PORT) instanceof Integer portInt) {
            port = portInt;
        } else {
            TreasureLogger.log(Level.WARNING, "Missing sql value for " + PORT);
        }
    }

    /**
     * opens a SQL-connection
     */
    private void connect() {
        // After 5 stop and don't attempt to connect, if the data wasn't valid
        if (++reconnected_Tries > 10 || !shouldConnect) {
            return;
        }

        // close reconnection thread
        closeThread();
        // close old connection
        if (hasConnection())
            closeConnection();

        // try to connect
        try {
            TreasureLogger.log(Level.FINE, "SQL-Connection");

            // preload class
            Class.forName("com.mysql.cj.jdbc.Driver");
            // verbinde
            conn = DriverManager.getConnection("jdbc:mysql://" + host + ":" + port + "/" + database, user, password);

            // creates a player table to look up uuids
            createTablePlayer();
        } catch (NoSQLConnectionException e) {
            TreasureLogger.log(Level.SEVERE, "Could not create tables.");
            e.printStackTrace();
        } catch (Exception e) {
            TreasureLogger.log(Level.WARNING, e.getMessage());
            TreasureLogger.log(Level.WARNING, "No Connection");
            TreasureLogger.log(Level.WARNING, "Try SQL-Reconnection in 30 seconds.");
            thread = Bukkit.getScheduler().runTaskLaterAsynchronously(plugin, this::connect, 30 * 20L);
        }
    }

    /**
     * reconnects and resets the thread
     */
    public void reconnect() {
        // if Thread exists stop
        closeThread();
        // if connection exists stop
        if (hasConnection())
            closeConnection();
        // connect
        reconnected_Tries = 0;
        connect();
    }

    /**
     * closes reconnection thread
     */
    private void closeThread() {
        if (thread != null) {
            thread.cancel();
            thread = null;
        }
    }

    /**
     * handles no connection error
     */
    public void noConnection() {
        //if Thread exists stop Thread
        closeThread();
        //if we have no Connection try to connect
        if (!hasConnection()) {
            // connect
            reconnected_Tries = 0;
            connect();
        }
    }

    /**
     * save closing of resources
     *
     * @param st PreparedStatment
     * @param rs ResultSet
     */
    private void closeResources(PreparedStatement st, ResultSet rs) {
        if (rs != null) {
            try {
                rs.close();
            } catch (SQLException e) {
                TreasureLogger.log(Level.SEVERE, "Could not close ResultSet");
                TreasureLogger.log(Level.SEVERE, e.getMessage());
            }
        }
        if (st != null) {
            try {
                st.close();
            } catch (SQLException e) {
                TreasureLogger.log(Level.SEVERE, "Could not close Statement");
                e.printStackTrace();
            }
        }
    }

    /**
     * closes a valid connection and sets the connection to null
     */
    public void closeConnection() {
        try {
            closeThread();
            if (hasConnection()) {
                conn.close();
                TreasureLogger.log(Level.FINE, "Logout");
            }
        } catch (SQLException e) {
            TreasureLogger.log(Level.SEVERE, "Could not close connection");
            e.printStackTrace();
        } finally {
            conn = null;
        }

    }

    /**
     * Creates a player table to look uuids up
     *
     * @throws NoSQLConnectionException if no connection was possible
     */
    private void createTablePlayer() throws NoSQLConnectionException {
        if (hasConnection()) {
            PreparedStatement st = null;
            try {
                st = conn.prepareStatement(String.format(
                        "CREATE TABLE IF NOT EXISTS %s (" +
                                "%s INT UNSIGNED AUTO_INCREMENT PRIMARY KEY, " +
                                "%s VARCHAR(16) NOT NULL UNIQUE, " +
                                "%s CHAR(36) NOT NULL UNIQUE)",
                        PLAYER_KEY,
                        PID_KEY,
                        NAME_KEY,
                        UUID_KEY));
                st.executeUpdate();
            } catch (SQLException e) {
                TreasureLogger.log(Level.SEVERE, "Could not create players.");
                e.printStackTrace();
            } finally {
                closeResources(st, null);
            }
        } else {
            throw new NoSQLConnectionException();
        }
    }

    /**
     * Tries to write players to the player table
     *
     * @param player Offline player
     */
    public void addPlayer(OfflinePlayer player) {
        if (hasConnection()) {
            PreparedStatement st = null;
            try {
                createTablePlayer();

                st = conn.prepareStatement(String.format(
                        "INSERT INTO `%s` (%s, %s)  VALUES (?, ?) " +
                                "ON DUPLICATE KEY UPDATE %s = VALUES(%s)", PLAYER_KEY, NAME_KEY, UUID_KEY, NAME_KEY, NAME_KEY));
                st.setString(1, player.getName());
                st.setString(2, player.getUniqueId().toString());
                st.executeUpdate();
            } catch (SQLException e) {
                TreasureLogger.log(Level.SEVERE, String.format("&cCould not add player &6'%s'&c.", player.getName()));
                e.printStackTrace();
            } catch (NoSQLConnectionException e) {
                TreasureLogger.log(Level.SEVERE, "&cCould not create player tables.");
                e.printStackTrace();
            } finally {
                closeResources(st, null);
            }
        }
    }
}
