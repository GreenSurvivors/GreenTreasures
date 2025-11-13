package de.greensurvivors.greentreasure.data.dao;

import org.jetbrains.annotations.NotNull;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;

abstract class BaseDao {
    protected DataSource dataSource;

    abstract public void createTable() throws SQLException;

    protected void createTable(String table, String fields) throws SQLException{
        if (dataSource != null) {
            final String statementStr = "CREATE TABLE IF NOT EXISTS " + table + " (" + fields + ")";

            try (final @NotNull Connection connection = dataSource.getConnection();
                 final @NotNull PreparedStatement preparedStatement = connection.prepareStatement(statementStr)) {
                preparedStatement.executeUpdate();
            }
        }
    }
}