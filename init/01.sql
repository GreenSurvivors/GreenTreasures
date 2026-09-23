# create databases
CREATE DATABASE IF NOT EXISTS `coreDatabase`;

# create users and grant rights
CREATE USER IF NOT EXISTS 'root'@'%' IDENTIFIED BY 'password';
GRANT ALL PRIVILEGES ON *.* TO 'root'@'%' WITH GRANT OPTION;

CREATE USER IF NOT EXISTS 'coreUser'@'%' IDENTIFIED BY 'strongPassword-ChangeMe!';
GRANT ALL PRIVILEGES ON coreDatabase.* TO 'coreUser'@'%';

# Reload all privileges
FLUSH PRIVILEGES;