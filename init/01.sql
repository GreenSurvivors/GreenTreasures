# create databases
CREATE DATABASE IF NOT EXISTS `greentreasure_database`;

# create users and grant rights
CREATE USER IF NOT EXISTS 'root'@'%' IDENTIFIED BY 'password';
GRANT ALL PRIVILEGES ON *.* TO 'root'@'%' WITH GRANT OPTION;

CREATE USER IF NOT EXISTS 'greentreasure'@'%' IDENTIFIED BY '{hangeTh!sPas5wordNÖw!';
GRANT ALL PRIVILEGES ON greentreasure_database.* TO 'greentreasure'@'%';

# Reload all privileges
FLUSH PRIVILEGES;