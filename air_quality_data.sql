CREATE DATABASE air_quality_data;
USE air_quality_data;

CREATE TABLE air_quality (
id INT AUTO_INCREMENT PRIMARY KEY,
`Målt (starttid)` DATETIME,
CO VARCHAR(10),
NO2 VARCHAR(10),
NOX VARCHAR(10),
SO2 VARCHAR(10),
O3 VARCHAR(10),
PM10 VARCHAR(10),
PM2_5 VARCHAR(10),
Scrape_time DATETIME,
Station VARCHAR(20) UNIQUE);

SHOW TABLES;
DESCRIBE air_quality;

ALTER TABLE air_quality CHANGE PM2_5 `PM2.5` VARCHAR(10);

SELECT * FROM air_quality;

TRUNCATE TABLE air_quality;
DROP table air_quality;

SET GLOBAL local_infile = 1;

SHOW VARIABLES LIKE 'local_infile';



