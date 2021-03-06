######## CREATE DATABASE FOR RUBRIC FILES IN AN AWS MySQL DATABASE #########
# Log in at: console.aws.amazon.com
# Create a MySQL database instance. Make sure that all inbound traffic is allowed
# DB instance identifier: imba
masterUser <- 'gies2021' # Master username
masterPw <- '$hdU8_z[uqi' # Master password
host <- 'imba.ccgqvhj5h01w.us-east-1.rds.amazonaws.com' # Host
dbName <- 'dashboardData' # DB name
# # Set environmental variables----
# Sys.setenv(covidDashboard_username  = masterUser
#            , covidDashboard_password = masterPw
#            , covidDashboard_host = host
#            ) 
####### IN R #########
library(tidyverse)
library(magrittr)
library(lubridate)
library(RMySQL)
library(DBI)



# Prepare data from existing rubric csv files----
con <- dbConnect(RMySQL::MySQL()
                 , user = masterUser
                 , password =  masterPw
                 , host = host
                 , dbname = ''
)
# Create the database if it doesn't exist----
createDb <- dbSendQuery(con, 'CREATE DATABASE dashboardData;')
dbClearResult(createDb)
# Create a new connection to the named database----
dbDisconnect(con)
con <- dbConnect(RMySQL::MySQL()
                 , user = masterUser
                 , password =  masterPw
                 , host = host
                 , dbname = 'dashboardData'
)
# stateData table----
stateData <- readRDS('stateData.rds')
reesults <- dbSendQuery(con, "DROP TABLE IF EXISTS stateData;")
dbClearResult(reesults)
createTableQuery <- paste0("CREATE TABLE stateData ("
                           , "date DATE, "
                           , "state VARCHAR(10), "
                           , "positive INT, "
                           , "negative INT, "
                           , "recovered INT, "
                           , "death INT, "
                           , "PRIMARY KEY (date, state));"
)
reesults <- dbSendQuery(con, createTableQuery)
dbClearResult(reesults)

dplyr::db_insert_into(con, 'stateData', stateData)
# Check to make sure it worked
testData <- dbReadTable(con, 'stateData')
rm(testData)

# stockData table----
stockData <- readRDS('allStocks.rds')
reesults <- dbSendQuery(con, "DROP TABLE IF EXISTS stockData;")
dbClearResult(reesults)
createTableQuery <- paste0("CREATE TABLE stockData ("
                           , "symbol VARCHAR(10), "
                           , "date DATE, "
                           , "open FLOAT, "
                           , "high FLOAT, "
                           , "low FLOAT, "
                           , "close FLOAT, "
                           , "volume INT, "
                           , "adjusted FLOAT, "
                           , "ticker VARCHAR(10), "
                           , "djia INT, "
                           , "PRIMARY KEY (symbol, date));"
)
reesults <- dbSendQuery(con, createTableQuery)
dbClearResult(reesults)

dplyr::db_insert_into(con, 'stockData', stockData)
# Check to make sure it worked
testData <- dbReadTable(con, 'stockData')
rm(testData)

# headlinesData table----
allHeadlines <- readRDS('allHeadlines.rds')
reesults <- dbSendQuery(con, "DROP TABLE IF EXISTS allHeadlines;")
dbClearResult(reesults)
createTableQuery <- paste0("CREATE TABLE allHeadlines ("
                           , "headlines BLOB, "
                           , "date DATE, "
                           , "type VARCHAR(100));"
)
reesults <- dbSendQuery(con, createTableQuery)
dbClearResult(reesults)

dplyr::db_insert_into(con, 'allHeadlines', allHeadlines)
# Check to make sure it worked
testData <- dbReadTable(con, 'allHeadlines')
rm(testData)

# countryConfirmedDeathRecovered table----
countryConfirmedDeathRecovered <- readRDS('countryConfirmedDeathRecovered.rds') %>%
  dplyr::rename(Lng = Long)
reesults <- dbSendQuery(con, "DROP TABLE IF EXISTS ccdr;")
dbClearResult(reesults)
createTableQuery <- paste0("CREATE TABLE ccdr ("
                           , "state VARCHAR(100), "
                           , "country VARCHAR(100), "
                           , "Lat FLOAT, "
                           , "Lng FLOAT, "
                           , "date DATE, "
                           , "confirmed INT, "
                           , "deaths INT, "
                           , "recovered INT, "
                           , "PRIMARY KEY (country, state, date));"
)
reesults <- dbSendQuery(con, createTableQuery)
dbClearResult(reesults)

dplyr::db_insert_into(con, 'ccdr', countryConfirmedDeathRecovered)
# Check to make sure it worked
testData <- dbReadTable(con, 'ccdr')
rm(testData)

# Disconnect----
dbDisconnect(con)
