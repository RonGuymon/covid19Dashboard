######## CREATE DATABASE FOR RUBRIC FILES IN AN AWS MySQL DATABASE #########
# Log in at: console.aws.amazon.com
# Create a database
# DB instance identifier: covid19Dashboard
# Master username: JeshuaOfJozadak
# Master password: (9HhG2M&$3
# Host: covid19dashboard.cut6x5kwuxm3.us-east-1.rds.amazonaws.com
# DB name: rubricsDb
# # Set environmental variables----
# Sys.setenv(covidDashboard_username  = 'JeshuaOfJozadak'
#            , covidDashboard_password = '(9HhG2M&$3'
#            , covidDashboard_host = 'covid19dashboard.cut6x5kwuxm3.us-east-1.rds.amazonaws.com' # Endpoint in Connectivity & security
#            ) 
####### IN R #########
library(tidyverse)
library(magrittr)
library(lubridate)
library(RMySQL)
library(DBI)



# Prepare data from existing rubric csv files----
con <- dbConnect(RMySQL::MySQL()
                 , user = 'JeshuaOfJozadak'
                 , password =  '(9HhG2M&$3'
                 , host = 'covid19dashboard.cut6x5kwuxm3.us-east-1.rds.amazonaws.com' 
                 , dbname = ''
)
# Create the database if it doesn't exist----
createDb <- dbSendQuery(con, 'CREATE DATABASE dashboardData;')
dbClearResult(createDb)
# Create a new connection to the named database----
dbDisconnect(con)
con <- dbConnect(RMySQL::MySQL()
                 , user = 'JeshuaOfJozadak'
                 , password =  '(9HhG2M&$3'
                 , host = 'covid19dashboard.cut6x5kwuxm3.us-east-1.rds.amazonaws.com' 
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
# Check to make sure it worked----
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
# Check to make sure it worked----
testData <- dbReadTable(con, 'allHeadlines')
rm(testData)
# Disconnect----
dbDisconnect(con)
