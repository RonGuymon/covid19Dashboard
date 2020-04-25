library(tidyverse) # Data prep
library(magrittr) # Pipes
library(lubridate) # Dates
library(tidyquant) # Stock market data
library(xml2) # For getting html data
library(rvest) # For getting tables from web pages

######### INITIAL DATA GATHERING ############
# dataFolder <- '/Users/rnguymon/Box Sync/(Focus Area 4) Business Analytics/1. Course 1 (Guymon & Khandelwal)/HE Material/Live Sessions/Live Session 1/covidDashboard/'
dataFolder <- ''
# Country Covid19 confirmed, deaths, recovered data function----
# Comes from a Github repository by JohnsHopkins
# https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
# Navigate to the raw version of each file and get the url
country_cdr <- function(){
  cConfirmedUrl <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
  cDeathsUrl <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
  cRecoveredUrl <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv'
  # Read in the data
  setupDf <- data.frame(url = c(cConfirmedUrl, cDeathsUrl, cRecoveredUrl)
                        , dfName = c('cc', 'cd', 'cr')
                        , valueCol = c('confirmed', 'deaths', 'recovered')
                        , stringsAsFactors = F)
  for(i in 1:nrow(setupDf)){
    temp <- read.csv(setupDf$url[i], header = T, sep = ',', stringsAsFactors = F) # Read in the file
    labelCols <- names(temp[grepl('X', x = names(temp))]) # Get date columns
    temp <- pivot_longer(temp, labelCols, names_to = 'date', values_to = setupDf$valueCol[i]) # Wide to long
    temp[,c(1,2,5)] <- apply(temp[,c(1,2,5)], 2, trimws)
    assign(setupDf$dfName[i], temp) # Assign name
  }
  rm(temp, setupDf, cConfirmedUrl, cDeathsUrl, cRecoveredUrl, i, labelCols) # Remove objects from global environment
  # Combine the data into a single df
  cr %<>% dplyr::mutate(date = gsub('2020', '20', date))
  ac <- left_join(cc, cd, by = c('Province.State', 'Country.Region', 'Lat', 'Long', 'date')) %>%
    left_join(cr, by = c('Province.State', 'Country.Region', 'Lat', 'Long', 'date') )
  rm(cc, cd, cr)
  # Convert date column to date type
  ac %<>% dplyr::mutate(
    date = gsub('X', '', date) %>% mdy(.)
  )
  # Clean up column names
  colnames(ac)[1:2] <- c('state', 'country')
  return(ac)
}


# Country Covid19 test data function----
# country_tests <- function(){
#   # Get tests per country
#   wp <- xml2::read_html('https://ourworldindata.org/coronavirus-testing-source-data')
#   cs <- rvest::html_table(wp)[[1]] %>%
#     as.data.frame(stringsAsFactors = F) %>%
#     .[,1:3]
#   colnames(cs) <- c('country', 'totalTests', 'date')
#   cs %<>% .[2:nrow(.),] %>%
#     dplyr::mutate(
#       date = dmy(date)
#       , totalTests = gsub(',', '', totalTests) %>% as.numeric(totalTests)
#       , baseCountry = gsub(' – .*$', '', country) %>% trimws(.)
#     ) %>%
#     group_by(baseCountry) %>%
#     dplyr::summarise(totalTests = sum(totalTests, na.rm = T)
#                      , date = max(date, na.rm = T)) %>%
#     ungroup() %>%
#     dplyr::rename(country = baseCountry)
#   return(cs)
# }


# Country population, density, age function----
country_pda <- function(){
  wp2 <- xml2::read_html('https://www.worldometers.info/world-population/population-by-country/')
  cp <- rvest::html_table(wp2)[[1]] %>%
    as.data.frame(stringsAsFactors = F) %>%
    .[,2:ncol(.)]
  colnames(cp) <- c('country', 'population', 'yearlyChangePct', 'netChange', 'density'
                    , 'landArea', 'netMigrants', 'fertRate', 'medAge', 'urbanPopPct', 'worldSharePct')
  
  cp[,2:ncol(cp)] %<>% apply(., 2, function(x) gsub(',|%| ', '', x) %>% as.numeric(.))
  return(cp)
}


# Get covid testing and results per state----
# url: https://covidtracking.com/api/
covid_states <- function(){
  wp0 <- xml2::read_html('https://covidtracking.com/api/states/daily')
  stateData <- wp0 %>% xml_child(1) %>% xml_text() %>%
    jsonlite::fromJSON() %>%
    dplyr::mutate(
      date = ymd(date)
    ) %>%
    dplyr::select(-dateChecked)
  return(stateData)
}


# Stock market data----
# tq_index_options() # Lists the different index for which we can get info
# sp500 <- tq_index('SP500') # Get a list of funds in S&P500
dj <- tq_index('DOW') # Get a list of funds in dow jones
additionalSymbols <- c('tsla', 'f', 'dal', 'aal', 'zm', 'twtr', 'nclh', 'rcl', 'regn', 'gis', 'cpb')
symbolsToGet <- c(dj$symbol, additionalSymbols)
stock_data <- function(symbolsToGet = 'tsla', startDate = '2020-01-01', endDate = Sys.Date()-1){
  allStocks <- data.frame()
  missingData <- 0
  for(i in symbolsToGet){
    cat('Starting', i)
    temp <- tq_get(i, get = 'stock.prices', from = startDate, endDate)
    if(is.na(temp)){
      break()
    }
    temp$ticker <- i
    temp$djia <- ifelse(i %in% dj$symbol, 1, 0) # Indicates if it's in the dow jones industrial average
    allStocks %<>% bind_rows(temp)
    cat(' Finished\n______________________\n')
  }
  return(allStocks)
}


# News headlines function----
# Create a sequence of dates for which to gather news headlines
get_headlines <- function(startDate = '2020-01-01', endDate = Sys.Date() - 1){
  daties <- seq.Date(from = ymd(startDate), to = endDate, by = 'day') %>% as.character()
  allHeadlines <- data.frame()
  for(d in daties){
    cat('Starting', d, ' ')
    dateToFetch <- ymd(d)
    
    # Get html of news headlines
    nh <- xml2::read_html(paste0('https://www.wincalendar.com/Calendar/Date/'
                                 , month(dateToFetch, label = T, abbr = F), '-'
                                 , mday(dateToFetch), '-'
                                 , year(dateToFetch)
    )
    )
    # nh %>% xml_child(2) %>% xml_child(1) %>% xml_text()
    # Get the relevant text from the relevant node
    nht <- nh %>% xml_child(2) %>% 
      xml_child(1) %>% 
      xml_child(4) %>% 
      xml_child(2) %>% 
      xml_text()
    # Top Tweets
    topTweets <- gsub('^.*Most liked, retweeted and popular Tweets', '', nht) %>%
      gsub('What happened on.*$', '', .) %>%
      gsub('\\t|\\n|\\r', '', .) %>%
      strsplit(., paste0(month(dateToFetch, label = T, abbr = F), ' '
                         , mday(dateToFetch), ', '
                         , year(dateToFetch)
      )
      )
    tempTt <- data.frame(headlines = unlist(topTweets)
                         , stringsAsFactors = F) %>%
      dplyr::mutate(
        date = dateToFetch
        , type = 'topTweets'
      )
    # Top news headlines
    topNewsHeadlines <- gsub('^.*Top news stories on this day', '', nht) %>%
      gsub('Trended on Twitter.*$', '', .) %>%
      gsub('\\r\\n', '|', .)%>%
      gsub('\\t|\\n|\\r', '', .) %>%
      gsub('^\\||\\|{2}', '', .) %>%
      strsplit(., '\\|')
    
    tempTnh <- data.frame(headlines = unlist(topNewsHeadlines)
                          , stringsAsFactors = F) %>%
      dplyr::mutate(
        date = dateToFetch
        , type = 'topNewsHeadlines'
      )
    # Top trending on social media
    trendedOnSm <- gsub('^.*Trended on Twitter, the Internet and Social Media on', '', nht) %>%
      gsub('Major Sports events.*$', '', .) %>%
      gsub('\\r\\n', '|', .) %>%
      gsub('\\t|\\n|\\r', '', .) %>%
      gsub(paste0('^.* ', year(dateToFetch), '\\|'), '', .) %>%
      gsub('^\\||\\|{2}', '', .) %>%
      strsplit(., '\\|')
    tempTos <- data.frame(headlines = unlist(trendedOnSm)
                          , stringsAsFactors = F) %>%
      dplyr::mutate(
        date = dateToFetch
        , type = 'topTrendingOnSm'
      )
    allHeadlines %<>% bind_rows(tempTt) %>%
      bind_rows(tempTnh) %>%
      bind_rows(tempTos)
    cat('--Done \n')
  }
  return(allHeadlines)
}

# Run functions and write data to disk for the first time----
countryConfirmedDeathRecovered <- country_cdr()
write_csv(countryConfirmedDeathRecovered, paste0(dataFolder, 'countryConfirmedDeathRecovered.csv'))
# countryTests <- country_tests()
# write_csv(countryTests, paste0(dataFolder, 'countryTests.csv'))
# countryPop <- country_pda() # Probably doesn't change on a daily basis
# write_csv(countryPop, paste0(dataFolder, 'countryPopulation.csv'))
countryPop <- read.csv(paste0(dataFolder, 'countryPopulation.csv'), stringsAsFactors = F)
# Combine and calculate tests per capita
# countryTestPop <- left_join(countryTests, countryPop, by = 'country') %>%
countryTestPop <- countryPop %>%
  dplyr::mutate(
    # testsPerMil = round(totalTests/(population*.000001), 1)
    country = case_when(
      country == 'United States' ~ 'US'
      , country == 'South Korea' ~ 'Korea, South'
      , country == 'Czech Republic' ~ 'Czechia'
      , T ~ country
    )
  )
write_csv(countryTestPop, paste0(dataFolder, 'countryTestPop.csv'))
stateData <- covid_states()
write_csv(stateData, paste0(dataFolder, 'stateData.csv'))
# allStocks <- stock_data(symbolsToGet = symbolsToGet)
# write_csv(allStocks, paste0(dataFolder, 'allStocks.csv'))
# allHeadlines <- get_headlines()
# write_csv(allHeadlines, paste0(dataFolder, 'allHeadlines.csv'))
######### UPDATE DATA FILES ###########
# Some files are already nicely prepared, so we just get all of the data again
# The only ones that should be incrementally updated are the stocks and headlines
# Update stock data----
allStocksOld <- read.csv(paste0(dataFolder, 'allStocks.csv'), stringsAsFactors = F) %>%
  dplyr::mutate(
    date = ymd(date)
  )
startDate <- (max(allStocksOld$date, na.rm = T)) %>% as.character()
if(ymd(startDate) < Sys.Date() - 1){
  allStocksNew <- stock_data(symbolsToGet = symbolsToGet
                             , startDate = startDate
                             , endDate = Sys.Date()-1)
  allStocksNew %<>% dplyr::mutate(
    joinCol = paste0(date, symbol)
  ) %>%
    .[!duplicated(.$joinCol),]
  if(sum(is.na(allStocksNew)) < nrow(allStocksNew) & nrow(allStocksNew) > 0){
    allStocksOld %<>% 
      dplyr::mutate(
        joinCol = paste0(date, symbol)
      ) %>%
      dplyr::filter(!joinCol %in% allStocksNew$joinCol)
    allStocks <- bind_rows(allStocksOld, allStocksNew) %>%
      dplyr::arrange(symbol, date) %>%
      .[!duplicated(.$joinCol),] %>% 
      dplyr::select(-joinCol) %>%
      dplyr::filter(!is.na(adjusted) & adjusted > 0)
    write_csv(allStocks, paste0(dataFolder, 'allStocks.csv'))
  }
}
# Update news data----
allHeadlinesOld <- read.csv(paste0(dataFolder, 'allHeadlines.csv'), stringsAsFactors = F) %>%
  dplyr::mutate(
    date = ymd(date)
  )
startDate <- (max(allHeadlinesOld$date, na.rm = T) + 1) %>% as.character()
if(ymd(startDate) < Sys.Date()){
  allHeadlinesNew <- get_headlines(startDate = startDate, endDate = Sys.Date() - 1)
  if(nrow(allHeadlinesNew) > 0){
    allHeadlines <- bind_rows(allHeadlinesOld, allHeadlinesNew) %>%
      unique()
    write_csv(allHeadlines, paste0(dataFolder, 'allHeadlines.csv'))
  }
}

cat('Updated coronavirus dashboard data', as.character(Sys.time())
    , file = paste0(dataFolder, 'covid19Data.txt'))
