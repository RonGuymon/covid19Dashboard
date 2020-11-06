############# Covid19 Dashboard for MBA 561 #############
# Libraries----
library(shinydashboard)
library(shiny)
library(shinyBS) # For popovers
library(shinyjs) # For hiding the sidebar by default
library(tidyverse)
library(zoo)
library(data.table)
library(lubridate)
library(magrittr)
library(plotly)
library(RColorBrewer)
library(shinycssloaders) # For loading spinners
# library(rhandsontable)
library(DT)
library(leaflet)
library(tidytext) # For text analysis
library(wordcloud2) # For wordcloud
library(DBI) # For remote database connection
library(RMySQL) # For using MySQL commands
# Read in data----
# dataFolder <- '/Users/rnguymon/Box Sync/(Focus Area 4) Business Analytics/1. Course 1 (Guymon & Khandelwal)/HE Material/Live Sessions/Live Session 1/covidDashboard/'
dataFolder <- ''
dataDeets <- data.frame(fileName = c('countryTestPop.rds')
               , dfName = c('ctp')
               , stringsAsFactors = F)

# State population data comes from: https://worldpopulationreview.com/states/
for(dn in 1:nrow(dataDeets)){
  temp <- readRDS(paste0(dataFolder, dataDeets$fileName[dn]))
  if('date' %in% names(temp)){
    temp %<>%
      dplyr::mutate(
        date = ymd(date)
      )
  }
  assign(dataDeets$dfName[dn], temp)
}
rm(temp, dataDeets)
sp <- readRDS('statePopulation.rds') # State population
ctr <- readRDS('countryTestPop.rds') # Country test population
source('databaseConnection.R')
# World data----  
ccr <- dbReadTable(con, 'ccdr') %>%
  dplyr::mutate(
    date = ymd(date)
  )
countrySummary <- ccr %>%
  dplyr::mutate(country = trimws(country)) %>%
  group_by(country, date) %>%
  dplyr::summarise(confirmed = sum(confirmed, na.rm = T)
                   , deaths = sum(deaths, na.rm = T)
                   , recovered = sum(recovered, na.rm = T)
                   , .groups = 'drop') %>%
  dplyr::ungroup() %>%
  dplyr::arrange(country, date) %>%
  left_join(ctp[which(!is.na(population)), c('country', 'population')], by = 'country') %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(
    maxDate = max(date, na.rm = T)
    , mostRecent = ifelse(date == maxDate, 1, 0)
    , population = population*.000001
    , confirmedPerMil = round(confirmed/population, 1)
    , recoveredPerMil = round(recovered/population, 1)
    , deathsPerMil = round(deaths/population, 1)
    , newConfirmed = confirmed - dplyr::lag(confirmed, 1)
    , newConfirmed = ifelse(is.na(newConfirmed), confirmed, newConfirmed)
    , newConfirmedPerMil = round(newConfirmed/population, 1)
    , newDeaths = deaths - dplyr::lag(deaths, 1)
    , newDeaths = ifelse(is.na(newDeaths), deaths, newDeaths)
    , newDeathsPerMil = round(newDeaths/population, 1)
    , newRecovered = recovered - dplyr::lag(recovered, 1)
    , newRecovered = ifelse(is.na(newRecovered), recovered, newRecovered)
    , newRecoveredPerMil = round(newRecovered/population, 1)
    , baseDay = ifelse(confirmed >=100, 1, 0)
    , baseDay = cumsum(baseDay)
    , deathRate = round(deaths/confirmed, 2)
  ) %>%
  ungroup()
mostRecent <- countrySummary %>% 
  dplyr::filter(mostRecent == 1) %>%
  dplyr::arrange(desc(newConfirmed))

ccr %<>%
  dplyr::mutate(country = trimws(country)) %>%
  dplyr::arrange(state, country, Lat, Lng, date) %>%
  group_by(state, country, Lat, Lng) %>%
  dplyr::mutate(
    confirmed = ifelse(is.na(confirmed) & date == min(date), 0, confirmed)
    , deaths = ifelse(is.na(deaths) & date == min(date), 0, deaths)
    , recovered = ifelse(is.na(recovered) & date == min(date), 0, recovered)
    , confirmed = na.locf(confirmed)
    , deaths = na.locf(deaths)
    , recovered = na.locf(recovered)
  ) %>%
  ungroup()

# State data----
sd <- dbReadTable(con, 'stateData') %>%
  dplyr::mutate(
    date = ymd(date)
  )
sd[is.na(sd)] <- 0
stateSummary <- sd %>%
  left_join(sp[,c('state', 'Pop')], by = 'state') %>%
  dplyr::rename(population = Pop) %>%
  dplyr::arrange(state, date) %>%
  group_by(state) %>%
  dplyr::mutate(
    maxDate = ifelse(date == max(date, na.rm = T), 1, 0)
    , population = population*.000001
    , totalTestResults = positive + negative
    , posPct = positive / totalTestResults
    , deathPct = death / positive
    , deathRate = death / positive
    # , hospPct = hospitalized / positive
    , posPerMil = positive / population
    , deathPerMil = death / population
    , recoveredPerMil = recovered / population
    , newPositive = positive - dplyr::lag(positive, 1)
    , newPositive = ifelse(is.na(newPositive), 0, newPositive)
    , newPositivePerMil = newPositive / population
    , newPositiveChange = newPositive - dplyr::lag(newPositive, 1)
    , newPositiveChange = ifelse(is.na(newPositiveChange), 0, newPositiveChange)
    , newDeaths = death - dplyr::lag(death, 1)
    , newDeaths = ifelse(is.na(newDeaths), 0, newDeaths)
    , newDeathsPerMil = newDeaths / population
    , newDeathsChange = newDeaths - dplyr::lag(newDeaths, 1)
    , newDeathsChange = ifelse(is.na(newDeathsChange), 0, newDeathsChange)
    , newTotal = totalTestResults - dplyr::lag(totalTestResults, 1)
    , newTotal = ifelse(is.na(newTotal), 0, newTotal)
    , newTotalChange = newTotal - dplyr::lag(newTotal, 1)
    , newTotalChange = ifelse(is.na(newTotalChange), 0, newTotalChange)
    , newRecovered = recovered - dplyr::lag(recovered, 1)
    , newRecovered = ifelse(is.na(newRecovered), 0, newRecovered)
    , newRecoveredPerMil = newRecovered / population
    , baseDay = ifelse(positive >=100, 1, 0)
    , baseDay = cumsum(baseDay)
  ) %>% 
  ungroup()

mostRecentState <- stateSummary %>%
  dplyr::filter(maxDate == 1) %>%
  dplyr::select(-maxDate) %>%
  dplyr::arrange(desc(deathRate))



# Stock data----
as <- dbReadTable(con, 'stockData') %>%
  dplyr::mutate(
    date = ymd(date)
  )
# Convert the stocks in the djia to something close to the average
as %<>% dplyr::filter(!is.na(adjusted) & adjusted > 0)
djia <- as %>% dplyr::filter(djia == 1) %>%
  group_by(date) %>%
  dplyr::summarise(adjusted = sum(adjusted, na.rm = T)/.1474  # For current divisor: https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average
                   , volume = sum(volume, na.rm = T)
                   , .groups = 'drop') %>% 
  ungroup() %>%
  dplyr::mutate(
    symbol = 'djia'
  )
as <- as %>% 
  dplyr::filter(djia == 0) %>%
  dplyr::select(date, symbol, adjusted, volume) %>%
  bind_rows(djia) %>%
  dplyr::arrange(symbol, date)

spd <- as %>% 
  dplyr::filter(adjusted > 0) %>%
  group_by(symbol) %>%
  dplyr::arrange(symbol, date) %>%
  dplyr::mutate(
    obs = 1
    , obs = cumsum(obs)
    , adjustedChange = ifelse(obs == 1, 1, ((adjusted - dplyr::lag(adjusted))/dplyr::lag(adjusted))+1)
    , cumChange = cumprod(adjustedChange)-1
    , mostRecent = ifelse(date == max(date, na.rm = T), 1, 0)
  ) %>%
  ungroup()
djiaChange <- spd %>% dplyr::filter(symbol == 'djia' & mostRecent == 1) %>% pull(adjustedChange) - 1
mostRecentStockChange <- spd %>% dplyr::filter(mostRecent == 1) %>%
  dplyr::arrange(desc(adjustedChange))

# Headline data----
ah <- dbReadTable(con, 'allHeadlines') %>%
  dplyr::mutate(
    date = ymd(date)
  )
headlines_tidy <- ah %>%
  dplyr::mutate(
    headlines = gsub('(<.*>)', '', headlines) %>% gsub('t\\.co|trump|https', '', .)
  ) %>%
  unnest_tokens(word, headlines)


# Options (like upload size)----
options(shiny.maxRequestSize=5000*1024^2)
# Colors----
giggColors <- data.frame(colorName = c("fireEngineRed", 
                                       "deepKoamaru", "persianGreen", "maximumYellowRed", 
                                       "graniteGray"
)
, hex = c("#D12229", "#28335C", "#00A499"
          , "#F3C454", "#63666A"
)
, rgb = c("rgb(209,34,41)", "rgb(40,51,92)", "rgb(0,164,153)"
          , "rgb(243,196,84)", "rgb(99,102,106)"
)
, stringsAsFactors = F)

themeColors <- data.frame(colorName = c('Illinois Orange', 'Lighter Orange', 'Illinois Blue', 'Lighter Blue', 'Darker Blue')
                          , colorHex = c('#ea4b38', '#D45228', '#17274b', '#203566', '#152242')
                          , stringsAsFactors = F)
coronaColors <- data.frame(status = c('confirmed', 'deaths', 'recovered')
                           , color = c('#f39c12', '#111111', '#00a65a')
                           , stringsAsFactors = F)


# Trademark pictures----
customerTrademarkUrl <- "https://giesbusiness.illinois.edu/profile/ronald-guymon"
customerHomepageUrl <- "https://giesbusiness.illinois.edu/profile/ronald-guymon"
giggTrademarkUrl <- "https://cdn.business.illinois.edu/business-images/gies-logo-blk-i.png"
giggHomepageUrl <- "https://giesbusiness.illinois.edu/"
# wd <- getwd()
############## TEXT BOX STUFF ##############
##### UI #####
# Header----
# header <- dashboardHeader(uiOutput("header"))
header <- dashboardHeader(
  title = span(tags$a(href = giggHomepageUrl
                      , tags$img(src='kingfisher.png'
                                 , title = "Illinois Kingfisher Logo"
                                 , height = "50px"
                                 , width = "94px"
                                 , position = "center")
                      # , style = "padding-left:30px;"
  )
  
  )
)
# Sidebar----
sidebar <- dashboardSidebar(uiOutput("sidebarpanel")
                            # , width = 350
                            , collapsed = T
)
# Body----
body <- dashboardBody(uiOutput("body")
                      , shinyjs::useShinyjs() # So that sidebar can open once you login, and to show messages from functions.
                      , tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "gradingTool.css")
                      )
)   
# UI----
ui <- dashboardPage(title = "MBA 561 Example", header, sidebar, body)
# Login and loading boxes----
login <- fluidPage(
  tags$style(".skin-blue .content{background-image: url(\"coronaVirus.jpg\");
             background-repeat: no-repeat;
             background-size: 150%;
             background-attachment: fixed;
             background-position: 45% 70%;
             background-color: #ffffff;
             }")
  , br()
  , br()
  , fluidRow(
    column(width = 8, offset = 0
           , textInput("userName", "Your Name")
           , passwordInput("passwd", "Password")
           , br()
           , actionButton("Login", "LOG IN",
                          style = "color: #FFFFFF; 
                          background-color: #17274b; 
                          border-color: #17274b; 
                          border-radius: 0;")
    )
  )
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
)





##### SERVER #####
server <- function(input, output, session) {
  # output$testText <- renderText({wd})
  
  remoteAddress <- isolate(paste(session$clientData$url_hostname
                                 , session$clientDAta$url_pathname
                                 , session$clientData$url_port
                                 , session$clientData$search
                                 , sep = "_")) # Use this to get ip address of user
  ####### FILTERED DATA #######
  fd <- reactive({
    rdList <- list()
    if(!is.null(input$worldTable_rows_selected)){
      mrWorld <- mostRecent[input$worldTable_rows_selected,]
      fbdw <- countrySummary %>% 
        dplyr::filter(country %in% mrWorld$country)
      mapData <- ccr %>% dplyr::filter(country %in% mrWorld$country)
    }else{
      mrWorld <- mostRecent
      fbdw <- countrySummary
      mapData <- ccr
    }
    rdList$mrWorld <- mrWorld
    rdList$countriesSelected <- mrWorld$country
    rdList$fbdw <- fbdw
    rdList$mapData <- mapData
    return(rdList)
  })
  
  fds <- reactive({
    rdList <- list()
    if(!is.null(input$stateTable_rows_selected)){
      mrState <- mostRecentState[input$stateTable_rows_selected,]
      fbds <- stateSummary %>% 
        dplyr::filter(state %in% mrState$state)
      # mapData <- ccr %>% dplyr::filter(state %in% mrWorld$country)
    }else{
      mrState <- mostRecentState
      fbds <- stateSummary
      # mapData <- ccr
    }
    rdList$mrState <- mrState
    rdList$fbds <- fbds
    # rdList$mapData <- mapData
    return(rdList)
  })
  
  fdw <- reactive({
    returnList <- list()
    # Data that will be filtered on the Stocks page
    # Worldwide confirmed cases
    sdt <- countrySummary %>%
      group_by(date) %>%
      dplyr::summarise(positive = sum(confirmed, na.rm = T)
                       , .groups = 'drop') %>%
      ungroup()
    
    # Daily adjusted closing price in long format
    djt <- as %>% 
      dplyr::select(date, symbol, adjusted) %>%
      pivot_wider(id_cols = c('date', 'symbol')
                  , names_from = c('symbol')
                  , values_from = 'adjusted')
    

    wordCount <- headlines_tidy
    datesToFilter <- event_data(event = "plotly_brushed", source = 'A')
    if(!is.null(datesToFilter)){
      firstDate <- as.Date(datesToFilter$x[1], origin = '1969-12-31')
      lastDate <- as.Date(datesToFilter$x[2], origin = '1969-12-31')
      wordCount %<>% dplyr::filter(date >= firstDate & date <= lastDate)
      sdt %<>% dplyr::filter(date >= firstDate & date <= lastDate)
      djt %<>% dplyr::filter(date >= firstDate & date <= lastDate)
    }
    # Word Cloud Data
    wordCount %<>%
      select(word) %>%
      anti_join(stop_words, by = 'word') %>%
      count(word, sort = T)
    returnList$wordCount <- wordCount
    
    # Correlation data
    cData <- left_join(sdt, djt, by = 'date') %>% 
      .[complete.cases(.),] %>%
      dplyr::arrange(date)
    returnList$cData <- cData
    
    # List to return
    return(returnList)
  })
  # output$testText <- renderPrint({
  #   # fd()$mrWorld$country
  #   mostRecent$country[input$worldTable_rows_selected]
  # })
  ###### WORLD #########
  # Daily World info----

  output$newConfirmedBox <- renderInfoBox({
    bd <- fd()$mrWorld
    if(input$tpcRadioCountry == 'total'){
      sumNum <- sum(bd$newConfirmed, na.rm = T) %>%
        prettyNum(big.mark = ',', interval = 3)
      boxText <- 'Confirmed Yesterday'
    }else{
      sumNum <- round((sum(bd$newConfirmed, na.rm = T) / sum(bd$population, na.rm = T)), 1) %>%
        prettyNum(big.mark = ',', interval = 3)
      boxText <- 'Confirmed Yesterday (per million)'
    }
    infoBox(boxText
            , sumNum
            , color = 'yellow'
            , fill = T
            , icon = icon('fa fa-lungs-virus', lib = 'font-awesome')
            )
  })
  output$newDeathsBox <- renderInfoBox({
    bd <- fd()$mrWorld
    if(input$tpcRadioCountry == 'total'){
      sumNum <- sum(bd$newDeaths, na.rm = T) %>%
        prettyNum(big.mark = ',', interval = 3)
      boxText <- 'Deaths Yesterday'
    }else{
      sumNum <- round((sum(bd$newDeaths, na.rm = T) / sum(bd$population, na.rm = T)), 1) %>%
        prettyNum(big.mark = ',', interval = 3)
      boxText <- 'Deaths Yesterday (per million)'
    }
    infoBox(boxText
            , sumNum
            , color = 'black'
            , fill = T
            , icon = icon('skull', lib = 'font-awesome')
    )
  })
  output$newRecoveredBox <- renderInfoBox({
    bd <- fd()$mrWorld
    if(input$tpcRadioCountry == 'total'){
      sumNum <- sum(bd$newRecovered, na.rm = T) %>%
        prettyNum(big.mark = ',', interval = 3)
      boxText <- 'Recovered Yesterday'
    }else{
      sumNum <- round((sum(bd$newRecovered, na.rm = T) / sum(bd$population, na.rm = T)), 1) %>%
        prettyNum(big.mark = ',', interval = 3)
      boxText <- 'Recovered Yesterday (per million)'
    }
    infoBox(boxText
            , sumNum
            , color = 'green'
            , fill = T
            , icon = icon('walking', lib = 'font-awesome')
    )
  })
  output$currentDayCurve <- renderPlotly({
    curveData <- countrySummary %>%
      dplyr::filter(country %in% fd()$mrWorld$country)
    if(input$tpcRadioCountry == 'total'){
      curveData %<>%
        group_by(date) %>%
        dplyr::summarise(CONFIRMED = sum(newConfirmed, na.rm = T)
                         , DEATHS = sum(newDeaths, na.rm = T)
                         , RECOVERED = sum(newRecovered, na.rm = T)
                         , .groups = 'drop') %>%
        ungroup() %>%
        pivot_longer(cols = c('CONFIRMED', 'DEATHS', 'RECOVERED')
                     , names_to = 'type')
      ylab <- 'Observations Per Day'
    } else{
      curveData %<>%
        group_by(date) %>%
        dplyr::summarise(CONFIRMED = sum(newConfirmed, na.rm = T)/sum(population, na.rm = T)
                         , DEATHS = sum(newDeaths, na.rm = T)/sum(population, na.rm = T)
                         , RECOVERED = sum(newRecovered, na.rm = T)/sum(population, na.rm = T)
                         , .groups = 'drop') %>%
        ungroup() %>%
        pivot_longer(cols = c('CONFIRMED', 'DEATHS', 'RECOVERED')
                     , names_to = 'type')
      ylab <- 'Observations Per Day (per million)'
    }
    curvePlot <- ggplot(curveData, aes(x = date, y = value, color = type)) +
      # geom_smooth() +
      geom_line() +
      scale_color_manual(values = coronaColors$color) +
      labs(title = 'Confirmed, Deaths, Recovered Per Day'
           , x = 'Date'
           , y = ylab)
    ggplotly(curvePlot)
  })

  # Cumulative world cases----
  output$totalConfirmedBox <- renderInfoBox({
    bd <- fd()$mrWorld
    if(input$tpcRadioCountry == 'total'){
      sumNum <- sum(bd$confirmed, na.rm = T) %>%
        prettyNum(big.mark = ',', interval = 3)
      boxText <- 'Total Confirmed'
    }else{
      sumNum <- round((sum(bd$confirmed, na.rm = T) / sum(bd$population, na.rm = T)), 1) %>%
        prettyNum(big.mark = ',', interval = 3)
      boxText <- 'Total Confirmed (per million)'
    }
    infoBox(boxText
            , sumNum
            , color = 'yellow'
            , fill = T
            , icon = icon('fa fa-lungs-virus', lib = 'font-awesome')
    )
  })
  
  output$totalDeathsBox <- renderInfoBox({
    bd <- fd()$mrWorld
    if(input$tpcRadioCountry == 'total'){
      sumNum <- sum(bd$deaths, na.rm = T) %>%
        prettyNum(big.mark = ',', interval = 3)
      sumRate <- round(sum(bd$deaths, na.rm = T) / sum(bd$confirmed, na.rm = T)*100, 1) %>%
        paste0(., '%')
      sumNumText <- paste0(sumNum, ' (', sumRate, ')')
      boxText <- 'Total Deaths (pct confirmed)'
    }else{
      sumNumText <- round((sum(bd$deaths, na.rm = T) / sum(bd$population, na.rm = T)), 1) %>%
        prettyNum(big.mark = ',', interval = 3)
      boxText <- 'Total Deaths per Million'
    }
    infoBox(boxText
            , sumNumText
            , color = 'black'
            , fill = T
            , icon = icon('skull', lib = 'font-awesome')
    )
  })
  
  
  output$totalRecoveredBox <- renderInfoBox({
    bd <- fd()$mrWorld
    if(input$tpcRadioCountry == 'total'){
      sumNum <- sum(bd$recovered, na.rm = T) %>%
        prettyNum(big.mark = ',', interval = 3)
      sumRate <- round(sum(bd$recovered, na.rm = T) / sum(bd$confirmed, na.rm = T)*100, 1) %>%
        paste0(., '%')
      sumNumText <- paste0(sumNum, ' (', sumRate, ')')
      boxText <- 'Total Recovered (pct confirmed)'
    }else{
      sumNumText <- round((sum(bd$recovered, na.rm = T) / sum(bd$population, na.rm = T)), 1) %>%
        prettyNum(big.mark = ',', interval = 3)
      boxText <- 'Total Recovered per Million'
    }
    infoBox(boxText
            , sumNumText
            , color = 'green'
            , fill = T
            , icon = icon('walking', lib = 'font-awesome')
    )
  })
  
  output$baseDayCountry <- renderPlotly({
    pd <- fd()$fbdw
    ncount <- length(unique(pd$country))
    
    if(input$tpcRadioCountry == 'total'){
      if(ncount > 10){
        pd %<>% group_by(date) %>%
          dplyr::summarise(confirmed = sum(confirmed, na.rm = T)
                           , .groups = 'drop') %>%
          ungroup()
        p <- ggplot(pd, aes(x = date, y = confirmed)) +
          geom_line() +
          labs(title = 'Worldwide Cases'
               , x = 'Date'
               , y = 'Cumulative Cases')
      }else{
        pd %<>% dplyr::filter(baseDay > 0)
        p <- ggplot(pd, aes(x = baseDay, y = confirmed, color = country)) +
          geom_line() +
          labs(title = 'Cumulative Cases Starting On Day When Cum Cases is 100'
               , x = 'Base Day'
               , y = 'Cumulative Cases')
      }
    }else{
      if(ncount > 10){
        pd %<>% group_by(date) %>%
          dplyr::summarise(confirmedPerMil = sum(confirmed, na.rm = T) /sum(population, na.rm = T)
                           , .groups = 'drop') %>%
          ungroup()
        p <- ggplot(pd, aes(x = date, y = confirmedPerMil)) +
          geom_line() +
          labs(title = 'Per Capita Worldwide Cases'
               , x = 'Date'
               , y = 'Cumulative Cases (per million)')
      }else{
        pd %<>% dplyr::filter(baseDay > 0)
        p <- ggplot(pd, aes(x = baseDay, y = confirmedPerMil, color = country)) +
          geom_line() +
          labs(title = 'Per Capita Cumulative Cases Starting On Day When Cum Cases is 100'
               , x = 'Base Day'
               , y = 'Cumulative Cases (Per Million)')
      }
    }
    ggplotly(p)
  })
  # Map the most recent day----
  output$mostRecentMap <- renderLeaflet({
    lpd <- fd()$mapData %>%
      group_by(Lat, Lng) %>%
      dplyr::mutate(
        maxDate = ifelse(date == max(date), 1, 0)
      ) %>%
      ungroup() %>%
      dplyr::filter(maxDate == 1) %>%
      group_by(Lat, Lng) %>%
      dplyr::mutate(
        pctDied = round(deaths/confirmed, 2)
        , pctDied = ifelse(confirmed == 0 & deaths == 0, 0, pctDied)
        , ldeaths = log1p(deaths)
        , state = ifelse(state == '', 'State Not Given', state)
        # , popUpContent = paste0('<p>',state, '</p><p>', country, '</p>')
        , stateCountry = paste(state, country, sep = ', ')
        , confirmedText = paste0('Confirmed: ', confirmed)
        , deathsText = paste0('Deaths: ', deaths, ' (', pctDied*100, '%)')
        , popUpContent = paste(stateCountry, confirmedText, deathsText, sep = '</p></p>')
        , popUpContent = paste0('<p>', popUpContent, '</p>')
      ) %>%
      ungroup() %>%
      dplyr::select(-maxDate, -stateCountry, -deathsText, -confirmedText)
    # myColors <- colorNumeric(palette = 'Blues'
    #                          , domain = lpd$newConfirmed
    #                          , na.color = 'transparent')
    m <- leaflet(lpd) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~Lng, lat = ~Lat
                       , radius = ~ldeaths
                       , color = 'red'
                       , fill = F
                       , label = ~lapply(popUpContent, htmltools::HTML)
      )
      # addPolygons(fillColor = ~myColors(newConfirmed), stroke = T)
    m
  })
  
  # World table----
  output$worldTable <- DT::renderDataTable({
    # Filter table
    tData <- mostRecent[,c('country', 'date', 'confirmed', 'deaths'
                           , 'recovered', 'newConfirmed', 'newDeaths'
                           , 'newRecovered', 'deathRate')]
    # Cols
    bigNums <- c('confirmed', 'deaths'
                 , 'recovered', 'newConfirmed', 'newDeaths'
                 , 'newRecovered')

    # Create table
    t <- DT::datatable(tData
                  , escape = F
                  , colnames = c('Country', 'Most Recent Date', 'Confirmed'
                                 , 'Deaths', 'Recovered', 'New Confirmed'
                                 , 'New Deaths', 'New Recovered'
                                 , 'Death Rate')
                  , selection = 'multiple'
                  , options = list(fillContainer = T
                                   , pageLength = 5)) %>%
      formatPercentage('deathRate', digits = 1) %>%
      formatDate('date', method = 'toDateString') %>%
      formatRound(bigNums, interval = 3, mark = ',', digits = 0)
      # formatStyle(c('newConfirmed')
      #             , backgroundColor = styleInterval(brks, clrs))
    
    for(colName in c(bigNums, 'deathRate')){
      # Create breaks for color coding cells
      brks <- quantile(tData[,colName][[1]]
                       , probs = seq(0, .95, .05)
                       , na.rm = T)
      clrs <- round(seq(255, 40, length.out = length(brks)+1), 0) %>%
        paste0('rgb(255,', ., ',', ., ')')
      t %<>% formatStyle(colName
                         , backgroundColor = styleInterval(brks, clrs))
    }
    t
  })
  # World summary report----
  output$worldSummaryReport <- downloadHandler(
    filename = 'worldSummaryReport.pdf'
    , content = function(file){
      # Copy the report to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), 'worldSummaryReport.Rmd')
      file.copy('worldSummaryReport.Rmd', tempReport, overwrite = T)
      
      # Set up parameters to pass to the Rmd document
      params <- list(username = USER$Username
                     , countrySummary = countrySummary
                     , mostRecent = mostRecent
                     , coronaColors = coronaColors
                     , tpcRadioCountry = input$tpcRadioCountry
                     , countriesSelected = fd()$countriesSelected
                     , fbdw = fd()$fbdw
                     )
      
      # Knit the document, passing in the `params` list, and evaluate it in a 
      # child of the global environment (this isolates teh code in the document
      # from the code in this app).
      rmarkdown::render(tempReport
                       , output_format = 'pdf_document'
                       , output_file = file
                       , params = params
                       , envir = new.env(parent = globalenv())
                       )
      
    }
  )
  ######### STOCKS ###########
  # Stock Cumulative Return----
  output$djiaChangeBox <- renderInfoBox({
    infoBox(ifelse(djiaChange > 0, 'DJIA Increase Yesterday', 'DJIA Decrease Yesterday')
            , paste0(round(djiaChange*100, 1), '%')
            , color = ifelse(djiaChange >= 0, 'green', 'red')
            , fill = T
            , icon = icon('money-check-alt', lib = 'font-awesome')
    )
  })
  output$bestPerformerBox <- renderInfoBox({
    infoBox(ifelse(mostRecentStockChange[1,'adjustedChange'][[1]]-1 > 0
                   , paste0('Best (Yesterday): ', mostRecentStockChange[1,'symbol'][[1]])
                   , paste0('Best (Yesterday): ', mostRecentStockChange[1,'symbol'][[1]]))
            , paste0(round((mostRecentStockChange[1,'adjustedChange'][[1]]-1)*100, 1), '%')
            , color = ifelse(mostRecentStockChange[1,'adjustedChange'][[1]] > 0, 'green', 'red')
            , fill = T
            , icon = icon(ifelse(mostRecentStockChange[1,'adjustedChange'][[1]]-1 > 0
                                 , 'thumbs-up', 'thumbs-down')
                          , lib = 'font-awesome')
    )
  })
  output$worstPerformerBox <- renderInfoBox({
    infoBox(ifelse(mostRecentStockChange[nrow(mostRecentStockChange),'adjustedChange'][[1]]-1 > 0
                   , paste0('Worst (Yesterday): ', mostRecentStockChange[nrow(mostRecentStockChange),'symbol'][[1]])
                   , paste0('Worst (Yesterday): ', mostRecentStockChange[nrow(mostRecentStockChange),'symbol'][[1]]))
            , paste0(round((mostRecentStockChange[nrow(mostRecentStockChange),'adjustedChange'][[1]]-1)*100, 1), '%')
            , color = ifelse(mostRecentStockChange[nrow(mostRecentStockChange),'adjustedChange'][[1]]-1 > 0, 'green', 'red')
            , fill = T
            , icon = icon(ifelse(mostRecentStockChange[nrow(mostRecentStockChange),'adjustedChange'][[1]]-1 > 0
                                 , 'thumbs-up', 'thumbs-down')
                          , lib = 'font-awesome')
    )
  })
  output$djiaPlot <- renderPlotly({
    pd <- spd
    # p <- plot_ly(pd, x = ~date, y = ~cumChange, color = ~symbol, source = 'A') %>%
    #   add_lines() 
    # p %>%
    #   layout(dragmode = "select") %>%
    #   event_register('plotly_selecting')
    slp <- ggplot(pd, aes(x = date, y = cumChange, color = symbol)) +
      geom_line() +
      labs(title = 'Cumulative Return YTD'
           , x = 'Date', y = 'Cumulative Return')
    ggplotly(slp, source = 'A', key = ~date) %>%
      layout(dragmode = "select") %>%
      event_register("plotly_selecting")
  })
  # output$lineChartTest <- renderPrint({
  #   datesToGet <- event_data(event = "plotly_brushed", source = 'A')
  #   if(is.null(datesToGet)){
  #     'Selected observations go here.'
  #   }else{
  #     firstDate <- as.Date(datesToGet$x[1], origin = '1970-01-01')
  #     firstDate
  #   }
  # })
  # Wordcloud----
  output$wordCloud <- renderWordcloud2({
    pd <- fdw()$wordCount
    pd %<>% dplyr::filter(word != 'trump')
    wordcloud2(pd, size = 1.6, fontFamily = 'Courier'
               , color=rep_len(themeColors$colorHex, nrow(pd))
               , backgroundColor = 'white')
  })
  # Correlation Positive and stock price----

  output$posStockCorPlot <- renderPlot({
    cData <- fdw()$cData
    corrplot::corrplot(cor(cData[,2:ncol(cData)])
                       , method = 'pie'
                       , order = 'hclust'
                       , addrect = 4)
  })

  output$stockPosPlot <- renderPlotly({
    symbolsToGet <- c('regn', 'zm', 'nclh', 'rcl')
    cData <- fdw()$cData
    cDataLong <- cData[,which(names(cData) %in% c('date', 'positive', symbolsToGet))] %>%
      pivot_longer(cols = c('positive', symbolsToGet)
                   , names_to = 'type') %>%
      dplyr::mutate(
        lvalue = log1p(value)
        , type = ifelse(type == 'positive', 'Confirmed Cases', type)
      ) %>%
      dplyr::arrange(date, type)

    p <- ggplot(cDataLong, aes(x = date, y = lvalue, color = type)) +
      geom_line() +
      scale_color_manual(values = c('#111111', '#D12229', '#D12260', '#00A499', '#00a65a')) +
      labs(title = 'Relationship Between COVID-19 and Stock Prices'
           , x = 'Date'
           , y = 'Worldwide Cases & Closing Price (Log)')
    ggplotly(p)
  })
  
  ######### US ##########
  # Daily State Info----
  output$newConfirmedStateBox <- renderInfoBox({
    bd <- fds()$mrState
    if(input$tpcRadioState == 'total'){
      sumNum <- sum(bd$newPositive, na.rm = T) %>%
        prettyNum(big.mark = ',', interval = 3)
      boxText <- 'Confirmed Yesterday'
    }else{
      sumNum <- round((sum(bd$newPositive, na.rm = T) / sum(bd$population, na.rm = T)), 1) %>%
        prettyNum(big.mark = ',', interval = 3)
      boxText <- 'Confirmed Yesterday (per million)'
    }
    infoBox(boxText
            , sumNum
            , color = 'yellow'
            , fill = T
            , icon = icon('fa fa-lungs-virus', lib = 'font-awesome')
    )
  })
  output$newDeathsStateBox <- renderInfoBox({
    bd <- fds()$mrState
    if(input$tpcRadioState == 'total'){
      sumNum <- sum(bd$newDeaths, na.rm = T) %>%
        prettyNum(big.mark = ',', interval = 3)
      boxText <- 'Deaths Yesterday'
    }else{
      sumNum <- round((sum(bd$newDeaths, na.rm = T) / sum(bd$population, na.rm = T)), 1) %>%
        prettyNum(big.mark = ',', interval = 3)
      boxText <- 'Deaths Yesterday (per million)'
    }
    infoBox(boxText
            , sumNum
            , color = 'black'
            , fill = T
            , icon = icon('skull', lib = 'font-awesome')
    )
  })
  output$newRecoveredStateBox <- renderInfoBox({
    bd <- fds()$mrState
    if(input$tpcRadioState == 'total'){
      sumNum <- sum(bd$newRecovered, na.rm = T) %>%
        prettyNum(big.mark = ',', interval = 3)
      boxText <- 'Recovered Yesterday'
    }else{
      sumNum <- round((sum(bd$newRecovered, na.rm = T) / sum(bd$population, na.rm = T)), 1) %>%
        prettyNum(big.mark = ',', interval = 3)
      boxText <- 'Recovered Yesterday (per million)'
    }
    infoBox(boxText
            , sumNum
            , color = 'green'
            , fill = T
            , icon = icon('walking', lib = 'font-awesome')
    )
  })
  output$currentDayCurveState <- renderPlotly({
    curveData <- stateSummary %>%
      dplyr::filter(state %in% fds()$mrState$state)
    if(input$tpcRadioState == 'total'){
      curveData %<>%
        group_by(date) %>%
        dplyr::summarise(CONFIRMED = sum(newPositive, na.rm = T)
                         , DEATHS = sum(newDeaths, na.rm = T)
                         , RECOVERED = sum(newRecovered, na.rm = T)
                         , .groups = 'drop') %>%
        ungroup() %>%
        pivot_longer(cols = c('CONFIRMED', 'DEATHS', 'RECOVERED')
                     , names_to = 'type')
      ylab <- 'Observations Per Day'
    } else{
      curveData %<>%
        group_by(date) %>%
        dplyr::summarise(CONFIRMED = sum(newPositive, na.rm = T)/sum(population, na.rm = T)
                         , DEATHS = sum(newDeaths, na.rm = T)/sum(population, na.rm = T)
                         , RECOVERED = sum(newRecovered, na.rm = T)/sum(population, na.rm = T)
                         , .groups = 'drop') %>%
        ungroup() %>%
        pivot_longer(cols = c('CONFIRMED', 'DEATHS', 'RECOVERED')
                     , names_to = 'type')
      ylab <- 'Observations Per Day (per million)'
    }
    curvePlot <- ggplot(curveData, aes(x = date, y = value, color = type)) +
      # geom_smooth() +
      geom_line() +
      scale_color_manual(values = coronaColors$color) +
      labs(title = 'Confirmed, Deaths, Recovered Per Day'
           , x = 'Date'
           , y = ylab)
    ggplotly(curvePlot)
  })
  
  # Cumulative state cases----
  output$totalConfirmedStateBox <- renderInfoBox({
    bd <- fds()$mrState
    if(input$tpcRadioState == 'total'){
      sumNum <- sum(bd$positive, na.rm = T) %>%
        prettyNum(big.mark = ',', interval = 3)
      boxText <- 'Total Confirmed'
    }else{
      sumNum <- round((sum(bd$positive, na.rm = T) / sum(bd$population, na.rm = T)), 1) %>%
        prettyNum(big.mark = ',', interval = 3)
      boxText <- 'Total Confirmed (per million)'
    }
    infoBox(boxText
            , sumNum
            , color = 'yellow'
            , fill = T
            , icon = icon('fa fa-lungs-virus', lib = 'font-awesome')
    )
  })
  
  output$totalDeathsStateBox <- renderInfoBox({
    bd <- fds()$mrState
    if(input$tpcRadioState == 'total'){
      sumNum <- sum(bd$death, na.rm = T) %>%
        prettyNum(big.mark = ',', interval = 3)
      sumRate <- round(sum(bd$death, na.rm = T) / sum(bd$positive, na.rm = T)*100, 1) %>%
        paste0(., '%')
      sumNumText <- paste0(sumNum, ' (', sumRate, ')')
      boxText <- 'Total Deaths (pct confirmed)'
    }else{
      sumNumText <- round((sum(bd$death, na.rm = T) / sum(bd$population, na.rm = T)), 1) %>%
        prettyNum(big.mark = ',', interval = 3)
      boxText <- 'Total Deaths per Million'
    }
    infoBox(boxText
            , sumNumText
            , color = 'black'
            , fill = T
            , icon = icon('skull', lib = 'font-awesome')
    )
  })
  
  
  output$totalRecoveredStateBox <- renderInfoBox({
    bd <- fds()$mrState
    if(input$tpcRadioState == 'total'){
      sumNum <- sum(bd$recovered, na.rm = T) %>%
        prettyNum(big.mark = ',', interval = 3)
      sumRate <- round(sum(bd$recovered, na.rm = T) / sum(bd$positive, na.rm = T)*100, 1) %>%
        paste0(., '%')
      sumNumText <- paste0(sumNum, ' (', sumRate, ')')
      boxText <- 'Total Recovered (pct confirmed)'
    }else{
      sumNumText <- round((sum(bd$recovered, na.rm = T) / sum(bd$population, na.rm = T)), 1) %>%
        prettyNum(big.mark = ',', interval = 3)
      boxText <- 'Total Recovered per Million'
    }
    infoBox(boxText
            , sumNumText
            , color = 'green'
            , fill = T
            , icon = icon('walking', lib = 'font-awesome')
    )
  })
  
  output$baseDayState <- renderPlotly({
    pd <- fds()$fbds
    ncount <- length(unique(pd$state))
    
    if(input$tpcRadioState == 'total'){
      if(ncount > 10){
        pd %<>% group_by(date) %>%
          dplyr::summarise(positive = sum(positive, na.rm = T)
                           , .groups = 'drop') %>%
          ungroup()
        p <- ggplot(pd, aes(x = date, y = positive)) +
          geom_line() +
          labs(title = 'US Cases'
               , x = 'Date'
               , y = 'Cumulative Cases')
      }else{
        pd %<>% dplyr::filter(baseDay > 0)
        p <- ggplot(pd, aes(x = baseDay, y = positive, color = state)) +
          geom_line() +
          labs(title = 'Cumulative Cases Starting On Day When Cum Cases is 100'
               , x = 'Base Day'
               , y = 'Cumulative Cases')
      }
    }else{
      if(ncount > 10){
        pd %<>% group_by(date) %>%
          dplyr::summarise(posPerMil = sum(positive, na.rm = T) /sum(population, na.rm = T)
                           , .groups = 'drop') %>%
          ungroup()
        p <- ggplot(pd, aes(x = date, y = posPerMil)) +
          geom_line() +
          labs(title = 'Per Capita US Cases'
               , x = 'Date'
               , y = 'Cumulative Cases (per million)')
      }else{
        pd %<>% dplyr::filter(baseDay > 0)
        p <- ggplot(pd, aes(x = baseDay, y = posPerMil, color = state)) +
          geom_line() +
          labs(title = 'Per Capita Cumulative Cases Starting On Day When Cum Cases is 100'
               , x = 'Base Day'
               , y = 'Cumulative Cases (Per Million)')
      }
    }
    ggplotly(p)
  })
  
  
  # State table----
  output$stateTable <- DT::renderDataTable({
    # Filter table
    tData <- mostRecentState[,c('state', 'date', 'positive', 'death'
                           , 'recovered', 'newPositive', 'newDeaths'
                           , 'newRecovered', 'deathRate')]
    # Cols
    bigNums <- c('positive', 'death'
                 , 'recovered', 'newPositive', 'newDeaths'
                 , 'newRecovered')
    
    # Create table
    t <- DT::datatable(tData
                       , escape = F
                       , colnames = c('State', 'Most Recent Date', 'Confirmed'
                                      , 'Deaths', 'Recovered', 'New Confirmed'
                                      , 'New Deaths', 'New Recovered'
                                      , 'Death Rate')
                       , selection = 'multiple'
                       , options = list(fillContainer = T
                                        , pageLength = 5)) %>%
      formatPercentage('deathRate', digits = 1) %>%
      formatDate('date', method = 'toDateString') %>%
      formatRound(bigNums, interval = 3, mark = ',', digits = 0)
    # formatStyle(c('newConfirmed')
    #             , backgroundColor = styleInterval(brks, clrs))
    
    for(colName in c(bigNums, 'deathRate')){
      # Create breaks for color coding cells
      brks <- quantile(tData[,colName][[1]]
                       , probs = seq(0, .95, .05)
                       , na.rm = T)
      clrs <- round(seq(255, 40, length.out = length(brks)+1), 0) %>%
        paste0('rgb(255,', ., ',', ., ')')

      t %<>% formatStyle(colName
                         , backgroundColor = styleInterval(brks, clrs))
    }
    t
  })
  
  ########### AUTHENTICATION ###########
  # Since observe() is used below, any variable that we want to persist in the
  # session outside of the observe() function, but not be available globally,
  # needs to be a reactiveValue(), which is like a list of objects.
  USER <- reactiveValues(Logged = F)
  
  # output$text1 <- renderText(ls())
  observe({
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          USER$Username <- isolate(input$userName)
          USER$Password <- isolate(input$passwd)
          Id.username <- 1
          # Id.username <- if(Username == ""){1}else{0} # put username in quotes
          Id.password <- if(USER$Password == "gies"){1}else{0} # put pw in quotes
          # Notification of wrong unsername/password
          if(Id.username == 0 | Id.password == 0){
            loginNotification <<- showNotification(
              ui = "Incorrect Username/Password",
              # duration = 5, 
              closeButton = T,
              type = "error"
            )
          }
          if (Id.username > 0 & Id.password > 0){
            if (Id.username == Id.password) {
              loginNotification <<- showNotification(
                ui = "Loading Data",
                duration = 180,
                closeButton = T,
                type = "default"
              )
              
              # To logout back to login page----
              USER$login.page = paste0(
                isolate(session$clientData$url_protocol) # e.g., https
                , "//"
                , isolate(session$clientData$url_hostname) # e.g., 127.0.0.1 or 
                , isolate(session$clientData$url_pathname) # e.g., /
                , isolate(session$clientData$url_port) # e.g., 1234
                , isolate(session$clientData$search) # e.g., ?foo=123&bar=somestring
              )

              
              # Give signal to set up the rest of the dashboard----
              USER$Logged <- TRUE
              isolate({updateTabItems(session, "tabs", "world")}) # Selects the overview tab after logging in
              # Remove the notification----
              removeNotification(loginNotification)
            }
          }
        }
      }
    }
  })
  ######### DYNAMIC SIDEBAR CONFIG ########
  output$sidebarpanel <- renderUI({
    if(USER$Logged == TRUE){
      div(
        sidebarUserPanel(
          h5(paste0(name = isolate(input$userName)))
          , subtitle = a(icon("usr"), icon("user"), "Logout", href = USER$login.page)
          # , image = "https://www.secondcity.com/wp-content/uploads/2014/09/SC_Alumni_Farley_Chris_600x600_001.jpg" # Chris Farley
          , image = 'https://localtvkfsm.files.wordpress.com/2019/09/mgn_1280x960_50614k00-kzoku.jpg?quality=85&strip=all&w=300&h=225' # Batman
        )
        # , h6(paste0("Last update: ", as.character(maxDate)))
        ,br()
        # , verbatimTextOutput("testText")
        , sidebarMenu(id = "tabs",
                      menuItem('World'
                               , tabName = "world"
                               # , icon = icon("arrow-alt-circle-up")
                      )
                      , conditionalPanel(condition = "input.tabs == 'world'"
                                         , radioButtons("tpcRadioCountry"
                                                        , label = "Total/PerCapita"
                                                        , choices = list("Total" = 'total', "Per Capita" = 'perCapita')
                                                        , selected = 'total')
                                         , downloadButton('worldSummaryReport', 'World Summary Report PDF')
                      )
                      , menuItem("US"
                                 , tabName = "us"
                      )
                      , conditionalPanel(condition = "input.tabs == 'us'"
                                         , radioButtons("tpcRadioState"
                                                        , label = "Total/PerCapita"
                                                        , choices = list("Total" = 'total', "Per Capita" = 'perCapita')
                                                        , selected = 'total')
                                         )
                      , menuItem('Stocks'
                                 , tabName = 'stocks')
                      )
        , br()
        , tags$a(href = giggHomepageUrl
                 , img(src = giggTrademarkUrl
                       , title = "Gigg Logo and Link"
                       , height = "20px"
                       , width = "100px")
                 , style = "padding-left:30px;"
        )
      )
    }
  })
  ######### DYNAMIC BODY CONFIG #########
  output$body <- renderUI({
    if(USER$Logged == T){
      removeClass(selector = "body", class = "sidebar-collapse")
      tabItems(
        # World----
        tabItem(tabName = 'world'
                , fluidRow(
                  box(width = 12
                      , title = 'Country Overview'
                      , solidHeader = T
                      , collapsible = T
                    , column(width = 6
                           , div(style = 'overflow-x: scroll; font-size: 75%'
                                 , DT::dataTableOutput('worldTable')))
                    , column(width = 6
                             , leafletOutput('mostRecentMap')
                    )
                  )
                )
                , fluidRow(
                  column(width = 8
                         , plotlyOutput('currentDayCurve')
                         # , textOutput('testText')
                         )
                  , column(width = 4
                           , infoBoxOutput('newConfirmedBox', width = NULL)
                           , infoBoxOutput('newDeathsBox', width = NULL)
                           , infoBoxOutput('newRecoveredBox', width = NULL)
                           )
                  )
                , fluidRow(
                  column(width = 8
                         , plotlyOutput('baseDayCountry')
                  )
                  , column(width = 4
                           , infoBoxOutput('totalConfirmedBox', width = NULL)
                           , infoBoxOutput('totalDeathsBox', width = NULL)
                           , infoBoxOutput('totalRecoveredBox', width = NULL)
                  )
                )
                )
        
        # US----
        , tabItem(tabName = 'us'
                  , fluidRow(
                    box(width = 12
                        , title = 'State Overview'
                        , solidHeader = T
                        , collapsible = T
                        , column(width = 12
                                 , div(style = 'overflow-x: scroll; font-size: 75%'
                                       , DT::dataTableOutput('stateTable')))
                        )
                    )
                  , fluidRow(width = 12
                             , column(width = 8
                                      , plotlyOutput('currentDayCurveState')
                             )
                             , column(width = 4
                                      , infoBoxOutput('newConfirmedStateBox', width = NULL)
                                      , infoBoxOutput('newDeathsStateBox', width = NULL)
                                      , infoBoxOutput('newRecoveredStateBox', width = NULL)
                             )
                  )
                  , fluidRow(
                    column(width = 8
                           , plotlyOutput('baseDayState')
                    )
                    , column(width = 4
                             , infoBoxOutput('totalConfirmedStateBox', width = NULL)
                             , infoBoxOutput('totalDeathsStateBox', width = NULL)
                             , infoBoxOutput('totalRecoveredStateBox', width = NULL)
                    )
                  )
        )
        # Stocks----
        , tabItem(tabName = 'stocks'
                  , fluidRow(
                    column(width = 8
                           , plotlyOutput('djiaPlot')
                    )
                    , column(width = 4
                             , infoBoxOutput('djiaChangeBox', width = NULL)
                             , infoBoxOutput('bestPerformerBox', width = NULL)
                             , infoBoxOutput('worstPerformerBox', width = NULL)
                    )
                  )
                  , fluidRow(box(width = 12
                                 , collapsible = T
                                 , title = 'Correlation Between Positive Covid-19 Cases and Stock Price'
                                 , column(width = 6
                                          , plotlyOutput('stockPosPlot')
                                 )
                                 , column(width = 6
                                          , h3('Correlation Matrix for Positive and Stock Prices')
                                          , plotOutput('posStockCorPlot')
                                 )
                  )
                  )
                  # , fluidRow(column(width = 12, verbatimTextOutput('lineChartTest')))
                  , fluidRow(box(width = 12
                                 , solidHeader = T
                                 , collapsible = T
                                 , collapsed = T
                                 , title = 'News Events'
                                  , wordcloud2Output("wordCloud", width = "100%"
                                                   # , height = "565px"
                                    )
                                 )
                             )
        )
        # Other Stuff----
      )
    }else{
      login
    }
  })
  
}

#### RUN THE APP ####
shinyApp(ui = ui, server = server)

