# covid19Dashboard
This dashboard is intended as an example of what can be done with data analysis for business purposes using R. It was designed as an example for MBA 561 at the University of Illinois. Students are not required to know all of this, but there are some who are interested, so the code is here. There are several general tasks that need to be completed for this dashboard. The tasks and associated files are listed below.

## Gathering data
The data is first gathered from other online resources using the gatheringData.R file. Once the initial data is created, it needs to be updated. The data is updated by running a different portion of the gatheringData.R file. Instructions for downloading the data for the first time vs. incremental updates are inlcuded in the gatheringData.R file. This file creates the various .csv files:

1. allHeadlines.csv
2. allStocks.csv
3. countryConfirmedDeathRecovered.csv
4. countryPopulation.csv
5. countryTestPop.csv
6. stateData.csv
7. statePopulation.csv

## Creating the dashboard
The script for creating the dashboard is in the covid19Dashboard.R file. The image and styling file are in the www folder: 

1. coronaVirus.jpg - an image from pexels.com used for the arrival screen
2. kingfisher.png - an image used for the top left-hand corner of the dashboard.
3. gradingTool.css - a file to indicate various styling choices, like colors.

## Creating a pdf file
The worldSummaryReport.Rmd file contains the instructions for creating the pdf file.
