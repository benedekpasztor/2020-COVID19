## package installations

#install.packages('WDI')
#install.packages('data.table')
#install.packages('countrycode')
#install.packages('tidyverse')
#install.packages('jsonlite')
#install.packages('filesstrings')
#install.packages('janitor')
#install.packages('WHO')



library(tidyverse)
library(dplyr)
library(lubridate)
library(reshape2)
library(jsonlite)
library(filesstrings)
library(WDI)
library(data.table)
library(countrycode)
library(janitor)
library(WHO)


source('src/DataDownload.R')
source('src/DataDownload_CountryStats.R')