library(tidyverse)
library(dplyr)
library(reshape2)


###### COVID-19 DATA ####
data_confirmed <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv')
data_deaths <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv')
data_recovered <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv')



data_confirmed <- melt(data_confirmed, id.vars = c("Province.State", "Country.Region", "Lat", "Long")) %>% plyr::rename(c('variable' = 'date', 
                                                                                                                          'value' = 'confirmed'))
data_deaths <- melt(data_deaths, id.vars = c("Province.State", "Country.Region", "Lat", "Long"))  %>% plyr::rename(c('variable' = 'date', 
                                                                                                                     'value' = 'deaths'))
data_recovered <- melt(data_recovered, id.vars = c("Province.State", "Country.Region", "Lat", "Long")) %>% plyr::rename(c('variable' = 'date', 
                                                                                                                          'value' = 'recovered'))

data_covid <- plyr::join(plyr::join(data_confirmed, data_deaths), data_recovered) %>% 
  plyr::rename(c('Country.Region' = 'country', 'Province.State' = 'province', 'Lat' = 'lat', 'Long' = 'lon')) %>% 
  mutate(date = substring(date, 2), date = paste0('0', date)) %>% 
  mutate(date = as.Date(date, format = "%m.%d.%y"))


rm(list = c('data_confirmed', 'data_deaths', 'data_recovered'))



########### Country specific data

library(WDI)
library(data.table)
library(countrycode)


data <- WDI(country = 'all', indicator = c("SP.POP.TOTL", "NY.GDP.PCAP.PP.KD", "ny.gdp.mktp.cd", "AG.SRF.TOTL.K2", 
                          "SP.DYN.LE00.IN", "SP.URB.TOTL.IN.ZS"), 
            start = 2017, end = 2017, extra = TRUE) 

## converting to data table
data <- data.table(data)

## namings
names(data)[which(names(data) == "SP.POP.TOTL")] <- "Population"
names(data)[which(names(data) == "NY.GDP.PCAP.PP.KD")] <- "GDPPerCap"
names(data)[which(names(data) == "ny.gdp.mktp.cd")] <- "GDP"
names(data)[which(names(data) == "AG.SRF.TOTL.K2")] <- "Area"
names(data)[which(names(data) == "SP.DYN.LE00.IN")] <- "LifeExp"
names(data)[which(names(data) == "SP.URB.TOTL.IN.ZS")] <- "UrbanPop"


## keeping only the necessary columns, creating full name and continent columns
data <- data[, .(iso3c, year, country, region, Population, UrbanPop, GDPPerCap, GDP, 
                 Area, LifeExp, capital, income, lending)]
data[, country := countrycode(data$iso3c, 'iso3c', destination = 'country.name')]
data[, continent := countrycode(data$iso3c, 'iso3c', 'continent')]
data <- data[region != "Aggregates", ]


# prepare for merging
names(data)[which(names(data) == "year")] <- "YEAR"
names(data)[which(names(data) == "iso3c")] <- "CCODE"

# Continents
data[, continent := countrycode(CCODE, 'iso3c', 'continent')]
data[CCODE == "ANT", continent := "Americas"]
data[CCODE == "CSK", continent := "Europe"]
data[CCODE == "DDR", continent := "Europe"]
data[CCODE == "YMD", continent := "Asia"]
data[CCODE == "NMP", continent := "Asia"]

#if country name doesn't exist -> country name := CCODE
data[is.na(country), country := CCODE]

# dropping values without valid continent value
data <- data[!is.na(continent)]

data

############### Weather data #### f652bf7e233ec0c2d7845413f2ad1e71 - https://api.darksky.net/forecast/f652bf7e233ec0c2d7845413f2ad1e71/37.8267,-122.4233
