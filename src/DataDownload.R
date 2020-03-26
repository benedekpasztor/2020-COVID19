
###### COVID-19 DATA ####
covid_refresh <- function(){
  
data_confirmed <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv')
data_deaths <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv')
data_recovered <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv')

# 
# data <- read.csv('https://query.data.world/s/6bv3kozmtz74uj4htfkgilyuouiolu')
# view(data %>% mutate(date = as.Date(ï..Date, format = '%m/%d/%Y')) %>% filter(Country_Region == 'US' & Case_Type == 'Recovered') %>% group_by(date) %>% summarize(sum(Cases)))
# 
# 
# names(data)

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

# Little data manipulation
data_covid <- data_covid %>%
  mutate(province = as.character(province),
         country = as.character(country)) %>% 
  mutate(province = ifelse(nchar(province) == 0, country, province))


data_covid_f <- data_covid %>% filter(!(confirmed == 0 & deaths == 0 & recovered == 0)) %>% 
  group_by(province) %>% 
  arrange((date), .by_group = TRUE) %>% 
  mutate(day_since_first_case_province = row_number()) %>% 
  as.data.frame()

# Country-wise day since
data_covid_c <- data_covid_f %>% 
  group_by(country, date) %>% 
  summarize(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered)) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  arrange(date, .by_group = TRUE) %>% 
  mutate(day_since_first_case_country = row_number()) %>% 
  select(country, date, day_since_first_case_country) %>% 
  as.data.frame()

data_covid_f <- plyr::join(data_covid_f, data_covid_c, by = c('country', 'date'))

view(data_covid_f %>% filter(country == 'Australia') %>% group_by(day_since_first_case_country) %>% summarize(sum(recovered)))


write.table(data_covid_f, 'data/raw/covid19_data.csv', sep = ',', dec = '.')

return()
}



# Refreshing weather data
ds_refresh <- function(){

covid <- read.table('data/raw/covid19_data.csv', sep = ',', dec = '.') %>% 
  mutate(date = as.Date(date))
ds_daily_orig <- read.table('data/raw/ds_daily.csv', sep = ',', dec = '.', header = T) %>% 
  mutate(date = as.Date(date))
ds_hourly_orig <- read.table('data/raw/ds_hourly.csv', sep = ',', dec = '.', header = T) %>% 
  mutate(date = as.Date(date))

### Example function
ds_keys <- c('46f93c9bb3e81f2ff9e21608eeee3e45')
ds_key <- ds_keys[1]

data_for_ds <- covid %>% 
  filter(date > max(ds_daily_orig$date)) %>% 
  mutate(ds_datetimes = paste0(date, "T00:00:00"))


ds_daily <- data.frame()
ds_hourly <- data.frame()

if (nrow(data_for_ds) != 0){
  
  for (i in 1:nrow(data_for_ds)){
    time_i <- paste0(data_for_ds[i, 'lat'])
    url <- paste0("https://api.darksky.net/forecast/", ds_key, "/", data_for_ds[[i, "lat"]], ",",
                  data_for_ds[[i, "lon"]], ",", data_for_ds[[i, "ds_datetimes"]], "?units=si&extend=hourly&lang=en")
    print(url)
    
    dsraw <- readLines(url)
    dslist <- fromJSON(dsraw)
  
    ds_hourly_i <- dslist$hourly$data
    ds_hourly_i <- ds_hourly_i %>% mutate(country = data_for_ds[[i, "country"]], province = data_for_ds[[i, "province"]], date = data_for_ds[[i, "date"]])
    ds_daily_i <- dslist$daily$data 
    ds_daily_i <- ds_daily_i %>% mutate(country = data_for_ds[[i, "country"]], province = data_for_ds[[i, "province"]], date = data_for_ds[[i, "date"]])
    
    ds_hourly <- plyr::rbind.fill(ds_hourly, ds_hourly_i)  
    ds_daily <- plyr::rbind.fill(ds_daily, ds_daily_i)  
  }
}

ds_daily_new <- plyr::rbind.fill(ds_daily_orig, ds_daily)
ds_hourly_new <- plyr::rbind.fill(ds_hourly_orig, ds_hourly)

file.move('data/raw/ds_daily.csv', paste0('data/raw/old/ds_daily_', Sys.Date(), '.csv'))
file.move('data/raw/ds_hourly.csv', paste0('data/raw/old/ds_hourly_', Sys.Date(), '.csv'))

write.table(ds_daily_new, 'data/raw/ds_daily.csv', row.names = F, dec = '.', sep = ',')
write.table(ds_hourly_new, 'data/raw/ds_hourly.csv', row.names = F, dec = '.', sep = ',')

return()

}


