source('src/setup_environment.R')


#covid_19_datarefresh -> ds_data refresh
covid_refresh()
ds_refresh()

# data read
covid <- read.csv('data/raw/covid19_data.csv')
ds_daily <- read.csv('data/raw/ds_daily.csv')
ds_hourly <- read.csv('data/raw/ds_hourly.csv')
who <- read.csv('data/raw/who_data.csv')
undata_deaths_by_month <- read.csv('data/raw/undata_deaths_by_month_lasts.csv')
worldbank <- read.csv('data/raw/worldbank_data.csv')


######## covid preparation
covid <- covid %>% 
  group_by(province) %>%
  arrange(date) %>%
  mutate(date = as.Date(date),
         active = confirmed - deaths - recovered) %>% 
  mutate(confirmed_daily = confirmed - lag(confirmed, default = first(confirmed)),
         deaths_daily = confirmed - lag(deaths, default = first(deaths)),
         recovered_daily = confirmed - lag(confirmed, default = first(recovered)),
         active_daily = confirmed - lag(confirmed, default = first(active))) %>% 
  mutate(confirmed_daily = ifelse(day_since_first_case_province == 1, confirmed, confirmed_daily),
         deaths_daily = ifelse(day_since_first_case_province == 1, deaths, deaths_daily),
         recovered_daily = ifelse(day_since_first_case_province == 1, recovered, recovered_daily),
         active_daily = ifelse(day_since_first_case_province == 1, active, active_daily)) %>% 
  as.data.frame() %>% 
  mutate(country = as.character(country),
         province = as.character(province))



######## undata preparation
undata_deaths_by_month_multiple <- undata_deaths_by_month %>% 
  filter(grepl('-', month)) %>% 
  separate(month, ' - ', remove = F, into = c('month_from', 'month_to'))
  
undata_deaths_by_month_multiple$month_from <- match(undata_deaths_by_month_multiple$month_from, month.name)
undata_deaths_by_month_multiple$month_to <- match(undata_deaths_by_month_multiple$month_to, month.name)

data_out <- data.frame()

for (country in as.character(unique(undata_deaths_by_month_multiple$country_or_area))){
  print(country)
  filter_country<- undata_deaths_by_month_multiple %>% filter(country_or_area == country)
  months <- as.character(unique(filter_country$month))
  
  for (m_long in months){
  
  data_new <- filter_country %>% filter(month == m_long)
  months_number <- data_new$month_to - data_new$month_from + 1
  
    for (i in seq(data_new$month_from, data_new$month_to, by = 1)){
      data_out_i <- data_new
      data_out_i$month <- i
      data_out_i$value <- round(data_new$value/months_number)
      
      data_out_i$month_from <- NULL
      data_out_i$month_to <- NULL
      
      data_out <- plyr::rbind.fill(data_out, data_out_i)
    }
  }
}


undata_deaths_by_month <- undata_deaths_by_month %>% 
  filter(!grepl('-', month))

undata_deaths_by_month$month <- match(undata_deaths_by_month$month, month.name)

undata_deaths_by_month <- undata_deaths_by_month %>% 
  filter(!is.na(month))

undata_deaths_by_month <- plyr::rbind.fill(undata_deaths_by_month, data_out)

undata_deaths_by_month <- undata_deaths_by_month %>% 
  select(country = country_or_area, deaths_monthly_no = value, deaths_monthly_datayear = year, month) %>% 
  mutate(country = as.character(country))


undata_deaths_by_month <- undata_deaths_by_month %>% 
  mutate(country = as.character(country)) %>% 
  mutate(country = ifelse(country == 'United States of America', 'United States', 
                          ifelse(country == 'United Kingdom of Great Britain and Northern Ireland', 'United Kingdom', 
                                 ifelse(country == 'United Republic of Tanzania', 'Tanzania',
                                        ifelse(country == 'Venezuela (Bolivarian Republic of)', 'Venezuela', 
                                               ifelse(country == 'Republic of Moldova', 'Moldova', 
                                                      ifelse(country == 'Republic of Korea', 'South Korea', 
                                                             ifelse(country == 'Democratic Republic of the Congo', 'Congo - Kinshasa', country)))))))) %>% 
  mutate(country = ifelse(country == 'Viet Nam', 'Vietnam', 
                          ifelse(country == 'Syrian Arab Republic', 'Syria',
                                 ifelse(country == 'Russian Federation', 'Russia', 
                                        ifelse(country == 'Myanmar', 'Myanmar (Burma)', 
                                               ifelse(country == 'Iran (Islamic Republic of)', 'Iran', 
                                                      ifelse(country == 'Bosnia and Herzegovina', 'Bosnia & Herzegovina',
                                                             ifelse(country == "Democratic People's Republic of Korea", 'North Korea',
                                                                    ifelse(country == "Lao People's Democratic Republic", 'Laos', 
                                                                           ifelse(country == "Côte d'Ivoire", "Cote d'Ivoire", country)))))))))) %>% 
  mutate(country = ifelse(country == 'Antigua and Barbuda', 'Antigua & Barbuda',
                          ifelse(country == 'United States', 'US', country)))



################### ds_hourly preparation -> std

ds_hourly_features <- ds_hourly %>% 
  group_by(country, province, date) %>% summarize(
    temperature_std = sd(temperature),
    apparenttemperature_std = sd(apparentTemperature),
    pressure_std = sd(pressure)) %>% 
  as.data.frame() %>% 
  mutate(country = as.character(country),
         province = as.character(province),
         date = as.Date(date))

rm('ds_hourly')


ds_daily <- ds_daily %>% 
  select(date, country, province, icon, temperatureHigh, temperatureLow, apparentTemperatureMin, apparentTemperatureMax, cloudCover, pressure) %>% 
  mutate(country = as.character(country),
         province = as.character(province),
         date = as.Date(date))

ds_daily

################### worldbank_data
worldbank <- worldbank %>% 
  select(-continent, -lending, -income.1, -income, -capital, -CCODE) %>% 
  plyr::rename(c('YEAR' = 'worldbankdata_datayear')) %>% 
  mutate(country = as.character(country))

worldbank <- worldbank %>% mutate(country = ifelse(country == 'São Tomé & Príncipe', 'Sao Tome and Principe', 
                                                   ifelse(country == "Côte d'Ivoire", "Cote d'Ivoire", country)))



#### WHO preparation
who <- who %>% 
  mutate(country = as.character(country)) %>% 
  mutate(country = ifelse(country == 'United States of America', 'United States', 
                      ifelse(country == 'United Kingdom of Great Britain and Northern Ireland', 'United Kingdom', 
                             ifelse(country == 'United Republic of Tanzania', 'Tanzania',
                                    ifelse(country == 'Venezuela (Bolivarian Republic of)', 'Venezuela', 
                                           ifelse(country == 'Republic of Moldova', 'Moldova', 
                                                  ifelse(country == 'Republic of Korea', 'South Korea', 
                                                         ifelse(country == 'Democratic Republic of the Congo', 'Congo - Kinshasa', country)))))))) %>% 
  mutate(country = ifelse(country == 'Viet Nam', 'Vietnam', 
                          ifelse(country == 'Syrian Arab Republic', 'Syria',
                                 ifelse(country == 'Russian Federation', 'Russia', 
                                        ifelse(country == 'Myanmar', 'Myanmar (Burma)', 
                                               ifelse(country == 'Iran (Islamic Republic of)', 'Iran', 
                                                      ifelse(country == 'Bosnia and Herzegovina', 'Bosnia & Herzegovina',
                                                             ifelse(country == "Democratic People's Republic of Korea", 'North Korea',
                                                             ifelse(country == "Lao People's Democratic Republic", 'Laos', 
                                                                    ifelse(country == "Côte d'Ivoire", "Cote d'Ivoire", country)))))))))) %>% 
  mutate(country = ifelse(country == 'Antigua and Barbuda', 'Antigua & Barbuda', country))


################## Joining & feature creation

stats_data <- plyr::join_all(list(worldbank, who), by = 'country', type = 'full') %>% 
  mutate(country = ifelse(country == 'United States', 'US', country))


ts_data <- plyr::join_all(list(covid, ds_daily, ds_hourly_features), by = c('country', 'province', 'date')) %>% 
  mutate(month = month(date))


ts_data_all <- plyr::join_all(list(ts_data, undata_deaths_by_month), by = c('country', 'month')) %>% 
  mutate(average_daily_deaths = deaths_monthly_no / days_in_month(date))


final_data <- plyr::join_all(list(ts_data_all, stats_data), by = 'country')


final_data <- final_data %>% mutate(pop_density_nkm2 = population_no / area_km2,
                                    population_million = population_no / 10^6,
                                    population_elderly = elderly_65above_no / population_no,
                                    deaths_per_cases = deaths / confirmed,
                                    confirmed_permillion = confirmed / population_million,
                                    deaths_permillion = deaths / population_million,
                                    daily_deaths_perc_to_lastyear_all = deaths_daily / average_daily_deaths)

final_data <- final_data %>% 
  group_by(province) %>% 
  arrange(date, .by_group = TRUE) %>% 
  mutate(confirmed_daily_increment_perc_province = (confirmed_daily - lag(confirmed_daily)) / lag(confirmed_daily),
         confirmed_daily_increment_perc_province = ifelse(is.infinite(confirmed_daily_increment_perc_province), 1, 
                                              ifelse(is.na(confirmed_daily_increment_perc_province), 0, confirmed_daily_increment_perc_province))) %>% 
  mutate(deaths_daily_increment_perc_province = (deaths_daily - lag(deaths_daily)) / lag(deaths_daily),
         deaths_daily_increment_perc_province = ifelse(is.infinite(deaths_daily_increment_perc_province), 1, 
                                                       ifelse(is.na(deaths_daily_increment_perc_province), 0, deaths_daily_increment_perc_province))) %>% 
  mutate(recovered_daily_increment_perc_province = (deaths_daily - lag(recovered_daily)) / lag(recovered_daily),
         recovered_daily_increment_perc_province = ifelse(is.infinite(recovered_daily_increment_perc_province), 1, 
                                                       ifelse(is.na(recovered_daily_increment_perc_province), 0, recovered_daily_increment_perc_province))) %>% 
  mutate(temperatureHigh_increment_perc_province = (confirmed_daily - lag(temperatureHigh)) / lag(temperatureHigh),
         temperatureHigh_increment_perc_province = ifelse(is.infinite(temperatureHigh_increment_perc_province), 1, 
                                                          ifelse(is.na(temperatureHigh_increment_perc_province), 0, temperatureHigh_increment_perc_province))) %>% 
  mutate(temperature_std_increment_perc_province = (temperature_std - lag(temperature_std)) / lag(temperature_std),
         temperature_std_increment_perc_province = ifelse(is.infinite(temperature_std_increment_perc_province), 1, 
                                                          ifelse(is.na(temperature_std_increment_perc_province), 0, temperature_std_increment_perc_province))) %>% 
  mutate(pressure_std_increment_perc_province = (pressure_std - lag(pressure_std)) / lag(pressure_std),
         pressure_std_increment_perc_province = ifelse(is.infinite(pressure_std_increment_perc_province), 1, 
                                                          ifelse(is.na(pressure_std_increment_perc_province), 0, pressure_std_increment_perc_province))) %>% 
  mutate(arrived_at_province = min(date)) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  mutate(arrived_at_country = min(date)) %>% 
  ungroup() %>% 
  mutate(internationaltourist_yearly_nopc = internationaltourist_yearly_no / population_no)
  
  


## death ratio calculations
final_data <- final_data %>% 
  mutate(deaths_to_confirmed = deaths / confirmed,
         deaths_to_recovered = deaths / recovered,
         recovered_to_confirmed = recovered / confirmed)


### creation of a final dataframe & csv

write.table(final_data, 'data/clean/datafordashboard.csv', row.names = F, sep = ',', dec = '.')
