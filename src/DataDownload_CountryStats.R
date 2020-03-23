##### Country data
download_worldbank_data <- function(){
  data <- WDI(country = 'all', indicator = c("SP.POP.TOTL", "NY.GDP.PCAP.PP.KD", "ny.gdp.mktp.cd", "AG.SRF.TOTL.K2", 
                                             "SP.DYN.LE00.IN", "SP.URB.TOTL.IN.ZS", "ST.INT.ARVL", "SP.POP.65UP.TO"), 
              start = 2017, end = 2017, extra = TRUE) 
  
  
  ## namings
  names(data)[which(names(data) == "SP.POP.TOTL")] <- "population_no"
  names(data)[which(names(data) == "NY.GDP.PCAP.PP.KD")] <- "gdppc_usdpc"
  names(data)[which(names(data) == "ny.gdp.mktp.cd")] <- "gdp_usd"
  names(data)[which(names(data) == "AG.SRF.TOTL.K2")] <- "area_km2"
  names(data)[which(names(data) == "SP.DYN.LE00.IN")] <- "lifeexp_y"
  names(data)[which(names(data) == "SP.URB.TOTL.IN.ZS")] <- "urbanpop_perc"
  names(data)[which(names(data) == "ST.INT.ARVL")] <- "internationaltourist_yearly_no"
  names(data)[which(names(data) == "SP.POP.65UP.TO")] <- "elderly_65above_no"
  
  
  ## keeping only the necessary columns, creating full name and continent columns
  data <- data.table(data)
  data <- data[, .(iso3c, year, country, region, population_no, gdppc_usdpc, gdp_usd, area_km2, lifeexp_y, urbanpop_perc, internationaltourist_yearly_no,
                   elderly_65above_no, capital, income, income, lending)]
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
  
  write.table(data, 'data/raw/worldbank_data.csv', sep = ',', dec = '.', row.names = F)
  
  return()
}


############## Mortality data downloaded manually from http://data.un.org/Data.aspx?d=POP&f=tableCode%3A65 - 
get_latest_mortality_data <- function(){
data <- read.table('data/raw/undata_deaths_by_month.csv', sep = ',', dec = '.', header = T) %>% clean_names()

max_years <- data %>% 
  group_by(country_or_area) %>% 
  summarize(year = max(year))

data_final <- data.frame()
for (country in max_years$country_or_area){
  corresponding_year <- max_years %>% filter(country_or_area == country)
  corresponding_year <- corresponding_year$year
  data_final_i <- data %>% 
    filter(country_or_area == country & year == corresponding_year)
  
  data_final <- plyr::rbind.fill(data_final, data_final_i)
}

write.table(data_final, 'data/raw/undata_deaths_by_month_lasts.csv', sep = ',', dec = '.', row.names = F)

}



############## WHO data download - 
get_latest_from_df <- function(df){
  max_years <- df %>% 
    group_by(country) %>% 
    summarize(year = max(year))
  
  data_final <- data.frame()
  countrylist <- Filter(function(a) any(!is.na(a)), max_years$country)
  for (c in countrylist){
    corresponding_year <- max_years %>% filter(country == c)
    corresponding_year <- corresponding_year$year
    data_final_i <- df %>% filter(country == c & year == corresponding_year)
    
    data_final <- plyr::rbind.fill(data_final, data_final_i)
  }
  return(data_final)
}


download_who_data <- function(){
  
median_ages <- get_data("WHS9_88") %>% clean_names()
exp_on_healthcare_total_usdpc <- get_data("WHS7_156") %>% clean_names()
exp_on_healthcare_government_usdpc <- get_data('WHS7_104') %>% clean_names() 

median_ages <- get_latest_from_df(median_ages)
exp_on_healthcare_government_usdpc <- get_latest_from_df(exp_on_healthcare_government_usdpc)
exp_on_healthcare_total_usdpc <- get_latest_from_df(exp_on_healthcare_total_usdpc)


median_ages <- median_ages %>% 
  select(country, year, value) %>% 
  plyr::rename(c('value' = 'age_median_y', 'year' = 'age_median_datayear'))

exp_on_healthcare_total_usdpc <- exp_on_healthcare_total_usdpc %>% 
  select(country, year, value) %>% 
  plyr::rename(c('value' = 'exp_on_healthcare_total_usdpc_y', 'year' = 'exp_on_healthcare_total_usdpc_datayear'))


exp_on_healthcare_government_usdpc <- exp_on_healthcare_government_usdpc %>% 
  select(country, year, value) %>% 
  plyr::rename(c('value' = 'exp_on_healthcare_government_usdpc_y', 'year' = 'exp_on_healthcare_government_usdpc_datayear'))


data_joined <- plyr::join_all(list(median_ages, exp_on_healthcare_government_usdpc, exp_on_healthcare_total_usdpc), by = 'country')



write.table(data_joined, 'data/raw/who_data.csv', sep = ',', dec = '.', row.names = F)

}

