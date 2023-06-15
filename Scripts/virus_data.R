# Collecting virus-related data

# Written by:  Jay H Arehart and Chris H Arehart
# Written on: March 20th, 2020
# Updated on: June 20th, 2020

# Update data?
# coronavirus::update_dataset(T)
'%ni%' <- Negate('%in%')


# TRUE if you want to scale by population
incidence_flag <- T
HubeiFlag <- T

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
# devtools::install_github("RamiKrispin/coronavirus", force=TRUE)
# OLD LINK TO THE REPO devtools::install_github("covid19r/coronavirus", force=TRUE)
library(coronavirus)
library(ggpubr)
library(readxl)
library(tidyverse)
library(purrr)
# moving average
library(zoo)

# Explore the data
data("coronavirus")
head(coronavirus)
max(coronavirus$date)

# Load in csv files
country_codes <- read.csv('./InputData/CountryCodes.csv')

# Load in population data
population <- read_excel("./InputData/pop.xlsx", sheet = "pop_1000s", col_names = T)

population_countrycodes <- merge(country_codes,population, by='ISO3')
population_countrycodes <- population_countrycodes %>%
  dplyr::mutate(Country = Country.x) %>%
  dplyr::select(-Country.x,-Country.y,-PopTotal_1000s)
glimpse(population_countrycodes)

# Add ISO country codes to project
raw_data = tbl_df(merge(coronavirus, population_countrycodes, by.x='iso3', by.y='ISO3'))
if(HubeiFlag==T){
  raw_data = dplyr::mutate(raw_data, country = replace(country,country == 'China', 'Hubei')) %>%
    # filter(country =='Hubei' | province != 'Hubei')
    filter(!province %in% c('Shandong','Guizhou','Hainan',
           'Macau','Ningxia','Inner Mongolia','Yunnan','Sichuan','Zhejiang','Guangxi','Guangdong','Hong Kong','Hunan',
           'Shaanxi','Shanxi','Tibet','Heilongjiang','Qinghai','Hebei','Chongqing','Liaoning','Xinjiang','Gansu',
           'Jiangsu','Jiangxi','Anhui','Henan','Shanghai','Fujian','Tianjin','Beijing','Jilin')) %>%
    dplyr::mutate(iso3 = replace(iso3, iso3=='CHN','HUB'))
    
}

## Functions to apply to a single country  ----
# Function to create a collapsed time series of data with cummulative sums for each country (if multiple regions exist)
country_timeseries <- function(df, cntry, incidence=T, plot=F){
  print(cntry)
  pop <- df %>%
    filter(iso3==cntry) %>%
    summarise(pop = mean(PopTotal))
  pop <- as.numeric(pop)
  output <- df %>% filter(iso3==cntry) %>%
    group_by(date, type) %>%
    summarise(daily_cases = sum(cases)) %>%
    pivot_wider(names_from = type,
                values_from = daily_cases) %>%
    ungroup()
  if(incidence==T){
    output <- output %>%
      dplyr::mutate(confirmed_cum_per_million = cumsum(confirmed) / (pop/1000000)) %>%
      # dplyr::mutate(confirmed_cum_per_million = confirmed / (pop/1000000)) %>%
      dplyr::mutate(death_cum_per_million = cumsum(death) / (pop/1000000)) %>%
      dplyr::mutate(confirmed_cum = cumsum(confirmed)) %>%
      dplyr::mutate(death_cum = cumsum(death))
  }else{
    output <- output %>%
      dplyr::mutate(confirmed_cum = cumsum(confirmed) ) %>%
      dplyr::mutate(death_cum = cumsum(death) )
  }
  if(plot==T){
    # Plot cummulative sum of cases
    p1 <- ggplot(output, aes(x=date, y=confirmed_cum)) +
      geom_line() +
      ggtitle(paste0("Cummulative cases in ", cntry))
    # Plot daily number of reported cases
    p2 <- ggplot(output, aes(x=date, y=confirmed)) +
      geom_line() +
      ggtitle(paste0("Reported number of cases in ", cntry))
    # Plot the two graphs
    my_plot <- ggarrange(p1, p2, ncol=1)
    plot(my_plot)
  }
  return(output)
}

# function to create lag factors for a country's time series
create_lag <- function(country_ts, num=10, incidence=T){
  # creating lag variables for number of reported cases, deaths, and recovered
  lags <- seq(num)   # set the number of lag factors here
  lag_names <- paste("lag", formatC(lags, width = nchar(max(lags)), flag = "0"), 
                     sep = "_")
  lag_functions <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)
  
  if(dim(country_ts)[2] > 7){
    # country_df <- country_ts %>% mutate_at(vars(confirmed, death, recovered), funs_(lag_functions)) %>%
    if(incidence==T){
      country_df <- country_ts %>% mutate_at(vars(confirmed_cum_per_million, death_cum_per_million), funs_(lag_functions)) #%>%
        # select(-values1_lag1, -values1_lag2, -values2_lag1, -values2_lag2)
    }else{
      country_df <- country_ts %>% mutate_at(vars(confirmed_cum, death_cum), funs_(lag_functions)) #%>%
        # select(-values1_lag1, -values1_lag2, -values2_lag1, -values2_lag2)
    }
  }else{
    # country_df <- country_ts %>% mutate_at(vars(confirmed, death, recovered), funs_(lag_functions))
    if(incidence==T){
      country_df <- country_ts %>% mutate_at(vars(confirmed_cum_per_million, death_cum_per_million), funs_(lag_functions))
    }else{
      country_df <- country_ts %>% mutate_at(vars(confirmed_cum, death_cum), funs_(lag_functions))
    }
  }
  return(country_df)
}

# function for creating full dataframe 
create_COVID_ML_df <- function(coronavirus, num_cases_min = 4000, num_lag=10, incidence_flag=T){
  # aggregate the raw data
  summary_df <- coronavirus %>%
    filter(country != 'Cruise Ship') %>%
    group_by(iso3, type) %>%
    summarise(total_cases = sum(cases)) %>%
    filter(type=='confirmed') %>%
    arrange(-total_cases)
  if(HubeiFlag == T){
    HUB = coronavirus %>%
      filter(country != 'Cruise Ship') %>%
      group_by(country, type) %>%
      filter(province == 'Hubei') %>%
      summarise(total_cases = sum(cases)) %>%
      filter(type=='confirmed')
    HUB[1,1] <- 'Hubei'
    summary_df = summary_df %>%
      filter(iso3 != 'CHN') %>%
      bind_rows(HUB) %>%
      arrange(-total_cases)
  }

  # filter by minimum number of cases
  countries_list = summary_df %>%
    filter(total_cases >= num_cases_min) %>%
    arrange(-total_cases)
  # collect ISO3 number
  countries_training = merge(countries_list, country_codes, by.x='iso3', by.y='ISO3')
  countries_training <- countries_training %>%
    arrange(-total_cases)
  
  # create the time series data and lag factors
  df_ts_lag_train <- data.frame()
  for(i in 1:length(countries_training$iso3)){
    df.ts <- country_timeseries(raw_data, countries_training$iso3[i], incidence=incidence_flag, plot = F)
    df.ts.lag <- create_lag(df.ts, num_lag, incidence=incidence_flag)
    df.ts.lag$iso3 <- countries_training$iso3[i]
    df.ts.lag$Country <- countries_training$Country[i]
    df_ts_lag_train <- rbind(df_ts_lag_train,df.ts.lag)
  }
  df_ts_lag_train <- dplyr::select(df_ts_lag_train, date, Country, iso3, everything())
  print(paste0("Total number of countries included in analysis are: ", n_distinct(df_ts_lag_train$Country)))
  # print(paste0("Countries time histories included are: ", distinct(df_ts_lag_train, Country)))
  print(unique(df_ts_lag_train$Country))
  print('Most recent date of data is:  ')
  print(max(df_ts_lag_train$date))
  return(df_ts_lag_train)
}

## Looking at a single country -----

country_ts <- country_timeseries(raw_data, 'USA', incidence = incidence_flag, plot = T) #create time series and plot
country_ts_lag <- create_lag(country_ts, num=10, incidence = incidence_flag)


## Creaint the full dataframe and saving the .csv file -----

output_df <- create_COVID_ML_df(coronavirus, num_cases_min = 1000, num_lag = 14, incidence_flag = incidence_flag)   # to change from cases per million to total cases, change default value in function defined above (country_timeseries)
names(output_df)[names(output_df) == 'iso3'] <- 'ISO3'
names(output_df)[names(output_df) == 'recovery'] <- 'recovered'
# write.csv(output_df, file="InputData/data_COVID_2020_04_02.csv")

## append the USA's state data -----


url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv'
USA_cases <- readr::read_csv(url)
USA_cases$fips <- as.numeric(USA_cases$fips)
unique(USA_cases$state[order(USA_cases$state)])

statesAbbrev <- c("USA_AK","USA_AL","USA_AR","USA_AS","USA_AZ","USA_CA","USA_CO","USA_CT","USA_DC","USA_DE","USA_FL","USA_GA","USA_GU","USA_HI","USA_IA","USA_ID","USA_IL","USA_IN","USA_KS","USA_KY","USA_LA","USA_MA","USA_MD","USA_ME","USA_MI","USA_MN","USA_MO","USA_MS","USA_MT","USA_NC","USA_ND","USA_NE","USA_NH","USA_NJ","USA_NM","USA_NV","USA_NY","USA_OH","USA_OK","USA_OR","USA_PA","USA_PR","USA_RI","USA_SC","USA_SD","USA_TN","USA_TX","USA_UT","USA_VA","USA_VI","USA_VT","USA_WA","USA_WI","USA_WV","USA_WY")
correspFips <- c("2","1","5","60","4","6","8","9","11","10","12","13","66","15","19","16","17","18","20","21","22","25","24","23","26","27","29","28","30","37","38","31","33","34","35","32","36","39","40","41","42","72","44","45","46","47","48","49","51","78","50","53","55","54","56")
# correspName <- c("Alaska","Alabama","Arkansas","American Samoa","Arizona","California","Colorado","Connecticut","District of Columbia","Delaware","Florida","Georgia","Guam","Hawaii","Iowa","Idaho","Illinois","Indiana","Kansas","Kentucky","Louisiana","Massachusetts","Maryland","Maine","Michigan","Minnesota","Missouri","Mississippi","Montana","North Carolina","North Dakota","Nebraska","New Hampshire","New Jersey","New Mexico","Nevada","New York","Ohio","Oklahoma","Oregon","Pennsylvania","Puerto Rico","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Virginia","Virgin Islands","Vermont","Washington","Wisconsin","West Virginia","Wyoming")
correspName <- paste0("USA ", c("Alaska","Alabama","Arkansas","American Samoa","Arizona","California","Colorado","Connecticut","District of Columbia","Delaware","Florida","Georgia","Guam","Hawaii","Iowa","Idaho","Illinois","Indiana","Kansas","Kentucky","Louisiana","Massachusetts","Maryland","Maine","Michigan","Minnesota","Missouri","Mississippi","Montana","North Carolina","North Dakota","Nebraska","New Hampshire","New Jersey","New Mexico","Nevada","New York","Ohio","Oklahoma","Oregon","Pennsylvania","Puerto Rico","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Virginia","Virgin Islands","Vermont","Washington","Wisconsin","West Virginia","Wyoming"))
stateIDs <- as.data.frame(cbind(statesAbbrev,correspFips,correspName))
colnames(stateIDs) <- c("statesAbbrev","statesFips","statesName")

USA_cases$statesAbbrev <- NA
USA_cases$statesFips <- NA
USA_cases$statesName <- NA

for(fip in correspFips){
  keySub <- subset(stateIDs, statesFips == fip)
  caseSub <- subset(USA_cases, fips == fip)
  caseSub <- 
    caseSub %>%
    arrange(date) %>%
    dplyr::mutate(new_cases = c(cases[1], diff(cases))) %>%
    dplyr::mutate(new_deaths = c(deaths[1], diff(deaths)))
  caseSub$statesAbbrev <- keySub$statesAbbrev[1]
  caseSub$statesFips <- keySub$statesFips[1]
  caseSub$statesName <- keySub$statesName[1]
  # print(str(caseSub))
  if(fip == correspFips[1]){
    USA_cases_post <- caseSub
  }else{
    USA_cases_post <- rbind(USA_cases_post,caseSub)
  }
}

finalUSAdf <- USA_cases_post[,c("date","statesName","statesAbbrev","new_cases","new_deaths")]
colnames(finalUSAdf) <- c( "date","Country","ISO3","confirmed","death")

finalUSAdf$recovered <- NA
finalUSAdf$confirmed_cum_per_million <- NA
finalUSAdf$death_cum_per_million <- NA
finalUSAdf$confirmed_cum <- USA_cases_post$cases
finalUSAdf$death_cum <- USA_cases_post$deaths
finalUSAdf$confirmed_cum_per_million_lag_01 <- NA
finalUSAdf$death_cum_per_million_lag_01 <- NA
finalUSAdf$confirmed_cum_per_million_lag_02 <- NA
finalUSAdf$death_cum_per_million_lag_02 <- NA
finalUSAdf$confirmed_cum_per_million_lag_03 <- NA
finalUSAdf$death_cum_per_million_lag_03 <- NA
finalUSAdf$confirmed_cum_per_million_lag_04 <- NA
finalUSAdf$death_cum_per_million_lag_04 <- NA
finalUSAdf$confirmed_cum_per_million_lag_05 <- NA
finalUSAdf$death_cum_per_million_lag_05 <- NA
finalUSAdf$confirmed_cum_per_million_lag_06 <- NA
finalUSAdf$death_cum_per_million_lag_06 <- NA
finalUSAdf$confirmed_cum_per_million_lag_07 <- NA
finalUSAdf$death_cum_per_million_lag_07 <- NA
finalUSAdf$confirmed_cum_per_million_lag_08 <- NA
finalUSAdf$death_cum_per_million_lag_08 <- NA
finalUSAdf$confirmed_cum_per_million_lag_09 <- NA
finalUSAdf$death_cum_per_million_lag_09 <- NA
finalUSAdf$confirmed_cum_per_million_lag_10 <- NA
finalUSAdf$death_cum_per_million_lag_10 <- NA
finalUSAdf$confirmed_cum_per_million_lag_11 <- NA
finalUSAdf$death_cum_per_million_lag_11 <- NA
finalUSAdf$confirmed_cum_per_million_lag_12 <- NA
finalUSAdf$death_cum_per_million_lag_12 <- NA
finalUSAdf$confirmed_cum_per_million_lag_13 <- NA
finalUSAdf$death_cum_per_million_lag_13 <- NA
finalUSAdf$confirmed_cum_per_million_lag_14 <- NA
finalUSAdf$death_cum_per_million_lag_14 <- NA

output_df <- rbind(output_df, finalUSAdf)

correspName[which(correspName %ni% output_df$Country)]

write.csv(output_df, file="InputData/data_COVID_for_pipeline.csv")
unique(output_df$Country)

print(max(coronavirus$date))
print(max(USA_cases$date))
print(max(finalUSAdf$date))

