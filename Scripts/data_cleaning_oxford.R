# Data Cleaning
#
# Written by: Chris H Arehart and Jay H Arehart
# Last Updated: June 15, 2020
#
#
# Script Description:
#   Clean the data and add all to be predictor variables
#
# Output
#   .csv file of features implementation in ML workflow

# Import packages
library(dplyr)
library(tidyr)
library(lubridate)
library(tidyverse)

downloadFlag = T

pre_autofill_Google = T
post_autofill_Google = T

# Load in raw data

raw_data_COVID <- read.csv('./InputData/data_COVID_2020_04_02.csv')

raw_data_static <- read.csv('./InputData/data_static_vars.csv')

## ---- COVID-related data cleaning and processing ----

# Manipulate original datasets (e.g., normalizing)
data_COVID <- raw_data_COVID %>%
  dplyr::select(-X) %>%
  mutate(date = ymd(date))
  # mutate(date = as.Date(as.character(date), format = "%m/%d/%Y"))


## ---- Static dataset cleaning and processing ----
data_static <- raw_data_static %>%
  # normalizing the male count by age group
  mutate(age_0_14_MalePercent = age_0_14_MaleCount / (age_0_14_MaleCount +age_15_24_MaleCount +
                                                        age_25_54_MaleCount + age_55_64_MaleCount + 
                                                        age_65_plus_MaleCount)) %>%
  mutate(age_15_24_MalePercent = age_15_24_MaleCount / (age_0_14_MaleCount +age_15_24_MaleCount +
                                                        age_25_54_MaleCount + age_55_64_MaleCount + 
                                                        age_65_plus_MaleCount)) %>%
  mutate(age_25_54_MalePercent = age_25_54_MaleCount / (age_0_14_MaleCount +age_15_24_MaleCount +
                                                         age_25_54_MaleCount + age_55_64_MaleCount + 
                                                         age_65_plus_MaleCount)) %>%
  mutate(age_55_64_MalePercent = age_55_64_MaleCount / (age_0_14_MaleCount +age_15_24_MaleCount +
                                                         age_25_54_MaleCount + age_55_64_MaleCount + 
                                                         age_65_plus_MaleCount)) %>%
  mutate(age_65_plus_MalePercent = age_65_plus_MaleCount / (age_0_14_MaleCount +age_15_24_MaleCount +
                                                         age_25_54_MaleCount + age_55_64_MaleCount + 
                                                         age_65_plus_MaleCount)) %>%
  # normalizing the female count by age group
  mutate(age_0_14_FemalePercent = age_0_14_FemaleCount / (age_0_14_FemaleCount +age_15_24_FemaleCount +
                                                            age_25_54_FemaleCount + age_55_64_FemaleCount + 
                                                            age_65_plus_FemaleCount)) %>%
  mutate(age_15_24_FemalePercent = age_15_24_FemaleCount / (age_0_14_FemaleCount +age_15_24_FemaleCount +
                                                              age_25_54_FemaleCount + age_55_64_FemaleCount + 
                                                              age_65_plus_FemaleCount)) %>%
  mutate(age_25_54_FemalePercent = age_25_54_FemaleCount / (age_0_14_FemaleCount +age_15_24_FemaleCount +
                                                              age_25_54_FemaleCount + age_55_64_FemaleCount + 
                                                              age_65_plus_FemaleCount)) %>%
  mutate(age_55_64_FemalePercent = age_55_64_FemaleCount / (age_0_14_FemaleCount +age_15_24_FemaleCount +
                                                              age_25_54_FemaleCount + age_55_64_FemaleCount + 
                                                              age_65_plus_FemaleCount)) %>%
  mutate(age_65_plus_FemalePercent = age_65_plus_FemaleCount / (age_0_14_FemaleCount +age_15_24_FemaleCount +
                                                                  age_25_54_FemaleCount + age_55_64_FemaleCount + 
                                                                  age_65_plus_FemaleCount)) %>%
  # drop count variables
  dplyr::select(-c(age_0_14_MaleCount, age_15_24_MaleCount, age_25_54_MaleCount, age_55_64_MaleCount, age_65_plus_MaleCount,
            age_0_14_FemaleCount, age_15_24_FemaleCount, age_25_54_FemaleCount, age_55_64_FemaleCount, age_65_plus_FemaleCount))


## ---- NPI data processing ----
# library(googlesheets4)
# # Load in data from google sheets
# ugly_url = 'https://docs.google.com/spreadsheets/d/1vrKvs52OAxuB7x2kT9r1q6IcIBxGEQsNRHsK_o7h3jo/edit#gid=284493712'
# meta_data <- sheets_get(ugly_url)
# npi_countries <- as.vector(unlist(meta_data$sheets[,1]))
# npi_countries <- npi_countries[c(-1,-2,-3)]

# function to create lag factors for a country's time series
# create_lag <- function(country_ts, num=10, incidence=T){
#   # creating lag variables for number of reported cases, deaths, and recovered
#   lags <- seq(num)   # set the number of lag factors here
#   lag_names <- paste("lag", formatC(lags, width = nchar(max(lags)), flag = "0"), 
#                      sep = "_")
#   lag_functions <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)
#   return(country_df)
# }

# Extract data from google sheets into a dataframe
# data_npi <- data.frame()
# for(i in 1:length(npi_countries)){
#   npi_df_i <- read_sheet(ugly_url, sheet = npi_countries[i]) %>%
#     mutate(date = ymd(date)) %>%
#     # add lag factors
#     mutate(Social_Distancing_Lag_03 = lag(Social_Distancing, n=3)) %>%
#     mutate(Social_Distancing_Lag_07 = lag(Social_Distancing, n=7)) %>%
#     mutate(Social_Distancing_Lag_10 = lag(Social_Distancing, n=10)) %>%
#     mutate(Social_Distancing_Lag_14 = lag(Social_Distancing, n=14)) %>%
#     mutate(Quaranting_Cases_Lag_03 = lag(Quaranting_Cases, n=3)) %>%
#     mutate(Quaranting_Cases_Lag_07 = lag(Quaranting_Cases, n=7)) %>%
#     mutate(Quaranting_Cases_Lag_10 = lag(Quaranting_Cases, n=10)) %>%
#     mutate(Quaranting_Cases_Lag_14 = lag(Quaranting_Cases, n=14)) %>%
#     mutate(Close_Border_Lag_03 = lag(Close_Border, n=3)) %>%
#     mutate(Close_Border_Lag_07 = lag(Close_Border, n=7)) %>%
#     mutate(Close_Border_Lag_10 = lag(Close_Border, n=10)) %>%
#     mutate(Close_Border_Lag_14 = lag(Close_Border, n=14))
#   data_npi <- bind_rows(data_npi,npi_df_i)
#   # Don't overload API, so chill for second.
#   Sys.sleep(1)
# }
# glimpse(data_npi)
# summary(data_npi)
# print("These countries have data collected for non-pharmaceutical interventions: ")
# unique(data_npi$ISO3)


#---Merging data sources and writing to a csv file---#########################################################################################################################################################################

# Merge together time series data (COVID and NPI)
# data_ts <- left_join(data_COVID, data_static, by = c('ISO3'))

data_features <- left_join(data_COVID, data_static, by = c('ISO3'))

# Check merged dataframe
dim(data_features)
sapply(data_features, function(x) sum(is.na(x)))
max(sapply(data_features, function(x) sum(is.na(x))))
summary(data_features)

#---Old way to Add Google Movement Data---#########################################################################################################################################################################
# GoogData <- read_excel("./InputData/Google_final_29_Mar.xlsx", sheet = "Google_final_29_Mar", col_names = T)
# for(i in 1:nrow(GoogData)){
#   if(is.na(GoogData$Country[i])){
#     GoogData$Country[i] <- GoogData$Country[i-1]
#   }
# }
# GoogData <- GoogData[!is.na(GoogData$Location),]
# GoogDataSpread <- GoogData %>% spread(key="Location", value="Percent_Change")
# GoogDataSpreadSeparate <- GoogDataSpread %>% separate(Country,into=c("Country","Fluff1"),convert=TRUE,sep=" March")
# GoogDataSpreadSeparate2 <- GoogDataSpreadSeparate %>% separate(Country,into=c("Country","Fluff2"),convert=TRUE,sep=" -")
# GoogDataSpreadSeparate2$Country <- as.character(GoogDataSpreadSeparate2$Country)
# data_features$Country.x <- as.character(data_features$Country.x)
# # fix any mismatching country names
# unique(data_featuresCountries[which(data_features$Country.x %ni% GoogDataSpreadSeparate2$Country)])
# GoogDataSpreadSeparate2$Country[GoogDataSpreadSeparate2$Country=="South Korea"] <- "Korea, South"
# GoogDataSpreadSeparate2$Country[GoogDataSpreadSeparate2$Country=="United States"] <- "US"
# unique(data_featuresCountries[which(data_features$Country.x %ni% GoogDataSpreadSeparate2$Country)])
# GoogDataSpreadSeparate2$Start_Date[GoogDataSpreadSeparate2$Start_Date=="Sun Feb 16"] <- "2020-02-16"
# GoogDataSpreadSeparate2 <- GoogDataSpreadSeparate2 %>%
#   mutate(Start_Date = ymd(Start_Date))
# GoogDataSpreadSeparate2$End_Date[GoogDataSpreadSeparate2$End_Date=="Sun Mar 29"] <- "2020-03-29"
# # GoogDataSpreadSeparate2$End_Date[GoogDataSpreadSeparate2$End_Date=="Sun Mar 29"] <- as.character(max(data_features$date))
# GoogDataSpreadSeparate2 <- GoogDataSpreadSeparate2 %>%
#   mutate(End_Date = ymd(End_Date))
# # initialize data_features with new variables
# for(i in 1:nrow(GoogDataSpreadSeparate2)){
#   toReplace <- which(data_features$Country.x==GoogDataSpreadSeparate2$Country[i])
#   if(length(toReplace)>0){
#     # create linear progression between two time Start_Date and End_Date
#     Grocery_pharmacy <- seq(from=1,to=1+as.numeric(GoogDataSpreadSeparate2$`Grocery & pharmacy`[i]),length.out=as.numeric(GoogDataSpreadSeparate2$End_Date[i] - GoogDataSpreadSeparate2$Start_Date[i])+1)
#     Parks <- seq(from=1,to=1+as.numeric(GoogDataSpreadSeparate2$Parks[i]),length.out=as.numeric(GoogDataSpreadSeparate2$End_Date[i] - GoogDataSpreadSeparate2$Start_Date[i])+1)
#     Residential <- seq(from=1,to=1+as.numeric(GoogDataSpreadSeparate2$Residential[i]),length.out=as.numeric(GoogDataSpreadSeparate2$End_Date[i] - GoogDataSpreadSeparate2$Start_Date[i])+1)
#     Retail_recreation <- seq(from=1,to=1+as.numeric(GoogDataSpreadSeparate2$`Retail & recreation`[i]),length.out=as.numeric(GoogDataSpreadSeparate2$End_Date[i] - GoogDataSpreadSeparate2$Start_Date[i])+1)
#     Transit_stations <- seq(from=1,to=1+as.numeric(GoogDataSpreadSeparate2$`Transit stations`[i]),length.out=as.numeric(GoogDataSpreadSeparate2$End_Date[i] - GoogDataSpreadSeparate2$Start_Date[i])+1)
#     Workplaces <- seq(from=1,to=1+as.numeric(GoogDataSpreadSeparate2$Workplaces[i]),length.out=as.numeric(GoogDataSpreadSeparate2$End_Date[i] - GoogDataSpreadSeparate2$Start_Date[i])+1)
#     # fill in the right parts of data_features with the new linear data
#     beginDate <- which(data_features[toReplace,c("date")] == GoogDataSpreadSeparate2$Start_Date[i])
#     stopDate <- which(data_features[toReplace,c("date")] == GoogDataSpreadSeparate2$End_Date[i])
#     data_features[toReplace,c("Google_Grocery_pharmacy")][beginDate:stopDate] <- Grocery_pharmacy
#     data_features[toReplace,c("Google_Parks")][beginDate:stopDate] <- Parks
#     data_features[toReplace,c("Google_Residential")][beginDate:stopDate] <- Residential
#     data_features[toReplace,c("Google_Retail_recreation")][beginDate:stopDate] <- Retail_recreation
#     data_features[toReplace,c("Google_Transit_stations")][beginDate:stopDate] <- Transit_stations
#     data_features[toReplace,c("Google_Workplaces")][beginDate:stopDate] <- Workplaces
#     # if pre_autofill_Google signifies, we fill everything pre google data with 1s
#     if(pre_autofill_Google == T){
#       data_features[toReplace,c("Google_Grocery_pharmacy")][1:(beginDate-1)] <- 1
#       data_features[toReplace,c("Google_Parks")][1:(beginDate-1)] <- 1
#       data_features[toReplace,c("Google_Residential")][1:(beginDate-1)] <- 1
#       data_features[toReplace,c("Google_Retail_recreation")][1:(beginDate-1)] <- 1
#       data_features[toReplace,c("Google_Transit_stations")][1:(beginDate-1)] <- 1
#       data_features[toReplace,c("Google_Workplaces")][1:(beginDate-1)] <- 1
#     }
#     # if post_autofill_Google signifies, we fill everything post google data with the last datapoint
#     if(post_autofill_Google == T & (stopDate+1) <= length(toReplace) ){
#       data_features[toReplace,c("Google_Grocery_pharmacy")][(stopDate+1):length(toReplace)] <- 1+as.numeric(GoogDataSpreadSeparate2$`Grocery & pharmacy`[i])
#       data_features[toReplace,c("Google_Parks")][(stopDate+1):length(toReplace)] <- 1+as.numeric(GoogDataSpreadSeparate2$Parks[i])
#       data_features[toReplace,c("Google_Residential")][(stopDate+1):length(toReplace)] <- 1+as.numeric(GoogDataSpreadSeparate2$Residential[i])
#       data_features[toReplace,c("Google_Retail_recreation")][(stopDate+1):length(toReplace)] <- 1+as.numeric(GoogDataSpreadSeparate2$`Retail & recreation`[i])
#       data_features[toReplace,c("Google_Transit_stations")][(stopDate+1):length(toReplace)] <- 1+as.numeric(GoogDataSpreadSeparate2$`Transit stations`[i])
#       data_features[toReplace,c("Google_Workplaces")][(stopDate+1):length(toReplace)] <- 1+as.numeric(GoogDataSpreadSeparate2$Workplaces[i])
#     }
#   }
# }

#---New way to Add Google Movement Data---#########################################################################################################################################################################
'%ni%' <- Negate('%in%')
if(downloadFlag == T){
  download.file("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv","./InputData/Global_Mobility_Report.csv")
}
GoogData <- read.csv("./InputData/Global_Mobility_Report.csv", header=T)
# Get just the overall country mobility stats
GoogData_sub <- subset(GoogData, sub_region_1 == "" & sub_region_2 == "")
GoogData_sub$country_region <- as.character(GoogData_sub$country_region)
unique(data_features$Country[which(data_features$Country %ni% GoogData_sub$country_region)])
GoogData_sub$country_region[GoogData_sub$country_region=="United States"] <- "US"
GoogData_sub$country_region[GoogData_sub$country_region=="South Korea"] <- "Korea, South"
GoogData_sub$country_region[GoogData_sub$country_region=="CÃ´te d'Ivoire"] <- "Cote d'Ivoire"
unique(data_features$Country[which(data_features$Country %ni% GoogData_sub$country_region)])

names(GoogData_sub)
GoogData_sub2 <- GoogData_sub[,c("country_region","date","retail_and_recreation_percent_change_from_baseline",
                                 "grocery_and_pharmacy_percent_change_from_baseline", 
                                 "parks_percent_change_from_baseline", 
                                 "transit_stations_percent_change_from_baseline",
                                 "workplaces_percent_change_from_baseline",
                                 "residential_percent_change_from_baseline")]
colnames(GoogData_sub2) <- c("country_region","date","Google_Retail_recreation",
                             "Google_Grocery_pharmacy",
                             "Google_Parks",
                             "Google_Transit_stations",
                             "Google_Workplaces",
                             "Google_Residential")

countryList <- unique(GoogData_sub2$country_region)
for(i in 1:length(countryList)){
  tsub <- subset(GoogData_sub2, country_region == countryList[i])
  for(j in c(2,5,8,11,14)){

    tsub[[paste0("Google_Retail_recreation","_lag_",j)]] <- lag(tsub$Google_Retail_recreation,j)
    tsub[[paste0("Google_Retail_recreation", "_lag_",j)]][1:j] <-
      as.integer(runif(j,
          min = - 3,
          max = + 3
        ))
    tsub[[paste0("Google_Grocery_pharmacy","_lag_",j)]] <- lag(tsub$Google_Grocery_pharmacy,j)
    tsub[[paste0("Google_Grocery_pharmacy", "_lag_",j)]][1:j] <-
      as.integer(runif(j,
                       min = - 3,
                       max = + 3
      ))
    tsub[[paste0("Google_Parks","_lag_",j)]] <- lag(tsub$Google_Parks,j)
    tsub[[paste0("Google_Parks", "_lag_",j)]][1:j] <-
      as.integer(runif(j,
                       min = - 3,
                       max = + 3
      ))
    tsub[[paste0("Google_Transit_stations","_lag_",j)]] <- lag(tsub$Google_Transit_stations,j)
    tsub[[paste0("Google_Transit_stations", "_lag_",j)]][1:j] <-
      as.integer(runif(j,
                       min = - 3,
                       max = + 3
      ))
    tsub[[paste0("Google_Workplaces","_lag_",j)]] <- lag(tsub$Google_Workplaces,j)
    tsub[[paste0("Google_Workplaces", "_lag_",j)]][1:j] <-
      as.integer(runif(j,
                       min = - 3,
                       max = + 3
      ))
    tsub[[paste0("Google_Residential","_lag_",j)]] <- lag(tsub$Google_Residential,j)
    tsub[[paste0("Google_Residential", "_lag_",j)]][1:j] <-
      as.integer(runif(j,
                       min = - 3,
                       max = + 3
      ))
  }
  if(i == 1){
    GoogData_sub3 <- tsub
  }else{
    GoogData_sub3 <- rbind(GoogData_sub3,tsub)
  }
}

# Merge the two dataframes by country name and date
GoogData_sub3$date <- as.character(GoogData_sub3$date)
data_features$date <- as.character(data_features$date)
GoogData_sub3$country_region <- as.character(GoogData_sub3$country_region)
data_features$Country <- as.character(data_features$Country)
GoogData_sub3 <- as_tibble(GoogData_sub3)
data_features <- as_tibble(data_features)
data_features3 <- inner_join(data_features, GoogData_sub3, by = c("date" = "date", "Country" = "country_region"))
# Check merged dataframe
dim(data_features3)
sapply(data_features3, function(x) sum(is.na(x)))
max(sapply(data_features3, function(x) sum(is.na(x))))
summary(data_features3)

# Add variable for day of the week
# View(data_features3[c("Country.x","date","Google_Grocery_pharmacy","Google_Parks","Google_Residential","Google_Retail_recreation","Google_Transit_stations","Google_Workplaces")])
data_features3$weekdays <- weekdays(as.Date(data_features3$date))
# data_features3$weekdays[data_features3$weekdays %in% c("Saturday","Sunday")] <- 0
# data_features3$weekdays[data_features3$weekdays %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")] <- 1
# Make the weekday a measure of the shortest distance from wednesday.
data_features3$weekdays[data_features3$weekdays %in% c("Sunday")] <- 3
data_features3$weekdays[data_features3$weekdays %in% c("Monday")] <- 2
data_features3$weekdays[data_features3$weekdays %in% c("Tuesday")] <- 1
data_features3$weekdays[data_features3$weekdays %in% c("Wednesday")] <- 0
data_features3$weekdays[data_features3$weekdays %in% c("Thursday")] <- 1
data_features3$weekdays[data_features3$weekdays %in% c("Friday")] <- 2
data_features3$weekdays[data_features3$weekdays %in% c("Saturday")] <- 3
data_features3$weekdays <- as.numeric(data_features3$weekdays)

# incorporate oxford NPI data
# https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/codebook.md
if(downloadFlag == T){
  download.file("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv","./InputData/OxCGRT_latest.csv")
}
OxData <- read.csv("./InputData/OxCGRT_latest.csv", header=T)
OxData$Date <- str_replace(OxData$Date,"(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3")
OxData$Date <- as.Date(OxData$Date)
OxData$CountryCode <- as.character(OxData$CountryCode)
unique(data_features3$ISO3[which(data_features3$ISO3 %ni% OxData$CountryCode)])
OxData$CountryCode[OxData$CountryCode=="CHN"] <- "HUB"
OxData$CountryCode[OxData$CountryCode=="NLD"] <- "ANT"
unique(data_features3$ISO3[which(data_features3$ISO3 %ni% OxData$CountryCode)])
OxData$CountryName <- NULL
OxData$Date <- as.Date(OxData$Date)
data_features3$date <- as.Date(data_features3$date)

# take the liberty of filling in no data
# Binary flag for geographic scope
OxData$C1_Flag[is.na(OxData$C1_Flag)] <- -999
OxData$C2_Flag[is.na(OxData$C2_Flag)] <- -999
OxData$C3_Flag[is.na(OxData$C3_Flag)] <- -999
OxData$C4_Flag[is.na(OxData$C4_Flag)] <- -999
OxData$C5_Flag[is.na(OxData$C5_Flag)] <- -999
OxData$C6_Flag[is.na(OxData$C6_Flag)] <- -999
OxData$C7_Flag[is.na(OxData$C7_Flag)] <- -999
OxData$E1_Flag[is.na(OxData$E1_Flag)] <- -999
OxData$H1_Flag[is.na(OxData$H1_Flag)] <- -999
OxData$M1_Wildcard <- NULL
OxData$ConfirmedCases <- NULL
OxData$ConfirmedDeaths <- NULL

# View(OxData[,c("Date","CountryCode","ContainmentHealthIndexForDisplay","ContainmentHealthIndex")])
OxData$ContainmentHealthIndex <- NULL
# View(OxData[,c("Date","CountryCode","StringencyIndexForDisplay","StringencyIndex","StringencyLegacyIndexForDisplay","StringencyLegacyIndex")])
OxData$StringencyIndex <- NULL
OxData$StringencyLegacyIndexForDisplay <- NULL
OxData$StringencyLegacyIndex <- NULL
# View(OxData[,c("Date","CountryCode","GovernmentResponseIndexForDisplay","GovernmentResponseIndex")])
OxData$GovernmentResponseIndex <- NULL
# View(OxData[,c("Date","CountryCode","EconomicSupportIndexForDisplay","EconomicSupportIndex")])
OxData$EconomicSupportIndex <- NULL

names(OxData)

countryList <- unique(OxData$CountryCode)
for(i in 1:length(countryList)){
  tsub <- subset(OxData, CountryCode == countryList[i])
  for(j in c(2,5,8,11,14)){
    # tsub[[paste0("C1_School.closing","_lag_",j)]] <- lag(tsub$C1_School.closing,j)
    # tsub[[paste0("C2_Workplace.closing","_lag_",j)]] <- lag(tsub$C2_Workplace.closing,j)
    # tsub[[paste0("C3_Cancel.public.events","_lag_",j)]] <- lag(tsub$C3_Cancel.public.events,j)
    # tsub[[paste0("C4_Restrictions.on.gatherings","_lag_",j)]] <- lag(tsub$C4_Restrictions.on.gatherings,j)
    # tsub[[paste0("C5_Close.public.transport","_lag_",j)]] <- lag(tsub$C5_Close.public.transport,j)
    # tsub[[paste0("C6_Stay.at.home.requirements","_lag_",j)]] <- lag(tsub$C6_Stay.at.home.requirements,j)
    # tsub[[paste0("C7_Restrictions.on.internal.movement","_lag_",j)]] <- lag(tsub$C7_Restrictions.on.internal.movement,j)
    # tsub[[paste0("C8_International.travel.controls","_lag_",j)]] <- lag(tsub$C8_International.travel.controls,j)
    # tsub[[paste0("E1_Income.support","_lag_",j)]] <- lag(tsub$E1_Income.support,j)
    # tsub[[paste0("E2_Debt.contract.relief","_lag_",j)]] <- lag(tsub$E2_Debt.contract.relief,j)
    # tsub[[paste0("E3_Fiscal.measures","_lag_",j)]] <- lag(tsub$E3_Fiscal.measures,j)
    # tsub[[paste0("E4_International.support","_lag_",j)]] <- lag(tsub$E4_International.support,j)
    # tsub[[paste0("H1_Public.information.campaigns","_lag_",j)]] <- lag(tsub$H1_Public.information.campaigns,j)
    # tsub[[paste0("H2_Testing.policy","_lag_",j)]] <- lag(tsub$H2_Testing.policy,j)
    # tsub[[paste0("H3_Contact.tracing","_lag_",j)]] <- lag(tsub$H3_Contact.tracing,j)
    # tsub[[paste0("H4_Emergency.investment.in.healthcare","_lag_",j)]] <- lag(tsub$H4_Emergency.investment.in.healthcare,j)
    # tsub[[paste0("H5_Investment.in.vaccines","_lag_",j)]] <- lag(tsub$H5_Investment.in.vaccines,j)
    tsub[[paste0("StringencyIndexForDisplay","_lag_",j)]] <- lag(tsub$StringencyIndexForDisplay,j)
    tsub[[paste0("StringencyIndexForDisplay", "_lag_",j)]][1:j] <-
      mean(tsub[[paste0("StringencyIndexForDisplay")]][1:3])
    tsub[[paste0("GovernmentResponseIndexForDisplay","_lag_",j)]] <- lag(tsub$GovernmentResponseIndexForDisplay,j)
    tsub[[paste0("GovernmentResponseIndexForDisplay", "_lag_",j)]][1:j] <-
      mean(tsub[[paste0("GovernmentResponseIndexForDisplay")]][1:3])
    tsub[[paste0("ContainmentHealthIndexForDisplay","_lag_",j)]] <- lag(tsub$ContainmentHealthIndexForDisplay,j)
    tsub[[paste0("ContainmentHealthIndexForDisplay", "_lag_",j)]][1:j] <-
      mean(tsub[[paste0("ContainmentHealthIndexForDisplay")]][1:3])
    tsub[[paste0("EconomicSupportIndexForDisplay","_lag_",j)]] <- lag(tsub$EconomicSupportIndexForDisplay,j)
    tsub[[paste0("EconomicSupportIndexForDisplay", "_lag_",j)]][1:j] <-
      mean(tsub[[paste0("EconomicSupportIndexForDisplay")]][1:3])
    }
  if(i == 1){
    OxData_1 <- tsub
  }else{
    OxData_1 <- rbind(OxData_1,tsub)
  }
}

OxData_1 <- as_tibble(OxData_1)
data_features3 <- as_tibble(data_features3)
data_features4 <- inner_join(data_features3, OxData_1, by = c("date" = "Date", "ISO3" = "CountryCode"))

# Check merged dataframe
data_features4$Percent_house_Skip_generation <- NULL
data_features4$Percent_house_Multi_generation <- NULL
data_features4$Percent_house_Three_generation <- NULL
data_features4$Percent_house_Nuclear <- NULL


# data_features4$PopulationSmoking_male <- NULL
# data_features4$PopulationSmoking_female <- NULL

dim(data_features4)
sapply(data_features4, function(x) sum(is.na(x)))
max(sapply(data_features4, function(x) sum(is.na(x))))
summary(data_features4)

nrow(data_features4)
colSums(is.na(data_features4))

# MARGIN=1, it applies over rows, whereas with MARGIN=2, it works over columns
nrow(data_features4)
sum(apply(data_features4, 1, anyNA))
ncol(data_features4)
sum(apply(data_features4, 2, anyNA))


# add lags
cat(names(data_features4),sep="\n")

# remove the country that fails Rt estimation
# data_features4 <- subset(data_features4, ISO3 %ni% c("HRV","SVN"))



# Filling in spotty data
# https://web.stanford.edu/group/fearon-research/cgi-bin/wordpress/wp-content/uploads/2013/10/Ethnic-and-Cultural-Diversity-by-Country.pdf
data_features4[which(data_features4$Country == "India"),c("EFindex")] <- 0.811

# https://tobaccoatlas.org/country/peru/
data_features4[which(data_features4$Country == "Peru"),c("PopulationSmoking_male")] <- 11.9

# https://web.stanford.edu/group/fearon-research/cgi-bin/wordpress/wp-content/uploads/2013/10/Ethnic-and-Cultural-Diversity-by-Country.pdf
data_features4[which(data_features4$Country == "France"),c("EFindex")] <- 0.272

# https://www.un.org/en/development/desa/population/publications/pdf/ageing/household_size_and_composition_around_the_world_2017_data_booklet.pdf
data_features4[which(data_features4$Country == "Saudi Arabia"),c("Ave_household_size")] <- 5.6

# https://www.un.org/en/development/desa/population/publications/pdf/ageing/household_size_and_composition_around_the_world_2017_data_booklet.pdf
data_features4[which(data_features4$Country == "Qatar"),c("Ave_household_size")] <- 5.3
# https://data.worldbank.org/indicator/SH.PRV.SMOK.MA # https://data.worldbank.org/indicator/SH.PRV.SMOK.FE
data_features4[which(data_features4$Country == "Qatar"),c("PopulationSmoking_male")] <- 27
data_features4[which(data_features4$Country == "Qatar"),c("PopulationSmoking_female")] <- 1
# http://hdr.undp.org/en/content/income-gini-coefficient
data_features4[which(data_features4$Country == "Qatar"),c("GINIindex")] <- 41.1

# https://www.un.org/en/development/desa/population/publications/pdf/ageing/household_size_and_composition_around_the_world_2017_data_booklet.pdf
data_features4[which(data_features4$Country == "Sweden"),c("Ave_household_size")] <- 2.2

# https://data.worldbank.org/indicator/SH.PRV.SMOK.MA # https://data.worldbank.org/indicator/SH.PRV.SMOK.FE
data_features4[which(data_features4$Country == "Netherlands"),c("PopulationSmoking_male")] <- 27
data_features4[which(data_features4$Country == "Netherlands"),c("PopulationSmoking_female")] <- 24

# https://data.worldbank.org/indicator/SH.PRV.SMOK.MA # https://data.worldbank.org/indicator/SH.PRV.SMOK.FE
data_features4[which(data_features4$Country == "United Arab Emirates"),c("PopulationSmoking_male")] <- 37
data_features4[which(data_features4$Country == "United Arab Emirates"),c("PopulationSmoking_female")] <- 1
# https://data.worldbank.org/indicator/SI.POV.GINI?locations=AE-IN
data_features4[which(data_features4$Country == "United Arab Emirates"),c("GINIindex")] <- 32.5
# https://hub.arcgis.com/datasets/bb9bf7c53c274d19b369901a3cbde406
data_features4[which(data_features4$Country == "United Arab Emirates"),c("Ave_household_size")] <- 6.3

# https://data.worldbank.org/indicator/SH.PRV.SMOK.MA # https://data.worldbank.org/indicator/SH.PRV.SMOK.FE
data_features4[which(data_features4$Country == "Kuwait"),c("PopulationSmoking_male")] <- 37
data_features4[which(data_features4$Country == "Kuwait"),c("PopulationSmoking_female")] <- 3
# https://data.worldbank.org/indicator/SI.POV.GINI?locations=AE-IN
data_features4[which(data_features4$Country == "Kuwait"),c("GINIindex")] <- NA
# https://www.un.org/en/development/desa/population/publications/pdf/ageing/household_size_and_composition_around_the_world_2017_data_booklet.pdf
data_features4[which(data_features4$Country == "Kuwait"),c("Ave_household_size")] <- 5.8

# https://tobaccoatlas.org/country/afghanistan/
data_features4[which(data_features4$Country == "Afghanistan"),c("PopulationSmoking_male")] <- 21.4
data_features4[which(data_features4$Country == "Afghanistan"),c("PopulationSmoking_female")] <- 1.8
# http://hdr.undp.org/en/content/income-gini-coefficient
data_features4[which(data_features4$Country == "Afghanistan"),c("GINIindex")] <- 27.8

# http://hdr.undp.org/en/content/income-gini-coefficient
data_features4[which(data_features4$Country == "Oman"),c("GINIindex")] <- NA

# https://www.un.org/en/development/desa/population/publications/pdf/ageing/household_size_and_composition_around_the_world_2017_data_booklet.pdf
data_features4[which(data_features4$Country == "Dominican Republic"),c("Ave_household_size")] <- 3.5
# https://web.stanford.edu/group/fearon-research/cgi-bin/wordpress/wp-content/uploads/2013/10/Ethnic-and-Cultural-Diversity-by-Country.pdf
data_features4[which(data_features4$Country == "Dominican Republic"),c("EFindex")] <- 0.387

# https://tobaccoatlas.org/country/afghanistan/
data_features4[which(data_features4$Country == "Iraq"),c("PopulationSmoking_male")] <- 23.8
data_features4[which(data_features4$Country == "Iraq"),c("PopulationSmoking_female")] <- 3
# http://hdr.undp.org/en/content/income-gini-coefficient
data_features4[which(data_features4$Country == "Iraq"),c("GINIindex")] <- 30.9

# https://www.un.org/en/development/desa/population/publications/pdf/ageing/household_size_and_composition_around_the_world_2017_data_booklet.pdf
data_features4[which(data_features4$Country == "Bahrain"),c("Ave_household_size")] <- 5.9
# http://hdr.undp.org/en/content/income-gini-coefficient
data_features4[which(data_features4$Country == "Bahrain"),c("GINIindex")] <- NA

# https://www.un.org/en/development/desa/population/publications/pdf/ageing/household_size_and_composition_around_the_world_2017_data_booklet.pdf
data_features4[which(data_features4$Country == "Denmark"),c("Ave_household_size")] <- 2.1

# Google
data_features4[which(data_features4$Country == "Serbia"),c("Latitude")] <- 44.0165
data_features4[which(data_features4$Country == "Serbia"),c("Longitude")] <- 21.059

# https://www.un.org/en/development/desa/population/publications/pdf/ageing/household_size_and_composition_around_the_world_2017_data_booklet.pdf
data_features4[which(data_features4$Country == "Moldova"),c("Ave_household_size")] <- 2.8

# https://tobaccoatlas.org/country/guatemala/
data_features4[which(data_features4$Country == "Guatemala"),c("PopulationSmoking_male")] <- 13.4
data_features4[which(data_features4$Country == "Guatemala"),c("PopulationSmoking_female")] <- 2.5

# https://web.stanford.edu/group/fearon-research/cgi-bin/wordpress/wp-content/uploads/2013/10/Ethnic-and-Cultural-Diversity-by-Country.pdf
data_features4[which(data_features4$Country == "Cameroon"),c("EFindex")] <- 0.887

# Google
data_features4[which(data_features4$Country == "Morocco"),c("Latitude")] <- 31.7917
data_features4[which(data_features4$Country == "Morocco"),c("Longitude")] <- -7.0926

# https://tobaccoatlas.org/country/cote-divoire/
data_features4[which(data_features4$Country == "Cote d'Ivoire"),c("PopulationSmoking_male")] <- 14.2
data_features4[which(data_features4$Country == "Cote d'Ivoire"),c("PopulationSmoking_female")] <- 1.4

# https://tobaccoatlas.org/country/tajikistan/
data_features4[which(data_features4$Country == "Tajikistan"),c("PopulationSmoking_male")] <- 19.6
data_features4[which(data_features4$Country == "Tajikistan"),c("PopulationSmoking_female")] <- 0.4

# https://tobaccoatlas.org/country/gabon/
data_features4[which(data_features4$Country == "Gabon"),c("PopulationSmoking_male")] <- 14.7
data_features4[which(data_features4$Country == "Gabon"),c("PopulationSmoking_female")] <- 2.2

# https://scholar.harvard.edu/files/alesina/files/fractionalization.pdf
data_features4[which(data_features4$Country == "Luxembourg"),c("EFindex")] <- 0.5302

# https://tobaccoatlas.org/country/el-salvador/
data_features4[which(data_features4$Country == "El Salvador"),c("PopulationSmoking_male")] <- 10
data_features4[which(data_features4$Country == "El Salvador"),c("PopulationSmoking_female")] <- 3.3

# https://tobaccoatlas.org/country/Venezuela/
data_features4[which(data_features4$Country == "Venezuela"),c("PopulationSmoking_male")] <- 16.7
data_features4[which(data_features4$Country == "Venezuela"),c("PopulationSmoking_female")] <- 9.9

# https://www.un.org/en/development/desa/population/publications/pdf/ageing/household_size_and_composition_around_the_world_2017_data_booklet.pdf
data_features4[which(data_features4$Country == "Bosnia and Herzegovina"),c("Ave_household_size")] <- 3.1

# https://www.un.org/en/development/desa/population/publications/pdf/ageing/household_size_and_composition_around_the_world_2017_data_booklet.pdf
data_features4[which(data_features4$Country == "Sri Lanka"),c("Ave_household_size")] <- 3.8
data_features4[which(data_features4$Country == "Sri Lanka"),c("Latitude")] <- 7.8731
data_features4[which(data_features4$Country == "Sri Lanka"),c("Longitude")] <- 80.7718

# https://tobaccoatlas.org/country/Nicaragua/
data_features4[which(data_features4$Country == "Nicaragua"),c("PopulationSmoking_male")] <- 12.6
data_features4[which(data_features4$Country == "Nicaragua"),c("PopulationSmoking_female")] <- 5.4

# https://tradingeconomics.com/venezuela/physicians-per-1-000-people-wb-data.html
data_features4[which(data_features4$Country == "Venezuela"),c("PhysicianDensity")] <- 1.9475

# https://tobaccoatlas.org/country/Nicaragua/
data_features4[which(data_features4$Country == "New Zealand"),c("PopulationSmoking_male")] <- 15.6
data_features4[which(data_features4$Country == "New Zealand"),c("PopulationSmoking_female")] <- 12.9

# https://data.worldbank.org/indicator/SI.POV.GINI?locations=LB
data_features4[which(data_features4$Country == "Lebanon"),c("GINIindex")] <- 31.8
# https://www.un.org/en/development/desa/population/publications/pdf/ageing/household_size_and_composition_around_the_world_2017_data_booklet.pdf
data_features4[which(data_features4$Country == "Lebanon"),c("Ave_household_size")] <- 4.3

# https://data.worldbank.org/indicator/EN.POP.DNST
data_features4[which(data_features4$Country == "Korea, South"),c("PopulationDensity")] <- 212

# https://tobaccoatlas.org/country/yemen/
data_features4[which(data_features4$Country == "Yemen"),c("PopulationSmoking_male")] <- 18.8
data_features4[which(data_features4$Country == "Yemen"),c("PopulationSmoking_female")] <- 6.3
data_features4[which(data_features4$Country == "Yemen"),c("Latitude")] <- 15.5527
data_features4[which(data_features4$Country == "Yemen"),c("Longitude")] <- 48.5164

# https://tobaccoatlas.org/country/rwanda/
data_features4[which(data_features4$Country == "Rwanda"),c("PopulationSmoking_male")] <- 12.4

# https://scholar.harvard.edu/files/alesina/files/fractionalization.pdf
data_features4[which(data_features4$Country == "Mozambique"),c("EFindex")] <- 0.6932

# https://tradingeconomics.com/namibia/physicians-per-1-000-people-wb-data.html
data_features4[which(data_features4$Country == "Namibia"),c("PhysicianDensity")] <- 0.4182

# These countries seem to currently have a bug in their google mobility data, and for now need to be removed
data_features4 <- subset(data_features4, Country!= "Afghanistan")
data_features4 <- subset(data_features4, Country!= "Serbia")
data_features4 <- subset(data_features4, Country!= "Georgia")

write.csv(data_features4, "./InputData/ML_features_oxford.csv", row.names = F)








