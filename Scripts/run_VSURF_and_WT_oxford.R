# Run VSURF and plot variable importance

# Written by:  Chris H Arehart
# Written on: March 24th, 2020
# Updated on: June 20th, 2020

library(tidyverse)
library(EpiEstim)
library(R0)
library(zoo)
library(VSURF)
library(caret)
library(doParallel)
library(ggplot2)
library(randomForest)
library(ranger)
library(viridis)
library(RColorBrewer)
library(randomcoloR)
library(reshape2)
# devtools::install_github("delabj/ggCyberPunk")
library(ggCyberPunk)
library(corrplot)
#
library(gridExtra)
library(grid)
library(ggpubr)
library(ggridges)
library(quantregForest)
library(scales)
library(ggrepel)
library(cowplot)
library(deSolve)
library(reshape2)
library(MASS)
library(ggstance)
library(jtools)
'%ni%' <- Negate('%in%')

#---dataSetup---#########################################################################################################################################################################

VSURFflag <- F
RunWT_R_flag <- T
# the number of lag factors you want
nLags <- 14
# Number of cores to use when running rf model and vsurf
num_cores = detectCores()
# What NA acion to use for models
nasaction = na.omit
# Number of trees to train on
number_trees = 1000

#---Flag setup---#########################################################################################################################################################################

# TRUE if you want to scale by population
incidence_flag <- T
# TRUE if you want to do deaths instead of cases
death_flag <- F
incidence_start_point <- 0.3
count_start_point <- 100
# if we are doing deaths, we want incidence start point to be about 5.9% of the case one becuase that's the approx mortality rate
if (death_flag == T) {
  incidence_start_point <- incidence_start_point * (5.9 / 100)
  count_start_point <- count_start_point * (5.9 / 100)
}

# the time you want to forecast the predictiont
# forecastingTime <- 14
forecastingTimeFlag = "fullRange"
# autofill the missing datapoints in NPI data
NPIflag1 <- "autofill"
# use the last NPI datapoints for the forecasting period
NPIflag2 <- "lastNPI"
# NPIflag2 <- "firstNPI"
# The Percent of the timeframe you want to reserve for testing a country (the rest of that country's time series is included into the model)
# for example if testingTimeFrame <- 0.8, then 20% of the timeseries will be used to train, and 80% will be used to predict
testingTimeFrame <- 1

'%ni%' <- Negate('%in%')
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)),
        substring(s, 2),
        sep = "",
        collapse = " ")
}


# cc=1

# Choose the testing country
testing_country <- "USA"

data_clean <- read.csv("./InputData/ML_features_oxford.csv")
data_clean$date <- as.Date(data_clean$date)
data_clean1 <- subset(data_clean, ISO3 == testing_country)
training_countries <- unique(as.character(data_clean$ISO3))
D4 <-
  data_clean1$date[which.max(data_clean1$confirmed_cum_per_million)]
D3 <-
  data_clean1$date[which.min(
    abs(
      data_clean1$confirmed_cum_per_million - data_clean1$confirmed_cum_per_million[which(data_clean1$date == D4)] *
        2 / 3
    )
  )]
D2 <-
  data_clean1$date[which.min(
    abs(
      data_clean1$confirmed_cum_per_million - data_clean1$confirmed_cum_per_million[which(data_clean1$date == D4)] *
        1 / 3
    )
  )]
D1 <-
  data_clean1$date[which.min(
    abs(
      data_clean1$confirmed_cum_per_million - data_clean1$confirmed_cum_per_million[which(data_clean1$date == D4)] *
        1 / 10
    )
  )]
earliestD <- which(data_clean1$confirmed_cum >= 50)[1]
# if(D1 - data_clean1$date[start])
D0 <- data_clean1$date[earliestD] + 18 #"2020-03-21"
print(D1)
print(D2)
print(D3)
print(D4)
dateList <- as.Date(c(D4, D3, D2, D1, D0))
dateList[dateList < D0] <- D0


# if(forecastingTimeFlag == "fullRange"){
#   forecastingTime <- as.numeric(as.Date(D4)+14-as.Date(timeChop))
# }else{
#   forecastingTime = 14
# }
forecastingTime = 30

data_clean <- read.csv("./InputData/ML_features_oxford.csv")
data_clean$date <- as.Date(data_clean$date)
data_clean_train <- data_clean
data_clean <- subset(data_clean, date <= D4)

# Looking at the data
glimpse(data_clean)
summary(data_clean)

#---trainingTestingDataFrames---#########################################################################################################################################################################
# Subset training country list to those countries that had enough cases (to calc R0) by the start point
lengthClist <- length(training_countries)
training_countries_OG <- training_countries
for (i in 1:lengthClist) {
  training_subset <-
    subset(data_clean_train, ISO3 %in% training_countries_OG[i])
  start <-
    which(training_subset$confirmed_cum_per_million >= incidence_start_point)[1]
  if (is.na(start) == F) {
    training_subset_aligned <-
      training_subset[start:nrow(training_subset),]
  }
  if (nrow(training_subset_aligned) < 18 | is.na(start)) {
    training_countries <-
      training_countries[which(training_countries != training_countries_OG[i])]
  } else{
    print(paste0(
      training_countries_OG[i],
      ": ",
      nrow(training_subset_aligned)
    ))
  }
}

# create training dataframe
pdf("R0_plot.pdf", width = 11, height = 8.5)
for (i in 1:length(training_countries)) {
  training_subset <-
    subset(data_clean_train, ISO3 %in% training_countries[i])
  
  print(i)
  print(training_countries[i])
  
  start <- which(training_subset$confirmed_cum >= 50)[1]
  
  training_subset_aligned <-
    training_subset[start:nrow(training_subset),]
  training_subset_aligned$time <-
    c(1:nrow(training_subset_aligned))
  
  training_subset_aligned$confirmed[training_subset_aligned$confirmed < 0] <- NA
  #Get of erroneous negative counts... they sneak throught the API sometimes.
  # But if thre is a negative at teh end... are the last one lets just make it equal to the n-1 one
  if (is.na(tail(training_subset_aligned$confirmed, 1))) {
    training_subset_aligned$confirmed[length(training_subset_aligned$confirmed)] <- training_subset_aligned$confirmed[length(training_subset_aligned$confirmed) - 1]
  }
  # If the NA is not at the end, Lets linearly interpolate them:
  training_subset_aligned$confirmed <- na.approx(training_subset_aligned$confirmed)
  training_subset_aligned$confirmed <- as.integer(training_subset_aligned$confirmed)
  
  # Smooth out incidence using moving average with a centered window of 7 datapoints (3 to the left, 3 to the right)
  # make sure the window is an odd integer
  recoveryTime = 14.5
  window <- 7
  # dim(training_subset_aligned)
  # length(rollmean(training_subset_aligned$confirmed, k=window))
  pre <- c()
  for(pre_i in 1:((window - 1) / 2)){
    pre <- c(pre, mean(training_subset_aligned$confirmed[1:pre_i]))
  }
  post <- c()
  for(post_i in rev(1:((window - 1) / 2))){
    post <- c(post, mean(training_subset_aligned$confirmed[(length(training_subset_aligned$confirmed) - post_i):(length(training_subset_aligned$confirmed))]))
  }
  training_subset_aligned$movingAverage <-
    c(
      # training_subset_aligned$confirmed[1:((window - 1) / 2)],
      pre,
      rollmean(
        training_subset_aligned$confirmed,
        k = window,
        align = "center"
      ),
      # training_subset_aligned$confirmed[(nrow(training_subset_aligned) - ((window -
      #                                                                        1) / 2) + 1):nrow(training_subset_aligned)]
      post
    )
  
  training_subset_aligned$derived_I_curve <- training_subset_aligned$movingAverage
  for(nn in (floor(recoveryTime)+1):nrow(training_subset_aligned)){
    training_subset_aligned$derived_I_curve[nn] <- training_subset_aligned$movingAverage[nn] + sum(training_subset_aligned$movingAverage[(nn-floor(recoveryTime)):nn])
  }
  for(j in 2:floor(recoveryTime)){
    training_subset_aligned$derived_I_curve[j] <- training_subset_aligned$movingAverage[j] + sum(training_subset_aligned$movingAverage[1:j])
  }
  
  plottingDF <- training_subset_aligned[,c("date","confirmed","movingAverage","derived_I_curve")]
  training_subset_aligned$date <- as.Date(training_subset_aligned$date)
  training_subset_aligned$confirmed <- as.numeric(training_subset_aligned$confirmed)
  training_subset_aligned$movingAverage <- as.numeric(training_subset_aligned$movingAverage)
  training_subset_aligned$derived_I_curve <- as.numeric(training_subset_aligned$derived_I_curve)
  plottingDF$derived_I_curve <- NULL
  melt_plottingDF <- melt(plottingDF, id="date")
  melt_plottingDF$date <- as.Date(melt_plottingDF$date)
  # Plot cases
  gg <- ggplot(melt_plottingDF, aes(x = date, y = value, color = variable)) +
    geom_line(size=2) +
    ggtitle(paste0("COVID19 in the ", training_countries[i]))+
    theme(legend.position = "top") +
    labs(colour = NULL) +
    xlab(NULL) +
    scale_x_date(
      date_breaks = "2 week",
      date_labels =  "%b %d",
      limits = c(min(melt_plottingDF$date), max(melt_plottingDF$date))
    ) +
    # ggtitle(testing_country)+
    theme(text = element_text(size = 14)) +
    # ylim(0,max(seirScaledPlotDF$R)*N)+
    scale_y_continuous(
      label = comma,
      # Features of the first axis
      name = "Population"
    )+
    scale_x_date(date_breaks = "3 week", date_labels =  "%b %d") +
    theme(legend.title = element_text(size = 14)) +
    theme(
      axis.text.x = element_text(
        color = "black",
        size = 13,
        angle = 60,
        hjust = 1,
        vjust = 1
      ),
      axis.text.y = element_text(
        color = "black",
        size = 13,
        angle = 0
      ),
      axis.title.x = element_text(
        color = "black",
        size = 13,
        angle = 0
      ),
      axis.title.y = element_text(
        color = "black",
        size = 13,
        angle = 90
      )
    ) +
    # scale_x_continuous(breaks=seq(1, 10, 1))+
    # scale_colour_manual(values=predictColor)+
    theme(legend.text = element_text(size = 16)) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    scale_color_manual(values = c("orange", "tomato3"), breaks = c("confirmed", "movingAverage"), labels = c("New Daily Cases", "Smoothed Moving Average"), name=NULL)
  # scale_color_manual(values = c("orange", "tomato3","purple"), breaks = c("confirmed", "movingAverage", "derived_I_curve"), labels = c("New Daily Cases", "Smoothed Moving Average", "Currently Infected"), name=NULL)
  print(gg)
  # Add moving average day lag and one day difference variables
  training_subset_aligned[["movingAverage_lag_1"]] <-
    lag(training_subset_aligned[["movingAverage"]], 1)
  training_subset_aligned[["movingAverage_lag_3"]] <-
    lag(training_subset_aligned[["movingAverage"]], 3)
  training_subset_aligned[["movingAverage_lag_7"]] <-
    lag(training_subset_aligned[["movingAverage"]], 7)
  training_subset_aligned[["movingAverage_lag_14"]] <-
    lag(training_subset_aligned[["movingAverage"]], 14)
  training_subset_aligned[["movingAverage_diff_1_3"]] <-
    training_subset_aligned[["movingAverage_lag_1"]] - training_subset_aligned[["movingAverage_lag_3"]]
  training_subset_aligned[["movingAverage_diff_1_7"]] <-
    training_subset_aligned[["movingAverage_lag_1"]] - training_subset_aligned[["movingAverage_lag_7"]]
  training_subset_aligned[["movingAverage_diff_1_14"]] <-
    training_subset_aligned[["movingAverage_lag_1"]] - training_subset_aligned[["movingAverage_lag_14"]]
  training_subset_aligned[["movingAverage_diff_3_7"]] <-
    training_subset_aligned[["movingAverage_lag_3"]] - training_subset_aligned[["movingAverage_lag_7"]]
  training_subset_aligned[["movingAverage_diff_7_14"]] <-
    training_subset_aligned[["movingAverage_lag_7"]] - training_subset_aligned[["movingAverage_lag_14"]]
  
  
  # toCalcR0 <- training_subset_aligned[,c("date","confirmed")]
  toCalcR0 <-
    training_subset_aligned[, c("date", "movingAverage")]
  colnames(toCalcR0) <- c("dates", "I")
  
  if (RunWT_R_flag == T) {
    
    incid <- as.numeric(toCalcR0$I)
    names(incid) <- as.Date(toCalcR0$dates)
    empez <- as.Date(toCalcR0$dates[1])
    fin <- as.Date(toCalcR0$dates[nrow(toCalcR0)])
    # mGT<-generation.time("gamma", c(wtSIs$mean_si, wtSIs$std_si))
    # https://epiforecasts.io/covid/methods.html
    # generation time with a mean of 3.6 days (sd: 0.7 days) and sd of 3 days (sd 0.8 days)
    # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7201952/
    # mean of 5.2 days and a standard deviation (SD) of 2.8 days
    # https://wwwnc.cdc.gov/eid/article/26/6/20-0357_article
    # mean interval was 3.96 days (95% CI 3.53–4.39 days), SD 4.75 days (95% CI 4.46–5.07 days)
    # https://science.sciencemag.org/content/368/6491/eabb6936
    # The distribution had a median of 5.0 days and standard deviation of 1.9 days. [...]  The distribution is best described by a Weibull distribution
    mGT <- generation.time("weibull", c(5.0, 1.9))
    incid[incid == 0] <- 1
    estR0 <- est.R0.TD(incid, mGT, begin=empez, end=fin, nsim=1000)
    ## An interesting way to look at these results is to agregate initial data by longest time unit,
    ## such as weekly incidence. This gives a global overview of the epidemic.
    # estR0.weekly <- smooth.Rt(estR0, 7)
    # estR0.weekly$R
    plot(estR0)
    # plot(estR0.weekly)
    
    save(estR0,
         file = paste0("./InputData/", training_countries[i], "_WT_R0.Rdata"))
  } else{
    res_uncertain_si <- NULL
    load(paste0("./InputData/", training_countries[i], "_WT_R0.Rdata"))
  }
  
  finalR_WT <- as.data.frame(estR0$R)
  finalR_WT$date <- rownames(finalR_WT)
  rownames(finalR_WT) <- NULL
  colnames(finalR_WT) <- c("R0","date")
  finalR_WT <- finalR_WT[, c("date","R0")]
  finalR_WT$R0[nrow(finalR_WT)] <- finalR_WT$R0[nrow(finalR_WT)-1]
  finalR_WT$date <- as.Date(finalR_WT$date)
  training_subset_aligned <- merge(training_subset_aligned, finalR_WT, by="date", all.x = T, all.y = F)
  training_subset_aligned[,c("date","R0")]
  listToLag <-
    c(
      "R0"
      # "Google_Retail_recreation",
      # "Google_Grocery_pharmacy",
      # "Google_Parks",
      # "Google_Transit_stations",
      # "Google_Workplaces",
      # "Google_Residential"
    )
  # c(2,5,8,11,14)
  for (npi in 1:length(listToLag)) {
    # Add 2 day lag factor for R0
    training_subset_aligned[[paste0(listToLag[npi], "_lag_2")]] <-
      lag(training_subset_aligned[[paste0(listToLag[npi])]], 2)
    training_subset_aligned[[paste0(listToLag[npi], "_lag_2")]][1:2] <-
      mean(training_subset_aligned[[paste0(listToLag[npi])]][1:3])
    # Add 5 day lag factor for R0
    training_subset_aligned[[paste0(listToLag[npi], "_lag_5")]] <-
      lag(training_subset_aligned[[paste0(listToLag[npi])]], 5)
    training_subset_aligned[[paste0(listToLag[npi], "_lag_5")]][1:5] <-
      mean(training_subset_aligned[[paste0(listToLag[npi])]][1:3])
    # Add 6 day lag factor for R0
    training_subset_aligned[[paste0(listToLag[npi], "_lag_6")]] <-
      lag(training_subset_aligned[[paste0(listToLag[npi])]], 6)
    training_subset_aligned[[paste0(listToLag[npi], "_lag_6")]][1:6] <-
      mean(training_subset_aligned[[paste0(listToLag[npi])]][1:3])
    # Add 7 day lag factor for R0
    training_subset_aligned[[paste0(listToLag[npi], "_lag_7")]] <-
      lag(training_subset_aligned[[paste0(listToLag[npi])]], 7)
    training_subset_aligned[[paste0(listToLag[npi], "_lag_7")]][1:7] <-
      mean(training_subset_aligned[[paste0(listToLag[npi])]][1:3])
    # Add 8 day lag factor for R0
    training_subset_aligned[[paste0(listToLag[npi], "_lag_8")]] <-
      lag(training_subset_aligned[[paste0(listToLag[npi])]], 8)
    training_subset_aligned[[paste0(listToLag[npi], "_lag_8")]][1:8] <-
      mean(training_subset_aligned[[paste0(listToLag[npi])]][1:3])
    # Add 9 day lag factor for R0
    training_subset_aligned[[paste0(listToLag[npi], "_lag_9")]] <-
      lag(training_subset_aligned[[paste0(listToLag[npi])]], 9)
    training_subset_aligned[[paste0(listToLag[npi], "_lag_9")]][1:9] <-
      mean(training_subset_aligned[[paste0(listToLag[npi])]][1:3])
    # Add 10 day lag factor for R0
    training_subset_aligned[[paste0(listToLag[npi], "_lag_10")]] <-
      lag(training_subset_aligned[[paste0(listToLag[npi])]], 10)
    training_subset_aligned[[paste0(listToLag[npi], "_lag_10")]][1:10] <-
      mean(training_subset_aligned[[paste0(listToLag[npi])]][1:3])
    # Add 11 day lag factor for R0
    training_subset_aligned[[paste0(listToLag[npi], "_lag_11")]] <-
      lag(training_subset_aligned[[paste0(listToLag[npi])]], 11)
    training_subset_aligned[[paste0(listToLag[npi], "_lag_11")]][1:11] <-
      mean(training_subset_aligned[[paste0(listToLag[npi])]][1:3])
    # Add 12 day lag factor for R0
    training_subset_aligned[[paste0(listToLag[npi], "_lag_12")]] <-
      lag(training_subset_aligned[[paste0(listToLag[npi])]], 12)
    training_subset_aligned[[paste0(listToLag[npi], "_lag_12")]][1:12] <-
      mean(training_subset_aligned[[paste0(listToLag[npi])]][1:3])
    # Add 13 day lag factor for R0
    training_subset_aligned[[paste0(listToLag[npi], "_lag_13")]] <-
      lag(training_subset_aligned[[paste0(listToLag[npi])]], 13)
    training_subset_aligned[[paste0(listToLag[npi], "_lag_13")]][1:13] <-
      mean(training_subset_aligned[[paste0(listToLag[npi])]][1:3])
    # Add 14 day lag factor for R0
    training_subset_aligned[[paste0(listToLag[npi], "_lag_14")]] <-
      lag(training_subset_aligned[[paste0(listToLag[npi])]], 14)
    training_subset_aligned[[paste0(listToLag[npi], "_lag_14")]][1:14] <-
      mean(training_subset_aligned[[paste0(listToLag[npi])]][1:3])
    
  }
  
  if (i == 1) {
    training_ready <- training_subset_aligned
  } else{
    training_ready <-
      as.data.frame(rbind(training_ready, training_subset_aligned))
  }
  
}

dev.off()

# preserve the original dataframes before we make changes to them...
training_ready_OG <- training_ready

# filter training data
training_ready_sub_tmp <- training_ready %>%
  dplyr::select(-contains("confirmed_cum_per_million")) %>%
  dplyr::select(-contains("derived_I_curve")) %>%
  dplyr::select(-contains("death_cum")) %>%
  dplyr::select(-contains("movingAverage")) %>%
  dplyr::select(-contains("MalePercent")) %>%
  dplyr::select(-contains("FemalePercent")) %>%
  dplyr::select(-contains("C1")) %>%
  dplyr::select(-contains("C2")) %>%
  dplyr::select(-contains("C3")) %>%
  dplyr::select(-contains("C4")) %>%
  dplyr::select(-contains("C5")) %>%
  dplyr::select(-contains("C6")) %>%
  dplyr::select(-contains("C7")) %>%
  dplyr::select(-contains("C8")) %>%
  dplyr::select(-contains("E1")) %>%
  dplyr::select(-contains("E2")) %>%
  dplyr::select(-contains("E3")) %>%
  dplyr::select(-contains("E4")) %>%
  dplyr::select(-contains("H1")) %>%
  dplyr::select(-contains("H2")) %>%
  dplyr::select(-contains("H3")) %>%
  dplyr::select(-contains("H4")) %>%
  dplyr::select(-contains("H5")) %>%
  dplyr::select(
    -c(
      confirmed_cum,
      date,
      # Country,
      ISO3,
      confirmed,
      death,
      FullName,
      recovered,
      time,
      R0_lag_2
    )
  )

countryCounterDF <- training_ready_sub_tmp[complete.cases(training_ready_sub_tmp), ]
unique(countryCounterDF$Country)
includedCountries <- as.character(unique(countryCounterDF$Country))
# temporaryClist <- cbind(includedCountries,as.character(unique(training_ready$ISO3)))
write.table(includedCountries,"countryList.txt",col.names = F,row.names = F,quote=F)
# write.table(temporaryClist,"countryList.txt",col.names = F,row.names = F,quote=F,sep=",")

consideredCountries <- as.character(unique(training_ready$Country))
includedCountries <- includedCountries[which(includedCountries %in% consideredCountries)]
excludedCountries <- consideredCountries[which(consideredCountries %ni% includedCountries)]

for(name in excludedCountries){
  check <- training_ready_sub_tmp[which(training_ready$Country == name),]
  print(name)
  checkNA <- check %>%
    summarise_all(funs(sum(is.na(.))))
  print(colnames(checkNA)[which(checkNA[1,]>=(.8*nrow(check)))])
  print("..............")
}

training_ready_sub2 <- training_ready %>%
  dplyr::select(-contains("confirmed_cum_per_million")) %>%
  dplyr::select(-contains("death_cum")) %>%
  dplyr::select(-contains("movingAverage")) %>%
  dplyr::select(-contains("MalePercent")) %>%
  dplyr::select(-contains("FemalePercent")) %>%
  dplyr::select(-contains("C1")) %>%
  dplyr::select(-contains("C2")) %>%
  dplyr::select(-contains("C3")) %>%
  dplyr::select(-contains("C4")) %>%
  dplyr::select(-contains("C5")) %>%
  dplyr::select(-contains("C6")) %>%
  dplyr::select(-contains("C7")) %>%
  dplyr::select(-contains("C8")) %>%
  dplyr::select(-contains("E1")) %>%
  dplyr::select(-contains("E2")) %>%
  dplyr::select(-contains("E3")) %>%
  dplyr::select(-contains("E4")) %>%
  dplyr::select(-contains("H1")) %>%
  dplyr::select(-contains("H2")) %>%
  dplyr::select(-contains("H3")) %>%
  dplyr::select(-contains("H4")) %>%
  dplyr::select(-contains("H5")) %>%
  dplyr::select(
    -c(
      confirmed_cum,
      date,
      Country,
      ISO3,
      confirmed,
      death,
      FullName,
      recovered,
      time,
      R0_lag_2
    )
  ) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_if(is.integer, as.numeric)

#---VSURF Variable Selection---#########################################################################################################################################################################
outcomeVariable <- "R0"
dim(training_ready_sub2)
sapply(training_ready_sub2, function(x) sum(is.na(x)))
dim(training_ready_sub2[complete.cases(training_ready_sub2), which(colnames(training_ready_sub2) %ni% outcomeVariable)])

mod_formula <- as.formula(paste(outcomeVariable, "~", "."))
if (VSURFflag == T) {
  print(
    paste0(
      'Number of cores being used = ',
      num_cores,
      ", of possible ",
      detectCores(),
      " cores"
    )
  )
  
  registerDoParallel(num_cores)
  
  
  set.seed(15)
  
  glimpse(training_ready_sub2)
  dim(training_ready_sub2)
  sapply(training_ready_sub2, function(x) sum(is.na(x)))
  max(sapply(training_ready_sub2, function(x) sum(is.na(x))))
  summary(training_ready_sub2)
  
  # Pick the best mtry
  x <-
    training_ready_sub2[complete.cases(training_ready_sub2), which(colnames(training_ready_sub2) %ni% outcomeVariable)]
  y <-
    training_ready_sub2[complete.cases(training_ready_sub2), which(colnames(training_ready_sub2) %in% outcomeVariable)]
  # bestMtry <-
  #   tuneRF(
  #     x,
  #     y,
  #     stepFactor = 1.5,
  #     improve = 1e-5,
  #     ntree = number_trees,
  #     na.action = nasaction
  #   )
  # bestMtry <- as.data.frame(bestMtry)
  # mtry_best <- bestMtry$mtry[which.min(bestMtry$OOBError)]
  mtry_best <- max(floor(ncol(x) / 3), 1)
  
  # Run VSURF to get the top variables
  results.vsurf <- VSURF(
    x,
    y,
    na.action = na.omit,
    mtry = mtry_best,
    n_tree = number_trees,
    parallel = TRUE,
    verbose = TRUE,
    ncores = num_cores,
    nmj = 1
  )
  nmj_used = 1
  results.vsurf.OG <- results.vsurf
  
  nVarInterp <-
    length(colnames(training_ready_sub2[, results.vsurf$varselect.interp]))
  nVarPred <-
    length(colnames(training_ready_sub2[, results.vsurf$varselect.pred]))
  
  # look at results of VSURF
  summary(results.vsurf)
  plot(results.vsurf)
  results.vsurf$varselect.thres
  results.vsurf$varselect.interp
  results.vsurf$varselect.pred
  
  # print the reduced number of variables that should be considered in model
  colnames(training_ready_sub2[, results.vsurf$varselect.thres])
  colnames(training_ready_sub2[, results.vsurf$varselect.interp])
  colnames(training_ready_sub2[, results.vsurf$varselect.pred])    # The final list of variables to be included according to the VSURF methodology.
  VSURF_thres_keepers <-
    colnames(training_ready_sub2[, results.vsurf$varselect.thres])
  VSURF_interp_keepers <-
    colnames(training_ready_sub2[, results.vsurf$varselect.interp])
  VSURF_pred_keepers <-
    colnames(training_ready_sub2[, results.vsurf$varselect.pred])
  # Save the final list from D4 to be used for D3, D2, and D1
  # save(nmj_used, VSURF_thres_keepers, VSURF_interp_keepers, VSURF_pred_keepers, results.vsurf, results.vsurf.OG, file = paste0("./InputData/",testing_country,"_VSURFkeepers_R0.Rdata"))
  
  
  
}else{
  # load("./InputData/tmpChris_All_Countries_VSURFkeepers_R0_oxford.Rdata")
  load(paste0("./InputData/",
              "All_Countries",
              "_VSURFkeepers_R0_oxford.Rdata"))
}

#---RF model---#########################################################################################################################################################################
print(
  paste0(
    'Number of cores being used = ',
    num_cores,
    ", of possible ",
    detectCores(),
    " cores"
  )
)
registerDoParallel(num_cores)

if (length(VSURF_pred_keepers) > 1) {
  training_ready_sub_vsurf_result = dplyr::select(training_ready_sub2,
                                                  c(outcomeVariable, VSURF_pred_keepers))
  training_ready_sub_vsurf_result_varImp = dplyr::select(training_ready_sub2,
                                                         c(outcomeVariable, VSURF_interp_keepers))
} else if (length(VSURF_pred_keepers) <= 1 &&
           length(VSURF_interp_keepers) > 1) {
  training_ready_sub_vsurf_result = dplyr::select(training_ready_sub2,
                                                  c(outcomeVariable, VSURF_interp_keepers))
  training_ready_sub_vsurf_result_varImp = dplyr::select(training_ready_sub2,
                                                         c(outcomeVariable, VSURF_interp_keepers))
} else if (length(VSURF_pred_keepers) <= 1 &&
           length(VSURF_interp_keepers) <= 1) {
  training_ready_sub_vsurf_result = dplyr::select(training_ready_sub2,
                                                  c(outcomeVariable, VSURF_thres_keepers))
  training_ready_sub_vsurf_result_varImp = dplyr::select(training_ready_sub2,
                                                         c(outcomeVariable, VSURF_thres_keepers))
}
glimpse(training_ready_sub_vsurf_result)
glimpse(training_ready_sub_vsurf_result_varImp)

# Pick the best mtry
x <-
  training_ready_sub_vsurf_result_varImp[complete.cases(training_ready_sub_vsurf_result_varImp), which(colnames(training_ready_sub_vsurf_result_varImp) %ni% outcomeVariable)]
y <-
  training_ready_sub_vsurf_result_varImp[complete.cases(training_ready_sub_vsurf_result_varImp), which(colnames(training_ready_sub_vsurf_result_varImp) %in% outcomeVariable)]
if (length(training_ready_sub_vsurf_result_varImp) > 2) {
  # bestMtry <-
  #   tuneRF(
  #     x,
  #     y,
  #     stepFactor = 1.5,
  #     improve = 1e-5,
  #     ntree = number_trees,
  #     na.action = nasaction
  #   )
  # bestMtry <- as.data.frame(bestMtry)
  # mtry_best <- bestMtry$mtry[which.min(bestMtry$OOBError)]
  mtry_best <- max(floor(ncol(x)/3), 1)
} else{
  mtry_best <- 1
}

tunegrid <- expand.grid(
  .mtry = mtry_best,
  .splitrule = c('gini'),
  .min.node.size = c(5, 10, 20)
)

control <- trainControl(method = "cv",
                        number = 3,
                        # repeats=3,
                        # verboseIter = T,
                        # classProbs = T,
                        allowParallel = TRUE)

rf.mod.varImp <- caret::train(
  mod_formula,
  data = training_ready_sub_vsurf_result_varImp,
  # data = training_ready_sub_vsurf_result,
  # method = 'ranger',
  method = 'rf',
  na.action = nasaction,
  keep.inbag = TRUE,
  replace = TRUE,
  # importance = "permutation", #***
  trControl = control
)
varImp(rf.mod.varImp)


library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
# exploring a linear model too...
# myLm <- lm(formula = mod_formula, data = training_ready_sub2[complete.cases(training_ready_sub2), ])
# myLmAIC <- stepAIC(myLm)
# # plot_summs(myLmAIC)
# plot_model(myLmAIC, sort.est = TRUE)
# 
# Pred <- training_ready_sub2[1:400, ]
# subPred <- Pred[complete.cases(Pred), ]
# postPred <- as.data.frame(predict(myLmAIC, newdata = subPred))
# postPred_RF <- as.data.frame(predict(rf.mod.varImp, newdata = subPred))
# colnames(postPred) <- c("R0_pred")
# colnames(postPred_RF) <- c("R0_pred_RF")
# ggplot() + 
#   geom_line(data = subPred, aes(x = 1:nrow(subPred), y = R0), color = "blue") +
#   geom_line(data = postPred, aes(x = 1:nrow(postPred), y = R0_pred), color = "red") +
#   geom_line(data = postPred_RF, aes(x = 1:nrow(postPred_RF), y = R0_pred_RF), color = "green") +
#   xlab('Index') +
#   ylab('R0') +
#   geom_hline(
#     yintercept = 1,
#     linetype = "twodash",
#     color = "seashell4",
#     size = 0.9
#   ) +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_blank(),
#     axis.line = element_line(colour = "black")
#   )

######################################
model_name = "rf.mod"
if ("rf.mod" == model_name) {
  print("Model chosen by caret is: randomForest")
  df_tmp <- varImp(rf.mod.varImp)
  df <- as.data.frame(df_tmp$importance)
  colnames(df) = c('imp')
}

df2 <- df %>%
  tibble::rownames_to_column() %>%
  dplyr::rename("variable" = rowname) %>%
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))

df2$variable <- as.character(df2$variable)
for (r in 1:nrow(df2)) {
  df2$variable[r] <-
    simpleCap(paste(unlist(strsplit(
      df2$variable[r], "_"
    )), sep = " ", collapse = " "))
}
df2 <- df2[order(df2$imp), ]
names1 <- df2$variable
names2 <- gsub(x = names1, pattern = "GovernmentResponseIndexForDisplay", "Government Response Index")
names2 <- gsub(x = names2, pattern = "StringencyIndexForDisplay", "Stringency Index")
names2 <- gsub(x = names2, pattern = "ContainmentHealthIndexForDisplay", "Containment Health Index")
names2 <- gsub(x = names2, pattern = "EconomicSupportIndexForDisplay", "Economic Support Index")
df2$variable <- names2

df2$variable <-
  factor(df2$variable,
         levels = df2$variable,
         order = T)

df2_R <- df2[grep("R0", df2$variable), ]
df2_nonR <- df2[grep("R0", df2$variable, invert = T), ]

plot_varimp <- ggplot2::ggplot(df2) +
  geom_segment(
    aes(
      x = variable,
      y = 0,
      xend = variable,
      yend = imp
    ),
    size = 1.5,
    alpha = 0.7
  ) +
  geom_point(aes(x = variable, y = imp, col = variable),
             size = 4,
             show.legend = F) +
  coord_flip() +
  labs(y = "Importance", x = NULL, title = "") +
  theme_bw() +
  theme(legend.title = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(
      color = "black",
      size = 13,
      angle = 0,
      hjust = .5,
      vjust = .5
    ),
    axis.text.y = element_text(
      color = "black",
      size = 10,
      angle = 0
    ),
    axis.title.x = element_text(
      color = "black",
      size = 13,
      angle = 0
    ),
    axis.title.y = element_text(
      color = "black",
      size = 13,
      angle = 90
    )
  )

plot_varimp_R <- ggplot2::ggplot(df2_R) +
  geom_segment(
    aes(
      x = variable,
      y = 0,
      xend = variable,
      yend = imp
    ),
    size = 1.5,
    alpha = 0.7
  ) +
  geom_point(aes(x = variable, y = imp, col = variable),
             size = 4,
             show.legend = F) +
  coord_flip() +
  labs(y = "Importance", x = NULL, title = "") +
  theme_bw() +
  theme(legend.title = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(
      color = "black",
      size = 13,
      angle = 0,
      hjust = .5,
      vjust = .5
    ),
    axis.text.y = element_text(
      color = "black",
      size = 13,
      angle = 0
    ),
    axis.title.x = element_text(
      color = "black",
      size = 13,
      angle = 0
    ),
    axis.title.y = element_text(
      color = "black",
      size = 13,
      angle = 90
    )
  )

plot_varimp_nonR <- ggplot2::ggplot(df2_nonR) +
  geom_segment(
    aes(
      x = variable,
      y = 0,
      xend = variable,
      yend = imp
    ),
    size = 1.5,
    alpha = 0.7
  ) +
  geom_point(aes(x = variable, y = imp, col = variable),
             size = 4,
             show.legend = F) +
  coord_flip() +
  labs(y = "Importance", x = NULL, title = "") +
  theme_bw() +
  theme(legend.title = element_text(size = 14)) +
  theme(
    axis.text.x = element_text(
      color = "black",
      size = 13,
      angle = 0,
      hjust = .5,
      vjust = .5
    ),
    axis.text.y = element_text(
      color = "black",
      size = length(unique(df2$variable))*1/5,
      angle = 0
    ),
    axis.title.x = element_text(
      color = "black",
      size = 13,
      angle = 0
    ),
    axis.title.y = element_text(
      color = "black",
      size = 13,
      angle = 90
    )
  )
plot_varimp
plot_varimp_R
plot_varimp_nonR
######################################

save(
  rf.mod.varImp,
  nmj_used,
  VSURF_thres_keepers,
  VSURF_interp_keepers,
  VSURF_pred_keepers,
  results.vsurf,
  results.vsurf.OG,
  plot_varimp,
  plot_varimp_R,
  plot_varimp_nonR,
  file = paste0("./InputData/",
                "All_Countries",
                "_VSURFkeepers_R0_oxford.Rdata")
)
