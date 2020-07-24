# Pre-running each leave-one-out training RF model

# Written by:  Chris H Arehart
# Written on: March 24th, 2020
# Updated on: June 20th, 2020


# setwd("C:/Users/chris/OneDrive/Desktop/SKOvid19")
library(shiny)
library(tidyverse)
library(EpiEstim)
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
library(shinyjs)

Clist <- read.csv("countryList_csv.csv", header=T, stringsAsFactors = F)
testing_countriesList <- Clist$iso[1:length(Clist$iso)]
# testing_countriesList <- c("USA")
# testing_countriesList <- c("USA","BRA","SWE","ITA")

for (cc in 1:length(testing_countriesList)) {
  
  #---Flag setup---#########################################################################################################################################################################
  # gwd <- getwd()
  gwd <- "."
  # TRUE if you want to scale by population
  incidence_flag <- T
  # TRUE if you want to do deaths instead of cases
  death_flag <- F
  incidence_start_point <- 0.3
  count_start_point <- 100
  # if we are doing deaths, we want incidence start point to be about 5.9% of the case one becuase that's the approx mortality rate
  # if (death_flag == T) {
  #   incidence_start_point <- incidence_start_point * (5.9 / 100)
  #   count_start_point <- count_start_point * (5.9 / 100)
  # }
  VSURFflag <- F
  RunWT_R_flag <- F
  # the number of lag factors you want
  nLags <- 14
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
  # Number of cores to use when running rf model and vsurf
  num_cores = detectCores()
  # What NA acion to use for models
  nasaction = na.omit
  # Number of trees to train on
  number_trees = 1000
  
  
  #---beginCode---#########################################################################################################################################################################
  
  '%ni%' <- Negate('%in%')
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)),
          substring(s, 2),
          sep = "",
          collapse = " ")
  }
  
  
    # Choose the testing country
    testing_country <- testing_countriesList[cc]
    
    data_clean <- read.csv(paste0(gwd,"/InputData/ML_features_oxford.csv"))
    
    training_countries_all <- as.character(unique(data_clean$ISO3))
    training_countries <-
      training_countries_all[which(training_countries_all != testing_country)]
    
    data_clean$date <- as.Date(data_clean$date)
    data_clean1 <- subset(data_clean, ISO3 == testing_country)
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
    D0 <- data_clean1$date[earliestD] + 17 #"2020-03-21"
    print(D1)
    print(D2)
    print(D3)
    print(D4)
    # dateList <- as.Date(rev(c(seq(from=D4, to=D3, length.out = 4), seq(from=D3, to=D2, length.out = 3), seq(from=D2, to=D1, length.out = 3), seq(from=D1, to=D0, length.out = 3))))[c(1,2,3,5,6,8,9,11,12,13)]
    dateList <- as.Date(rev(c(D4,D3,D2,D1,D0)))
    dateList[dateList < D0] <- D0
    
    
    # if(forecastingTimeFlag == "fullRange"){
    #   forecastingTime <- as.numeric(as.Date(D4)+14-as.Date(timeChop))
    # }else{
    #   forecastingTime = 14
    # }
    forecastingTime = 30
    
    data_clean <- read.csv(paste0(gwd,"/InputData/ML_features_oxford.csv"))
    data_clean$date <- as.Date(data_clean$date)
    data_clean_train <- data_clean
    data_clean <- subset(data_clean, date <= D4)
    
    # Looking at the data
    glimpse(data_clean)
    summary(data_clean)

    
    #---trainingTestingDataFrames---#########################################################################################################################################################################
    # Subset training country list to those countries that had enough cases (to calc R0) by the start point
    # subTrainList <- read.table("monitoring_detection_training.txt",header=F,stringsAsFactors = F)
    # training_countries <- training_countries[which(training_countries %in% subTrainList$V1)]
    lengthClist <- length(training_countries)
    training_countries_OG <- training_countries
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
      
      # Smooth out incidence using moving average with a centered window of 7 datapoints (3 to the left, 3 to the right)
      # make sure the window is an odd integer
      window <- 11
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
      # Plot cases
      gg <- ggplot(training_subset_aligned) +
        geom_line(aes(x = date, y = confirmed), color = "red") +
        geom_line(aes(x = date, y = movingAverage), color = "blue") +
        ggtitle(paste0("Reported cases in ", training_countries[i]))
      # print(gg)
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
      toCalcR0$I[toCalcR0$I < 0] <- NA
      #Get of erroneous negative counts... they sneak throught the API sometimes.
      # But if thre is a negative at teh end... are the last one lets just make it equal to the n-1 one
      if (is.na(tail(toCalcR0$I, 1))) {
        toCalcR0$I[length(toCalcR0$I)] <- toCalcR0$I[length(toCalcR0$I) - 1]
      }
      # If the NA is not at the end, Lets linearly interpolate them:
      toCalcR0$I <- na.approx(toCalcR0$I)
      toCalcR0$I <- as.integer(toCalcR0$I)
      
      # res_uncertain_si <- estimate_R(toCalcR0,
      #                                method = "uncertain_si",
      #                                config = config)
      # save(res_uncertain_si,
      #      file = paste0("./InputData/", training_countries[i], "_WT_R0.Rdata"))
      
      if (RunWT_R_flag == T) {
        incid <- as.numeric(toCalcR0$I)
        names(incid) <- as.Date(toCalcR0$dates)
        empez <- as.Date(toCalcR0$dates[1])
        fin <- as.Date(toCalcR0$dates[nrow(toCalcR0)])
        # mGT<-generation.time("gamma", c(wtSIs$mean_si, wtSIs$std_si))
        # https://epiforecasts.io/covid/methods.html
        # generation time with a mean of 3.6 days (sd: 0.7 days) 
        mGT <- generation.time("gamma", c(3.6,0.7))
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
        # Add 8 day lag factor for R0
        training_subset_aligned[[paste0(listToLag[npi], "_lag_8")]] <-
          lag(training_subset_aligned[[paste0(listToLag[npi])]], 8)
        training_subset_aligned[[paste0(listToLag[npi], "_lag_8")]][1:8] <-
          mean(training_subset_aligned[[paste0(listToLag[npi])]][1:3])
        # Add 11 day lag factor for R0
        training_subset_aligned[[paste0(listToLag[npi], "_lag_11")]] <-
          lag(training_subset_aligned[[paste0(listToLag[npi])]], 11)
        training_subset_aligned[[paste0(listToLag[npi], "_lag_11")]][1:11] <-
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
    
    # create testing dataframe
    for (i in 1:1) {
      testing_subset <-
        subset(data_clean_train, ISO3 == testing_country)
      
      print(i)
      print(testing_country)
      
      start <- which(testing_subset$confirmed_cum >= 50)[1]
      
      testing_subset_aligned <-
        testing_subset[start:nrow(testing_subset),]
      testing_subset_aligned$time <-
        c(1:nrow(testing_subset_aligned))
      
      # Smooth out incidence using moving average with a centered window of 7 datapoints (3 to the left, 3 to the right)
      # make sure the window is an odd integer
      window <- 11
      # dim(testing_subset_aligned)
      # length(rollmean(testing_subset_aligned$confirmed, k=window))
      pre <- c()
      for(pre_i in 1:((window - 1) / 2)){
        pre <- c(pre, mean(testing_subset_aligned$confirmed[1:pre_i]))
      }
      post <- c()
      for(post_i in rev(1:((window - 1) / 2))){
        post <- c(post, mean(testing_subset_aligned$confirmed[(length(testing_subset_aligned$confirmed) - post_i):(length(testing_subset_aligned$confirmed))]))
      }
      testing_subset_aligned$movingAverage <-
        c(
          # testing_subset_aligned$confirmed[1:((window - 1) / 2)],
          pre,
          rollmean(
            testing_subset_aligned$confirmed,
            k = window,
            align = "center"
          ),
          # testing_subset_aligned$confirmed[(nrow(testing_subset_aligned) - ((window -
          #                                                                        1) / 2) + 1):nrow(testing_subset_aligned)]
          post
        )
      # Plot cases
      gg <- ggplot(testing_subset_aligned) +
        geom_line(aes(x = date, y = confirmed), color = "red") +
        geom_line(aes(x = date, y = movingAverage), color = "blue") +
        ggtitle(paste0("Reported cases in ", testing_country))
      print(gg)
      # Add moving average day lag and one day difference variables
      testing_subset_aligned[["movingAverage_lag_1"]] <-
        lag(testing_subset_aligned[["movingAverage"]], 1)
      testing_subset_aligned[["movingAverage_lag_3"]] <-
        lag(testing_subset_aligned[["movingAverage"]], 3)
      testing_subset_aligned[["movingAverage_lag_7"]] <-
        lag(testing_subset_aligned[["movingAverage"]], 7)
      testing_subset_aligned[["movingAverage_lag_14"]] <-
        lag(testing_subset_aligned[["movingAverage"]], 14)
      testing_subset_aligned[["movingAverage_diff_1_3"]] <-
        testing_subset_aligned[["movingAverage_lag_1"]] - testing_subset_aligned[["movingAverage_lag_3"]]
      testing_subset_aligned[["movingAverage_diff_1_7"]] <-
        testing_subset_aligned[["movingAverage_lag_1"]] - testing_subset_aligned[["movingAverage_lag_7"]]
      testing_subset_aligned[["movingAverage_diff_1_14"]] <-
        testing_subset_aligned[["movingAverage_lag_1"]] - testing_subset_aligned[["movingAverage_lag_14"]]
      testing_subset_aligned[["movingAverage_diff_3_7"]] <-
        testing_subset_aligned[["movingAverage_lag_3"]] - testing_subset_aligned[["movingAverage_lag_7"]]
      testing_subset_aligned[["movingAverage_diff_7_14"]] <-
        testing_subset_aligned[["movingAverage_lag_7"]] - testing_subset_aligned[["movingAverage_lag_14"]]
      
      
      # toCalcR0 <- testing_subset_aligned[,c("date","confirmed")]
      toCalcR0 <-
        testing_subset_aligned[, c("date", "movingAverage")]
      colnames(toCalcR0) <- c("dates", "I")
      toCalcR0$I[toCalcR0$I < 0] <- NA
      #Get of erroneous negative counts... they sneak throught the API sometimes.
      # But if thre is a negative at teh end... are the last one lets just make it equal to the n-1 one
      if (is.na(tail(toCalcR0$I, 1))) {
        toCalcR0$I[length(toCalcR0$I)] <- toCalcR0$I[length(toCalcR0$I) - 1]
      }
      # If the NA is not at the end, Lets linearly interpolate them:
      toCalcR0$I <- na.approx(toCalcR0$I)
      toCalcR0$I <- as.integer(toCalcR0$I)
      
      # res_uncertain_si <- estimate_R(toCalcR0,
      #                                method = "uncertain_si",
      #                                config = config)
      # save(res_uncertain_si,
      #      file = paste0("./InputData/", testing_countries[i], "_WT_R0.Rdata"))
      
      if (RunWT_R_flag == T) {
        incid <- as.numeric(toCalcR0$I)
        names(incid) <- as.Date(toCalcR0$dates)
        empez <- as.Date(toCalcR0$dates[1])
        fin <- as.Date(toCalcR0$dates[nrow(toCalcR0)])
        # mGT<-generation.time("gamma", c(wtSIs$mean_si, wtSIs$std_si))
        # https://epiforecasts.io/covid/methods.html
        # generation time with a mean of 3.6 days (sd: 0.7 days) 
        mGT <- generation.time("gamma", c(3.6,0.7))
        estR0 <- est.R0.TD(incid, mGT, begin=empez, end=fin, nsim=1000)
        ## An interesting way to look at these results is to agregate initial data by longest time unit,
        ## such as weekly incidence. This gives a global overview of the epidemic.
        # estR0.weekly <- smooth.Rt(estR0, 7)
        # estR0.weekly$R
        plot(estR0)
        # plot(estR0.weekly)
        
        save(estR0,
             file = paste0("./InputData/", testing_country, "_WT_R0.Rdata"))
      } else{
        res_uncertain_si <- NULL
        load(paste0("./InputData/", testing_country, "_WT_R0.Rdata"))
      }
      
      finalR_WT <- as.data.frame(estR0$R)
      finalR_WT$date <- rownames(finalR_WT)
      rownames(finalR_WT) <- NULL
      colnames(finalR_WT) <- c("R0","date")
      finalR_WT <- finalR_WT[, c("date","R0")]
      finalR_WT$R0[nrow(finalR_WT)] <- finalR_WT$R0[nrow(finalR_WT)-1]
      finalR_WT$date <- as.Date(finalR_WT$date)
      testing_subset_aligned <- merge(testing_subset_aligned, finalR_WT, by="date", all.x = T, all.y = F)
      testing_subset_aligned[,c("date","R0")]
      
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
        testing_subset_aligned[[paste0(listToLag[npi], "_lag_2")]] <-
          lag(testing_subset_aligned[[paste0(listToLag[npi])]], 2)
        testing_subset_aligned[[paste0(listToLag[npi], "_lag_2")]][1:2] <-
          mean(testing_subset_aligned[[paste0(listToLag[npi])]][1:3])
        # Add 5 day lag factor for R0
        testing_subset_aligned[[paste0(listToLag[npi], "_lag_5")]] <-
          lag(testing_subset_aligned[[paste0(listToLag[npi])]], 5)
        testing_subset_aligned[[paste0(listToLag[npi], "_lag_5")]][1:5] <-
          mean(testing_subset_aligned[[paste0(listToLag[npi])]][1:3])
        # Add 8 day lag factor for R0
        testing_subset_aligned[[paste0(listToLag[npi], "_lag_8")]] <-
          lag(testing_subset_aligned[[paste0(listToLag[npi])]], 8)
        testing_subset_aligned[[paste0(listToLag[npi], "_lag_8")]][1:8] <-
          mean(testing_subset_aligned[[paste0(listToLag[npi])]][1:3])
        # Add 11 day lag factor for R0
        testing_subset_aligned[[paste0(listToLag[npi], "_lag_11")]] <-
          lag(testing_subset_aligned[[paste0(listToLag[npi])]], 11)
        testing_subset_aligned[[paste0(listToLag[npi], "_lag_11")]][1:11] <-
          mean(testing_subset_aligned[[paste0(listToLag[npi])]][1:3])
        # Add 14 day lag factor for R0
        testing_subset_aligned[[paste0(listToLag[npi], "_lag_14")]] <-
          lag(testing_subset_aligned[[paste0(listToLag[npi])]], 14)
        testing_subset_aligned[[paste0(listToLag[npi], "_lag_14")]][1:14] <-
          mean(testing_subset_aligned[[paste0(listToLag[npi])]][1:3])
      }

      
      tmp <- testing_subset_aligned[rep(1, forecastingTime), ]
      tmp[, grep(
        "cum|StringencyIndexForDisplay|GovernmentResponseIndexForDisplay|ContainmentHealthIndexForDisplay|EconomicSupportIndexForDisplay|Google|R0|date|confirmed",
        colnames(tmp)
      )] <- NA
      # tmp$Social_Distancing <- NA
      # tmp$Quaranting_Cases <- NA
      # tmp$Close_Border <- NA
      testing_subset_aligned_predictNA <-
        rbind(testing_subset_aligned, tmp)
      testing_subset_aligned_predictNA$time <-
        c(1:nrow(testing_subset_aligned_predictNA))
      
      testing_ready <- testing_subset_aligned_predictNA
    }
    # dev.off()
    
    # preserve the original dataframes before we make changes to them...
    # testing_ready <- testing_ready_OG
    # training_ready <- training_ready_OG
    testing_ready_OG <- testing_ready
    training_ready_OG <- training_ready
    # Let's use some of the country's data for training based on testingTimeFrame
    NrowToSaveForTesting <-
      round(nrow(testing_ready_OG) * testingTimeFrame)
    breakpoint <- nrow(testing_ready_OG) - NrowToSaveForTesting
    testing_ready <-
      testing_ready_OG[(breakpoint + 1):nrow(testing_ready_OG), ]
    training_ready <-
      as.data.frame(rbind(training_ready_OG, training_ready_OG[1:breakpoint, ]))
    
    #---NPIflag1---#########################################################################################################################################################################
    # Here we write a little loop that takes care of the fact that the Johns hopkins count data is
    # Updated much more frequently than the NPI data.  So
    # setting the NPIflag1 to "autofill" is our method of saying that we want to fill all the NAs in the time period with the last empirical time points' NPI values
    # We will worry about the NPIflag2 later to specify if we want to fill the projection timeperiod the same way
    # NPIflag1 <- "autofill"
    
    peek_at_NPIs_training1 <-
      training_ready[, c(c("date", "time", "Country", "ISO3", "confirmed"),
                         names(training_ready)[grep(
                           "GovernmentResponseIndexForDisplay|ContainmentHealthIndexForDisplay|EconomicSupportIndexForDisplay|Google|R0",
                           names(training_ready)
                         )])]
    # breaker <- nrow(testing_ready)-forecastingTime+1-addToBreaker #breaker for R0
    # breaker2 <- nrow(testing_ready)-forecastingTime+1 #breaker for all of the NPIs for our cutoff date
    peek_at_NPIs_testing1 <-
      testing_ready[, c(c("date", "time", "Country", "ISO3", "confirmed"),
                        names(testing_ready)[grep(
                          "GovernmentResponseIndexForDisplay|ContainmentHealthIndexForDisplay|EconomicSupportIndexForDisplay|Google|R0",
                          names(testing_ready)
                        )])]
    # tail(peek_at_NPIs_testing1[1:breaker])
    #---Filtering Columns in Dataframes---#########################################################################################################################################################################
    # Columns you don't want to be in the model
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
    
    # filter testing data
    testing_ready_sub2 <- testing_ready %>%
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
    mod_formula <- as.formula(paste(outcomeVariable, "~", "."))
    # if (VSURFflag == T) {
    #   print(
    #     paste0(
    #       'Number of cores being used = ',
    #       num_cores,
    #       ", of possible ",
    #       detectCores(),
    #       " cores"
    #     )
    #   )
    #   
    #   registerDoParallel(num_cores)
    #   
    #   
    #   set.seed(15)
    #   
    #   glimpse(training_ready_sub2)
    #   
    #   # Pick the best mtry
    #   x <-
    #     training_ready_sub2[complete.cases(training_ready_sub2), which(colnames(training_ready_sub2) %ni% outcomeVariable)]
    #   y <-
    #     training_ready_sub2[complete.cases(training_ready_sub2), which(colnames(training_ready_sub2) %in% outcomeVariable)]
    #   # bestMtry <-
    #   #   tuneRF(
    #   #     x,
    #   #     y,
    #   #     stepFactor = 1.5,
    #   #     improve = 1e-5,
    #   #     ntree = number_trees,
    #   #     na.action = nasaction
    #   #   )
    #   # bestMtry <- as.data.frame(bestMtry)
    #   # mtry_best <- bestMtry$mtry[which.min(bestMtry$OOBError)]
    #   mtry_best <- max(floor(ncol(x)/3), 1)
    #   
    #   # Run VSURF to get the top variables
    #   results.vsurf <- VSURF(
    #     x,
    #     y,
    #     na.action = na.omit,
    #     mtry = mtry_best,
    #     n_tree = number_trees,
    #     parallel = TRUE,
    #     verbose = TRUE,
    #     ncores = num_cores,
    #     nmj = 1
    #   )
    #   nmj_used = 1
    #   results.vsurf.OG <- results.vsurf
    #   
    #   nVarInterp <-
    #     length(colnames(training_ready_sub2[, results.vsurf$varselect.interp]))
    #   nVarPred <-
    #     length(colnames(training_ready_sub2[, results.vsurf$varselect.pred]))
    #   
    #   # If the selection process between interpretation step and prediction step essentially eliminated all of the variables,
    #   # lets lower the nmj threshold (since there is no natural tuning of this parameter)
    #   if (nVarInterp <= 4) {
    #     print(
    #       "we have less than 4 variables in the interpretation set, so lets predict on all the variables from the interpretation step..."
    #     )
    #     results.vsurf$varselect.pred <-
    #       results.vsurf$varselect.interp
    #   } else{
    #     if (nVarPred <= 4 && nVarInterp > 4) {
    #       print("nmj = 1 gave me less than 4 variables to predict on, lets try nmj = 0.1 ...")
    #       results.vsurf_pred_redo <- VSURF_pred(
    #         x,
    #         y,
    #         na.action = na.omit,
    #         mtry = mtry_best,
    #         n_tree = number_trees,
    #         parallel = TRUE,
    #         verbose = TRUE,
    #         ncores = num_cores,
    #         nmj = 0.1,
    #         err.interp = results.vsurf$err.interp,
    #         varselect.interp = results.vsurf$varselect.interp
    #       )
    #       results.vsurf$varselect.pred <-
    #         results.vsurf_pred_redo$varselect.pred
    #       nVarPred <-
    #         length(colnames(training_ready_sub2[, results.vsurf$varselect.pred]))
    #       nmj_used = 0.1
    #     }
    #     # If we still have less than 4 variables lets lower nmj again
    #     if (nVarPred <= 4 && nVarInterp > 4) {
    #       print("nmj = 0.1 gave me less than 4 variables to predict on, lets try nmj = 0.01 ...")
    #       results.vsurf_pred_redo <- VSURF_pred(
    #         x,
    #         y,
    #         na.action = na.omit,
    #         mtry = mtry_best,
    #         n_tree = number_trees,
    #         parallel = TRUE,
    #         verbose = TRUE,
    #         ncores = num_cores,
    #         nmj = 0.01,
    #         err.interp = results.vsurf$err.interp,
    #         varselect.interp = results.vsurf$varselect.interp
    #       )
    #       results.vsurf$varselect.pred <-
    #         results.vsurf_pred_redo$varselect.pred
    #       nVarPred <-
    #         length(colnames(training_ready_sub2[, results.vsurf$varselect.pred]))
    #       nmj_used = 0.01
    #     }
    #     # If we still have less than 4 variables in the predict set, lets just use the interpretation variable set for the prediction model
    #     if (nVarPred <= 4 && nVarInterp > 4) {
    #       print("this shouldnt happen very often....")
    #       print(
    #         "nmj = 0.01 gave me less than 4 variables to predict on, so lets stop trying to adjust nmj and just predict on all the variables from the interpretation step"
    #       )
    #       results.vsurf$varselect.pred <-
    #         results.vsurf$varselect.interp
    #       nmj_used = 0
    #     }
    #   }
    #   
    #   # look at results of VSURF
    #   summary(results.vsurf)
    #   plot(results.vsurf)
    #   results.vsurf$varselect.thres
    #   results.vsurf$varselect.interp
    #   results.vsurf$varselect.pred
    #   
    #   # print the reduced number of variables that should be considered in model
    #   colnames(training_ready_sub2[, results.vsurf$varselect.thres])
    #   colnames(training_ready_sub2[, results.vsurf$varselect.interp])
    #   colnames(training_ready_sub2[, results.vsurf$varselect.pred])    # The final list of variables to be included according to the VSURF methodology.
    #   VSURF_thres_keepers <-
    #     colnames(training_ready_sub2[, results.vsurf$varselect.thres])
    #   VSURF_interp_keepers <-
    #     colnames(training_ready_sub2[, results.vsurf$varselect.interp])
    #   VSURF_pred_keepers <-
    #     colnames(training_ready_sub2[, results.vsurf$varselect.pred])
    #   # Save the final list from D4 to be used for D3, D2, and D1
    #   # save(nmj_used, VSURF_thres_keepers, VSURF_interp_keepers, VSURF_pred_keepers, results.vsurf, results.vsurf.OG, file = paste0(gwd,"/InputData/",testing_country,"_VSURFkeepers_R0.Rdata"))
    #   save(
    #     nmj_used,
    #     VSURF_thres_keepers,
    #     VSURF_interp_keepers,
    #     VSURF_pred_keepers,
    #     results.vsurf,
    #     results.vsurf.OG,
    #     file = paste0(
    #       gwd,"/InputData/",
    #       "All_Countries",
    #       "_VSURFkeepers_R0.Rdata"
    #     )
    #   )
    #   
    #   
    # }
    VSURF_thres_keepers <- NULL
    VSURF_interp_keepers <- NULL
    VSURF_pred_keepers <- NULL
    # load(paste0(gwd,"/InputData/",testing_country,"_VSURFkeepers_R0.Rdata"))
    load(paste0(gwd,"/InputData/", "All_Countries", "_VSURFkeepers_R0_oxford.Rdata"))
    # VSURF_pred_keepers <- VSURF_pred_keepers
    # training dataframe with reduced number of variables
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
    
    # testing dataframe with reduced number of variables
    if (length(VSURF_pred_keepers) > 1) {
      testing_ready_sub_vsurf_result = dplyr::select(testing_ready_sub2,
                                                      c(outcomeVariable, VSURF_pred_keepers))
      testing_ready_sub_vsurf_result_varImp = dplyr::select(testing_ready_sub2,
                                                             c(outcomeVariable, VSURF_interp_keepers))
    } else if (length(VSURF_pred_keepers) <= 1 &&
               length(VSURF_interp_keepers) > 1) {
      testing_ready_sub_vsurf_result = dplyr::select(testing_ready_sub2,
                                                      c(outcomeVariable, VSURF_interp_keepers))
      testing_ready_sub_vsurf_result_varImp = dplyr::select(testing_ready_sub2,
                                                             c(outcomeVariable, VSURF_interp_keepers))
    } else if (length(VSURF_pred_keepers) <= 1 &&
               length(VSURF_interp_keepers) <= 1) {
      testing_ready_sub_vsurf_result = dplyr::select(testing_ready_sub2,
                                                      c(outcomeVariable, VSURF_thres_keepers))
      testing_ready_sub_vsurf_result_varImp = dplyr::select(testing_ready_sub2,
                                                             c(outcomeVariable, VSURF_thres_keepers))
    }
    glimpse(testing_ready_sub_vsurf_result)
    glimpse(testing_ready_sub_vsurf_result_varImp)
    
    closeAllConnections()
    
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
    
    # Pick the best mtry
    x <-
      training_ready_sub_vsurf_result[complete.cases(training_ready_sub_vsurf_result), which(colnames(training_ready_sub_vsurf_result) %ni% outcomeVariable)]
    y <-
      training_ready_sub_vsurf_result[complete.cases(training_ready_sub_vsurf_result), which(colnames(training_ready_sub_vsurf_result) %in% outcomeVariable)]
    if (length(training_ready_sub_vsurf_result) > 2) {
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
    
    rf.mod <- caret::train(
      mod_formula,
      # data = training_ready_sub_vsurf_result_varImp,
      data = training_ready_sub_vsurf_result,
      # method = 'ranger',
      method = 'rf',
      na.action = nasaction,
      keep.inbag = TRUE,
      replace = TRUE,
      # importance = "permutation", #***
      trControl = control
    )
    
    qrF <- quantregForest(
      x = x,
      y = y,
      # data = training_ready_sub_vsurf_result_varImp,
      data = training_ready_sub_vsurf_result,
      # method = 'ranger',
      method = 'rf',
      na.action = nasaction,
      keep.inbag = TRUE,
      replace = TRUE,
      # importance = "permutation", #***
      trControl = control
    )
    
    model_name = paste0("rf.mod")
    best_model = rf.mod
    
    closeAllConnections()
    #---predictFunction---#########################################################################################################################################################################
    # the predict function is called when we want to use the tree to make predictions
    predictFunction <-
      function(name = best_model,
               mod_name = model_name,
               dd = testing_ready_pred,
               n_trees = (1:30) * 25,
               nasaction = na.omit) {
        if ("rf.mod" == mod_name) {
          predict_tmp <- predict(name, dd, na.action = nasaction)
          # predict_tmp <- predict(name, dd, na.action = nasaction, type='se', se.method='infjack')
        }
        return(predict_tmp)
      }
    
    save(list = ls(all.names = TRUE), file = paste0("./InputData/",testing_country,"_LOO.RData"), envir = .GlobalEnv)
    rm(list=setdiff(ls(), c("cc", "testing_countriesList")))
}
