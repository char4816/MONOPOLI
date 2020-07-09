
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
# testing_countriesList <- c("USA","BRA","SWE","DEN","GBR")

for (cc in 1:length(testing_countriesList)) {
  # # Estimate Percent of Peak your country has achieved
  # percPeak <- 1.0
  # # Choose a timepoint between 0 and 5,
  # # 0 = early curve, 5 = late curve
  # dateAlongCurve <- 2
  # # Choose an NPI profile from "Status Quo", "Pre-COVID-NPI", "Extreme-NPI", "Custom-NPI"
  # NPIprofile <- c("Status Quo", "Pre-COVID-NPI", "Extreme-NPI", "Custom-NPI")[4]
  # # In general, -80 -> +80 should bound these percentages
  # Google_Retail_recreation_Custom <- -30
  # Google_Grocery_pharmacy_Custom <- -10
  # Google_Parks_Custom <- +0
  # Google_Transit_stations_Custom <- -35
  # Google_Workplaces_Custom <- -20
  # Google_Residential_Custom <- +30
  # # # 0 = No social distancing measures implemented
  # # # 1 = Large gatherings banned (e.g. concerts, sporting events, conferences)
  # # # 2 = Mid-sized gatherings are voluntarily closed
  # # # 3 = Service industries are closed (e.g., restaurants and pubs)
  # # # 4 = Shelter-in-place orders for non-essential workers
  # # # 5 = Lockdown except for essentials (e.g. grocery shopping)
  # # Social_Distancing_Custom <- 3
  # # # 0 = No action is taken to quarantine cases
  # # # 1 = Infected individuals are quarantined
  # # # 2 = Households of infected individuals are quarantined
  # # # 3 = Others in contact with infected individuals are tracked and then quarantined
  # # Quaranting_Cases_Custom <- 2
  # # # 0 = No restrictions at the border
  # # # 1 = Closed to Wuhan
  # # # 2 = Closed to multiple highly infected countries
  # # # 3 = Closed except for essential travel
  # # # 4 = Closed fully
  # # Close_Border_Custom <- 3
  # 
  # 
  # StringencyIndexForDisplay_Custom <- 70
  # GovernmentResponseIndexForDisplay_Custom <- 60
  # ContainmentHealthIndexForDisplay_Custom <- 80
  # EconomicSupportIndexForDisplay_Custom <- 40
  
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
    
    # make country lists, these are the ones that we have NPI data collected for
    # https://docs.google.com/spreadsheets/d/1vrKvs52OAxuB7x2kT9r1q6IcIBxGEQsNRHsK_o7h3jo/edit#gid=378237553
    # training_countries_all <- c("ITA","FRA","GBR")
    # training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","MYS","USA","SWE","AUT","CHE","DEU","FRA","DZA","ISR")
    # training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","KOR","USA","SWE","AUT","CHE","DEU","FRA","DZA","IRN","CAN","PRT","ISR","RUS","NOR","AUS","DNK","CHL","CZE","JPN","UKR","MAR","ARG")
    # training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","MYS","HUB","KOR","USA","SWE","AUT","CHE","DEU","FRA","DZA","IRN","CAN","TUR","BEL","ANT","PRT","ISR","RUS","NOR","IRL","AUS","IND","DNK","CHL","CZE","JPN","UKR","MAR","ARG","SGP","ROU")
    
    # training_countries_all <-
    #   c(
    #     "ITA",
    #     "GBR",
    #     "ZAF",
    #     "BRA",
    #     "ESP",
    #     "MYS",
    #     "KOR",
    #     "USA",
    #     "SWE",
    #     "AUT",
    #     "CHE",
    #     "DEU",
    #     "FRA",
    #     "DZA",
    #     "IRN",
    #     "CAN",
    #     "TUR",
    #     "BEL",
    #     "ANT",
    #     "PRT",
    #     "ISR",
    #     # "RUS",
    #     "NOR",
    #     "IRL",
    #     "AUS",
    #     "IND",
    #     "DNK",
    #     "CHL",
    #     "CZE",
    #     "JPN",
    #     "UKR",
    #     "MAR",
    #     "ARG",
    #     "SGP",
    #     "ROU"
    #   )
    # training_countries <- training_countries_all
    
    
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
    D0 <- data_clean1$date[earliestD] + 15 #"2020-03-21"
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
    
    #---Estimating reproduction numbers, R0---#########################################################################################################################################################################
    # # We are using the epiestim package to calculate the R0 for countries. This requires two things
    # # 1. specific information about the serial intervals for COVID
    # # 2. the timeseries incidence data.
    # # "SI for Serial Intervals.
    # # Determination of the serial interval, the time between the start of symptoms in the primary patient (infector)
    # # and onset of symptoms in the patient receiving that infection from the infector (the infectee)"
    # # Table 1 from https://www.medrxiv.org/content/10.1101/2020.04.13.20062760v1
    # # "we calculate a weighted mean of the published parameters and inferred a serial interval described
    # # by a gamma distribution, parameterised with mean SI of 4.56 days (credible interval: 2.54 - 7.36)
    # # and standard deviation 4.53 days (credible interval 4.17 - 5.05)."
    # 
    # serialIntervals = tibble(
    #   mean_si_estimate = c(3.96, 6.3, 4.22, 4.56, 3.95, 5.21, 4.7, 7.5, 6.6),
    #   mean_si_estimate_low_ci = c(3.53, 5.2, 3.43, 2.69, -4.47,-3.35, 3.7, 5.3, 0.7),
    #   mean_si_estimate_high_ci = c(4.39, 7.6, 5.01, 6.42, 12.51, 13.94, 6.0, 19.0, 19.0),
    #   std_si_estimate = c(4.75, 4.2, 0.4, 0.95, 4.24, 4.32, 2.3, 3.4, NA),
    #   std_si_estimate_low_ci = c(4.46, 3.1, NA, NA, 4.03, 4.06, 1.6, NA, NA),
    #   std_si_estimate_high_ci = c(5.07, 5.3, NA, NA, 4.95, 5.58, 3.5, NA, NA),
    #   sample_size = c(468, 48, 135, 93, 45, 54, 28, 16, 90),
    #   population = c(
    #     "China",
    #     "Shenzhen",
    #     "Taijin",
    #     "Singapore",
    #     "Taijin",
    #     "Singapore",
    #     "SE Asia",
    #     "Wuhan",
    #     "Italy"
    #   ),
    #   source = c(
    #     "Zhanwei Du et al. Serial Interval of COVID-19 among Publicly Reported Confirmed Cases. Emerging Infectious Disease journal 26, (2020)",
    #     "Bi, Q. et al. Epidemiology and Transmission of COVID-19 in Shenzhen China: Analysis of 391 cases and 1,286 of their close contacts. Infectious Diseases (except HIV/AIDS) (2020) doi:10.1101/2020.03.03.20028423",
    #     "Tindale, L. et al. Transmission interval estimates suggest pre-symptomatic spread of COVID-19. Epidemiology (2020) doi:10.1101/2020.03.03.20029983",
    #     "Tindale, L. et al. Transmission interval estimates suggest pre-symptomatic spread of COVID-19. Epidemiology (2020) doi:10.1101/2020.03.03.20029983",
    #     "Ganyani, T. et al. Estimating the generation interval for COVID-19 based on symptom onset data. Infectious Diseases (except HIV/AIDS) (2020) doi:10.1101/2020.03.05.20031815",
    #     "Ganyani, T. et al. Estimating the generation interval for COVID-19 based on symptom onset data. Infectious Diseases (except HIV/AIDS) (2020) doi:10.1101/2020.03.05.20031815",
    #     "Nishiura, H., Linton, N. M. & Akhmetzhanov, A. R. Serial interval of novel coronavirus (COVID-19) infections. Int. J. Infect. Dis. (2020) doi:10.1016/j.ijid.2020.02.060",
    #     "Li, Q. et al. Early Transmission Dynamics in Wuhan, China, of Novel Coronavirus-Infected Pneumonia. N. Engl. J. Med. (2020) doi:10.1056/NEJMoa2001316",
    #     "Cereda, D. et al. The early phase of the COVID-19 outbreak in Lombardy, Italy. arXiv [q-bio.PE] (2020)"
    #   )
    # )
    # 
    # unk = function(x)
    #   ifelse(is.na(x), "unk", x)
    # 
    # SItable1 = serialIntervals %>% mutate(
    #   `Mean SI\n(95% CrI) days` = paste0(
    #     mean_si_estimate,
    #     "\n(",
    #     unk(mean_si_estimate_low_ci),
    #     "-",
    #     unk(mean_si_estimate_high_ci),
    #     ")"
    #   ),
    #   `Std SI\n(95% CrI) days` = paste0(
    #     unk(std_si_estimate),
    #     "\n(",
    #     unk(std_si_estimate_low_ci),
    #     "-",
    #     unk(std_si_estimate_high_ci),
    #     ")"
    #   )
    # ) %>% dplyr::select(-contains("estimate")) %>% dplyr::select(
    #   `Reference` = source,
    #   `Mean SI\n(95% CrI) days`,
    #   `Std SI\n(95% CrI) days`,
    #   `N` = sample_size,
    #   `Population` = population
    # )
    # 
    # wtSIs = serialIntervals %>% summarise(
    #   mean_si = weighted.mean(mean_si_estimate, sample_size, na.rm = TRUE),
    #   min_mean_si = weighted.mean(mean_si_estimate_low_ci, sample_size, na.rm = TRUE),
    #   max_mean_si = weighted.mean(mean_si_estimate_high_ci, sample_size, na.rm = TRUE),
    #   std_si  = weighted.mean(ifelse(is.na(
    #     std_si_estimate_low_ci
    #   ), NA, 1) * std_si_estimate, sample_size, na.rm = TRUE),
    #   min_std_si  = weighted.mean(std_si_estimate_low_ci, sample_size, na.rm = TRUE),
    #   max_std_si  = weighted.mean(std_si_estimate_high_ci, sample_size, na.rm = TRUE)
    #   #total = sum(sample_size)
    # ) %>% mutate(
    #   std_mean_si = (max_mean_si - min_mean_si) / 3.92,
    #   # TODO: fit gamma
    #   std_std_si = (max_std_si - min_std_si) / 3.92
    # )
    # 
    # config = make_config(
    #   list(
    #     si_parametric_distr = "G",
    #     mean_si = wtSIs$mean_si,
    #     std_mean_si = wtSIs$std_mean_si,
    #     min_mean_si = wtSIs$min_mean_si,
    #     max_mean_si = wtSIs$max_mean_si,
    #     std_si = wtSIs$std_si,
    #     std_std_si = wtSIs$std_si,
    #     min_std_si = wtSIs$min_std_si,
    #     max_std_si = wtSIs$max_std_si
    #   ),
    #   method = "uncertain_si"
    # )
    
    
    
    #---trainingTestingDataFrames---#########################################################################################################################################################################
    # Subset training country list to those countries that had enough cases (to calc R0) by the start point
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
        # res_uncertain_si <-
        #   wallinga_teunis(
        #     toCalcR0,
        #     method = "parametric_si",
        #     config = list(
        #       si_parametric_distr = "G",
        #       t_start = seq(1, nrow(toCalcR0) -
        #                       6),
        #       t_end = seq(7, nrow(toCalcR0)),
        #       mean_si = wtSIs$mean_si,
        #       std_mean_si = wtSIs$std_mean_si,
        #       min_mean_si = wtSIs$min_mean_si,
        #       max_mean_si = wtSIs$max_mean_si,
        #       std_si = wtSIs$std_si,
        #       std_std_si = wtSIs$std_si,
        #       min_std_si = wtSIs$min_std_si,
        #       max_std_si = wtSIs$max_std_si,
        #       n_sim = 100
        #     )
        #   )
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
      
      
      
      # training_subset_aligned$R0 <- NA
      # training_subset_aligned$R0[head(res_uncertain_si[["R"]]$`t_start`, 1):(tail(res_uncertain_si[["R"]]$`t_start`, 1))] <-
      #   res_uncertain_si[["R"]]$`Mean(R)`
      # # Autofill beginning R0s with first value
      # training_subset_aligned$R0[1:head(res_uncertain_si[["R"]]$`t_start`, 1)] <-
      #   mean(head(res_uncertain_si[["R"]]$`Mean(R)`, 1))
      # # Autofill ending R0s with linear estimation from last week of values
      # fitFrame <-
      #   as.data.frame(cbind(c(1:7), tail(res_uncertain_si[["R"]]$`Mean(R)`, 7)))
      # fit <- lm(V2 ~ V1, data = fitFrame)
      # fitPred <-
      #   as.data.frame(8:(8 + length((tail(res_uncertain_si[["R"]]$`t_start`, 1)):nrow(training_subset_aligned)
      #   )))
      # colnames(fitPred) <- c("V1")
      # training_subset_aligned$R0[(tail(res_uncertain_si[["R"]]$`t_start`, 1)):nrow(training_subset_aligned)] <-
      #   NA #predict.lm(fit,fitPred) #mean(tail(res_uncertain_si[["R"]]$`Mean(R)`,5))
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
      # Fix lags for non-updated NPIs
      # training_subset_aligned$Social_Distancing[is.na(training_subset_aligned$Social_Distancing)] <-
      #   tail(training_subset_aligned$Social_Distancing[!is.na(training_subset_aligned$Social_Distancing)], 1)
      # training_subset_aligned$Quaranting_Cases[is.na(training_subset_aligned$Quaranting_Cases)] <-
      #   tail(training_subset_aligned$Quaranting_Cases[!is.na(training_subset_aligned$Quaranting_Cases)], 1)
      # training_subset_aligned$Close_Border[is.na(training_subset_aligned$Close_Border)] <-
      #   tail(training_subset_aligned$Close_Border[!is.na(training_subset_aligned$Close_Border)], 1)
      # listToLag <-
      #   c("Social_Distancing", "Quaranting_Cases", "Close_Border")
      # for (npi in 1:length(listToLag)) {
      #   # Add 3 day lag factor
      #   training_subset_aligned[[paste0(listToLag[npi], "_Lag_03")]] <-
      #     lag(training_subset_aligned[[paste0(listToLag[npi])]], 3)
      #   training_subset_aligned[[paste0(listToLag[npi], "_Lag_03")]][1:3] <-
      #     mean(training_subset_aligned[[paste0(listToLag[npi])]][1:1])
      #   # Add 7 day lag factor
      #   training_subset_aligned[[paste0(listToLag[npi], "_Lag_07")]] <-
      #     lag(training_subset_aligned[[paste0(listToLag[npi])]], 7)
      #   training_subset_aligned[[paste0(listToLag[npi], "_Lag_07")]][1:7] <-
      #     mean(training_subset_aligned[[paste0(listToLag[npi])]][1:1])
      #   # Add 10 day lag factor
      #   training_subset_aligned[[paste0(listToLag[npi], "_lag_10")]] <-
      #     lag(training_subset_aligned[[paste0(listToLag[npi])]], 10)
      #   training_subset_aligned[[paste0(listToLag[npi], "_lag_10")]][1:10] <-
      #     mean(training_subset_aligned[[paste0(listToLag[npi])]][1:1])
      #   # Add 14 day lag factor
      #   training_subset_aligned[[paste0(listToLag[npi], "_lag_14")]] <-
      #     lag(training_subset_aligned[[paste0(listToLag[npi])]], 14)
      #   training_subset_aligned[[paste0(listToLag[npi], "_lag_14")]][1:14] <-
      #     mean(training_subset_aligned[[paste0(listToLag[npi])]][1:1])
      # }
      
      
      # plot.new()
      # plot(res_uncertain_si, legend = T)
      # mtext(
      #   training_countries[i],
      #   outer = TRUE,
      #   cex = 1,
      #   line = -.5
      # )
      
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
        # res_uncertain_si <-
        #   wallinga_teunis(
        #     toCalcR0,
        #     method = "parametric_si",
        #     config = list(
        #       si_parametric_distr = "G",
        #       t_start = seq(1, nrow(toCalcR0) -
        #                       6),
        #       t_end = seq(7, nrow(toCalcR0)),
        #       mean_si = wtSIs$mean_si,
        #       std_mean_si = wtSIs$std_mean_si,
        #       min_mean_si = wtSIs$min_mean_si,
        #       max_mean_si = wtSIs$max_mean_si,
        #       std_si = wtSIs$std_si,
        #       std_std_si = wtSIs$std_si,
        #       min_std_si = wtSIs$min_std_si,
        #       max_std_si = wtSIs$max_std_si,
        #       n_sim = 100
        #     )
        #   )
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
      
      
      
      # testing_subset_aligned$R0 <- NA
      # testing_subset_aligned$R0[head(res_uncertain_si[["R"]]$`t_start`, 1):(tail(res_uncertain_si[["R"]]$`t_start`, 1))] <-
      #   res_uncertain_si[["R"]]$`Mean(R)`
      # # Autofill beginning R0s with first value
      # testing_subset_aligned$R0[1:head(res_uncertain_si[["R"]]$`t_start`, 1)] <-
      #   mean(head(res_uncertain_si[["R"]]$`Mean(R)`, 1))
      # # Autofill ending R0s with linear estimation from last week of values
      # fitFrame <-
      #   as.data.frame(cbind(c(1:7), tail(res_uncertain_si[["R"]]$`Mean(R)`, 7)))
      # fit <- lm(V2 ~ V1, data = fitFrame)
      # fitPred <-
      #   as.data.frame(8:(8 + length((tail(res_uncertain_si[["R"]]$`t_start`, 1)):nrow(testing_subset_aligned)
      #   )))
      # colnames(fitPred) <- c("V1")
      # testing_subset_aligned$R0[(tail(res_uncertain_si[["R"]]$`t_start`, 1)):nrow(testing_subset_aligned)] <-
      #   NA #predict.lm(fit,fitPred) #mean(tail(res_uncertain_si[["R"]]$`Mean(R)`,5))
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
      # Fix lags for non-updated NPIs
      # testing_subset_aligned$Social_Distancing[is.na(testing_subset_aligned$Social_Distancing)] <-
      #   tail(testing_subset_aligned$Social_Distancing[!is.na(testing_subset_aligned$Social_Distancing)], 1)
      # testing_subset_aligned$Quaranting_Cases[is.na(testing_subset_aligned$Quaranting_Cases)] <-
      #   tail(testing_subset_aligned$Quaranting_Cases[!is.na(testing_subset_aligned$Quaranting_Cases)], 1)
      # testing_subset_aligned$Close_Border[is.na(testing_subset_aligned$Close_Border)] <-
      #   tail(testing_subset_aligned$Close_Border[!is.na(testing_subset_aligned$Close_Border)], 1)
      # listToLag <-
      #   c("Social_Distancing", "Quaranting_Cases", "Close_Border")
      # for (npi in 1:length(listToLag)) {
      #   # Add 3 day lag factor
      #   testing_subset_aligned[[paste0(listToLag[npi], "_Lag_03")]] <-
      #     lag(testing_subset_aligned[[paste0(listToLag[npi])]], 3)
      #   testing_subset_aligned[[paste0(listToLag[npi], "_Lag_03")]][1:3] <-
      #     mean(testing_subset_aligned[[paste0(listToLag[npi])]][1:1])
      #   # Add 7 day lag factor
      #   testing_subset_aligned[[paste0(listToLag[npi], "_Lag_07")]] <-
      #     lag(testing_subset_aligned[[paste0(listToLag[npi])]], 7)
      #   testing_subset_aligned[[paste0(listToLag[npi], "_Lag_07")]][1:7] <-
      #     mean(testing_subset_aligned[[paste0(listToLag[npi])]][1:1])
      #   # Add 10 day lag factor
      #   testing_subset_aligned[[paste0(listToLag[npi], "_lag_10")]] <-
      #     lag(testing_subset_aligned[[paste0(listToLag[npi])]], 10)
      #   testing_subset_aligned[[paste0(listToLag[npi], "_lag_10")]][1:10] <-
      #     mean(testing_subset_aligned[[paste0(listToLag[npi])]][1:1])
      #   # Add 14 day lag factor
      #   testing_subset_aligned[[paste0(listToLag[npi], "_lag_14")]] <-
      #     lag(testing_subset_aligned[[paste0(listToLag[npi])]], 14)
      #   testing_subset_aligned[[paste0(listToLag[npi], "_lag_14")]][1:14] <-
      #     mean(testing_subset_aligned[[paste0(listToLag[npi])]][1:1])
      # }
      
      
      # plot.new()
      # plot(res_uncertain_si, legend = T)
      # mtext(
      #   testing_countries[i],
      #   outer = TRUE,
      #   cex = 1,
      #   line = -.5
      # )
      
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
      mtry_best <- max(floor(ncol(x)/3), 1)
      
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
      
      # If the selection process between interpretation step and prediction step essentially eliminated all of the variables,
      # lets lower the nmj threshold (since there is no natural tuning of this parameter)
      if (nVarInterp <= 4) {
        print(
          "we have less than 4 variables in the interpretation set, so lets predict on all the variables from the interpretation step..."
        )
        results.vsurf$varselect.pred <-
          results.vsurf$varselect.interp
      } else{
        if (nVarPred <= 4 && nVarInterp > 4) {
          print("nmj = 1 gave me less than 4 variables to predict on, lets try nmj = 0.1 ...")
          results.vsurf_pred_redo <- VSURF_pred(
            x,
            y,
            na.action = na.omit,
            mtry = mtry_best,
            n_tree = number_trees,
            parallel = TRUE,
            verbose = TRUE,
            ncores = num_cores,
            nmj = 0.1,
            err.interp = results.vsurf$err.interp,
            varselect.interp = results.vsurf$varselect.interp
          )
          results.vsurf$varselect.pred <-
            results.vsurf_pred_redo$varselect.pred
          nVarPred <-
            length(colnames(training_ready_sub2[, results.vsurf$varselect.pred]))
          nmj_used = 0.1
        }
        # If we still have less than 4 variables lets lower nmj again
        if (nVarPred <= 4 && nVarInterp > 4) {
          print("nmj = 0.1 gave me less than 4 variables to predict on, lets try nmj = 0.01 ...")
          results.vsurf_pred_redo <- VSURF_pred(
            x,
            y,
            na.action = na.omit,
            mtry = mtry_best,
            n_tree = number_trees,
            parallel = TRUE,
            verbose = TRUE,
            ncores = num_cores,
            nmj = 0.01,
            err.interp = results.vsurf$err.interp,
            varselect.interp = results.vsurf$varselect.interp
          )
          results.vsurf$varselect.pred <-
            results.vsurf_pred_redo$varselect.pred
          nVarPred <-
            length(colnames(training_ready_sub2[, results.vsurf$varselect.pred]))
          nmj_used = 0.01
        }
        # If we still have less than 4 variables in the predict set, lets just use the interpretation variable set for the prediction model
        if (nVarPred <= 4 && nVarInterp > 4) {
          print("this shouldnt happen very often....")
          print(
            "nmj = 0.01 gave me less than 4 variables to predict on, so lets stop trying to adjust nmj and just predict on all the variables from the interpretation step"
          )
          results.vsurf$varselect.pred <-
            results.vsurf$varselect.interp
          nmj_used = 0
        }
      }
      
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
      # save(nmj_used, VSURF_thres_keepers, VSURF_interp_keepers, VSURF_pred_keepers, results.vsurf, results.vsurf.OG, file = paste0(gwd,"/InputData/",testing_country,"_VSURFkeepers_R0.Rdata"))
      save(
        nmj_used,
        VSURF_thres_keepers,
        VSURF_interp_keepers,
        VSURF_pred_keepers,
        results.vsurf,
        results.vsurf.OG,
        file = paste0(
          gwd,"/InputData/",
          "All_Countries",
          "_VSURFkeepers_R0.Rdata"
        )
      )
      
      
    }
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
    } else if (length(VSURF_pred_keepers) <= 1 &&
               length(VSURF_interp_keepers) > 1) {
      testing_ready_sub_vsurf_result = dplyr::select(testing_ready_sub2,
                                              c(outcomeVariable, VSURF_interp_keepers))
    } else if (length(VSURF_pred_keepers) <= 1 &&
               length(VSURF_interp_keepers) <= 1) {
      testing_ready_sub_vsurf_result = dplyr::select(testing_ready_sub2,
                                              c(outcomeVariable, VSURF_thres_keepers))
    }
    glimpse(testing_ready_sub_vsurf_result)
    
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
      data = training_ready_sub_vsurf_result,
      # method = 'ranger',
      method = 'rf',
      na.action = nasaction,
      keep.inbag = TRUE,
      replace = TRUE,
      # importance = "permutation", #***
      trControl = control
    )
    
    # # Pick the best mtry
    # x <-
    #   training_ready_sub_vsurf_result_varImp[complete.cases(training_ready_sub_vsurf_result_varImp), which(colnames(training_ready_sub_vsurf_result_varImp) %ni% outcomeVariable)]
    # y <-
    #   training_ready_sub_vsurf_result_varImp[complete.cases(training_ready_sub_vsurf_result_varImp), which(colnames(training_ready_sub_vsurf_result_varImp) %in% outcomeVariable)]
    # if (length(training_ready_sub_vsurf_result_varImp) > 2) {
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
    # } else{
    #   mtry_best <- 1
    # }
    
    # rf.mod.varImp should be loaded from   file = paste0("./InputData/",
    #                                                     "All_Countries",
    #                                                     "_VSURFkeepers_R0.Rdata")
    # rf.mod.varImp <- caret::train(
    #   mod_formula,
    #   data = training_ready_sub_vsurf_result_varImp,
    #   # method = 'ranger',
    #   method = 'rf',
    #   na.action = nasaction,
    #   keep.inbag = TRUE,
    #   replace = TRUE,
    #   # importance = "permutation", #***
    #   trControl = control
    # )
    # varImp(rf.mod.varImp)
    
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
