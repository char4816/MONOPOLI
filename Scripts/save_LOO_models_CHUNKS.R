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
'%ni%' <- Negate('%in%')

args <- commandArgs(trailingOnly = TRUE)
arraynum <- as.numeric(args[1])
ncores <- as.numeric(args[2])
print(paste0("Using ", ncores, " cores"))

data_clean <- read.csv("./InputData/ML_features_oxford.csv")
pairedList <- unique(paste0(data_clean$Country,":",data_clean$ISO3))
pairedList <- as.data.frame(pairedList)
pairedList <- pairedList %>% separate(pairedList, into=c("Country","ISO3"),sep=":")
Clist <- read.csv("countryList.txt", header=F, stringsAsFactors = F, sep="\t")
pairedList$Country <- as.character(pairedList$Country)
Clist$V1 <- as.character(Clist$V1)
Clist$V1[which(Clist$V1 %ni% pairedList$Country)]
pairedListSub <- subset(pairedList, pairedList$Country %in% Clist$V1)
testing_countriesList <- pairedListSub$ISO3

# testing_countriesList <- testing_countriesList[arraynum]

print("=======================")
print(testing_countriesList)
print("=======================")

# testing_countriesList <- testing_countriesList[62:length(testing_countriesList)]
length(testing_countriesList)
# length(Clist$V1) 
# testing_countriesList <- c("USA_CO")
# testing_countriesListdone <- c("USA_CO","USA_NY","BRA","SWE","ITA","ESP","GBR", "IND", "ZAF", "DEU", "USA_CA", "USA_TX", "ARG", "JPN")
# testing_countriesList <- testing_countriesList[which(testing_countriesList %ni% testing_countriesListdone)]

# cat(paste(shQuote(testing_countriesList, type="cmd"), collapse=", "))
# testing_countriesListA <- c("RUS", "FRA", "COL", "MEX", "PER", "CHL", "IRQ", "BGD", "IDN", "PHL", "UKR", "TUR", "BEL", "SAU", "PAK", "ANT", "ISR", "POL", "CZE", "CAN", "ROU", "MAR", "ECU", "NPL", "BOL", "CHE", "QAT", "PAN", "ARE", "PRT", "DOM", "KAZ", "EGY", "CRI", "GTM", "BLR", "HND", "VEN", "AUT", "LBN", "MDA", "HUN", "NGA", "JOR", "PRY", "IRL", "SGP", "KGZ", "KEN", "SVK", "GHA", "BGR", "BIH", "DNK", "HRV", "GRC", "SLV", "MYS", "SVN", "AUS", "KOR", "CMR", "NOR", "ZMB", "LUX", "SEN", "FIN", "NAM", "MOZ", "LTU", "UGA", "TJK", "AGO", "LKA", "HTI", "GAB", "JAM", "ZWE", "BWA", "TTO", "NIC", "LVA" 
#                             # "RWA", "EST", "THA", "MLI", "BLZ", "URY", "BEN", "BFA", "TGO", "YEM", "NZL", "NER", "VNM", "USA_AK", "USA_AL", "USA_AR", "USA_AZ", "USA_CT", "USA_DC", "USA_DE", "USA_FL", "USA_GA", "USA_HI", "USA_IA", "USA_ID", "USA_IL", "USA_IN", "USA_KS", "USA_KY", "USA_LA", "USA_MA", "USA_MD", "USA_ME", "USA_MI", "USA_MN", "USA_MO", "USA_MS", "USA_MT", "USA_NC", "USA_ND", "USA_NE", "USA_NH", "USA_NJ", "USA_NM", "USA_NV", "USA_OH", "USA_OK", "USA_OR", "USA_PA", "USA_RI", "USA_SC", "USA_SD", "USA_TN", "USA_UT", "USA_VA", "USA_VT", "USA_WA", "USA_WI", "USA_WV", "USA_WY"
#                             )
# testing_countriesListB <- c(# "RUS", "FRA", "COL", "MEX", "PER", "CHL", "IRQ", "BGD", "IDN", "PHL", "UKR", "TUR", "BEL", "SAU", "PAK", "ANT", "ISR", "POL", "CZE", "CAN", "ROU", "MAR", "ECU", "NPL", "BOL", "CHE", "QAT", "PAN", "ARE", "PRT", "DOM", "KAZ", "EGY", "CRI", "GTM", "BLR", "HND", "VEN", "AUT", "LBN", "MDA", "HUN", "NGA", "JOR", "PRY", "IRL", "SGP", "KGZ", "KEN", "SVK", "GHA", "BGR", "BIH", "DNK", "HRV", "GRC", "SLV", "MYS", "SVN", "AUS", "KOR", "CMR", "NOR", "ZMB", "LUX", "SEN", "FIN", "NAM", "MOZ", "LTU", "UGA", "TJK", "AGO", "LKA", "HTI", "GAB", "JAM", "ZWE", "BWA", "TTO", "NIC", "LVA", 
#                             "RWA", "EST", "THA", "MLI", "BLZ", "URY", "BEN", "BFA", "TGO", "YEM", "NZL", "NER", "VNM", "USA_AK", "USA_AL", "USA_AR", "USA_AZ", "USA_CT", "USA_DC", "USA_DE", "USA_FL", "USA_GA", "USA_HI", "USA_IA", "USA_ID", "USA_IL", "USA_IN", "USA_KS", "USA_KY", "USA_LA", "USA_MA", "USA_MD", "USA_ME", "USA_MI", "USA_MN", "USA_MO", "USA_MS", "USA_MT", "USA_NC", "USA_ND", "USA_NE", "USA_NH", "USA_NJ", "USA_NM", "USA_NV", "USA_OH", "USA_OK", "USA_OR", "USA_PA", "USA_RI", "USA_SC", "USA_SD", "USA_TN", "USA_UT", "USA_VA", "USA_VT", "USA_WA", "USA_WI", "USA_WV", "USA_WY"
#                             )

# testing_countriesList <- testing_countriesListA
# testing_countriesList <- c(testing_countriesListdone, testing_countriesListB)

# testing_countriesList <- c("JPN")

# for (cc in 1:length(testing_countriesList)) {
  
  cc <- arraynum

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
  # num_cores = detectCores() - 1
  num_cores = ncores - 1
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
    training_countries <- training_countries_all
    # make sure testing country comes last in the list to not mess up if statement at end of loop
    training_countries <- c(training_countries_all[which(training_countries_all != testing_country)],testing_country)
    
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
      # for (i in c(c(1:2),c(116, 145, 154, 115))) {
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
      melt_plottingDF_confirmed <- subset(melt_plottingDF, variable == "confirmed")
      melt_plottingDF_MA <- subset(melt_plottingDF, variable == "movingAverage")
      
      # Plot cases
      # gg <- ggplot(melt_plottingDF, aes(x = date, y = value, color = variable)) +
      # # gg <- ggplot() +
      #   # geom_line(size=1.5) +
      #   geom_point(data = melt_plottingDF_confirmed, aes(x = date, y = value, color = variable), size=1.5) +
      #   geom_line(data = melt_plottingDF_confirmed, aes(x = date, y = value, color = variable), size=0.8, linetype = "dotted") +
      #   geom_line(data = melt_plottingDF_MA, aes(x = date, y = value, color = variable), size=1.4, linetype = "solid") +
      #   ggtitle(paste0("COVID-19 in ", training_countries[i]))+
      #   theme(legend.position = "top") +
      #   labs(colour = NULL) +
      #   xlab(NULL) +
      #   scale_x_date(
      #     date_breaks = "2 week",
      #     date_labels =  "%b %d",
      #     limits = c(min(melt_plottingDF$date), max(melt_plottingDF$date))
      #   ) +
      #   # ggtitle(testing_country)+
      #   theme(text = element_text(size = 14)) +
      #   # ylim(0,max(seirScaledPlotDF$R)*N)+
      #   scale_y_continuous(
      #     label = comma,
      #     # Features of the first axis
      #     name = "Population"
      #   )+
      #   scale_x_date(date_breaks = "3 week", date_labels =  "%b %d") +
      #   theme(legend.title = element_text(size = 14)) +
      #   theme(
      #     axis.text.x = element_text(
      #       color = "black",
      #       size = 13,
      #       angle = 60,
      #       hjust = 1,
      #       vjust = 1
      #     ),
      #     axis.text.y = element_text(
      #       color = "black",
      #       size = 13,
      #       angle = 0
      #     ),
      #     axis.title.x = element_text(
      #       color = "black",
      #       size = 13,
      #       angle = 0
      #     ),
      #     axis.title.y = element_text(
      #       color = "black",
      #       size = 13,
      #       angle = 90
      #     )
      #   ) +
      #   # scale_x_continuous(breaks=seq(1, 10, 1))+
      #   # scale_colour_manual(values=predictColor)+
      #   theme(legend.text = element_text(size = 16)) +
      #   theme(
      #     panel.grid.major = element_blank(),
      #     panel.grid.minor = element_blank(),
      #     panel.background = element_blank(),
      #     axis.line = element_line(colour = "black")
      #   ) +
      #   scale_color_manual(values = c("firebrick2", "dodgerblue3"), breaks = c("confirmed", "movingAverage"), labels = c("New Daily Cases", paste0(window,"-Day Moving Average")), name=NULL)
      # # scale_color_manual(values = c("orange", "tomato3","purple"), breaks = c("confirmed", "movingAverage", "derived_I_curve"), labels = c("New Daily Cases", "Smoothed Moving Average", "Currently Infected"), name=NULL)
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
      
      if (RunWT_R_flag == T) {
        
        # incid <- as.numeric(toCalcR0$I)
        # names(incid) <- as.Date(toCalcR0$dates)
        # empez <- as.Date(toCalcR0$dates[1])
        # fin <- as.Date(toCalcR0$dates[nrow(toCalcR0)])
        # # mGT<-generation.time("gamma", c(wtSIs$mean_si, wtSIs$std_si))
        # # https://epiforecasts.io/covid/methods.html
        # # generation time with a mean of 3.6 days (sd: 0.7 days) and sd of 3 days (sd 0.8 days)
        # # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7201952/
        # # mean of 5.2 days and a standard deviation (SD) of 2.8 days
        # # https://wwwnc.cdc.gov/eid/article/26/6/20-0357_article
        # # mean interval was 3.96 days (95% CI 3.53–4.39 days), SD 4.75 days (95% CI 4.46–5.07 days)
        # # https://science.sciencemag.org/content/368/6491/eabb6936
        # # The distribution had a median of 5.0 days and standard deviation of 1.9 days. [...]  The distribution is best described by a Weibull distribution
        # mGT <- generation.time("weibull", c(5.0, 1.9))
        # incid[incid == 0] <- 1
        # estR0 <- est.R0.TD(incid, mGT, begin=empez, end=fin, nsim=1000)
        # ## An interesting way to look at these results is to agregate initial data by longest time unit,
        # ## such as weekly incidence. This gives a global overview of the epidemic.
        # # estR0.weekly <- smooth.Rt(estR0, 7)
        # # estR0.weekly$R
        # plot(estR0)
        # # plot(estR0.weekly)
        
        boolFalse<-F
        # window = 7
        while(boolFalse==F)
        {
          tryCatch({
            myRt <- rtlive(data_clean_train = data_clean_train, state_selected = training_countries[i], window_smooth = window, oldData = F)
            Rt_output <- myRt[[1]]
            Rt_outplot <- myRt[[2]]
            print(Rt_outplot)
            boolFalse<-T
          },error=function(e){
          },finally={})
          window = window+1
          if(window > 22){
            stop(paste0(training_countries[i]," seems to be having trouble with R(t) calculations"))
          }
        }
        
        # myRt <- rtlive(data_clean_train = data_clean_train, state_selected = training_countries[i])
        # Rt_output <- myRt[[1]]
        # Rt_outplot <- myRt[[2]]
        # print(Rt_outplot)
        
        save(Rt_output, Rt_outplot,
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

      # finalR_WT <- Rt_output[, c("date","r_t_most_likely")]
      # colnames(finalR_WT) <- c("date", "R0")

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
      if(is.na(training_subset_aligned[[paste0("R0")]][1])){
        training_subset_aligned[[paste0("R0")]][1] <- training_subset_aligned[[paste0("R0")]][2]
      }
      # c(2,5,8,11,14)
      for (npi in 1:length(listToLag)) {
        # Add 1 day lag factor for R0
        training_subset_aligned[[paste0(listToLag[npi], "_lag_1")]] <-
          lag(training_subset_aligned[[paste0(listToLag[npi])]], 1)
        training_subset_aligned[[paste0(listToLag[npi], "_lag_1")]][1:1] <-
          mean(training_subset_aligned[[paste0(listToLag[npi])]][1:3])
        # Add 2 day lag factor for R0
        training_subset_aligned[[paste0(listToLag[npi], "_lag_2")]] <-
          lag(training_subset_aligned[[paste0(listToLag[npi])]], 2)
        training_subset_aligned[[paste0(listToLag[npi], "_lag_2")]][1:2] <-
          mean(training_subset_aligned[[paste0(listToLag[npi])]][1:3])
        # Add 3 day lag factor for R0
        training_subset_aligned[[paste0(listToLag[npi], "_lag_3")]] <-
          lag(training_subset_aligned[[paste0(listToLag[npi])]], 3)
        training_subset_aligned[[paste0(listToLag[npi], "_lag_3")]][1:3] <-
          mean(training_subset_aligned[[paste0(listToLag[npi])]][1:3])
        # Add 4 day lag factor for R0
        training_subset_aligned[[paste0(listToLag[npi], "_lag_4")]] <-
          lag(training_subset_aligned[[paste0(listToLag[npi])]], 4)
        training_subset_aligned[[paste0(listToLag[npi], "_lag_4")]][1:4] <-
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
      
      if (training_countries[i] == testing_country) {
        testing_subset_aligned <- training_subset_aligned
        # Fill in the blanks
        to_replace_raw <- 
          na.approx(
            testing_subset_aligned[, grep("StringencyIndexForDisplay|GovernmentResponseIndexForDisplay|ContainmentHealthIndexForDisplay|EconomicSupportIndexForDisplay|C2_Workplace.closing|openness_risk|Google",colnames(testing_subset_aligned))]
          )
        s1 <- sum(is.na(testing_subset_aligned[, grep("StringencyIndexForDisplay|GovernmentResponseIndexForDisplay|ContainmentHealthIndexForDisplay|EconomicSupportIndexForDisplay|C2_Workplace.closing|openness_risk|Google",colnames(testing_subset_aligned))]))
        s2 <- sum(is.na(to_replace_raw))
        na_FilledWarning <- paste0("Lineraly interpolated ", s1 - s2, " NA entries across ", ncol(to_replace_raw), " columns")
        testing_subset_aligned[, grep("StringencyIndexForDisplay|GovernmentResponseIndexForDisplay|ContainmentHealthIndexForDisplay|EconomicSupportIndexForDisplay|C2_Workplace.closing|openness_risk|Google",colnames(testing_subset_aligned))] <- 
          to_replace_raw

        # testing_subset_aligned
        tmp <- testing_subset_aligned[rep(1, forecastingTime), ]
        tmp[, grep(
          "cum|StringencyIndexForDisplay|GovernmentResponseIndexForDisplay|ContainmentHealthIndexForDisplay|EconomicSupportIndexForDisplay|C2_Workplace.closing|openness_risk|Google|R0|date|confirmed",
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
      }else{
        if (i == 1) {
          training_ready <- training_subset_aligned
        }else{
          training_ready <-
            as.data.frame(rbind(training_ready, training_subset_aligned))
        }
      }
      
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
      dplyr::select(-contains("H6")) %>%
      dplyr::select(-contains("H7")) %>%
      dplyr::select(-contains("H8")) %>%
      dplyr::select(-contains("V1")) %>%
      dplyr::select(-contains("V2")) %>%
      dplyr::select(-contains("V3")) %>%
      dplyr::select(-contains("V4")) %>%
      dplyr::select(-contains("cases_controlled")) %>%
      dplyr::select(-contains("community_understanding")) %>%
      dplyr::select(-contains("endemic_factor")) %>%
      dplyr::select(-contains("manage_imported_cases")) %>%
      dplyr::select(-contains("test_and_trace")) %>%
      dplyr::select(-contains("_lag_2")) %>%
      dplyr::select(-contains("_lag_4")) %>%
      dplyr::select(-contains("_lag_5")) %>%
      dplyr::select(-contains("_lag_6")) %>%
      dplyr::select(-contains("_lag_8")) %>%
      dplyr::select(-contains("_lag_9")) %>%
      dplyr::select(-contains("_lag_10")) %>%
      dplyr::select(-contains("_lag_11")) %>%
      dplyr::select(-contains("_lag_12")) %>%
      dplyr::select(-contains("_lag_13")) %>%
      dplyr::select(
        -c(
          confirmed_cum,
          # date,
          Country,
          ISO3,
          confirmed,
          death,
          FullName,
          recovered,
          time,
          # R0_lag_1,
          # R0_lag_2,
          # R0_lag_3,
          Jurisdiction,
          MajorityVaccinated,
          Tropical,
          PopulationVaccinated,
          EconomicSupportIndex
        )
      ) %>%
      mutate_if(is.factor, as.character) %>%
      mutate_if(is.character, as.numeric) %>%
      mutate_if(is.integer, as.numeric)
    
    # filter testing data
    testing_ready_sub2 <- testing_ready %>%
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
      dplyr::select(-contains("H6")) %>%
      dplyr::select(-contains("H7")) %>%
      dplyr::select(-contains("H8")) %>%
      dplyr::select(-contains("V1")) %>%
      dplyr::select(-contains("V2")) %>%
      dplyr::select(-contains("V3")) %>%
      dplyr::select(-contains("V4")) %>%
      dplyr::select(-contains("cases_controlled")) %>%
      dplyr::select(-contains("community_understanding")) %>%
      dplyr::select(-contains("endemic_factor")) %>%
      dplyr::select(-contains("manage_imported_cases")) %>%
      dplyr::select(-contains("test_and_trace")) %>%
      dplyr::select(-contains("_lag_2")) %>%
      dplyr::select(-contains("_lag_4")) %>%
      dplyr::select(-contains("_lag_5")) %>%
      dplyr::select(-contains("_lag_6")) %>%
      dplyr::select(-contains("_lag_8")) %>%
      dplyr::select(-contains("_lag_9")) %>%
      dplyr::select(-contains("_lag_10")) %>%
      dplyr::select(-contains("_lag_11")) %>%
      dplyr::select(-contains("_lag_12")) %>%
      dplyr::select(-contains("_lag_13")) %>%
      dplyr::select(
        -c(
          confirmed_cum,
          # date,
          Country,
          ISO3,
          confirmed,
          death,
          FullName,
          recovered,
          time,
          # R0_lag_1,
          # R0_lag_2,
          # R0_lag_3,
          Jurisdiction,
          MajorityVaccinated,
          Tropical,
          PopulationVaccinated,
          EconomicSupportIndex
        )
      ) %>%
      mutate_if(is.factor, as.character) %>%
      mutate_if(is.character, as.numeric) %>%
      mutate_if(is.integer, as.numeric)
    
    
    # subset to only include data from before the target date
    dim(training_ready_sub2)
    training_ready_sub2 <- subset(training_ready_sub2, date < as.Date("2020-11-03"))
    dim(training_ready_sub2)
    training_ready_sub2 <- training_ready_sub2 %>% dplyr::select(-c(date))
    
    dim(testing_ready_sub2)
    testing_ready_sub2 <- subset(testing_ready_sub2, date < as.Date("2020-11-03"))
    dim(testing_ready_sub2)
    testing_ready_sub2 <- testing_ready_sub2 %>% dplyr::select(-c(date))
    
    
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
    # if (length(VSURF_pred_keepers) > 1) {
    #   training_ready_sub_vsurf_result = dplyr::select(training_ready_sub2,
    #                                            c(outcomeVariable, VSURF_pred_keepers))
    #   training_ready_sub_vsurf_result_varImp = dplyr::select(training_ready_sub2,
    #                                                   c(outcomeVariable, VSURF_interp_keepers))
    # } else if (length(VSURF_pred_keepers) <= 1 &&
    #            length(VSURF_interp_keepers) > 1) {
    #   training_ready_sub_vsurf_result = dplyr::select(training_ready_sub2,
    #                                            c(outcomeVariable, VSURF_interp_keepers))
    #   training_ready_sub_vsurf_result_varImp = dplyr::select(training_ready_sub2,
    #                                                   c(outcomeVariable, VSURF_interp_keepers))
    # } else if (length(VSURF_pred_keepers) <= 1 &&
    #            length(VSURF_interp_keepers) <= 1) {
    #   training_ready_sub_vsurf_result = dplyr::select(training_ready_sub2,
    #                                            c(outcomeVariable, VSURF_thres_keepers))
    #   training_ready_sub_vsurf_result_varImp = dplyr::select(training_ready_sub2,
    #                                                   c(outcomeVariable, VSURF_thres_keepers))
    # }
    # glimpse(training_ready_sub_vsurf_result)
    # glimpse(training_ready_sub_vsurf_result_varImp)
    
    training_ready_sub_vsurf_result = dplyr::select(training_ready_sub2,
                                                    c(outcomeVariable, VSURF_interp_keepers))
    training_ready_sub_vsurf_result_varImp = dplyr::select(training_ready_sub2,
                                                           c(outcomeVariable, VSURF_interp_keepers))
    
    # testing dataframe with reduced number of variables
    # if (length(VSURF_pred_keepers) > 1) {
    #   testing_ready_sub_vsurf_result = dplyr::select(testing_ready_sub2,
    #                                                   c(outcomeVariable, VSURF_pred_keepers))
    #   testing_ready_sub_vsurf_result_varImp = dplyr::select(testing_ready_sub2,
    #                                                          c(outcomeVariable, VSURF_interp_keepers))
    # } else if (length(VSURF_pred_keepers) <= 1 &&
    #            length(VSURF_interp_keepers) > 1) {
    #   testing_ready_sub_vsurf_result = dplyr::select(testing_ready_sub2,
    #                                                   c(outcomeVariable, VSURF_interp_keepers))
    #   testing_ready_sub_vsurf_result_varImp = dplyr::select(testing_ready_sub2,
    #                                                          c(outcomeVariable, VSURF_interp_keepers))
    # } else if (length(VSURF_pred_keepers) <= 1 &&
    #            length(VSURF_interp_keepers) <= 1) {
    #   testing_ready_sub_vsurf_result = dplyr::select(testing_ready_sub2,
    #                                                   c(outcomeVariable, VSURF_thres_keepers))
    #   testing_ready_sub_vsurf_result_varImp = dplyr::select(testing_ready_sub2,
    #                                                          c(outcomeVariable, VSURF_thres_keepers))
    # }
    # glimpse(testing_ready_sub_vsurf_result)
    # glimpse(testing_ready_sub_vsurf_result_varImp)
    
    testing_ready_sub_vsurf_result = dplyr::select(testing_ready_sub2,
                                                   c(outcomeVariable, VSURF_interp_keepers))
    testing_ready_sub_vsurf_result_varImp = dplyr::select(testing_ready_sub2,
                                                          c(outcomeVariable, VSURF_interp_keepers))
    
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
    
    print(names(training_ready_sub_vsurf_result))
    
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
    print(na_FilledWarning)
    rm(list=setdiff(ls(), c("cc", "testing_countriesList")))
# }

