# Tetsing the model on specified contry

# Written by:  Chris H Arehart
# Written on: March 24th, 2020
# Updated on: June 20th, 2020

# library(shiny)
# library(tidyverse)
# library(EpiEstim)
# library(zoo)
# library(VSURF)
# library(caret)
# library(doParallel)
# library(ggplot2)
# library(randomForest)
# library(ranger)
# library(viridis)
# library(RColorBrewer)
# library(randomcoloR)
# library(reshape2)
# # devtools::install_github("delabj/ggCyberPunk")
# library(ggCyberPunk)
# library(corrplot)
# #
# library(gridExtra)
# library(grid)
# library(ggpubr)
# library(ggridges)
# library(quantregForest)
# library(scales)
# library(ggrepel)
# library(cowplot)
# library(deSolve)
# library(reshape2)
# library(shinyjs)
# library(plotly)
# library(data.table)
# # # # # 
# # # # # #---dataSetup---#########################################################################################################################################################################
# 
# testing_countriesList <- c("USA")
# dateAlongCurve <- as.Date("2020-06-27")
# # # # # Choose an NPI profile from "Status Quo", "Pre-COVID-NPI", "Extreme-NPI", "Custom-NPI"
# NPIprofile <- c("Status Quo", "Pre-COVID-NPI", "Extreme-NPI", "Custom-NPI")[4]
# # # # # In general, -80 -> +80 should bound these percentages
# Google_Retail_recreation_Custom <- -5
# Google_Grocery_pharmacy_Custom <- -1
# Google_Parks_Custom <- +56
# Google_Transit_stations_Custom <- -5
# Google_Workplaces_Custom <- -5
# Google_Residential_Custom <- +5
# StringencyIndexForDisplay_Custom <- 10
# GovernmentResponseIndexForDisplay_Custom <- 25
# ContainmentHealthIndexForDisplay_Custom <- 30
# EconomicSupportIndexForDisplay_Custom <- 30
# chosen_initialInfected <- 30000
# chosen_initialExposed <- 30000
# recoveryTime <- 14.5
# latencyTime <- 5.2


# for(cc in 1:length(testing_countriesList)){
  cc-1
  testing_country <- testing_countriesList[cc]
  load(paste0("./InputData/",testing_country,"_LOO.RData"))

  
  
  #---Scenarios---#########################################################################################################################################################################
  # for (timeChop in as.Date(dateAlongCurve)) {
  timeChop = as.Date(dateAlongCurve)
    if(as.Date(timeChop) < as.Date(D0)){
      timeChop = as.Date(D0)
    }
    if(as.Date(timeChop) > as.Date(D4)){
      timeChop = as.Date(D4)
    }
    # timeChop <- as.Date("2020-06-13")
    # for(timeChop in as.Date(c(D3))){
    # setting the NPIflag2 to "lastNPI" is our method of saying that we want to fill all the NAs in the forecasting period with the last empirical time points' NPI values
    # NPIflag2 <- "lastNPI"
    # scenario <- 1
    for (scenario in c(1:4)) {
      testing_ready_pred <- testing_ready_sub_vsurf_result
      DFtoBuildLags <-
        testing_ready_sub2[, c(
          "Google_Retail_recreation",
          "Google_Grocery_pharmacy",
          "Google_Parks",
          "Google_Transit_stations",
          "Google_Workplaces",
          "Google_Residential",
          "StringencyIndexForDisplay",
          "GovernmentResponseIndexForDisplay",
          "ContainmentHealthIndexForDisplay",
          "EconomicSupportIndexForDisplay"
        )]
      breakerA <-
        which(is.na(DFtoBuildLags$Google_Retail_recreation))[1]
      breakerB <- which(testing_ready$date == timeChop)
      breaker <-
        c(breakerA, breakerB)[which.min(c(breakerA, breakerB))]
      # breaker <- nrow(testing_ready_pred)-forecastingTime+1-addToBreaker #breaker for R0
      breaker2 <-
        nrow(testing_ready_pred) - forecastingTime + 1 #breaker for all of the NPIs for our cutoff date
      
      NPInames <-
        names(testing_ready_pred)[grep(
          "StringencyIndexForDisplay|GovernmentResponseIndexForDisplay|ContainmentHealthIndexForDisplay|EconomicSupportIndexForDisplay|Google",
          names(testing_ready_pred)
        )]
      NPInamesMain <- NPInames[grep("lag|Lag", NPInames, invert = T)]
      NPInamesLag <- NPInames[grep("lag|Lag", NPInames, invert = F)]
      
      # Scenario 1: The country keeps doing what they are currently doing
      DFtoBuildLagsScenario1 <- DFtoBuildLags

      Google_Retail_recreation_SQ <-
        mean(tail(DFtoBuildLagsScenario1$Google_Retail_recreation[!is.na(DFtoBuildLagsScenario1$Google_Retail_recreation)], 5))
      Google_Grocery_pharmacy_SQ <-
        mean(tail(DFtoBuildLagsScenario1$Google_Grocery_pharmacy[!is.na(DFtoBuildLagsScenario1$Google_Grocery_pharmacy)], 5))
      Google_Parks_SQ <-
        mean(tail(DFtoBuildLagsScenario1$Google_Parks[!is.na(DFtoBuildLagsScenario1$Google_Parks)], 5))
      Google_Transit_stations_SQ <-
        mean(tail(DFtoBuildLagsScenario1$Google_Transit_stations[!is.na(DFtoBuildLagsScenario1$Google_Transit_stations)], 5))
      Google_Workplaces_SQ <-
        mean(tail(DFtoBuildLagsScenario1$Google_Workplaces[!is.na(DFtoBuildLagsScenario1$Google_Workplaces)], 5))
      Google_Residential_SQ <-
        mean(tail(DFtoBuildLagsScenario1$Google_Residential[!is.na(DFtoBuildLagsScenario1$Google_Residential)], 5))
      StringencyIndexForDisplay_SQ <-
        mean(tail(DFtoBuildLagsScenario1$StringencyIndexForDisplay[!is.na(DFtoBuildLagsScenario1$StringencyIndexForDisplay)], 1))
      GovernmentResponseIndexForDisplay_SQ <-
        mean(tail(DFtoBuildLagsScenario1$GovernmentResponseIndexForDisplay[!is.na(DFtoBuildLagsScenario1$GovernmentResponseIndexForDisplay)], 1))
      ContainmentHealthIndexForDisplay_SQ <-
        mean(tail(DFtoBuildLagsScenario1$ContainmentHealthIndexForDisplay[!is.na(DFtoBuildLagsScenario1$ContainmentHealthIndexForDisplay)], 1))
      EconomicSupportIndexForDisplay_SQ <-
        mean(tail(DFtoBuildLagsScenario1$EconomicSupportIndexForDisplay[!is.na(DFtoBuildLagsScenario1$EconomicSupportIndexForDisplay)], 1))
      DFtoBuildLagsScenario1 <- DFtoBuildLags
      DFtoBuildLagsScenario1$Google_Retail_recreation[breaker:nrow(DFtoBuildLagsScenario1)] <-
        as.integer(
          runif(
            length(breaker:nrow(DFtoBuildLagsScenario1)),
            min = Google_Retail_recreation_SQ - 5,
            max = Google_Retail_recreation_SQ + 5
          )
        )
      DFtoBuildLagsScenario1$Google_Grocery_pharmacy[breaker:nrow(DFtoBuildLagsScenario1)] <-
        as.integer(
          runif(
            length(breaker:nrow(DFtoBuildLagsScenario1)),
            min = Google_Grocery_pharmacy_SQ - 5,
            max = Google_Grocery_pharmacy_SQ + 5
          )
        )
      DFtoBuildLagsScenario1$Google_Parks[breaker:nrow(DFtoBuildLagsScenario1)] <-
        as.integer(runif(
          length(breaker:nrow(DFtoBuildLagsScenario1)),
          min = Google_Parks_SQ - 5,
          max = Google_Parks_SQ + 5
        ))
      DFtoBuildLagsScenario1$Google_Transit_stations[breaker:nrow(DFtoBuildLagsScenario1)] <-
        as.integer(
          runif(
            length(breaker:nrow(DFtoBuildLagsScenario1)),
            min = Google_Transit_stations_SQ - 5,
            max = Google_Transit_stations_SQ + 5
          )
        )
      DFtoBuildLagsScenario1$Google_Workplaces[breaker:nrow(DFtoBuildLagsScenario1)] <-
        as.integer(runif(
          length(breaker:nrow(DFtoBuildLagsScenario1)),
          min = Google_Workplaces_SQ - 5,
          max = Google_Workplaces_SQ + 5
        ))
      DFtoBuildLagsScenario1$Google_Residential[breaker:nrow(DFtoBuildLagsScenario1)] <-
        as.integer(runif(
          length(breaker:nrow(DFtoBuildLagsScenario1)),
          min = Google_Residential_SQ - 5,
          max = Google_Residential_SQ + 5
        ))
      DFtoBuildLagsScenario1$StringencyIndexForDisplay[breaker:nrow(DFtoBuildLagsScenario1)] <-
        StringencyIndexForDisplay_SQ
      DFtoBuildLagsScenario1$GovernmentResponseIndexForDisplay[breaker:nrow(DFtoBuildLagsScenario1)] <-
        GovernmentResponseIndexForDisplay_SQ
      DFtoBuildLagsScenario1$ContainmentHealthIndexForDisplay[breaker:nrow(DFtoBuildLagsScenario1)] <-
        ContainmentHealthIndexForDisplay_SQ
      DFtoBuildLagsScenario1$EconomicSupportIndexForDisplay[breaker:nrow(DFtoBuildLagsScenario1)] <-
        EconomicSupportIndexForDisplay_SQ
      
      # Scenario 2: The country opens things up like they were Pre-COVID
      DFtoBuildLagsScenario2 <- DFtoBuildLags
      DFtoBuildLagsScenario2$Google_Retail_recreation[breaker:nrow(DFtoBuildLagsScenario2)] <-
        as.integer(runif(length(
          breaker:nrow(DFtoBuildLagsScenario2)
        ), min = -5, max = 5))
      DFtoBuildLagsScenario2$Google_Grocery_pharmacy[breaker:nrow(DFtoBuildLagsScenario2)] <-
        as.integer(runif(length(
          breaker:nrow(DFtoBuildLagsScenario2)
        ), min = -5, max = 5))
      DFtoBuildLagsScenario2$Google_Parks[breaker:nrow(DFtoBuildLagsScenario2)] <-
        as.integer(runif(length(
          breaker:nrow(DFtoBuildLagsScenario2)
        ), min = -5, max = 5))
      DFtoBuildLagsScenario2$Google_Transit_stations[breaker:nrow(DFtoBuildLagsScenario2)] <-
        as.integer(runif(length(
          breaker:nrow(DFtoBuildLagsScenario2)
        ), min = -5, max = 5))
      DFtoBuildLagsScenario2$Google_Workplaces[breaker:nrow(DFtoBuildLagsScenario2)] <-
        as.integer(runif(length(
          breaker:nrow(DFtoBuildLagsScenario2)
        ), min = -5, max = 5))
      DFtoBuildLagsScenario2$Google_Residential[breaker:nrow(DFtoBuildLagsScenario2)] <-
        as.integer(runif(length(
          breaker:nrow(DFtoBuildLagsScenario2)
        ), min = -5, max = 5))
      DFtoBuildLagsScenario2$StringencyIndexForDisplay[breaker:nrow(DFtoBuildLagsScenario2)] <-
        0
      DFtoBuildLagsScenario2$GovernmentResponseIndexForDisplay[breaker:nrow(DFtoBuildLagsScenario2)] <-
        0
      DFtoBuildLagsScenario2$ContainmentHealthIndexForDisplay[breaker:nrow(DFtoBuildLagsScenario2)] <-
        0
      DFtoBuildLagsScenario2$EconomicSupportIndexForDisplay[breaker:nrow(DFtoBuildLagsScenario2)] <-
        0
      
      # Scenario 3: The country closes everything down to it's most extreme
      # Empirically upper thresholds for google data are 80-90% and then for residential google is around 40-50%
      # Intervention measures are set to their max values
      DFtoBuildLagsScenario3 <- DFtoBuildLags
      DFtoBuildLagsScenario3$Google_Retail_recreation[breaker:nrow(DFtoBuildLagsScenario3)] <-
        as.integer(runif(
          length(breaker:nrow(DFtoBuildLagsScenario3)),
          min = -90,
          max = -80
        ))
      DFtoBuildLagsScenario3$Google_Grocery_pharmacy[breaker:nrow(DFtoBuildLagsScenario3)] <-
        as.integer(runif(
          length(breaker:nrow(DFtoBuildLagsScenario3)),
          min = -90,
          max = -80
        ))
      DFtoBuildLagsScenario3$Google_Parks[breaker:nrow(DFtoBuildLagsScenario3)] <-
        as.integer(runif(
          length(breaker:nrow(DFtoBuildLagsScenario3)),
          min = -90,
          max = -80
        ))
      DFtoBuildLagsScenario3$Google_Transit_stations[breaker:nrow(DFtoBuildLagsScenario3)] <-
        as.integer(runif(
          length(breaker:nrow(DFtoBuildLagsScenario3)),
          min = -90,
          max = -80
        ))
      DFtoBuildLagsScenario3$Google_Workplaces[breaker:nrow(DFtoBuildLagsScenario3)] <-
        as.integer(runif(
          length(breaker:nrow(DFtoBuildLagsScenario3)),
          min = -90,
          max = -80
        ))
      DFtoBuildLagsScenario3$Google_Residential[breaker:nrow(DFtoBuildLagsScenario3)] <-
        as.integer(runif(length(
          breaker:nrow(DFtoBuildLagsScenario3)
        ), min = 40, max = 50))
      DFtoBuildLagsScenario3$StringencyIndexForDisplay[breaker:nrow(DFtoBuildLagsScenario3)] <-
        100
      DFtoBuildLagsScenario3$GovernmentResponseIndexForDisplay[breaker:nrow(DFtoBuildLagsScenario3)] <-
        100
      DFtoBuildLagsScenario3$ContainmentHealthIndexForDisplay[breaker:nrow(DFtoBuildLagsScenario3)] <-
        100
      DFtoBuildLagsScenario3$EconomicSupportIndexForDisplay[breaker:nrow(DFtoBuildLagsScenario3)] <-
        100
      
      # Scenario 4: Custom, choose your own values
      DFtoBuildLagsScenario4 <- DFtoBuildLags
      DFtoBuildLagsScenario4$Google_Retail_recreation[breaker:nrow(DFtoBuildLagsScenario4)] <-
        as.integer(
          runif(
            length(breaker:nrow(DFtoBuildLagsScenario4)),
            min = Google_Retail_recreation_Custom - 5,
            max = Google_Retail_recreation_Custom + 5
          )
        )
      DFtoBuildLagsScenario4$Google_Grocery_pharmacy[breaker:nrow(DFtoBuildLagsScenario4)] <-
        as.integer(
          runif(
            length(breaker:nrow(DFtoBuildLagsScenario4)),
            min = Google_Grocery_pharmacy_Custom - 5,
            max = Google_Grocery_pharmacy_Custom + 5
          )
        )
      DFtoBuildLagsScenario4$Google_Parks[breaker:nrow(DFtoBuildLagsScenario4)] <-
        as.integer(runif(
          length(breaker:nrow(DFtoBuildLagsScenario4)),
          min = Google_Parks_Custom - 5,
          max = Google_Parks_Custom + 5
        ))
      DFtoBuildLagsScenario4$Google_Transit_stations[breaker:nrow(DFtoBuildLagsScenario4)] <-
        as.integer(
          runif(
            length(breaker:nrow(DFtoBuildLagsScenario4)),
            min = Google_Transit_stations_Custom - 5,
            max = Google_Transit_stations_Custom + 5
          )
        )
      DFtoBuildLagsScenario4$Google_Workplaces[breaker:nrow(DFtoBuildLagsScenario4)] <-
        as.integer(
          runif(
            length(breaker:nrow(DFtoBuildLagsScenario4)),
            min = Google_Workplaces_Custom - 5,
            max = Google_Workplaces_Custom + 5
          )
        )
      DFtoBuildLagsScenario4$Google_Residential[breaker:nrow(DFtoBuildLagsScenario4)] <-
        as.integer(
          runif(
            length(breaker:nrow(DFtoBuildLagsScenario4)),
            min = Google_Residential_Custom - 5,
            max = Google_Residential_Custom + 5
          )
        )
      DFtoBuildLagsScenario4$StringencyIndexForDisplay[breaker:nrow(DFtoBuildLagsScenario4)] <-
        StringencyIndexForDisplay_Custom
      DFtoBuildLagsScenario4$GovernmentResponseIndexForDisplay[breaker:nrow(DFtoBuildLagsScenario4)] <-
        GovernmentResponseIndexForDisplay_Custom
      DFtoBuildLagsScenario4$ContainmentHealthIndexForDisplay[breaker:nrow(DFtoBuildLagsScenario4)] <-
        ContainmentHealthIndexForDisplay_Custom
      DFtoBuildLagsScenario4$EconomicSupportIndexForDisplay[breaker:nrow(DFtoBuildLagsScenario4)] <-
        EconomicSupportIndexForDisplay_Custom
      ##---whichCustom---###########################################################
      # OVERWRITE CUSTOM AND MAKE IT THE SAME AS STATUS QUO?
      if(NPIprofile == "Status Quo"){DFtoBuildLagsScenario4 <- DFtoBuildLagsScenario1}
      if(NPIprofile == "Pre-COVID-NPI"){DFtoBuildLagsScenario4 <- DFtoBuildLagsScenario2}
      if(NPIprofile == "Extreme-NPI"){DFtoBuildLagsScenario4 <- DFtoBuildLagsScenario3}
      if(NPIprofile == "Custom-NPI"){DFtoBuildLagsScenario4 <- DFtoBuildLagsScenario4}
      
      if (scenario == 1) {
        DFtoBuildLagsScenarioChosen <- DFtoBuildLagsScenario1
      }
      if (scenario == 2) {
        DFtoBuildLagsScenarioChosen <- DFtoBuildLagsScenario2
      }
      if (scenario == 3) {
        DFtoBuildLagsScenarioChosen <- DFtoBuildLagsScenario3
      }
      if (scenario == 4) {
        DFtoBuildLagsScenarioChosen <- DFtoBuildLagsScenario4
      }
      if (length(NPInamesMain) > 0) {
        for (j in 1:length(NPInamesMain)) {
          testing_ready_pred[breaker:nrow(testing_ready_pred), NPInamesMain[j]] <-
            DFtoBuildLagsScenarioChosen[breaker:nrow(testing_ready_pred), NPInamesMain[j]]
        }
      }
      if (length(NPInamesLag) > 0) {
        for (jj in 1:length(NPInamesLag)) {
          myStringSplit <- unlist(strsplit(NPInamesLag[jj], "_"))
          string1 <-
            paste(myStringSplit[1:(which(myStringSplit %in% c("Lag", "lag")) - 1)],
                  sep = "",
                  collapse = "_")
          lagtime <- as.numeric(tail(myStringSplit, 1))
          testing_ready_pred[breaker:nrow(testing_ready_pred), NPInamesLag[jj]] <-
            lag(DFtoBuildLagsScenarioChosen[[paste0(string1)]], lagtime)[breaker:nrow(testing_ready_pred)]
        }
      }
      # Make 0 for google data before it has started (as baseline)
      # testing_ready_pred$Google_Retail_recreation[is.na(testing_ready_pred$Google_Retail_recreation)] <- 0
      # testing_ready_pred$Google_Grocery_pharmacy[is.na(testing_ready_pred$Google_Grocery_pharmacy)] <- 0
      # testing_ready_pred$Google_Parks[is.na(testing_ready_pred$Google_Parks)] <- 0
      # testing_ready_pred$Google_Transit_stations[is.na(testing_ready_pred$Google_Transit_stations)] <- 0
      # testing_ready_pred$Google_Workplaces[is.na(testing_ready_pred$Google_Workplaces)] <- 0
      # testing_ready_pred$Google_Residential[is.na(testing_ready_pred$Google_Residential)] <- 0
      testing_ready_pred[is.na(testing_ready_pred)] <- 0
      # DFtoBuildLags
      # if(NPIflag2 == "lastNPI"){
      #   for(i in breaker:nrow(testing_ready_pred)){
      #     for(j in NPInames){
      #       if(is.na(testing_ready_pred[[j]][i])){testing_ready_pred[[j]][i] <- testing_ready_pred[[j]][i-1]}
      #     }
      #   }
      # }else if(NPIflag2 == "firstNPI"){
      #   for(i in breaker:nrow(testing_ready_pred)){
      #     for(j in NPInames){testing_ready_pred[[j]][i] <- hindsight_ready[[j]][which.min(hindsight_ready$date)]}
      #     }
      # }
      # Check before and after if you so desire, for the filling in of NPI data in the forecasting period.
      # testing_ready$Social_Distancing
      # testing_ready_pred$Social_Distancing
      
      NPInames <-
        names(testing_ready_pred)[grep("R0", names(testing_ready_pred))]
      testing_ready_pred[breaker:nrow(testing_ready_pred), c(NPInames)] <-
        NA
      
      # testing_ready_pred
      #---makePrediction---#########################################################################################################################################################################
      p1 <-
        predict(best_model,
                testing_ready_pred[1:(breaker - 1), ],
                na.action = na.omit,
                n.trees = number_trees)
      # p1_tmp <- predictFunction(name=best_model, mod_name=model_name, dd=testing_ready_pred[1:(breaker-1),], n_trees = number_trees)
      # p1 <- as.data.frame(cbind(p1_tmp$predictions,p1_tmp$se))
      # colnames(p1) <- c("prediction","se")
      for (i in breaker:nrow(testing_ready_pred)) {
        for (l in 1:nLags) {
          if (c(paste0("R0_lag_", l)) %in% colnames(testing_ready_pred)) {
            testing_ready_pred[i, c(paste0("R0_lag_", l))] <-
              testing_ready_pred[i - l, c(paste0(outcomeVariable))]
          }
        }
        if (i == breaker) {
          pN <-        predict(best_model,
                               testing_ready_pred[i, ],
                               na.action = na.omit,
                               n.trees = number_trees)
          pAll <- c(p1, pN)
        } else{
          pN <-
            predict(best_model,
                    testing_ready_pred[i, ],
                    na.action = na.omit,
                    n.trees = number_trees)
          pAll <- c(pAll, pN)
        }
        testing_ready_pred[i, c(paste0(outcomeVariable))] <- NA
        testing_ready_pred[i, c(paste0(outcomeVariable))] <-
          predict(best_model,
                  testing_ready_pred[i, ],
                  na.action = na.omit,
                  n.trees = number_trees)
        
      }
      
      # Collect the data to be rady for ggplot
      pAll <- as.data.frame(pAll)
      pAll$time <- testing_ready$time
      pAll$date <- testing_ready$date
      areNA <- which(is.na(pAll$date))
      for (i in areNA) {
        pAll$date[i] <- pAll$date[i - 1] + 1
      }
      plot1Data_tmp <-
        testing_ready_OG[, c("FullName", "time", outcomeVariable)]
      
      #---plotPrediction---#########################################################################################################################################################################
      # Set up prediction intervals
      # res_uncertain_si[["R"]]$`Quantile.0.025(R)`
      
      # Organize the data to be ready for ggplot
      plot1Data <-
        plot1Data_tmp %>% left_join(pAll, by = c("time" = "time"))
      colnames(plot1Data) <-
        c("country",
          "time",
          "Empirical R0 Timeseries",
          "Prediction",
          "date")
      plot1Data$time <- NULL
      plot1Data$date <- testing_ready_OG$date
      areNA <- which(is.na(plot1Data$date))
      for (i in areNA) {
        plot1Data$date[i] <- plot1Data$date[i - 1] + 1
      }
      plot1Data <- plot1Data[order(plot1Data$date), ]
      plot1Data$Prediction <- as.numeric(plot1Data$Prediction)
      plot1Data$`Empirical R0 Timeseries` <-
        as.numeric(plot1Data$`Empirical R0 Timeseries`)
      plot1Data$country <- as.character(plot1Data$country)
      str(plot1Data)
      
      # hindsight <- subset(hindsightAll, ISO3 == testing_country)
      # plot1Data <- merge(plot1Data, hindsight_ready[,c("date",outcomeVariable)], by="date", all.x=T, all.y=T)
      # colnames(plot1Data) <- c("date", "country", "Empirical R0 Timeseries", "Prediction", "Hindsight R0 Timeseries")
      plot1Data <-
        plot1Data[, c("date",
                      "country",
                      "Empirical R0 Timeseries",
                      "Prediction")]
      colnames(plot1Data) <-
        c("date",
          "country",
          "Empirical R0 Timeseries",
          "Prediction")
      
      if (scenario == 1) {
        colnames(plot1Data) <-
          c("date",
            "country",
            "Empirical R0 Timeseries",
            "Status Quo Prediction")
        plot1DataScenario1 <- plot1Data
        Scenario1LU <-
          as.data.frame(
            predict(
              qrF,
              testing_ready_pred,
              na.action = na.pass,
              n.trees = number_trees,
              quantiles = c(0.1, 0.5, 0.9),
              all = T
            )
          )
        Scenariotmp <-
          Scenario1LU[rep(1, (nrow(plot1DataScenario1) - nrow(Scenario1LU))), ]
        # Scenariotmp[(1:nrow(Scenariotmp)),(1:ncol(Scenariotmp))] <- NA
        # Scenario1LU <- rbind(Scenario1LU,Scenariotmp)
      }
      if (scenario == 2) {
        colnames(plot1Data) <-
          c("date",
            "country",
            "Empirical R0 Timeseries",
            "Pre-COVID-NPI Prediction")
        plot1DataScenario2 <- plot1Data
        Scenario2LU <-
          as.data.frame(
            predict(
              qrF,
              testing_ready_pred,
              na.action = na.pass,
              n.trees = number_trees,
              quantiles = c(0.1, 0.5, 0.9),
              all = T
            )
          )
        # Scenario2LU <- rbind(Scenario2LU,Scenariotmp)
      }
      if (scenario == 3) {
        colnames(plot1Data) <-
          c("date",
            "country",
            "Empirical R0 Timeseries",
            "Extreme-NPI Prediction")
        plot1DataScenario3 <- plot1Data
        Scenario3LU <-
          as.data.frame(
            predict(
              qrF,
              testing_ready_pred,
              na.action = na.pass,
              n.trees = number_trees,
              quantiles = c(0.1, 0.5, 0.9),
              all = T
            )
          )
        # Scenario3LU <- rbind(Scenario3LU,Scenariotmp)
      }
      if (scenario == 4) {
        colnames(plot1Data) <-
          c("date",
            "country",
            "Empirical R0 Timeseries",
            "Custom-NPI Prediction")
        plot1DataScenario4 <- plot1Data
        Scenario4LU <-
          as.data.frame(
            predict(
              qrF,
              testing_ready_pred,
              na.action = na.pass,
              n.trees = number_trees,
              quantiles = c(0.1, 0.5, 0.9),
              all = T
            )
          )
        # Scenario4LU <- rbind(Scenario4LU,Scenariotmp)
      }
      
    }
    
    plot1Data <-
      cbind(
        plot1DataScenario1,
        Scenario1LU$`quantile= 0.1`,
        Scenario1LU$`quantile= 0.9`,
        plot1DataScenario2$`Pre-COVID-NPI Prediction`,
        Scenario2LU$`quantile= 0.1`,
        Scenario2LU$`quantile= 0.9`,
        plot1DataScenario3$`Extreme-NPI Prediction`,
        Scenario3LU$`quantile= 0.1`,
        Scenario3LU$`quantile= 0.9`,
        plot1DataScenario4$`Custom-NPI Prediction`,
        Scenario4LU$`quantile= 0.1`,
        Scenario4LU$`quantile= 0.9`
      )
    colnames(plot1Data) <-
      c(
        "date",
        "country",
        "Empirical R0 Timeseries",
        "Status Quo Prediction",
        "Status Quo Prediction Lower",
        "Status Quo Prediction Upper",
        "Pre-COVID-NPI Prediction",
        "Pre-COVID-NPI Prediction Lower",
        "Pre-COVID-NPI Prediction Upper",
        "Extreme-NPI Prediction",
        "Extreme-NPI Prediction Lower",
        "Extreme-NPI Prediction Upper",
        "Custom-NPI Prediction",
        "Custom-NPI Prediction Lower",
        "Custom-NPI Prediction Upper"
      )
    
    plot1Data$`Empirical R0 Timeseries` <- NA
    plot1Data$`Empirical R0 Timeseries`[1:length(estR0$R)] <- estR0$R
    plot1Data$`Empirical R0 Timeseries`[length(estR0$R)] <- NA
    
    plot1Data$R0_lower <- NA
    plot1Data$R0_lower[1:length(estR0$R)] <- estR0$conf.int$lower
    plot1Data$R0_lower[length(estR0$R)] <- NA
    
    plot1Data$R0_upper <- NA
    plot1Data$R0_upper[1:length(estR0$R)] <- estR0$conf.int$upper
    plot1Data$R0_upper[length(estR0$R)] <- NA
    
    plot1Data$date <- as.Date(plot1Data$date)
    # Reshape the data to be ready for ggplot
    m1 <- reshape2::melt(plot1Data, id = c("country", "date"))
    m1$date <- as.numeric(m1$date)
    m1$value <- as.numeric(m1$value)
    m1$variable <- as.factor(m1$variable)
    m1$country <- as.factor(m1$country)
    str(m1)
    m1 <- m1[order(m1$date), ]
    
    # Set the colors for red = actual, blue = prediction
    m1$date <- as.Date(m1$date)
    predictColor <- rep("deepskyblue2", 5)
    # alphabeticalList <- sort(c("Status Quo Prediction","Pre-COVID-NPI Prediction","Extreme-NPI Prediction","Empirical R0 Timeseries","Hindsight R0 Timeseries","Custom-NPI Prediction"))
    alphabeticalList <-
      sort(
        c(
          "Status Quo Prediction",
          "Pre-COVID-NPI Prediction",
          "Extreme-NPI Prediction",
          "Empirical R0 Timeseries",
          "Custom-NPI Prediction"
        )
      )
    alphabeticalIndex <-
      which(alphabeticalList == "Empirical R0 Timeseries")
    predictColor[alphabeticalIndex] <- "red"
    # alphabeticalIndex <- which(alphabeticalList == "Hindsight R0 Timeseries")
    # predictColor[alphabeticalIndex] <- "orange"
    alphabeticalIndex <-
      which(alphabeticalList == "Status Quo Prediction")
    predictColor[alphabeticalIndex] <- "deepskyblue2"
    alphabeticalIndex <-
      which(alphabeticalList == "Pre-COVID-NPI Prediction")
    predictColor[alphabeticalIndex] <- "purple"
    alphabeticalIndex <-
      which(alphabeticalList == "Extreme-NPI Prediction")
    predictColor[alphabeticalIndex] <- "seagreen"
    alphabeticalIndex <-
      which(alphabeticalList == "Custom-NPI Prediction")
    predictColor[alphabeticalIndex] <- "darkgoldenrod2"
    
    # make the plot
    plot_predict <- ggplot()
    plot_predict <-
      subset(m1, variable == "Empirical R0 Timeseries") %>%
      ggplot(aes(x = date, y = value, color = variable)) +
      geom_glowing_line(size=0.5)
    plot_predict <- plot_predict +
      # geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=subset(m1, variable == "Hindsight R0 Timeseries"), aes(x = date, y = value*1, group = variable, color = variable), size=1.5,alpha=.9,show.legend = T,linetype = "solid")+
      geom_ribbon(
        aes(ymin = plot1Data$R0_lower, ymax = plot1Data$R0_upper),
        alpha = 0.01,
        color = "red",
        fill = "red",
        linetype = "dashed"
      )
    plot_predict <- plot_predict +
      geom_line(position = position_dodge2(width = 0.5, padding = 0.2), 
        data = subset(m1, variable == "Status Quo Prediction"),
        aes(
          x = date,
          y = value,
          group = variable,
          color = variable
        ),
        size = 1.2,
        alpha = .9,
        show.legend = T,
        linetype = "solid"
      ) +
      geom_ribbon(
        aes(
          ymin = plot1Data$`Status Quo Prediction Lower`,
          ymax = plot1Data$`Status Quo Prediction Upper`
        ),
        alpha = 0.05,
        color = "deepskyblue2",
        fill = "deepskyblue2",
        linetype = "dashed"
      )
    plot_predict <- plot_predict +
      geom_line(position = position_dodge2(width = 0.5, padding = 0.2), 
        data = subset(m1, variable == "Pre-COVID-NPI Prediction"),
        aes(
          x = date,
          y = value,
          group = variable,
          color = variable
        ),
        size = 1.2,
        alpha = .9,
        show.legend = T,
        linetype = "solid"
      ) +
      geom_ribbon(
        aes(
          ymin = plot1Data$`Pre-COVID-NPI Prediction Lower`,
          ymax = plot1Data$`Pre-COVID-NPI Prediction Upper`
        ),
        alpha = 0.05,
        color = "purple",
        fill = "purple",
        linetype = "dashed"
      )
    plot_predict <- plot_predict +
      geom_line(position = position_dodge2(width = 0.5, padding = 0.2), 
        data = subset(m1, variable == "Extreme-NPI Prediction"),
        aes(
          x = date,
          y = value,
          group = variable,
          color = variable
        ),
        size = 1.2,
        alpha = .9,
        show.legend = T,
        linetype = "solid"
      ) +
      geom_ribbon(
        aes(
          ymin = plot1Data$`Extreme-NPI Prediction Lower`,
          ymax = plot1Data$`Extreme-NPI Prediction Upper`
        ),
        alpha = 0.05,
        color = "seagreen",
        fill = "seagreen",
        linetype = "dashed"
      )
    plot_predict <- plot_predict +
      geom_line(position = position_dodge2(width = 0.5, padding = 0.2), 
        data = subset(m1, variable == "Custom-NPI Prediction"),
        aes(
          x = date,
          y = value,
          group = variable,
          color = variable
        ),
        size = 1.2,
        alpha = .9,
        show.legend = T,
        linetype = "solid"
      ) +
      geom_ribbon(
        aes(
          ymin = plot1Data$`Custom-NPI Prediction Lower`,
          ymax = plot1Data$`Custom-NPI Prediction Upper`
        ),
        alpha = 0.05,
        color = "darkgoldenrod2",
        fill = "darkgoldenrod2",
        linetype = "dashed"
      )
    plot_predict <- plot_predict +
      labs(y = "R0", title = "")
    plot_predict <- plot_predict +
      theme(legend.position = "top") +
      geom_vline(
        xintercept = as.numeric(pAll$date[breaker]),
        linetype = "twodash",
        color = "seashell4",
        size = 0.9
      ) +
      annotate(
        "text",
        x = pAll$date[breaker - 4],
        y = 0.93 * max(m1$value, na.rm = T),
        label = as.character(format.Date(pAll$date[breaker], "%b-%d")),
        color = "gray17",
        angle = 90
      ) +
      # geom_text(aes(x=pAll$date[breaker-2], label=as.character(format.Date(pAll$date[breaker],"%b-%d-%Y")), y=0.93*max(m1$value,na.rm=T)), colour="seashell4", angle=90, text=element_text(size=12)) +
      # scale_x_date(breaks = seq(from=min(m1$date,na.rm=T),to=max(m1$date,na.rm=T),length.out=5), labels = date_format("%d"))+
      scale_x_date(date_breaks = "2 week", date_labels =  "%b %d") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      geom_hline(
        yintercept = 1,
        linetype = "twodash",
        color = "seashell4",
        size = 0.9
      ) +
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
      ) +
      # scale_x_continuous(breaks=seq(1, 10, 1))+
      scale_colour_manual(values = predictColor) +
      theme(legend.text = element_text(size = 16)) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")
      ) +
      labs(x = NULL) + guides(colour = guide_legend(nrow = 3, title = NULL))+
      theme(axis.text.x = element_text(angle = 60, hjust=0.9,vjust=0.9))
    # plot_predict
    #---All Country plot---#########################################################################################################################################################################
    # lineColors <- c("firebrick","darkgoldenrod1", "darkviolet", "limegreen", "dodgerblue")
    colorList <- c(randomColor(length(training_countries) + 1))
    alphabeticalList <-
      sort(c(unique(
        as.character(testing_ready_OG$FullName)
      ), unique(
        as.character(training_ready$FullName)
      )))
    alphabeticalIndex <-
      which(alphabeticalList == unique(as.character(testing_ready_OG$FullName)))
    colorList[alphabeticalIndex] <- "red"
    
    plot1 <- ggplot()
    if (incidence_flag == T && death_flag == F) {
      # geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=testing_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size=1, linetype = "3313",alpha=1) +
      # geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=testing_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size = 3, colour = 'red', alpha = 0.1) +
      # geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=testing_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size = 2, colour = 'red', alpha = 0.2) +
      # geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=testing_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size = 1, colour = 'red', alpha = 0.5) +
      # geom_glowing_line(aes(x = testing_ready$time[1:(nrow(testing_ready)-forecastingTime)], y = testing_ready$confirmed_cum_per_million[1:(nrow(testing_ready)-forecastingTime)], color = FullName), alpha = 1, size = 1, glow_alpha = 0.03)+
      # geom_glowing_line(aes(x = training_ready$time, y = training_ready$confirmed_cum_per_million, color = FullName, fill = FullName), alpha = 1, size = 1, glow_alpha = 0.03)+
      plot1 <- testing_ready_OG %>%
        ggplot(aes(x = time, y = confirmed_cum_per_million, color = FullName)) +
        geom_glowing_line(size=0.5) +
        labs(
          x = paste0(
            "Days Since ",
            50,
            " Cumulative Cases"
          ),
          y = "Confirmed Cumulative Cases per Million",
          title = ""
        )
      plot1 <- plot1 +
        geom_line(position = position_dodge2(width = 0.5, padding = 0.2), 
          data = training_ready,
          aes(
            x = time,
            y = confirmed_cum_per_million,
            group = FullName,
            color = FullName
          ),
          size = 0.8,
          alpha = .7
        )
    } else if (incidence_flag == T && death_flag == T) {
      # plot1 <- plot1 +
      #   geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=training_ready, aes(x = time, y = death_cum_per_million, group = FullName, color = FullName), size=0.8,alpha=.7)+
      #   # geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=testing_ready, aes(x = time, y = death_cum_per_million, group = FullName, color = FullName), size=1, linetype = "3313",alpha=1)+
      #   geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=testing_ready, aes(x = time, y = death_cum_per_million, group = FullName, color = FullName), size = 3, colour = 'red', alpha = 0.1) +
      #   geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=testing_ready, aes(x = time, y = death_cum_per_million, group = FullName, color = FullName), size = 2, colour = 'red', alpha = 0.2) +
      #   geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=testing_ready, aes(x = time, y = death_cum_per_million, group = FullName, color = FullName), size = 1, colour = 'red', alpha = 0.5) +
      #   labs(x=paste0("Days Since ",incidence_start_point," Cumulative Deaths per Million"), y = "Confirmed Cumulative Deaths per Million", title="")
      plot1 <- testing_ready_OG %>%
        ggplot(aes(x = time, y = death_cum_per_million, color = FullName)) +
        geom_glowing_line(size=0.5) +
        labs(
          x = paste0(
            "Days Since ",
            incidence_start_point,
            " Cumulative Deaths per Million"
          ),
          y = "Confirmed Cumulative Deaths per Million",
          title = ""
        )
      plot1 <- plot1 +
        geom_line(position = position_dodge2(width = 0.5, padding = 0.2), 
          data = training_ready,
          aes(
            x = time,
            y = death_cum_per_million,
            group = FullName,
            color = FullName
          ),
          size = 0.8,
          alpha = .7
        )
    } else if (incidence_flag == F && death_flag == F) {
      # plot1 <- plot1 +
      #   geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=training_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size=0.8,alpha=.7)+
      #   # geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=testing_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size=1, linetype = "3313",alpha=1)+
      #   geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=testing_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size = 3, colour = 'red', alpha = 0.1) +
      #   geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=testing_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size = 2, colour = 'red', alpha = 0.2) +
      #   geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=testing_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size = 1, colour = 'red', alpha = 0.5) +
      #   labs(x=paste0("Days Since ",count_start_point," Cumulative Counts"), y = "Confirmed Cumulative Cases", title="")
      plot1 <- testing_ready_OG %>%
        ggplot(aes(x = time, y = confirmed_cum, color = FullName)) +
        geom_glowing_line(size=0.5) +
        labs(
          x = paste0("Days Since ", count_start_point, " Cumulative Counts"),
          y = "Confirmed Cumulative Cases",
          title = ""
        )
      plot1 <- plot1 +
        geom_line(position = position_dodge2(width = 0.5, padding = 0.2), 
          data = training_ready,
          aes(
            x = time,
            y = confirmed_cum,
            group = FullName,
            color = FullName
          ),
          size = 0.8,
          alpha = .7
        )
    } else if (incidence_flag == F && death_flag == T) {
      # plot1 <- plot1 +
      #   geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=training_ready, aes(x = time, y = death_cum, group = FullName, color = FullName), size=0.8,alpha=.7)+
      #   # geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=testing_ready, aes(x = time, y = death_cum, group = FullName, color = FullName), size=1, linetype = "3313",alpha=1)+
      #   geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=testing_ready, aes(x = time, y = death_cum, group = FullName, color = FullName), size = 3, colour = 'red', alpha = 0.1) +
      #   geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=testing_ready, aes(x = time, y = death_cum, group = FullName, color = FullName), size = 2, colour = 'red', alpha = 0.2) +
      #   geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=testing_ready, aes(x = time, y = death_cum, group = FullName, color = FullName), size = 1, colour = 'red', alpha = 0.5) +
      #   labs(x=paste0("Days Since ",count_start_point," Cumulative Deaths"), y = "Confirmed Cumulative Deaths", title="")
      plot1 <- testing_ready_OG %>%
        ggplot(aes(x = time, y = death_cum, color = FullName)) +
        geom_glowing_line(size=0.5) +
        labs(
          x = paste0("Days Since ", count_start_point, " Cumulative Deaths"),
          y = "Confirmed Cumulative Deaths",
          title = ""
        )
      plot1 <- plot1 +
        geom_line(position = position_dodge2(width = 0.5, padding = 0.2), 
          data = training_ready,
          aes(
            x = time,
            y = death_cum,
            group = FullName,
            color = FullName
          ),
          size = 0.8,
          alpha = .7
        )
    }
    plot1 <- plot1 +
      guides(color = guide_legend(title = "")) +
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
      ) +
      # scale_x_continuous(breaks=seq(1, 10, 1))+
      scale_colour_manual(values = colorList, aesthetics = "colour") +
      theme(legend.text = element_text(size = 9)) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")
      )
    # plot1
    
    #---R0 plot---#########################################################################################################################################################################
    # lineColors <- c("firebrick","darkgoldenrod1", "darkviolet", "limegreen", "dodgerblue")
    plot2 <- ggplot()
      plot2 <- testing_ready_OG %>%
        ggplot(aes(x = time, y = R0, color = FullName)) +
        geom_glowing_line(size=0.5) +
        labs(
          x = paste0(
            "Days Since ",
            50,
            " Cumulative Cases"
          ),
          y = "R0",
          title = ""
        )
      plot2 <- plot2 +
        geom_line(position = position_dodge2(width = 0.5, padding = 0.2), 
          data = training_ready,
          aes(
            x = time,
            y = R0,
            group = FullName,
            color = FullName
          ),
          size = 0.8,
          alpha = .7,
          show.legend = T
        )
    plot2 <- plot2 +
      # ylim(c(0,7))+
      guides(color = guide_legend(title = "")) +
      geom_hline(
        yintercept = 1,
        linetype = "twodash",
        color = "seashell4",
        size = 0.9
      ) +
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
      ) +
      # scale_x_continuous(breaks=seq(1, 10, 1))+
      scale_colour_manual(values = colorList, aesthetics = "colour") +
      theme(legend.text = element_text(size = 9)) +
      # theme(legend.position = "none") +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")
      )
    
    #---NPI plots---#########################################################################################################################################################################
    # n <- 6
    # qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    # col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    # myCols <- sample(col_vector, n)
    myCols <-
      c("#d1c700",
        "#66A61E",
        "orangered",
        "deepskyblue",
        "red4",
        "purple",
        "grey",
        "blue",
        "green",
        "yellow")
    # pie(rep(1,n), col=myCols)
    
    plot34Data <-
      testing_ready_OG[1:(nrow(testing_ready_OG) - forecastingTime), c(
        "date",
        "R0",
        "StringencyIndexForDisplay",
        "GovernmentResponseIndexForDisplay",
        "ContainmentHealthIndexForDisplay",
        "EconomicSupportIndexForDisplay",
        "Google_Grocery_pharmacy",
        "Google_Parks",
        "Google_Residential",
        "Google_Retail_recreation",
        "Google_Transit_stations",
        "Google_Workplaces"
      )]
    plot34Data <- plot34Data %>% gather(key, value, -date)
    # plot34Data$date <- as.date(plot34Data$date)
    plot34Data$key <- as.factor(plot34Data$key)
    plot34Data$value <- as.numeric(plot34Data$value)
    plot3Data <-
      subset(
        plot34Data,
        key %ni% c(
          "R0",
          "StringencyIndexForDisplay",
          "GovernmentResponseIndexForDisplay",
          "ContainmentHealthIndexForDisplay",
          "EconomicSupportIndexForDisplay"
        )
      )
    plot4Data <-
      subset(
        plot34Data,
        key %in% c(
          "R0",
          "StringencyIndexForDisplay",
          "GovernmentResponseIndexForDisplay",
          "ContainmentHealthIndexForDisplay",
          "EconomicSupportIndexForDisplay"
        )
      )
    
    
    
    plot34Data_dashed <- cbind(
      testing_ready_OG$date[1:(nrow(testing_ready_OG) - forecastingTime)],
      DFtoBuildLagsScenario4[1:(nrow(DFtoBuildLagsScenario4) - forecastingTime), c(
        "StringencyIndexForDisplay",
        "GovernmentResponseIndexForDisplay",
        "ContainmentHealthIndexForDisplay",
        "EconomicSupportIndexForDisplay",
        "Google_Grocery_pharmacy",
        "Google_Parks",
        "Google_Residential",
        "Google_Retail_recreation",
        "Google_Transit_stations",
        "Google_Workplaces"
      )]
    )
    colnames(plot34Data_dashed) <- c("date",        
                                     "StringencyIndexForDisplay",
                                     "GovernmentResponseIndexForDisplay",
                                     "ContainmentHealthIndexForDisplay",
                                     "EconomicSupportIndexForDisplay",
                                     "Google_Grocery_pharmacy",
                                     "Google_Parks",
                                     "Google_Residential",
                                     "Google_Retail_recreation",
                                     "Google_Transit_stations",
                                     "Google_Workplaces"
    )
    
    plot34Data_dashed <- plot34Data_dashed %>% gather(key, value, -date)
    # plot34Data_dashed$date <- as.date(plot34Data_dashed$date)
    plot34Data_dashed$key <- as.factor(plot34Data_dashed$key)
    plot34Data_dashed$value <- as.numeric(plot34Data_dashed$value)
    plot3Data_dashed <-
      subset(
        plot34Data_dashed,
        key %ni% c(
          "R0",
          "StringencyIndexForDisplay",
          "GovernmentResponseIndexForDisplay",
          "ContainmentHealthIndexForDisplay",
          "EconomicSupportIndexForDisplay"
        )
      )
    plot4Data_dashed <-
      subset(
        plot34Data_dashed,
        key %in% c(
          "R0",
          "StringencyIndexForDisplay",
          "GovernmentResponseIndexForDisplay",
          "ContainmentHealthIndexForDisplay",
          "EconomicSupportIndexForDisplay"
        )
      )
    
    plot3Data$key <- as.character(plot3Data$key)
    for (r in 1:nrow(plot3Data)) {
      plot3Data$key[r] <-
        simpleCap(paste(unlist(strsplit(
          plot3Data$key[r], "_"
        )), sep = " ", collapse = " "))
    }
    plot3Data$key <-
      factor(plot3Data$key,
             levels = unique(plot3Data$key),
             order = T)
    
    plot3Data_dashed$key <- as.character(plot3Data_dashed$key)
    for (r in 1:nrow(plot3Data_dashed)) {
      plot3Data_dashed$key[r] <-
        simpleCap(paste(unlist(strsplit(
          plot3Data_dashed$key[r], "_"
        )), sep = " ", collapse = " "))
    }
    plot3Data_dashed$key <-
      factor(plot3Data_dashed$key,
             levels = unique(plot3Data_dashed$key),
             order = T)
    
    plot3 <- ggplot()
    plot3 <- plot3 +
      # geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=training_ready, aes(x = time, y = R0, group = FullName, color = FullName), size=0.8,alpha=.7)+
      # geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=subset(plot3Data, key %in% c("R0","Social_Distancing","Quaranting_Cases","Close_Border")), aes(x = date, y = value, group = key, color = key), size=1, linetype = "solid",alpha=1) +
      geom_vline(
        xintercept = as.numeric(pAll$date[breaker]),
        linetype = "twodash",
        color = "seashell4",
        size = 0.9
      ) +
      annotate(
        "text",
        x = pAll$date[breaker - 4],
        y = 0.7 * max(c(plot3Data$value,plot3Data_dashed$value),na.rm=T) * 1.1,
        label = as.character(format.Date(pAll$date[breaker], "%b-%d")),
        color = "gray17",
        angle = 90
      ) +
      geom_line(position = position_dodge2(width = 0.5, padding = 0.2), 
        data = plot3Data,
        aes(
          x = date,
          y = value,
          group = key,
          color = key
        ),
        size = 1,
        linetype = "solid",
        alpha = 1
      ) +
      geom_line(position = position_dodge2(width = 0.5, padding = 0.2), 
        data = plot3Data_dashed,
        aes(
          x = date,
          y = value,
          group = key,
          color = key
        ),
        size = 0.8,
        linetype = "dotted",
        alpha = 0.8
      ) +
      # geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=testing_ready, aes(x = time, y = R0, group = FullName, color = FullName), size = 3, colour = 'red', alpha = 0.1) +
      # geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=testing_ready, aes(x = time, y = R0, group = FullName, color = FullName), size = 2, colour = 'red', alpha = 0.2) +
      # geom_line(position = position_dodge2(width = 0.5, padding = 0.2), data=testing_ready, aes(x = time, y = R0, group = FullName, color = FullName), size = 1, colour = 'red', alpha = 0.5) +
      labs(
        x = paste0(
          "Days Since ",
          incidence_start_point,
          " Cumulative Counts per Million"
        ),
        y = "% Change From Baseline",
        title = ""
      )
    plot3 <- plot3 +
      guides(color = guide_legend(title = "")) +
      theme(legend.position = "top") +
      geom_hline(
        yintercept = 0,
        linetype = "twodash",
        color = "seashell4",
        size = 0.9
      ) +
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
      ) +
      # scale_x_continuous(breaks=seq(1, 10, 1))+
      # scale_colour_manual(values=randomColor(length(unique(plot3Data$key))), aesthetics = "colour") +
      scale_colour_manual(values = myCols, aesthetics = "colour") +
      theme(legend.text = element_text(size = 9)) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")
      ) +
      labs(x = NULL) +
      theme(legend.text = element_text(color = "black", size = 14)) +
      scale_x_date(date_breaks = "2 week", date_labels =  "%b %d") +
      theme(axis.text.x = element_text(angle = 60, hjust=0.9,vjust=0.9))
    # plot3
    # plot4Data <- subset(plot34Data, key %in% c("R0","Social_Distancing","Quaranting_Cases","Close_Border"))
    plot4Data$date <- as.Date(plot4Data$date)
    plot4Data$key <-
      factor(
        plot4Data$key,
        levels = c(
          "StringencyIndexForDisplay",
          "GovernmentResponseIndexForDisplay",
          "ContainmentHealthIndexForDisplay",
          "EconomicSupportIndexForDisplay",
          "R0"
        ),
        ordered = T
      )
    
    plot4_NPI <- subset(plot4Data, key %ni% c("R0"))
    plot4_NPI$key <-
      factor(
        plot4_NPI$key,
        levels = c(
          "StringencyIndexForDisplay",
          "GovernmentResponseIndexForDisplay",
          "ContainmentHealthIndexForDisplay",
          "EconomicSupportIndexForDisplay",
          "R0"
        ),
        ordered = T
      )
    plot4_R0 <- subset(plot4Data, key %in% c("R0"))
    plot4_R0$key <-
      factor(
        plot4_R0$key,
        levels = c(
          "StringencyIndexForDisplay",
          "GovernmentResponseIndexForDisplay",
          "ContainmentHealthIndexForDisplay",
          "EconomicSupportIndexForDisplay",
          "R0"
        ),
        ordered = T
      )
    
    #
    
    plot4Data_dashed$date <- as.Date(plot4Data_dashed$date)
    plot4Data_dashed$key <-
      factor(
        plot4Data_dashed$key,
        levels = c(
          "StringencyIndexForDisplay",
          "GovernmentResponseIndexForDisplay",
          "ContainmentHealthIndexForDisplay",
          "EconomicSupportIndexForDisplay",
          "R0"
        ),
        ordered = T
      )
    
    plot4_NPI_dashed <- subset(plot4Data_dashed, key %ni% c("R0"))
    plot4_NPI_dashed$key <-
      factor(
        plot4_NPI_dashed$key,
        levels = c(
          "StringencyIndexForDisplay",
          "GovernmentResponseIndexForDisplay",
          "ContainmentHealthIndexForDisplay",
          "EconomicSupportIndexForDisplay",
          "R0"
        ),
        ordered = T
      )
    
    plot4_R0_dashed <- as.data.frame(plot1Data[["Custom-NPI Prediction"]][!is.na(plot1Data[["Custom-NPI Prediction"]])])
    colnames(plot4_R0_dashed) <- c("value")
    plot4_R0_dashed$key <- "R0"
    plot4_R0_dashed$date <- NA
    plot4_R0_dashed$date <- as.Date(seq(from=as.Date(plot4_R0$date[1]),to=as.Date(plot4_R0$date[1]+length(plot4_R0_dashed$date)),length.out=length(plot4_R0_dashed$date)))
    
    
    maxy <-
      ifelse((max(plot4_R0$value,na.rm=T) > 10), 10, max(plot4_R0$value,na.rm=T))
    plot5 <- plot4_R0 %>%
      ggplot(aes(
        x = date,
        y = plot4_R0$value,
        color = key
      )) +
      geom_vline(
        xintercept = as.numeric(pAll$date[breaker]),
        linetype = "twodash",
        color = "seashell4",
        size = 0.9
      ) +
      annotate(
        "text",
        x = pAll$date[breaker - 4],
        y = 0.72 * maxy ,
        label = as.character(format.Date(pAll$date[breaker], "%b-%d")),
        color = "gray17",
        angle = 90
      ) +
      # geom_glowing_line(size=0.5) +
      geom_line(position = position_dodge2(width = 0.5, padding = 0.2), 
        data = plot4_R0,
        aes(
          x = date,
          y = value,
          group = key,
          color = key
        ),
        size = 1,
        linetype = "solid",
        color="red",
        alpha = 1
      ) +
      geom_line(position = position_dodge2(width = 0.5, padding = 0.2), 
        data = plot4_R0_dashed,
        aes(
          x = date,
          y = value,
          group = key,
          color = key
        ),
        size = 1,
        linetype = "solid",
        color="orange",
        alpha = 1
      ) +
      labs(x = "", y = "R0", title = "") +
      scale_colour_manual(values = "red") +
      scale_y_continuous(name = "R0", limits = c(0, maxy)) +
      geom_hline(
        yintercept = 1,
        linetype = "twodash",
        color = "seashell4",
        size = 0.9
      ) +    theme(legend.title = element_text(size = 14)) +
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
      ) +
      guides(color = guide_legend(title = "")) +
      theme(legend.position = "top") +
      theme(legend.text = element_text(size = 9)) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")
      ) +
      theme(legend.text = element_text(color = "black", size = 14)) +
      scale_x_date(date_breaks = "2 week", date_labels =  "%b %d") +
      theme(axis.text.x = element_text(angle = 60, hjust=0.9,vjust=0.9))
    # plot5
    
    
    plot4_NPI$key <- as.character(plot4_NPI$key)
    for (r in 1:nrow(plot4_NPI)) {
      plot4_NPI$key[r] <-
        simpleCap(paste(unlist(strsplit(
          plot4_NPI$key[r], "_"
        )), sep = " ", collapse = " "))
    }
    plot4_NPI$key <-
      factor(plot4_NPI$key,
             levels = unique(plot4_NPI$key),
             order = T)
    
    plot4_NPI_dashed$key <- as.character(plot4_NPI_dashed$key)
    for (r in 1:nrow(plot4_NPI_dashed)) {
      plot4_NPI_dashed$key[r] <-
        simpleCap(paste(unlist(strsplit(
          plot4_NPI_dashed$key[r], "_"
        )), sep = " ", collapse = " "))
    }
    plot4_NPI_dashed$key <-
      factor(plot4_NPI_dashed$key,
             levels = unique(plot4_NPI_dashed$key),
             order = T)
    colorsPlot4 <-
      c(brewer.pal(4, "Set2")[1:4])
    plot4 <- ggplot()
    # plot4 <- plot4_R0 %>%
    #   ggplot(aes(x = date, y = value*5/(max(plot4_R0$value)), color = key))+
    #   geom_glowing_line(size=0.5)+
    #   labs(x=paste0("Days Since ",incidence_start_point," Cumulative Deaths per Million"), y = "R0", title="")
    names1 <- plot4_NPI$key
    names2 <- gsub(x = names1, pattern = "GovernmentResponseIndexForDisplay", "Government Response Index")
    names2 <- gsub(x = names2, pattern = "StringencyIndexForDisplay", "Stringency Index")
    names2 <- gsub(x = names2, pattern = "ContainmentHealthIndexForDisplay", "Containment Health Index")
    names2 <- gsub(x = names2, pattern = "EconomicSupportIndexForDisplay", "Economic Support Index")
    plot4_NPI$key <- names2
    names1 <- plot4_NPI_dashed$key
    names2 <- gsub(x = names1, pattern = "GovernmentResponseIndexForDisplay", "Government Response Index")
    names2 <- gsub(x = names2, pattern = "StringencyIndexForDisplay", "Stringency Index")
    names2 <- gsub(x = names2, pattern = "ContainmentHealthIndexForDisplay", "Containment Health Index")
    names2 <- gsub(x = names2, pattern = "EconomicSupportIndexForDisplay", "Economic Support Index")
    plot4_NPI_dashed$key <- names2
    plot4 <- plot4 +
      geom_vline(
        xintercept = as.numeric(pAll$date[breaker]),
        linetype = "twodash",
        color = "seashell4",
        size = 0.9
      ) +
      annotate(
        "text",
        x = pAll$date[breaker - 4],
        # y = 0.9 * max(plot4_NPI$value,na.rm=T) * 1.1,
        y = 50,
        label = as.character(format.Date(pAll$date[breaker], "%b-%d")),
        color = "gray17",
        angle = 90
      ) +
      geom_line(position = position_dodge2(width = 0.5, padding = 0.2), 
        data = plot4_NPI,
        aes(
          x = date,
          y = value,
          group = key,
          color = key
        ),
        size = 1.2,
        linetype = "solid",
        alpha = .9,
        show.legend = T
      )+
      geom_line(position = position_dodge2(width = 0.5, padding = 0.2), 
        data = plot4_NPI_dashed,
        aes(
          x = date,
          y = value,
          group = key,
          color = key
        ),
        size = 1.3,
        linetype = "dotted",
        alpha = .9,
        show.legend = T
      )
    plot4 <- plot4 +
      guides(color = guide_legend(title = "")) +
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
      ) +
      # scale_x_continuous(breaks=seq(1, 10, 1))+
      # scale_colour_manual(values=randomColor(length(unique(plot3Data$key))), aesthetics = "colour") +
      # scale_y_continuous(name = "NPI Policy Scale", sec.axis = sec_axis(~./(5/(max(plot4_R0$value))), name = "R0"),limits = c(0,5))+
      scale_y_continuous(name = "NPI Policy Scale", limits = c(0, 100)) +
      scale_colour_manual(values = colorsPlot4, 
                          # breaks = c("StringencyIndexForDisplay", "GovernmentResponseIndexForDisplay", "ContainmentHealthIndexForDisplay", "EconomicSupportIndexForDisplay"),
                          # labels = c("Stringency Index", "Government Response Index", "Containment Health Index", "Economic Support Index"),
                          aesthetics = "colour") +
      theme(legend.text = element_text(size = 9)) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")
      ) +
      theme(legend.position = "top") +
      labs(x = NULL) +
      theme(legend.text = element_text(color = "black", size = 14)) +
      scale_x_date(date_breaks = "2 week", date_labels =  "%b %d") +
      theme(axis.text.x = element_text(angle = 60, hjust=0.9,vjust=0.9))
    # plot4
    # ---variableImportance Plot---#########################################################################################################################################################################
    
    # This plot is now loaded in from from file = paste0("./InputData/",
    #                                                     "All_Countries",
    #                                                     "_VSURFkeepers_R0.Rdata")
    
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
          size = 4, #length(unique(df2$variable))*1/8,
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
    
    plot_varimp <- plot_varimp+theme(
        axis.text.x = element_text(
          color = "black",
          size = 13,
          angle = 0,
          hjust = .5,
          vjust = .5
        ),
        axis.text.y = element_text(
          color = "black",
          size = 7, #length(unique(df2$variable))*1/8,
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
    
    #---SEIR---#########################################################################################################################################################################
    seir <- function(t, x, parms)
    {
      S <- x["S"]
      E <- x["E"]
      I <- x["I"]
      R <- x["R"]
      with(as.list(c(parms)), {
        ds <- -b * I * S
        de <- b * I * S - a * E
        di <- a * E - r * I
        dr <- r * I
        der <- c(ds, de, di, dr)
        list(der)
      })
    }
    
    # SEIR Solver function
    
    SEIR.determ <- function(parms, initial, time.window, ntime) {
      require(deSolve)
      times <- seq(time.window[1], time.window[2], length = ntime)
      as.data.frame(lsoda(initial, times, seir, parms))
    }
    
    #---Initial_I---#########################################################################################################################################################################
    # recoveryTime = 14.5
    # latencyTime = 5.2
    meanSerialInterval = 5.0
    timeExtension = 100
    # nowrun <- "Empirical R0 Timeseries"
    nowrun <- "Status Quo Prediction"
    nonNAR <- plot1Data[[nowrun]][!is.na(plot1Data[[nowrun]])]
    # RctSeries <- c(nonNAR, rep(mean(tail(nonNAR, 7)), timeExtension))
    RctSeries <- c(nonNAR)
    
    
    initialExposureCoef = chosen_initialExposed/chosen_initialInfected #5*meanSerialInterval*recoveryTime/meanSerialInterval
    print(paste0("initialExposureCoef = ",initialExposureCoef))
    # initialInfectedSeq <-
    #   c(seq(from = 1, to = 4001, by = 100),
    #     seq(from = 4201, to = 10001, by = 200))
    print("Choosing Initial I")
    # initialInfectedSeq <-
    #   c(seq(from = 1, to = 401, by = 50),seq(from = 501, to = 4001, by = 250),seq(from = 4301, to = 10001, by = 350))
    # for (II in 1:length(initialInfectedSeq)) {
    #   initialInfected <- initialInfectedSeq[II]
    # 
    #   for (i in 1:length(RctSeries)) {
    #     Rct = RctSeries[i]
    #     tmax = 1
    #     t.seq <- seq(0, 25, .1)
    # 
    #     N <- testing_subset_aligned$Population_mill[1] * 1000000
    #     if (i == 1) {
    #       initial <-
    #         c(
    #           S = (N - initialInfected) / N,
    #           E = initialInfected*initialExposureCoef / N,
    #           I = initialInfected / N,
    #           R = 0 / N
    #         )
    # 
    #     } else{
    #       initial <-
    #         c(
    #           S = seirFinal$S[nrow(seirFinal)],
    #           E = seirFinal$E[nrow(seirFinal)],
    #           I = seirFinal$I[nrow(seirFinal)],
    #           R = seirFinal$R[nrow(seirFinal)]
    #         )
    # 
    #     }
    #     parms <- c(b = 1/recoveryTime * Rct,
    #                a = 1/latencyTime,
    #                r = 1/recoveryTime)
    # 
    #     time.window <- c(i - 1, i)
    # 
    #     ntime = 100
    # 
    #     seir1 <- SEIR.determ(parms, initial, time.window, ntime)
    #     seir1[seir1 < 0] <- 0
    #     names(seir1)
    #     # "time" "S"    "E"    "I"    "R"
    #     if (i == 1) {
    #       seirFinal <- seir1
    #     } else{
    #       seirFinal <- rbind(seirFinal, seir1)
    #     }
    #   }
    #   IpeakDiff <-
    #     abs(max(seirFinal$I * N) - max(testing_subset_aligned$movingAverage)/percPeak)
    #   if (II == 1) {
    #     chosen_initialInfected <- initialInfected
    #     chosen_IpeakDiff <- IpeakDiff
    #   } else if (IpeakDiff < chosen_IpeakDiff) {
    #     chosen_initialInfected <- initialInfected
    #     chosen_IpeakDiff <- IpeakDiff
    #   }
    # }

    print(chosen_initialInfected)
    # print(chosen_IpeakDiff)
    
    # chosen_initialInfected <- 2251
    
    #---Custom_SEIR_Curve---#########################################################################################################################################################################
    timeExtension = 150
    initialInfected <- chosen_initialInfected
    
    nowrun <- "Custom-NPI Prediction"
    nonNAR <- plot1Data[[nowrun]][!is.na(plot1Data[[nowrun]])]
    RctSeries <- c(nonNAR, rep(mean(tail(nonNAR, 7)), timeExtension))
    for (i in 1:length(RctSeries)) {
      Rct = RctSeries[i]
      tmax = 1
      t.seq <- seq(0, 25, .1)
      
      N <- testing_subset_aligned$Population_mill[1] * 1000000
      if (i == 1) {
        initial <-
          c(
            S = (N - initialInfected) / N,
            E = initialInfected*initialExposureCoef / N,
            I = initialInfected / N,
            R = 0 / N
          )
        
      } else{
        initial <-
          c(
            S = seirFinal$S[nrow(seirFinal)],
            E = seirFinal$E[nrow(seirFinal)],
            I = seirFinal$I[nrow(seirFinal)],
            R = seirFinal$R[nrow(seirFinal)]
          )
        
      }
      parms <- c(b = 1/recoveryTime * Rct,
                 a = 1/latencyTime,
                 r = 1/recoveryTime)
      
      time.window <- c(i - 1, i)
      
      ntime = 100
      
      seir1 <- SEIR.determ(parms, initial, time.window, ntime)
      seir1[seir1 < 0] <- 0
      names(seir1)
      # "time" "S"    "E"    "I"    "R"
      if (i == 1) {
        seirFinal <- seir1
      } else{
        seirFinal <- rbind(seirFinal, seir1)
      }
    }
    
    nowrun <- "Custom-NPI Prediction Lower"
    nonNAR <- plot1Data[[nowrun]][!is.na(plot1Data[[nowrun]])]
    RctSeries <- c(nonNAR, rep(mean(tail(nonNAR, 7)), timeExtension))
    for (i in 1:length(RctSeries)) {
      Rct = RctSeries[i]
      tmax = 1
      t.seq <- seq(0, 25, .1)
      
      N <- testing_subset_aligned$Population_mill[1] * 1000000
      if (i == 1) {
        initial <-
          c(
            S = (N - initialInfected) / N,
            E = initialInfected*initialExposureCoef / N,
            I = initialInfected / N,
            R = 0 / N
          )
        
      } else{
        initial <-
          c(
            S = seirFinal_lower$S[nrow(seirFinal_lower)],
            E = seirFinal_lower$E[nrow(seirFinal_lower)],
            I = seirFinal_lower$I[nrow(seirFinal_lower)],
            R = seirFinal_lower$R[nrow(seirFinal_lower)]
          )
        
      }
      parms <- c(b = 1/recoveryTime * Rct,
                 a = 1/latencyTime,
                 r = 1/recoveryTime)
      
      time.window <- c(i - 1, i)
      
      ntime = 100
      
      seir1 <- SEIR.determ(parms, initial, time.window, ntime)
      seir1[seir1 < 0] <- 0
      names(seir1)
      # "time" "S"    "E"    "I"    "R"
      if (i == 1) {
        seirFinal_lower <- seir1
      } else{
        seirFinal_lower <- rbind(seirFinal_lower, seir1)
      }
    }
    
    nowrun <- "Custom-NPI Prediction Upper"
    nonNAR <- plot1Data[[nowrun]][!is.na(plot1Data[[nowrun]])]
    RctSeries <- c(nonNAR, rep(mean(tail(nonNAR, 7)), timeExtension))
    for (i in 1:length(RctSeries)) {
      Rct = RctSeries[i]
      tmax = 1
      t.seq <- seq(0, 25, .1)
      
      N <- testing_subset_aligned$Population_mill[1] * 1000000
      if (i == 1) {
        initial <-
          c(
            S = (N - initialInfected) / N,
            E = initialInfected*initialExposureCoef / N,
            I = initialInfected / N,
            R = 0 / N
          )
        
      } else{
        initial <-
          c(
            S = seirFinal_upper$S[nrow(seirFinal_upper)],
            E = seirFinal_upper$E[nrow(seirFinal_upper)],
            I = seirFinal_upper$I[nrow(seirFinal_upper)],
            R = seirFinal_upper$R[nrow(seirFinal_upper)]
          )
        
      }
      parms <- c(b = 1/recoveryTime * Rct,
                 a = 1/latencyTime,
                 r = 1/recoveryTime)
      
      time.window <- c(i - 1, i)
      
      ntime = 100
      
      seir1 <- SEIR.determ(parms, initial, time.window, ntime)
      seir1[seir1 < 0] <- 0
      names(seir1)
      # "time" "S"    "E"    "I"    "R"
      if (i == 1) {
        seirFinal_upper <- seir1
      } else{
        seirFinal_upper <- rbind(seirFinal_upper, seir1)
      }
    }
    minDate <- as.Date(testing_subset_aligned$date[1]) - 1
    maxDate <- as.Date(format.Date(Sys.Date() + timeExtension, "%Y-%m-%d") )
    
    #---Plot SEIR Curves---#########################################################################################################################################################################
    #average
    seirScaledPlotDF <- seirFinal
    seirScaledPlotDF$date <-
      seq(
        from = as.POSIXct(paste(plot1Data$date[1], "00:00:00"), tz = "America/Chicago"),
        to = as.POSIXct(paste(
          as.Date(plot1Data$date[length(plot1Data$date)] + timeExtension), "23:59:59"
        ), tz = "America/Chicago"),
        length.out = nrow(seirFinal)
      )
    #lower
    seirScaledPlotDF_lower <- seirFinal_lower
    seirScaledPlotDF_lower$date <-
      seq(
        from = as.POSIXct(paste(plot1Data$date[1], "00:00:00"), tz = "America/Chicago"),
        to = as.POSIXct(paste(
          as.Date(plot1Data$date[length(plot1Data$date)] + timeExtension), "23:59:59"
        ), tz = "America/Chicago"),
        length.out = nrow(seirFinal)
      )
    # seirScaledPlotDF_lower$time <- NULL
    # seirScaledPlotDF_lower$date <- NULL
    colnames(seirScaledPlotDF_lower)[which(colnames(seirScaledPlotDF_lower) %ni% c("date", "time"))] <-
      paste0(colnames(seirScaledPlotDF_lower), "_lower")[which(colnames(seirScaledPlotDF_lower) %ni% c("date", "time"))]
    seirScaledPlotDF_lower_melt <-
      melt(seirScaledPlotDF_lower, id = c("date", "time"))
    #upper
    seirScaledPlotDF_upper <- seirFinal_upper
    seirScaledPlotDF_upper$date <-
      seq(
        from = as.POSIXct(paste(plot1Data$date[1], "00:00:00"), tz = "America/Chicago"),
        to = as.POSIXct(paste(
          as.Date(plot1Data$date[length(plot1Data$date)] + timeExtension), "23:59:59"
        ), tz = "America/Chicago"),
        length.out = nrow(seirFinal)
      )
    # seirScaledPlotDF_upper$time <- NULL
    # seirScaledPlotDF_upper$date <- NULL
    colnames(seirScaledPlotDF_upper) <-
      paste0(colnames(seirScaledPlotDF_upper), "_upper")
    seirScaledPlotDF_upper_melt <-
      melt(seirScaledPlotDF_upper, id = c("date_upper", "time_upper"))
    #combine
    seirScaledPlotDF_LowerUpper <-
      cbind(seirScaledPlotDF_lower_melt,
            seirScaledPlotDF_upper_melt)
    colnames(seirScaledPlotDF_LowerUpper) <-
      c(
        "date",
        "time",
        "variable_lower",
        "value_lower",
        "date_upper",
        "time_upper",
        "variable_upper",
        "value_upper"
      )
    seirScaledPlotDF_LowerUpper$variable <- NA
    seirScaledPlotDF_LowerUpper$variable[seirScaledPlotDF_LowerUpper$variable_lower ==
                                           "S_lower"] <- "S"
    seirScaledPlotDF_LowerUpper$variable[seirScaledPlotDF_LowerUpper$variable_lower ==
                                           "E_lower"] <- "E"
    seirScaledPlotDF_LowerUpper$variable[seirScaledPlotDF_LowerUpper$variable_lower ==
                                           "I_lower"] <- "I"
    seirScaledPlotDF_LowerUpper$variable[seirScaledPlotDF_LowerUpper$variable_lower ==
                                           "R_lower"] <- "R"
    labelMax <- max(seirScaledPlotDF$I)
    seirScaledPlotDF_melt <-
      melt(seirScaledPlotDF, id = c("date", "time"))
    seirScaledPlotDF_melt$date <-
      as.Date(seirScaledPlotDF_melt$date)
    seirScaledPlotDF_melt$value <- seirScaledPlotDF_melt$value * N
    seirScaledPlotDF_LowerUpper$date <-
      as.Date(seirScaledPlotDF_LowerUpper$date)
    seirScaledPlotDF_LowerUpper$value_lower <-
      seirScaledPlotDF_LowerUpper$value_lower * N
    seirScaledPlotDF_LowerUpper$value_upper <-
      seirScaledPlotDF_LowerUpper$value_upper * N
    #labels
    dataMax <-
      seirScaledPlotDF_melt %>% group_by(variable) %>% summarise(Max = max(value))
    dataMin <-
      seirScaledPlotDF_melt %>% group_by(variable) %>% summarise(Min = min(value))
    dataMax$Min <- dataMin$Min
    dataMax$text <-
      c(
        paste0(
          format.Date(seirScaledPlotDF$date[which(seirScaledPlotDF$S == dataMax$Min[1] /
                                                    N)], "%b %d"),
          " ",
          "Min S: ",
          formatC(
            dataMax$Min[1],
            format = "f",
            big.mark = ",",
            digits = 0
          )
        )[1],
        paste0(
          format.Date(seirScaledPlotDF$date[which(seirScaledPlotDF$E == dataMax$Max[2] /
                                                    N)], "%b %d"),
          " ",
          "Max E: ",
          formatC(
            dataMax$Max[2],
            format = "f",
            big.mark = ",",
            digits = 0
          )
        )[1],
        paste0(
          format.Date(seirScaledPlotDF$date[which(seirScaledPlotDF$I == dataMax$Max[3] /
                                                    N)], "%b %d"),
          " ",
          "Max I: ",
          formatC(
            dataMax$Max[3],
            format = "f",
            big.mark = ",",
            digits = 0
          )
        )[1],
        paste0(
          format.Date(seirScaledPlotDF$date[which(seirScaledPlotDF$R == dataMax$Max[4] /
                                                    N)], "%b %d"),
          " ",
          "Max R: ",
          formatC(
            dataMax$Max[4],
            format = "f",
            big.mark = ",",
            digits = 0
          )
        )[1]
      )
    seirScaledPlotDF_melt$label <-
      rep(NA, nrow(seirScaledPlotDF_melt))
    seirScaledPlotDF_melt$label[which(
      seirScaledPlotDF_melt$value == dataMax$Min[1] &
        seirScaledPlotDF_melt$variable == "S"
    )[1]] <- dataMax$text[1]
    seirScaledPlotDF_melt$label[which(
      seirScaledPlotDF_melt$value == dataMax$Max[2] &
        seirScaledPlotDF_melt$variable == "E"
    )[1]] <- dataMax$text[2]
    seirScaledPlotDF_melt$label[which(
      seirScaledPlotDF_melt$value == dataMax$Max[3] &
        seirScaledPlotDF_melt$variable == "I"
    )[1]] <- dataMax$text[3]
    seirScaledPlotDF_melt$label[which(
      seirScaledPlotDF_melt$value == dataMax$Max[4] &
        seirScaledPlotDF_melt$variable == "R"
    )[1]] <- dataMax$text[4]
    seirScaledPlotDF_melt$label[which(
      seirScaledPlotDF_melt$value == initialInfected &
        seirScaledPlotDF_melt$variable == "I"
    )[1]] <-
      paste0(
        format.Date(seirScaledPlotDF$date[which(seirScaledPlotDF$I == initialInfected /
                                                  N)], "%b %d"),
        ", ",
        "Initial I: ",
        formatC(
          initialInfected,
          format = "f",
          big.mark = ",",
          digits = 0
        )
      )[1]
    seirScaledPlotDF_melt$label[tail(which(
      round(seirScaledPlotDF_melt$value) == round(initialInfected / 1) &
        seirScaledPlotDF_melt$variable == "I"
    ),
    1)] <-
      tail(paste0(
        format.Date(seirScaledPlotDF$date[which(round(seirScaledPlotDF$I * N) == round(initialInfected /
                                                                                         1))], "%b %d"),
        ", ",
        "I: ",
        formatC(
          initialInfected / 1,
          format = "f",
          big.mark = ",",
          digits = 0
        )
      ), 1)
    seirScaledPlotDF_melt$label[tail(which(
      round(seirScaledPlotDF_melt$value) == round(initialInfected / 10) &
        seirScaledPlotDF_melt$variable == "I"
    ),
    1)] <-
      tail(paste0(
        format.Date(seirScaledPlotDF$date[which(round(seirScaledPlotDF$I * N) == round(initialInfected /
                                                                                         10))], "%b %d"),
        ", ",
        "I: ",
        formatC(
          initialInfected / 10,
          format = "f",
          big.mark = ",",
          digits = 0
        )
      ), 1)
    
    unique(seirScaledPlotDF_melt$label)
    plotColors1 <-
      c(rep("black", 2),
        rep("red", 2),
        rep("forestgreen", 2),
        rep("blue", 2))
    plotColors2 <-
      c(rep("black", 1),
        rep("red", 1),
        rep("forestgreen", 1),
        rep("blue", 1))
    # seirScaledPlotDF_melt$variable <-  factor(seirScaledPlotDF_melt$variable, levels = c("S","S_lower","S_upper","E","E_lower","E_upper","I","I_lower","I_upper","R","R_lower","R_upper"), ordered = T)
    seirScaledPlotDF_melt$variable <- as.character(seirScaledPlotDF_melt$variable)
    seirScaledPlotDF_melt$variable[seirScaledPlotDF_melt$variable=="S"] <- "Susceptible"
    seirScaledPlotDF_melt$variable[seirScaledPlotDF_melt$variable=="E"] <- "Exposed"
    seirScaledPlotDF_melt$variable[seirScaledPlotDF_melt$variable=="I"] <- "Infected"
    seirScaledPlotDF_melt$variable[seirScaledPlotDF_melt$variable=="R"] <- "Recovered"
    seirScaledPlotDF_melt$variable <-
      factor(
        seirScaledPlotDF_melt$variable,
        levels = c("Susceptible", "Exposed", "Infected", "Recovered"),
        # levels = c("S", "E", "I", "R"),
        ordered = T
      )
    seirScaledPlotDF_LowerUpper$variable_lower <-
      factor(
        seirScaledPlotDF_LowerUpper$variable,
        levels = c("S_lower", "E_lower", "I_lower", "R_lower"),
        ordered = T
      )
    seirScaledPlotDF_LowerUpper$variable_upper <-
      factor(
        seirScaledPlotDF_LowerUpper$variable,
        levels = c("S_upper", "E_upper", "I_upper", "R_upper"),
        ordered = T
      )
    
    seirScaledPlotDF_LowerUpper$variable <- as.character(seirScaledPlotDF_LowerUpper$variable)
    seirScaledPlotDF_LowerUpper$variable[seirScaledPlotDF_LowerUpper$variable=="S"] <- "Susceptible"
    seirScaledPlotDF_LowerUpper$variable[seirScaledPlotDF_LowerUpper$variable=="E"] <- "Exposed"
    seirScaledPlotDF_LowerUpper$variable[seirScaledPlotDF_LowerUpper$variable=="I"] <- "Infected"
    seirScaledPlotDF_LowerUpper$variable[seirScaledPlotDF_LowerUpper$variable=="R"] <- "Recovered"
    seirScaledPlotDF_LowerUpper$variable <-
      factor(
        seirScaledPlotDF_LowerUpper$variable,
        levels = c("Susceptible", "Exposed", "Infected", "Recovered"),
        # levels = c("S", "E", "I", "R"),
        
        ordered = T
      )

    seirScaled_gg <-
      ggplot(data = seirScaledPlotDF_melt) + #
      geom_vline(
        xintercept = as.numeric(pAll$date[breaker]),
        linetype = "twodash",
        color = "seashell4",
        size = 0.9
      ) +
      # geom_vline(
      #   xintercept = as.numeric(Sys.Date()),
      #   linetype = "twodash",
      #   color = "seashell4",
      #   size = 0.9
      # ) +
      annotate(
        "text",
        x = pAll$date[breaker - 10],
        y = 0.85 * max(seirScaledPlotDF$I)*N*3 * 1.1,
        label = as.character(format.Date(pAll$date[breaker], "%b-%d")),
        color = "gray17",
        angle = 90
      ) +
      # annotate(
      #   "text",
      #   x = Sys.Date() + 10,
      #   y = 0.8 * max(seirScaledPlotDF$I)*N*3 * 1.1,
      #   label = paste0("Today: ", as.character(format.Date(
      #     Sys.Date(), "%b-%d"
      #   ))),
      #   color = "gray17",
      #   angle = 90
      # ) +
      
      # geom_text(aes(x=pAll$date[breaker-2], label=as.character(format.Date(pAll$date[breaker],"%b-%d-%Y")), y=0.93*max(seirScaledPlotDF$R)*N*1.1), colour="seashell4", angle=90, text=element_text(size=12)) +
      # geom_line(position = position_dodge2(width = 0.5, padding = 0.2), linetype = "twodash",size=1.2)+
      # geom_ribbon(aes(ymin=plot1Data$R0_lower, ymax=plot1Data$R0_upper), alpha=0.1, color="forestgreen", fill="forestgreen", linetype="dashed")+
      
      geom_line(position = position_dodge2(width = 0.5, padding = 0.2), 
        data = seirScaledPlotDF_melt,
        aes(x = date, y = value, color = variable),
        size = 1.2,
        alpha = .9,
        linetype = "solid",
        show.legend = T
      ) +
      geom_ribbon(
        data = seirScaledPlotDF_LowerUpper,
        aes(
          x = date,
          ymin = value_lower,
          ymax = value_upper,
          fill = variable
        ),
        alpha = 0.1,
        linetype = "twodash",
        show.legend = F
      ) +
      
      theme(legend.position = "top") +
      labs(colour = NULL) +
      xlab(NULL) +
      scale_x_date(
        date_breaks = "2 week",
        date_labels =  "%b %d",
        limits = c(minDate, maxDate)
      ) +
      # ggtitle(testing_country)+
      theme(text = element_text(size = 14)) +
      # ylim(0,max(seirScaledPlotDF$R)*N)+
      scale_y_continuous(
        label = comma,
        # Features of the first axis
        name = "Population",
        # limits = c(0, max(seirFinal_upper$R) * N * 1),
        # Add a second axis and specify its features
        sec.axis = sec_axis(trans =  ~ . / N * 100, name =
                              "% Population")
      ) +
      # geom_label_repel(
      #   data = seirScaledPlotDF_melt,
      #   aes(
      #     x = date,
      #     y = value,
      #     label = label,
      #     fill = variable
      #   ),
      #   color = 'white',
      #   size = 4,
      #   show.legend = F,
      #   segment.color = "black",
      #   box.padding = 6
      # ) +
    
      # scale_x_date(breaks = seq(from=min(m1$date,na.rm=T),to=max(m1$date,na.rm=T),length.out=5), labels = date_format("%d"))+
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
      labs(x = NULL)+
      coord_cartesian(ylim = c(0, max(seirScaledPlotDF$I)*N*3), xlim = as.Date(c(min(seirScaledPlotDF$date),max(seirScaledPlotDF$date))) )+
      scale_color_manual(values = plotColors2, breaks = c("Susceptible", "Exposed", "Infected", "Recovered"), labels = c("Susceptible", "Exposed", "Infected", "Recovered"), name=NULL) +
      scale_fill_manual(values = plotColors2, breaks = c("Susceptible", "Exposed", "Infected", "Recovered"), labels = c("Susceptible", "Exposed", "Infected", "Recovered"), name=NULL)
      # scale_color_manual(values = plotColors2, breaks = c("S", "E", "I", "R"), labels = c("Susceptible", "Exposed", "Infected", "Recovered"), name=NULL) +
      # scale_fill_manual(values = plotColors2, breaks = c("S", "E", "I", "R"), labels = c("Susceptible", "Exposed", "Infected", "Recovered"), name=NULL)
    
      # setClass("Class", slots = c(name = "type"))y_continuous(trans='log10')
    # seirScaled_gg
    
    incidenceDF <-
      testing_subset_aligned[, c("date", "confirmed", "movingAverage")]
    colnames(incidenceDF) <-
      c("Date", "New Cases", "Moving Average")
    merge1 <- seirScaledPlotDF[, c("date", "I")]
    colnames(merge1) <- c("date", "SEIR I-Curve")
    merge1$date <- as.Date(merge1$date)
    incidenceDF_premelt <-
      merge(
        merge1,
        incidenceDF,
        seirScaledPlotDF,
        by.x = "date",
        by.y = "Date",
        all.x = T,
        all.y = T
      )
    incidenceDF_premelt$`SEIR I-Curve` <-
      incidenceDF_premelt$`SEIR I-Curve` * N
    Icurve <- seirScaledPlotDF$`I`[seq(from=1,to=nrow(seirScaledPlotDF),by=100)] * N
    Idiff <- rep(NA,length(Icurve))
    for(i in 0:(length(Icurve)-1)){
      if(i==0){
        Idiff[i] <- Icurve[1]
      }else{
        Idiff[i] <- Icurve[i+1]-Icurve[i]
      }
    }
    Rcurve <- seirScaledPlotDF$`R`[seq(from=1,to=nrow(seirScaledPlotDF),by=100)] * N
    Rdiff <- rep(NA,length(Rcurve))
    for(i in 0:(length(Rcurve)-1)){
      if(i==0){
        Rdiff[i] <- Rcurve[1]
      }else{
        Rdiff[i] <- Rcurve[i+1]-Rcurve[i]
      }
    }
    
    incidenceDF_premelt$`SEIR New Infections` <- rep(Idiff,each=100)+rep(Rdiff,each=100)
    # for(i in (floor(recoveryTime)+1):nrow(incidenceDF_premelt)){
    #   incidenceDF_premelt$`Currently Infected`[i] <- incidenceDF_premelt$`Moving Average`[i] + sum(incidenceDF_premelt$`Moving Average`[(i-floor(recoveryTime)):i])
    # }
    # for(j in 2:floor(recoveryTime)){
    #   incidenceDF_premelt$`Currently Infected`[j] <- incidenceDF_premelt$`Moving Average`[j] + sum(incidenceDF_premelt$`Moving Average`[1:j])
    # }
    
    ####
    incidenceDF_premelt$`SEIR I-Curve` <- NULL
    ####
    
    incidenceDF_melt <- melt(incidenceDF_premelt, id = c("date"))
    incidenceDF_melt$date <- as.Date(incidenceDF_melt$date)
    maxy <-
      max(c(
        max(incidenceDF_melt$value, na.rm = T),
        subset(seirScaledPlotDF_LowerUpper, variable == "Infected")[["value_upper"]]
      ), na.rm = T)
    
    incidence_gg <- ggplot(data = incidenceDF_melt) +
      geom_line(position = position_dodge2(width = 0.5, padding = 0.2), 
        data = incidenceDF_melt,
        aes(x = date, y = value, color = variable),
        linetype = "solid",
        size = 1.2
      ) +
      # geom_ribbon(
      #   data = subset(seirScaledPlotDF_LowerUpper, variable == "Infected"),
      #   aes(x = date, ymin = value_lower, ymax = value_upper),
      #   fill = "forestgreen",
      #   color = "forestgreen",
      #   alpha = 0.1,
      #   linetype = "twodash",
      #   show.legend = F
      # ) +
      geom_vline(
        xintercept = as.numeric(pAll$date[breaker]),
        linetype = "twodash",
        color = "seashell4",
        size = 0.9
      ) +
      # geom_vline(
      #   xintercept = as.numeric(Sys.Date()),
      #   linetype = "twodash",
      #   color = "seashell4",
      #   size = 0.9
      # ) +
      annotate(
        "text",
        x = pAll$date[breaker - 10],
        y = 0.85 * max(incidenceDF_premelt[c("SEIR New Infections","Moving Average")],na.rm=T)*1.1 * 1.1,
        label = as.character(format.Date(pAll$date[breaker], "%b-%d")),
        color = "gray17",
        angle = 90
      ) +
      # annotate(
      #   "text",
      #   x = Sys.Date() + 10,
      #   y = 0.85 * max(incidenceDF_premelt[c("SEIR New Infections","Moving Average")],na.rm=T)*1.3 * 1.1,
      #   label = paste0("Today: ", as.character(format.Date(
      #     Sys.Date(), "%b-%d"
      #   ))),
      #   color = "gray17",
      #   angle = 90
      # ) +
      
      # geom_text(aes(x=pAll$date[breaker-2], label=as.character(format.Date(pAll$date[breaker],"%b-%d-%Y")), y=0.93*max(incidenceDF_melt$value,na.rm=T)), colour="seashell4", angle=90, text=element_text(size=12)) +
      # scale_color_manual(breaks = c("New Cases", "Moving Average", "SEIR New Infections", "SEIR I-Curve"), values = c("orange", "tomato3", "turquoise2", "forestgreen")) +
      scale_color_manual(breaks = c("New Cases", "Moving Average", "SEIR New Infections"), values = c("orange", "tomato3", "turquoise2")) +
      theme(legend.position = "top") +
      labs(colour = NULL) +
      xlab(NULL) +
      scale_x_date(
        date_breaks = "2 week",
        date_labels =  "%b %d",
        limits = c(minDate, maxDate)
      ) +
      # ggtitle(paste0("COVID-19 Cases in ", as.character(testing_subset_aligned$Country[1]))) +
      theme(text = element_text(size = 14)) +
      scale_y_continuous(label = comma,
                         # Features of the first axis
                         name = "Population"
                         # limits = c(0, max(seirFinal$I) * N * 2.4)
      )+
      # Add a second axis and specify its features
      # sec.axis = sec_axis( trans=~., name="New Daily Cases")) +
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
      labs(x = NULL)+
      coord_cartesian(ylim = c(0, max(incidenceDF_premelt[c("SEIR New Infections","Moving Average")],na.rm=T)*1.1), xlim = as.Date(c(min(seirScaledPlotDF$date),max(seirScaledPlotDF$date))) )
      # coord_cartesian(ylim = c(0, max(seirScaledPlotDF$I)*N*2), xlim = as.Date(c(min(seirScaledPlotDF$date),max(seirScaledPlotDF$date))) )
    
    incidence_gg
    
    
    
    
    #---cumulativePlot---#########################################################################################################################################################################
    # rmsedf <-
    #   plot1Data[1:(breaker-1), ]
    # rmse_val <-
    #   sqrt(sum(
    #     (
    #       rmsedf$`Status Quo Prediction` - rmsedf$`Empirical R0 Timeseries`
    #     ) ^ 2 ,
    #     na.rm = T
    #   ) / (length(rmsedf$`Status Quo Prediction`[!is.na(rmsedf$`Status Quo Prediction`)])))
    # rmse_val <- round(rmse_val, 3)
    # gl <-
    #   list(
    #     plot1,
    #     plot2,
    #     plot_predict,
    #     plot_varimp_R,
    #     plot3,
    #     plot4,
    #     plot5,
    #     plot_varimp_nonR,
    #     incidence_gg,
    #     seirScaled_gg
    #   )
    # dateName <- format(as.Date(timeChop), "%Y-%m-%d")
    # pdf(
    #   paste0(
    #     gwd,"/Output_R0/finalPlot_",
    #     testing_country,
    #     "_",
    #     dateName,
    #     "_R0_",
    #     "Scenarios",
    #     ".pdf"
    #   ),
    #   width = 24,
    #   height = 24
    # )
    # grid.arrange(
    #   grobs = gl,
    #   top = textGrob(
    #     paste0(
    #       testing_ready$FullName[1],
    #       " ",
    #       "Data Through ",
    #       dateName,
    #       ", Empirical R0 Timeseries -vs- Status Quo Prediction RMSE = ",
    #       rmse_val
    #     ),
    #     gp = gpar(fontsize = 20)
    #   ),
    #   layout_matrix = rbind(
    #     c(1, 1, 1, 1, 1, 4, 4, 4),
    #     c(1, 1, 1, 1, 1, 4, 4, 4),
    #     c(1, 1, 1, 1, 1, 8, 8, 8),
    #     c(1, 1, 1, 1, 1, 8, 8, 8),
    #     c(1, 1, 1, 1, 1, 8, 8, 8),
    #     c(1, 1, 1, 1, 1, 8, 8, 8),
    #     c(2, 2, 2, 2, NA, 8, 8, 8),
    #     c(2, 2, 2, 2, NA, 8, 8, 8),
    #     c(2, 2, 2, 2, 5, 5, 5, 5),
    #     c(2, 2, 2, 2, 5, 5, 5, 5),
    #     c(2, 2, 2, 2, 5, 5, 5, 5),
    #     c(3, 3, 3, 3, 6, 6, 6, 6),
    #     c(3, 3, 3, 3, 6, 6, 6, 6),
    #     c(3, 3, 3, 3, 6, 6, 6, 6),
    #     c(3, 3, 3, 3, 7, 7, 7, 7),
    #     c(3, 3, 3, 3, 7, 7, 7, 7),
    #     c(3, 3, 3, 3, 7, 7, 7, 7),
    #     c(9, 9, 9, 9, 10, 10, 10, 10),
    #     c(9, 9, 9, 9, 10, 10, 10, 10),
    #     c(9, 9, 9, 9, 10, 10, 10, 10),
    #     c(9, 9, 9, 9, 10, 10, 10, 10),
    #     c(9, 9, 9, 9, 10, 10, 10, 10)
    #   )
    # )
    # dev.off()
    # #
    # # plot(best_model)
    # #

    #---Output variables for RShiny App---#########################################################################################################################################################################
    
    # Saving on object in RData format
    # save.image(file = paste0(
    #   gwd,"/COVID-19_Shiny_Web_App/Inputs/",
    #   "R0_",
    #   "Scenarios",
    #   ".RData"
    # ))
    
    save(
      plot1,
      plot_predict,
      plot2,
      plot3,
      plot4,
      plot5,
      plot_varimp,
      plot_varimp_R,
      plot_varimp_nonR,
      seirScaled_gg,
      incidence_gg,
      best_model,
      rf.mod,
      rf.mod.varImp,
      testing_subset_aligned,
      plot1Data,
      pAll,
      m1,
      breaker,
      file = paste0(
        gwd,"/InputData/",
        "R0_",
        "Scenarios",
        ".RData"
      )
    )
    
    

