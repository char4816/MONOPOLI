# # # # 
# # # # #---dataSetup---#########################################################################################################################################################################
# # # #
# # # # Pick a 3 Leter Code corresponding to your country of interest
# # # # 1	  ITA	Italy
# # # # 2	  GBR	United Kingdom
# # # # 3	  ZAF	South Africa
# # # # 4	  BRA	Brazil
# # # # 5	  ESP	Spain
# # # # 6	  MYS	Malaysia
# # # # 7	  KOR	South Korea
# # # # 8	  USA	United States
# # # # 9	  SWE	Sweden
# # # # 10	AUT	Austria
# # # # 11	CHE	Switzerland
# # # # 12	DEU	Germany
# # # # 13	FRA	France
# # # # 14	DZA	Algeria
# # # # 15	IRN	Iran
# # # # 16	CAN	Canada
# # # # 17	TUR	Turkey
# # # # 18	BEL	Belgium
# # # # 19	ANT	Netherlands
# # # # 20	PRT	Portugal
# # # # 21	ISR	Israel
# # # # 22	RUS	Russia
# # # # 23	NOR	Norway
# # # # 24	IRL	Ireland
# # # # 25	AUS	Australia
# # # # 26	IND	India
# # # # 27	DNK	Denmark
# # # # 28	CHL	Chile
# # # # 29	CZE	Czechia
# # # # 30	JPN	Japan
# # # # 31	UKR	Ukraine
# # # # 32	MAR	Morocco
# # # # 33	ARG	Argentina
# # # # 34	SGP	Singapore
# # # # 35	ROU	Romania
# testing_countriesList <- c("USA")
# # chosen_initialInfected <- 15000
# # # # # Estimate Percent of Peak your country has achieved
# # percPeak <- 0.9
# # # # # Choose a timepoint between 0 and 5,
# # # # # 0 = early curve, 5 = late curve
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
# # # # # # 0 = No social distancing measures implemented
# # # # # # 1 = Large gatherings banned (e.g. concerts, sporting events, conferences)
# # # # # # 2 = Mid-sized gatherings are voluntarily closed
# # # # # # 3 = Service industries are closed (e.g., restaurants and pubs)
# # # # # # 4 = Shelter-in-place orders for non-essential workers
# # # # # # 5 = Lockdown except for essentials (e.g. grocery shopping)
# # # # # Social_Distancing_Custom <- 3
# # # # # # 0 = No action is taken to quarantine cases
# # # # # # 1 = Infected individuals are quarantined
# # # # # # 2 = Households of infected individuals are quarantined
# # # # # # 3 = Others in contact with infected individuals are tracked and then quarantined
# # # # # Quaranting_Cases_Custom <- 2
# # # # # # 0 = No restrictions at the border
# # # # # # 1 = Closed to Wuhan
# # # # # # 2 = Closed to multiple highly infected countries
# # # # # # 3 = Closed except for essential travel
# # # # # # 4 = Closed fully
# # # # # Close_Border_Custom <- 3
# # # #
# # # #
# StringencyIndexForDisplay_Custom <- 10
# GovernmentResponseIndexForDisplay_Custom <- 25
# ContainmentHealthIndexForDisplay_Custom <- 30
# EconomicSupportIndexForDisplay_Custom <- 30
# chosen_initialInfected <- 30000
# chosen_initialExposed <- 30000
# # 
# #---Flag setup---#########################################################################################################################################################################
# # gwd <- getwd()
# gwd <- "."
# # TRUE if you want to scale by population
# incidence_flag <- T
# # TRUE if you want to do deaths instead of cases
# death_flag <- F
# incidence_start_point <- 0.3
# count_start_point <- 100
# # if we are doing deaths, we want incidence start point to be about 5.9% of the case one becuase that's the approx mortality rate
# # if (death_flag == T) {
# #   incidence_start_point <- incidence_start_point * (5.9 / 100)
# #   count_start_point <- count_start_point * (5.9 / 100)
# # }
# VSURFflag <- F
# RunWT_R_flag <- F
# # the number of lag factors you want
# nLags <- 14
# # the time you want to forecast the predictiont
# # forecastingTime <- 14
# forecastingTimeFlag = "fullRange"
# # autofill the missing datapoints in NPI data
# NPIflag1 <- "autofill"
# # use the last NPI datapoints for the forecasting period
# NPIflag2 <- "lastNPI"
# # NPIflag2 <- "firstNPI"
# # The Percent of the timeframe you want to reserve for testing a country (the rest of that country's time series is included into the model)
# # for example if testingTimeFrame <- 0.8, then 20% of the timeseries will be used to train, and 80% will be used to predict
# testingTimeFrame <- 1
# # Number of cores to use when running rf model and vsurf
# num_cores = detectCores()
# # What NA acion to use for models
# nasaction = na.omit
# # Number of trees to train on
# number_trees = 1000
# 
# 
# #---beginCode---#########################################################################################################################################################################
# 
# '%ni%' <- Negate('%in%')
# simpleCap <- function(x) {
#   s <- strsplit(x, " ")[[1]]
#   paste(toupper(substring(s, 1, 1)),
#         substring(s, 2),
#         sep = "",
#         collapse = " ")
# }
# 
# 
# for (cc in 1:length(testing_countriesList)) {
#   # cc=1
#   
#   # Choose the testing country
#   testing_country <- testing_countriesList[cc]
#   
#   # make country lists, these are the ones that we have NPI data collected for
#   # https://docs.google.com/spreadsheets/d/1vrKvs52OAxuB7x2kT9r1q6IcIBxGEQsNRHsK_o7h3jo/edit#gid=378237553
#   # training_countries_all <- c("ITA","FRA","GBR")
#   # training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","MYS","USA","SWE","AUT","CHE","DEU","FRA","DZA","ISR")
#   # training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","KOR","USA","SWE","AUT","CHE","DEU","FRA","DZA","IRN","CAN","PRT","ISR","RUS","NOR","AUS","DNK","CHL","CZE","JPN","UKR","MAR","ARG")
#   # training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","MYS","HUB","KOR","USA","SWE","AUT","CHE","DEU","FRA","DZA","IRN","CAN","TUR","BEL","ANT","PRT","ISR","RUS","NOR","IRL","AUS","IND","DNK","CHL","CZE","JPN","UKR","MAR","ARG","SGP","ROU")
#   
#   # training_countries_all <-
#   #   c(
#   #     "ITA",
#   #     "GBR",
#   #     "ZAF",
#   #     "BRA",
#   #     "ESP",
#   #     "MYS",
#   #     "KOR",
#   #     "USA",
#   #     "SWE",
#   #     "AUT",
#   #     "CHE",
#   #     "DEU",
#   #     "FRA",
#   #     "DZA",
#   #     "IRN",
#   #     "CAN",
#   #     "TUR",
#   #     "BEL",
#   #     "ANT",
#   #     "PRT",
#   #     "ISR",
#   #     # "RUS",
#   #     "NOR",
#   #     "IRL",
#   #     "AUS",
#   #     "IND",
#   #     "DNK",
#   #     "CHL",
#   #     "CZE",
#   #     "JPN",
#   #     "UKR",
#   #     "MAR",
#   #     "ARG",
#   #     "SGP",
#   #     "ROU"
#   #   )
#   # training_countries <- training_countries_all
# 
#   
#   data_clean <- read.csv(paste0(gwd,"/InputData/ML_features_oxford.csv"))
#   
#   training_countries_all <- as.character(unique(data_clean$ISO3))
#   training_countries <-
#     training_countries_all[which(training_countries_all != testing_country)]
#   
#   data_clean$date <- as.Date(data_clean$date)
#   data_clean1 <- subset(data_clean, ISO3 == testing_country)
#   D4 <-
#     data_clean1$date[which.max(data_clean1$confirmed_cum_per_million)]
#   D3 <-
#     data_clean1$date[which.min(
#       abs(
#         data_clean1$confirmed_cum_per_million - data_clean1$confirmed_cum_per_million[which(data_clean1$date == D4)] *
#           2 / 3
#       )
#     )]
#   D2 <-
#     data_clean1$date[which.min(
#       abs(
#         data_clean1$confirmed_cum_per_million - data_clean1$confirmed_cum_per_million[which(data_clean1$date == D4)] *
#           1 / 3
#       )
#     )]
#   D1 <-
#     data_clean1$date[which.min(
#       abs(
#         data_clean1$confirmed_cum_per_million - data_clean1$confirmed_cum_per_million[which(data_clean1$date == D4)] *
#           1 / 10
#       )
#     )]
#   earliestD <- which(data_clean1$confirmed_cum >= 50)[1]
#   # if(D1 - data_clean1$date[start])
#   D0 <- data_clean1$date[earliestD] + 15 #"2020-03-21"
#   print(D1)
#   print(D2)
#   print(D3)
#   print(D4)
#   dateList <- as.Date(rev(c(D4, D3, D2, D1, D0)))
#   dateList[dateList < D0] <- D0
#   
#   
#   # if(forecastingTimeFlag == "fullRange"){
#   #   forecastingTime <- as.numeric(as.Date(D4)+14-as.Date(timeChop))
#   # }else{
#   #   forecastingTime = 14
#   # }
#   forecastingTime = 20
#   
#   data_clean <- read.csv(paste0(gwd,"/InputData/ML_features_oxford.csv"))
#   data_clean$date <- as.Date(data_clean$date)
#   data_clean_train <- data_clean
#   data_clean <- subset(data_clean, date <= D4)
#   
#   # Looking at the data
#   glimpse(data_clean)
#   summary(data_clean)
#   
#   #---Estimating reproduction numbers, R0---#########################################################################################################################################################################
#   # # We are using the epiestim package to calculate the R0 for countries. This requires two things
#   # # 1. specific information about the serial intervals for COVID
#   # # 2. the timeseries incidence data.
#   # # "SI for Serial Intervals.
#   # # Determination of the serial interval, the time between the start of symptoms in the primary patient (infector)
#   # # and onset of symptoms in the patient receiving that infection from the infector (the infectee)"
#   # # Table 1 from https://www.medrxiv.org/content/10.1101/2020.04.13.20062760v1
#   # # "we calculate a weighted mean of the published parameters and inferred a serial interval described
#   # # by a gamma distribution, parameterised with mean SI of 4.56 days (credible interval: 2.54 - 7.36)
#   # # and standard deviation 4.53 days (credible interval 4.17 - 5.05)."
#   # 
#   # serialIntervals = tibble(
#   #   mean_si_estimate = c(3.96, 6.3, 4.22, 4.56, 3.95, 5.21, 4.7, 7.5, 6.6),
#   #   mean_si_estimate_low_ci = c(3.53, 5.2, 3.43, 2.69, -4.47,-3.35, 3.7, 5.3, 0.7),
#   #   mean_si_estimate_high_ci = c(4.39, 7.6, 5.01, 6.42, 12.51, 13.94, 6.0, 19.0, 19.0),
#   #   std_si_estimate = c(4.75, 4.2, 0.4, 0.95, 4.24, 4.32, 2.3, 3.4, NA),
#   #   std_si_estimate_low_ci = c(4.46, 3.1, NA, NA, 4.03, 4.06, 1.6, NA, NA),
#   #   std_si_estimate_high_ci = c(5.07, 5.3, NA, NA, 4.95, 5.58, 3.5, NA, NA),
#   #   sample_size = c(468, 48, 135, 93, 45, 54, 28, 16, 90),
#   #   population = c(
#   #     "China",
#   #     "Shenzhen",
#   #     "Taijin",
#   #     "Singapore",
#   #     "Taijin",
#   #     "Singapore",
#   #     "SE Asia",
#   #     "Wuhan",
#   #     "Italy"
#   #   ),
#   #   source = c(
#   #     "Zhanwei Du et al. Serial Interval of COVID-19 among Publicly Reported Confirmed Cases. Emerging Infectious Disease journal 26, (2020)",
#   #     "Bi, Q. et al. Epidemiology and Transmission of COVID-19 in Shenzhen China: Analysis of 391 cases and 1,286 of their close contacts. Infectious Diseases (except HIV/AIDS) (2020) doi:10.1101/2020.03.03.20028423",
#   #     "Tindale, L. et al. Transmission interval estimates suggest pre-symptomatic spread of COVID-19. Epidemiology (2020) doi:10.1101/2020.03.03.20029983",
#   #     "Tindale, L. et al. Transmission interval estimates suggest pre-symptomatic spread of COVID-19. Epidemiology (2020) doi:10.1101/2020.03.03.20029983",
#   #     "Ganyani, T. et al. Estimating the generation interval for COVID-19 based on symptom onset data. Infectious Diseases (except HIV/AIDS) (2020) doi:10.1101/2020.03.05.20031815",
#   #     "Ganyani, T. et al. Estimating the generation interval for COVID-19 based on symptom onset data. Infectious Diseases (except HIV/AIDS) (2020) doi:10.1101/2020.03.05.20031815",
#   #     "Nishiura, H., Linton, N. M. & Akhmetzhanov, A. R. Serial interval of novel coronavirus (COVID-19) infections. Int. J. Infect. Dis. (2020) doi:10.1016/j.ijid.2020.02.060",
#   #     "Li, Q. et al. Early Transmission Dynamics in Wuhan, China, of Novel Coronavirus-Infected Pneumonia. N. Engl. J. Med. (2020) doi:10.1056/NEJMoa2001316",
#   #     "Cereda, D. et al. The early phase of the COVID-19 outbreak in Lombardy, Italy. arXiv [q-bio.PE] (2020)"
#   #   )
#   # )
#   # 
#   # unk = function(x)
#   #   ifelse(is.na(x), "unk", x)
#   # 
#   # SItable1 = serialIntervals %>% mutate(
#   #   `Mean SI\n(95% CrI) days` = paste0(
#   #     mean_si_estimate,
#   #     "\n(",
#   #     unk(mean_si_estimate_low_ci),
#   #     "-",
#   #     unk(mean_si_estimate_high_ci),
#   #     ")"
#   #   ),
#   #   `Std SI\n(95% CrI) days` = paste0(
#   #     unk(std_si_estimate),
#   #     "\n(",
#   #     unk(std_si_estimate_low_ci),
#   #     "-",
#   #     unk(std_si_estimate_high_ci),
#   #     ")"
#   #   )
#   # ) %>% dplyr::select(-contains("estimate")) %>% dplyr::select(
#   #   `Reference` = source,
#   #   `Mean SI\n(95% CrI) days`,
#   #   `Std SI\n(95% CrI) days`,
#   #   `N` = sample_size,
#   #   `Population` = population
#   # )
#   # 
#   # wtSIs = serialIntervals %>% summarise(
#   #   mean_si = weighted.mean(mean_si_estimate, sample_size, na.rm = TRUE),
#   #   min_mean_si = weighted.mean(mean_si_estimate_low_ci, sample_size, na.rm = TRUE),
#   #   max_mean_si = weighted.mean(mean_si_estimate_high_ci, sample_size, na.rm = TRUE),
#   #   std_si  = weighted.mean(ifelse(is.na(
#   #     std_si_estimate_low_ci
#   #   ), NA, 1) * std_si_estimate, sample_size, na.rm = TRUE),
#   #   min_std_si  = weighted.mean(std_si_estimate_low_ci, sample_size, na.rm = TRUE),
#   #   max_std_si  = weighted.mean(std_si_estimate_high_ci, sample_size, na.rm = TRUE)
#   #   #total = sum(sample_size)
#   # ) %>% mutate(
#   #   std_mean_si = (max_mean_si - min_mean_si) / 3.92,
#   #   # TODO: fit gamma
#   #   std_std_si = (max_std_si - min_std_si) / 3.92
#   # )
#   # 
#   # config = make_config(
#   #   list(
#   #     si_parametric_distr = "G",
#   #     mean_si = wtSIs$mean_si,
#   #     std_mean_si = wtSIs$std_mean_si,
#   #     min_mean_si = wtSIs$min_mean_si,
#   #     max_mean_si = wtSIs$max_mean_si,
#   #     std_si = wtSIs$std_si,
#   #     std_std_si = wtSIs$std_si,
#   #     min_std_si = wtSIs$min_std_si,
#   #     max_std_si = wtSIs$max_std_si
#   #   ),
#   #   method = "uncertain_si"
#   # )
#   
#   
#   
#   #---trainingTestingDataFrames---#########################################################################################################################################################################
#   # Subset training country list to those countries that had enough cases (to calc R0) by the start point
#   lengthClist <- length(training_countries)
#   training_countries_OG <- training_countries
#   for (i in 1:length(training_countries)) {
#     training_subset <-
#       subset(data_clean_train, ISO3 %in% training_countries[i])
#     
#     print(i)
#     print(training_countries[i])
#     
#     start <- which(training_subset$confirmed_cum >= 50)[1]
#     
#     training_subset_aligned <-
#       training_subset[start:nrow(training_subset),]
#     training_subset_aligned$time <-
#       c(1:nrow(training_subset_aligned))
#     
#     # Smooth out incidence using moving average with a centered window of 7 datapoints (3 to the left, 3 to the right)
#     # make sure the window is an odd integer
#     window <- 7
#     # dim(training_subset_aligned)
#     # length(rollmean(training_subset_aligned$confirmed, k=window))
#     training_subset_aligned$movingAverage <-
#       c(
#         training_subset_aligned$confirmed[1:((window - 1) / 2)],
#         rollmean(
#           training_subset_aligned$confirmed,
#           k = window,
#           align = "center"
#         ),
#         training_subset_aligned$confirmed[(nrow(training_subset_aligned) - ((window -
#                                                                                1) / 2) + 1):nrow(training_subset_aligned)]
#       )
#     # Plot cases
#     gg <- ggplot(training_subset_aligned) +
#       geom_line(position = position_dodge2(width = 0.5, padding = 0.2), aes(x = date, y = confirmed), color = "red") +
#       geom_line(position = position_dodge2(width = 0.5, padding = 0.2), aes(x = date, y = movingAverage), color = "blue") +
#       ggtitle(paste0("Reported cases in ", training_countries[i]))
#     # print(gg)
#     # Add moving average day lag and one day difference variables
#     training_subset_aligned[["movingAverage_lag_1"]] <-
#       lag(training_subset_aligned[["movingAverage"]], 1)
#     training_subset_aligned[["movingAverage_lag_3"]] <-
#       lag(training_subset_aligned[["movingAverage"]], 3)
#     training_subset_aligned[["movingAverage_lag_7"]] <-
#       lag(training_subset_aligned[["movingAverage"]], 7)
#     training_subset_aligned[["movingAverage_lag_14"]] <-
#       lag(training_subset_aligned[["movingAverage"]], 14)
#     training_subset_aligned[["movingAverage_diff_1_3"]] <-
#       training_subset_aligned[["movingAverage_lag_1"]] - training_subset_aligned[["movingAverage_lag_3"]]
#     training_subset_aligned[["movingAverage_diff_1_7"]] <-
#       training_subset_aligned[["movingAverage_lag_1"]] - training_subset_aligned[["movingAverage_lag_7"]]
#     training_subset_aligned[["movingAverage_diff_1_14"]] <-
#       training_subset_aligned[["movingAverage_lag_1"]] - training_subset_aligned[["movingAverage_lag_14"]]
#     training_subset_aligned[["movingAverage_diff_3_7"]] <-
#       training_subset_aligned[["movingAverage_lag_3"]] - training_subset_aligned[["movingAverage_lag_7"]]
#     training_subset_aligned[["movingAverage_diff_7_14"]] <-
#       training_subset_aligned[["movingAverage_lag_7"]] - training_subset_aligned[["movingAverage_lag_14"]]
#     
#     
#     # toCalcR0 <- training_subset_aligned[,c("date","confirmed")]
#     toCalcR0 <-
#       training_subset_aligned[, c("date", "movingAverage")]
#     colnames(toCalcR0) <- c("dates", "I")
#     toCalcR0$I[toCalcR0$I < 0] <- NA
#     #Get of erroneous negative counts... they sneak throught the API sometimes.
#     # But if thre is a negative at teh end... are the last one lets just make it equal to the n-1 one
#     if (is.na(tail(toCalcR0$I, 1))) {
#       toCalcR0$I[length(toCalcR0$I)] <- toCalcR0$I[length(toCalcR0$I) - 1]
#     }
#     # If the NA is not at the end, Lets linearly interpolate them:
#     toCalcR0$I <- na.approx(toCalcR0$I)
#     toCalcR0$I <- as.integer(toCalcR0$I)
#     
#     # res_uncertain_si <- estimate_R(toCalcR0,
#     #                                method = "uncertain_si",
#     #                                config = config)
#     # save(res_uncertain_si,
#     #      file = paste0("./InputData/", training_countries[i], "_WT_R0.Rdata"))
#     
#     if (RunWT_R_flag == T) {
#       # res_uncertain_si <-
#       #   wallinga_teunis(
#       #     toCalcR0,
#       #     method = "parametric_si",
#       #     config = list(
#       #       si_parametric_distr = "G",
#       #       t_start = seq(1, nrow(toCalcR0) -
#       #                       6),
#       #       t_end = seq(7, nrow(toCalcR0)),
#       #       mean_si = wtSIs$mean_si,
#       #       std_mean_si = wtSIs$std_mean_si,
#       #       min_mean_si = wtSIs$min_mean_si,
#       #       max_mean_si = wtSIs$max_mean_si,
#       #       std_si = wtSIs$std_si,
#       #       std_std_si = wtSIs$std_si,
#       #       min_std_si = wtSIs$min_std_si,
#       #       max_std_si = wtSIs$max_std_si,
#       #       n_sim = 100
#       #     )
#       #   )
#       incid <- as.numeric(toCalcR0$I)
#       names(incid) <- as.Date(toCalcR0$dates)
#       empez <- as.Date(toCalcR0$dates[1])
#       fin <- as.Date(toCalcR0$dates[nrow(toCalcR0)])
#       # mGT<-generation.time("gamma", c(wtSIs$mean_si, wtSIs$std_si))
#       # https://epiforecasts.io/covid/methods.html
#       # generation time with a mean of 3.6 days (sd: 0.7 days) 
#       mGT <- generation.time("gamma", c(3.6,0.7))
#       estR0 <- est.R0.TD(incid, mGT, begin=empez, end=fin, nsim=1000)
#       ## An interesting way to look at these results is to agregate initial data by longest time unit,
#       ## such as weekly incidence. This gives a global overview of the epidemic.
#       # estR0.weekly <- smooth.Rt(estR0, 7)
#       # estR0.weekly$R
#       plot(estR0)
#       # plot(estR0.weekly)
#       
#       save(estR0,
#            file = paste0("./InputData/", training_countries[i], "_WT_R0.Rdata"))
#     } else{
#       res_uncertain_si <- NULL
#       load(paste0("./InputData/", training_countries[i], "_WT_R0.Rdata"))
#     }
#     
#     finalR_WT <- as.data.frame(estR0$R)
#     finalR_WT$date <- rownames(finalR_WT)
#     rownames(finalR_WT) <- NULL
#     colnames(finalR_WT) <- c("R0","date")
#     finalR_WT <- finalR_WT[, c("date","R0")]
#     finalR_WT$R0[nrow(finalR_WT)] <- finalR_WT$R0[nrow(finalR_WT)-1]
#     finalR_WT$date <- as.Date(finalR_WT$date)
#     training_subset_aligned <- merge(training_subset_aligned, finalR_WT, by="date", all.x = T, all.y = F)
#     training_subset_aligned[,c("date","R0")]
#     
#     
#     
#     # training_subset_aligned$R0 <- NA
#     # training_subset_aligned$R0[head(res_uncertain_si[["R"]]$`t_start`, 1):(tail(res_uncertain_si[["R"]]$`t_start`, 1))] <-
#     #   res_uncertain_si[["R"]]$`Mean(R)`
#     # # Autofill beginning R0s with first value
#     # training_subset_aligned$R0[1:head(res_uncertain_si[["R"]]$`t_start`, 1)] <-
#     #   mean(head(res_uncertain_si[["R"]]$`Mean(R)`, 1))
#     # # Autofill ending R0s with linear estimation from last week of values
#     # fitFrame <-
#     #   as.data.frame(cbind(c(1:7), tail(res_uncertain_si[["R"]]$`Mean(R)`, 7)))
#     # fit <- lm(V2 ~ V1, data = fitFrame)
#     # fitPred <-
#     #   as.data.frame(8:(8 + length((tail(res_uncertain_si[["R"]]$`t_start`, 1)):nrow(training_subset_aligned)
#     #   )))
#     # colnames(fitPred) <- c("V1")
#     # training_subset_aligned$R0[(tail(res_uncertain_si[["R"]]$`t_start`, 1)):nrow(training_subset_aligned)] <-
#     #   NA #predict.lm(fit,fitPred) #mean(tail(res_uncertain_si[["R"]]$`Mean(R)`,5))
#     listToLag <-
#       c(
#         "R0"
#         # "Google_Retail_recreation",
#         # "Google_Grocery_pharmacy",
#         # "Google_Parks",
#         # "Google_Transit_stations",
#         # "Google_Workplaces",
#         # "Google_Residential"
#       )
#     # c(2,5,8,11,14)
#     for (npi in 1:length(listToLag)) {
#       # Add 2 day lag factor for R0
#       training_subset_aligned[[paste0(listToLag[npi], "_lag_2")]] <-
#         lag(training_subset_aligned[[paste0(listToLag[npi])]], 2)
#       training_subset_aligned[[paste0(listToLag[npi], "_lag_2")]][1:2] <-
#         mean(training_subset_aligned[[paste0(listToLag[npi])]][1:3])
#       # Add 5 day lag factor for R0
#       training_subset_aligned[[paste0(listToLag[npi], "_lag_5")]] <-
#         lag(training_subset_aligned[[paste0(listToLag[npi])]], 5)
#       training_subset_aligned[[paste0(listToLag[npi], "_lag_5")]][1:5] <-
#         mean(training_subset_aligned[[paste0(listToLag[npi])]][1:3])
#       # Add 8 day lag factor for R0
#       training_subset_aligned[[paste0(listToLag[npi], "_lag_8")]] <-
#         lag(training_subset_aligned[[paste0(listToLag[npi])]], 8)
#       training_subset_aligned[[paste0(listToLag[npi], "_lag_8")]][1:8] <-
#         mean(training_subset_aligned[[paste0(listToLag[npi])]][1:3])
#       # Add 11 day lag factor for R0
#       training_subset_aligned[[paste0(listToLag[npi], "_lag_11")]] <-
#         lag(training_subset_aligned[[paste0(listToLag[npi])]], 11)
#       training_subset_aligned[[paste0(listToLag[npi], "_lag_11")]][1:11] <-
#         mean(training_subset_aligned[[paste0(listToLag[npi])]][1:3])
#       # Add 14 day lag factor for R0
#       training_subset_aligned[[paste0(listToLag[npi], "_lag_14")]] <-
#         lag(training_subset_aligned[[paste0(listToLag[npi])]], 14)
#       training_subset_aligned[[paste0(listToLag[npi], "_lag_14")]][1:14] <-
#         mean(training_subset_aligned[[paste0(listToLag[npi])]][1:3])
#     }
#     # Fix lags for non-updated NPIs
#     # training_subset_aligned$Social_Distancing[is.na(training_subset_aligned$Social_Distancing)] <-
#     #   tail(training_subset_aligned$Social_Distancing[!is.na(training_subset_aligned$Social_Distancing)], 1)
#     # training_subset_aligned$Quaranting_Cases[is.na(training_subset_aligned$Quaranting_Cases)] <-
#     #   tail(training_subset_aligned$Quaranting_Cases[!is.na(training_subset_aligned$Quaranting_Cases)], 1)
#     # training_subset_aligned$Close_Border[is.na(training_subset_aligned$Close_Border)] <-
#     #   tail(training_subset_aligned$Close_Border[!is.na(training_subset_aligned$Close_Border)], 1)
#     # listToLag <-
#     #   c("Social_Distancing", "Quaranting_Cases", "Close_Border")
#     # for (npi in 1:length(listToLag)) {
#     #   # Add 3 day lag factor
#     #   training_subset_aligned[[paste0(listToLag[npi], "_Lag_03")]] <-
#     #     lag(training_subset_aligned[[paste0(listToLag[npi])]], 3)
#     #   training_subset_aligned[[paste0(listToLag[npi], "_Lag_03")]][1:3] <-
#     #     mean(training_subset_aligned[[paste0(listToLag[npi])]][1:1])
#     #   # Add 7 day lag factor
#     #   training_subset_aligned[[paste0(listToLag[npi], "_Lag_07")]] <-
#     #     lag(training_subset_aligned[[paste0(listToLag[npi])]], 7)
#     #   training_subset_aligned[[paste0(listToLag[npi], "_Lag_07")]][1:7] <-
#     #     mean(training_subset_aligned[[paste0(listToLag[npi])]][1:1])
#     #   # Add 10 day lag factor
#     #   training_subset_aligned[[paste0(listToLag[npi], "_lag_10")]] <-
#     #     lag(training_subset_aligned[[paste0(listToLag[npi])]], 10)
#     #   training_subset_aligned[[paste0(listToLag[npi], "_lag_10")]][1:10] <-
#     #     mean(training_subset_aligned[[paste0(listToLag[npi])]][1:1])
#     #   # Add 14 day lag factor
#     #   training_subset_aligned[[paste0(listToLag[npi], "_lag_14")]] <-
#     #     lag(training_subset_aligned[[paste0(listToLag[npi])]], 14)
#     #   training_subset_aligned[[paste0(listToLag[npi], "_lag_14")]][1:14] <-
#     #     mean(training_subset_aligned[[paste0(listToLag[npi])]][1:1])
#     # }
#     
#     
#     # plot.new()
#     # plot(res_uncertain_si, legend = T)
#     # mtext(
#     #   training_countries[i],
#     #   outer = TRUE,
#     #   cex = 1,
#     #   line = -.5
#     # )
#     
#     if (i == 1) {
#       training_ready <- training_subset_aligned
#     } else{
#       training_ready <-
#         as.data.frame(rbind(training_ready, training_subset_aligned))
#     }
#     
#   }
#   
#   # create testing dataframe
#   for (i in 1:1) {
#     testing_subset <-
#       subset(data_clean_train, ISO3 == testing_country)
#     
#     print(i)
#     print(testing_country)
#     
#     start <- which(testing_subset$confirmed_cum >= 50)[1]
#     
#     testing_subset_aligned <-
#       testing_subset[start:nrow(testing_subset),]
#     testing_subset_aligned$time <-
#       c(1:nrow(testing_subset_aligned))
#     
#     # Smooth out incidence using moving average with a centered window of 7 datapoints (3 to the left, 3 to the right)
#     # make sure the window is an odd integer
#     window <- 7
#     # dim(testing_subset_aligned)
#     # length(rollmean(testing_subset_aligned$confirmed, k=window))
#     testing_subset_aligned$movingAverage <-
#       c(
#         testing_subset_aligned$confirmed[1:((window - 1) / 2)],
#         rollmean(
#           testing_subset_aligned$confirmed,
#           k = window,
#           align = "center"
#         ),
#         testing_subset_aligned$confirmed[(nrow(testing_subset_aligned) - ((window -
#                                                                                1) / 2) + 1):nrow(testing_subset_aligned)]
#       )
#     # Plot cases
#     gg <- ggplot(testing_subset_aligned) +
#       geom_line(position = position_dodge2(width = 0.5, padding = 0.2), aes(x = date, y = confirmed), color = "red") +
#       geom_line(position = position_dodge2(width = 0.5, padding = 0.2), aes(x = date, y = movingAverage), color = "blue") +
#       ggtitle(paste0("Reported cases in ", testing_country))
#     print(gg)
#     # Add moving average day lag and one day difference variables
#     testing_subset_aligned[["movingAverage_lag_1"]] <-
#       lag(testing_subset_aligned[["movingAverage"]], 1)
#     testing_subset_aligned[["movingAverage_lag_3"]] <-
#       lag(testing_subset_aligned[["movingAverage"]], 3)
#     testing_subset_aligned[["movingAverage_lag_7"]] <-
#       lag(testing_subset_aligned[["movingAverage"]], 7)
#     testing_subset_aligned[["movingAverage_lag_14"]] <-
#       lag(testing_subset_aligned[["movingAverage"]], 14)
#     testing_subset_aligned[["movingAverage_diff_1_3"]] <-
#       testing_subset_aligned[["movingAverage_lag_1"]] - testing_subset_aligned[["movingAverage_lag_3"]]
#     testing_subset_aligned[["movingAverage_diff_1_7"]] <-
#       testing_subset_aligned[["movingAverage_lag_1"]] - testing_subset_aligned[["movingAverage_lag_7"]]
#     testing_subset_aligned[["movingAverage_diff_1_14"]] <-
#       testing_subset_aligned[["movingAverage_lag_1"]] - testing_subset_aligned[["movingAverage_lag_14"]]
#     testing_subset_aligned[["movingAverage_diff_3_7"]] <-
#       testing_subset_aligned[["movingAverage_lag_3"]] - testing_subset_aligned[["movingAverage_lag_7"]]
#     testing_subset_aligned[["movingAverage_diff_7_14"]] <-
#       testing_subset_aligned[["movingAverage_lag_7"]] - testing_subset_aligned[["movingAverage_lag_14"]]
#     
#     
#     # toCalcR0 <- testing_subset_aligned[,c("date","confirmed")]
#     toCalcR0 <-
#       testing_subset_aligned[, c("date", "movingAverage")]
#     colnames(toCalcR0) <- c("dates", "I")
#     toCalcR0$I[toCalcR0$I < 0] <- NA
#     #Get of erroneous negative counts... they sneak throught the API sometimes.
#     # But if thre is a negative at teh end... are the last one lets just make it equal to the n-1 one
#     if (is.na(tail(toCalcR0$I, 1))) {
#       toCalcR0$I[length(toCalcR0$I)] <- toCalcR0$I[length(toCalcR0$I) - 1]
#     }
#     # If the NA is not at the end, Lets linearly interpolate them:
#     toCalcR0$I <- na.approx(toCalcR0$I)
#     toCalcR0$I <- as.integer(toCalcR0$I)
#     
#     # res_uncertain_si <- estimate_R(toCalcR0,
#     #                                method = "uncertain_si",
#     #                                config = config)
#     # save(res_uncertain_si,
#     #      file = paste0("./InputData/", testing_countries[i], "_WT_R0.Rdata"))
#     
#     if (RunWT_R_flag == T) {
#       # res_uncertain_si <-
#       #   wallinga_teunis(
#       #     toCalcR0,
#       #     method = "parametric_si",
#       #     config = list(
#       #       si_parametric_distr = "G",
#       #       t_start = seq(1, nrow(toCalcR0) -
#       #                       6),
#       #       t_end = seq(7, nrow(toCalcR0)),
#       #       mean_si = wtSIs$mean_si,
#       #       std_mean_si = wtSIs$std_mean_si,
#       #       min_mean_si = wtSIs$min_mean_si,
#       #       max_mean_si = wtSIs$max_mean_si,
#       #       std_si = wtSIs$std_si,
#       #       std_std_si = wtSIs$std_si,
#       #       min_std_si = wtSIs$min_std_si,
#       #       max_std_si = wtSIs$max_std_si,
#       #       n_sim = 100
#       #     )
#       #   )
#       incid <- as.numeric(toCalcR0$I)
#       names(incid) <- as.Date(toCalcR0$dates)
#       empez <- as.Date(toCalcR0$dates[1])
#       fin <- as.Date(toCalcR0$dates[nrow(toCalcR0)])
#       # mGT<-generation.time("gamma", c(wtSIs$mean_si, wtSIs$std_si))
#       # https://epiforecasts.io/covid/methods.html
#       # generation time with a mean of 3.6 days (sd: 0.7 days) 
#       mGT <- generation.time("gamma", c(3.6,0.7))
#       estR0 <- est.R0.TD(incid, mGT, begin=empez, end=fin, nsim=1000)
#       ## An interesting way to look at these results is to agregate initial data by longest time unit,
#       ## such as weekly incidence. This gives a global overview of the epidemic.
#       # estR0.weekly <- smooth.Rt(estR0, 7)
#       # estR0.weekly$R
#       plot(estR0)
#       # plot(estR0.weekly)
#       
#       save(estR0,
#            file = paste0("./InputData/", testing_country, "_WT_R0.Rdata"))
#     } else{
#       res_uncertain_si <- NULL
#       load(paste0("./InputData/", testing_country, "_WT_R0.Rdata"))
#     }
#     
#     finalR_WT <- as.data.frame(estR0$R)
#     finalR_WT$date <- rownames(finalR_WT)
#     rownames(finalR_WT) <- NULL
#     colnames(finalR_WT) <- c("R0","date")
#     finalR_WT <- finalR_WT[, c("date","R0")]
#     finalR_WT$R0[nrow(finalR_WT)] <- finalR_WT$R0[nrow(finalR_WT)-1]
#     finalR_WT$date <- as.Date(finalR_WT$date)
#     testing_subset_aligned <- merge(testing_subset_aligned, finalR_WT, by="date", all.x = T, all.y = F)
#     testing_subset_aligned[,c("date","R0")]
#     
#     
#     
#     # testing_subset_aligned$R0 <- NA
#     # testing_subset_aligned$R0[head(res_uncertain_si[["R"]]$`t_start`, 1):(tail(res_uncertain_si[["R"]]$`t_start`, 1))] <-
#     #   res_uncertain_si[["R"]]$`Mean(R)`
#     # # Autofill beginning R0s with first value
#     # testing_subset_aligned$R0[1:head(res_uncertain_si[["R"]]$`t_start`, 1)] <-
#     #   mean(head(res_uncertain_si[["R"]]$`Mean(R)`, 1))
#     # # Autofill ending R0s with linear estimation from last week of values
#     # fitFrame <-
#     #   as.data.frame(cbind(c(1:7), tail(res_uncertain_si[["R"]]$`Mean(R)`, 7)))
#     # fit <- lm(V2 ~ V1, data = fitFrame)
#     # fitPred <-
#     #   as.data.frame(8:(8 + length((tail(res_uncertain_si[["R"]]$`t_start`, 1)):nrow(testing_subset_aligned)
#     #   )))
#     # colnames(fitPred) <- c("V1")
#     # testing_subset_aligned$R0[(tail(res_uncertain_si[["R"]]$`t_start`, 1)):nrow(testing_subset_aligned)] <-
#     #   NA #predict.lm(fit,fitPred) #mean(tail(res_uncertain_si[["R"]]$`Mean(R)`,5))
#     listToLag <-
#       c(
#         "R0"
#         # "Google_Retail_recreation",
#         # "Google_Grocery_pharmacy",
#         # "Google_Parks",
#         # "Google_Transit_stations",
#         # "Google_Workplaces",
#         # "Google_Residential"
#       )
#     # c(2,5,8,11,14)
#     for (npi in 1:length(listToLag)) {
#       # Add 2 day lag factor for R0
#       testing_subset_aligned[[paste0(listToLag[npi], "_lag_2")]] <-
#         lag(testing_subset_aligned[[paste0(listToLag[npi])]], 2)
#       testing_subset_aligned[[paste0(listToLag[npi], "_lag_2")]][1:2] <-
#         mean(testing_subset_aligned[[paste0(listToLag[npi])]][1:3])
#       # Add 5 day lag factor for R0
#       testing_subset_aligned[[paste0(listToLag[npi], "_lag_5")]] <-
#         lag(testing_subset_aligned[[paste0(listToLag[npi])]], 5)
#       testing_subset_aligned[[paste0(listToLag[npi], "_lag_5")]][1:5] <-
#         mean(testing_subset_aligned[[paste0(listToLag[npi])]][1:3])
#       # Add 8 day lag factor for R0
#       testing_subset_aligned[[paste0(listToLag[npi], "_lag_8")]] <-
#         lag(testing_subset_aligned[[paste0(listToLag[npi])]], 8)
#       testing_subset_aligned[[paste0(listToLag[npi], "_lag_8")]][1:8] <-
#         mean(testing_subset_aligned[[paste0(listToLag[npi])]][1:3])
#       # Add 11 day lag factor for R0
#       testing_subset_aligned[[paste0(listToLag[npi], "_lag_11")]] <-
#         lag(testing_subset_aligned[[paste0(listToLag[npi])]], 11)
#       testing_subset_aligned[[paste0(listToLag[npi], "_lag_11")]][1:11] <-
#         mean(testing_subset_aligned[[paste0(listToLag[npi])]][1:3])
#       # Add 14 day lag factor for R0
#       testing_subset_aligned[[paste0(listToLag[npi], "_lag_14")]] <-
#         lag(testing_subset_aligned[[paste0(listToLag[npi])]], 14)
#       testing_subset_aligned[[paste0(listToLag[npi], "_lag_14")]][1:14] <-
#         mean(testing_subset_aligned[[paste0(listToLag[npi])]][1:3])
#     }
#     # Fix lags for non-updated NPIs
#     # testing_subset_aligned$Social_Distancing[is.na(testing_subset_aligned$Social_Distancing)] <-
#     #   tail(testing_subset_aligned$Social_Distancing[!is.na(testing_subset_aligned$Social_Distancing)], 1)
#     # testing_subset_aligned$Quaranting_Cases[is.na(testing_subset_aligned$Quaranting_Cases)] <-
#     #   tail(testing_subset_aligned$Quaranting_Cases[!is.na(testing_subset_aligned$Quaranting_Cases)], 1)
#     # testing_subset_aligned$Close_Border[is.na(testing_subset_aligned$Close_Border)] <-
#     #   tail(testing_subset_aligned$Close_Border[!is.na(testing_subset_aligned$Close_Border)], 1)
#     # listToLag <-
#     #   c("Social_Distancing", "Quaranting_Cases", "Close_Border")
#     # for (npi in 1:length(listToLag)) {
#     #   # Add 3 day lag factor
#     #   testing_subset_aligned[[paste0(listToLag[npi], "_Lag_03")]] <-
#     #     lag(testing_subset_aligned[[paste0(listToLag[npi])]], 3)
#     #   testing_subset_aligned[[paste0(listToLag[npi], "_Lag_03")]][1:3] <-
#     #     mean(testing_subset_aligned[[paste0(listToLag[npi])]][1:1])
#     #   # Add 7 day lag factor
#     #   testing_subset_aligned[[paste0(listToLag[npi], "_Lag_07")]] <-
#     #     lag(testing_subset_aligned[[paste0(listToLag[npi])]], 7)
#     #   testing_subset_aligned[[paste0(listToLag[npi], "_Lag_07")]][1:7] <-
#     #     mean(testing_subset_aligned[[paste0(listToLag[npi])]][1:1])
#     #   # Add 10 day lag factor
#     #   testing_subset_aligned[[paste0(listToLag[npi], "_lag_10")]] <-
#     #     lag(testing_subset_aligned[[paste0(listToLag[npi])]], 10)
#     #   testing_subset_aligned[[paste0(listToLag[npi], "_lag_10")]][1:10] <-
#     #     mean(testing_subset_aligned[[paste0(listToLag[npi])]][1:1])
#     #   # Add 14 day lag factor
#     #   testing_subset_aligned[[paste0(listToLag[npi], "_lag_14")]] <-
#     #     lag(testing_subset_aligned[[paste0(listToLag[npi])]], 14)
#     #   testing_subset_aligned[[paste0(listToLag[npi], "_lag_14")]][1:14] <-
#     #     mean(testing_subset_aligned[[paste0(listToLag[npi])]][1:1])
#     # }
#     
#     
#     # plot.new()
#     # plot(res_uncertain_si, legend = T)
#     # mtext(
#     #   testing_countries[i],
#     #   outer = TRUE,
#     #   cex = 1,
#     #   line = -.5
#     # )
#   
#     tmp <- testing_subset_aligned[rep(1, forecastingTime), ]
#     tmp[, grep(
#       "cum|StringencyIndexForDisplay|GovernmentResponseIndexForDisplay|ContainmentHealthIndexForDisplay|EconomicSupportIndexForDisplay|Google|R0|date|confirmed",
#       colnames(tmp)
#     )] <- NA
#     # tmp$Social_Distancing <- NA
#     # tmp$Quaranting_Cases <- NA
#     # tmp$Close_Border <- NA
#     testing_subset_aligned_predictNA <-
#       rbind(testing_subset_aligned, tmp)
#     testing_subset_aligned_predictNA$time <-
#       c(1:nrow(testing_subset_aligned_predictNA))
#     
#     testing_ready <- testing_subset_aligned_predictNA
#   }
#   # dev.off()
#   
#   # preserve the original dataframes before we make changes to them...
#   # testing_ready <- testing_ready_OG
#   # training_ready <- training_ready_OG
#   testing_ready_OG <- testing_ready
#   training_ready_OG <- training_ready
#   # Let's use some of the country's data for training based on testingTimeFrame
#   NrowToSaveForTesting <-
#     round(nrow(testing_ready_OG) * testingTimeFrame)
#   breakpoint <- nrow(testing_ready_OG) - NrowToSaveForTesting
#   testing_ready <-
#     testing_ready_OG[(breakpoint + 1):nrow(testing_ready_OG), ]
#   training_ready <-
#     as.data.frame(rbind(training_ready_OG, training_ready_OG[1:breakpoint, ]))
#   
#   #---NPIflag1---#########################################################################################################################################################################
#   # Here we write a little loop that takes care of the fact that the Johns hopkins count data is
#   # Updated much more frequently than the NPI data.  So
#   # setting the NPIflag1 to "autofill" is our method of saying that we want to fill all the NAs in the time period with the last empirical time points' NPI values
#   # We will worry about the NPIflag2 later to specify if we want to fill the projection timeperiod the same way
#   # NPIflag1 <- "autofill"
#   
#   peek_at_NPIs_training1 <-
#     training_ready[, c(c("date", "time", "Country", "ISO3", "confirmed"),
#                        names(training_ready)[grep(
#                          "GovernmentResponseIndexForDisplay|ContainmentHealthIndexForDisplay|EconomicSupportIndexForDisplay|Google|R0",
#                          names(training_ready)
#                        )])]
#   # breaker <- nrow(testing_ready)-forecastingTime+1-addToBreaker #breaker for R0
#   # breaker2 <- nrow(testing_ready)-forecastingTime+1 #breaker for all of the NPIs for our cutoff date
#   peek_at_NPIs_testing1 <-
#     testing_ready[, c(c("date", "time", "Country", "ISO3", "confirmed"),
#                       names(testing_ready)[grep(
#                         "GovernmentResponseIndexForDisplay|ContainmentHealthIndexForDisplay|EconomicSupportIndexForDisplay|Google|R0",
#                         names(testing_ready)
#                       )])]
#   # tail(peek_at_NPIs_testing1[1:breaker])
#   #---Filtering Columns in Dataframes---#########################################################################################################################################################################
#   # Columns you don't want to be in the model
#   training_ready_sub2 <- training_ready %>%
#     dplyr::select(-contains("confirmed_cum_per_million")) %>%
#     dplyr::select(-contains("death_cum")) %>%
#     dplyr::select(-contains("movingAverage")) %>%
#     dplyr::select(-contains("MalePercent")) %>%
#     dplyr::select(-contains("FemalePercent")) %>%
#     dplyr::select(-contains("C1")) %>%
#     dplyr::select(-contains("C2")) %>%
#     dplyr::select(-contains("C3")) %>%
#     dplyr::select(-contains("C4")) %>%
#     dplyr::select(-contains("C5")) %>%
#     dplyr::select(-contains("C6")) %>%
#     dplyr::select(-contains("C7")) %>%
#     dplyr::select(-contains("C8")) %>%
#     dplyr::select(-contains("E1")) %>%
#     dplyr::select(-contains("E2")) %>%
#     dplyr::select(-contains("E3")) %>%
#     dplyr::select(-contains("E4")) %>%
#     dplyr::select(-contains("H1")) %>%
#     dplyr::select(-contains("H2")) %>%
#     dplyr::select(-contains("H3")) %>%
#     dplyr::select(-contains("H4")) %>%
#     dplyr::select(-contains("H5")) %>%
#     dplyr::select(
#       -c(
#         confirmed_cum,
#         date,
#         Country,
#         ISO3,
#         confirmed,
#         death,
#         FullName,
#         recovered,
#         time,
#         R0_lag_2
#       )
#     ) %>%
#     mutate_if(is.factor, as.character) %>%
#     mutate_if(is.character, as.numeric) %>%
#     mutate_if(is.integer, as.numeric)
#   
#   # filter testing data
#   testing_ready_sub2 <- testing_ready %>%
#     dplyr::select(-contains("confirmed_cum_per_million")) %>%
#     dplyr::select(-contains("death_cum")) %>%
#     dplyr::select(-contains("movingAverage")) %>%
#     dplyr::select(-contains("MalePercent")) %>%
#     dplyr::select(-contains("FemalePercent")) %>%
#     dplyr::select(-contains("C1")) %>%
#     dplyr::select(-contains("C2")) %>%
#     dplyr::select(-contains("C3")) %>%
#     dplyr::select(-contains("C4")) %>%
#     dplyr::select(-contains("C5")) %>%
#     dplyr::select(-contains("C6")) %>%
#     dplyr::select(-contains("C7")) %>%
#     dplyr::select(-contains("C8")) %>%
#     dplyr::select(-contains("E1")) %>%
#     dplyr::select(-contains("E2")) %>%
#     dplyr::select(-contains("E3")) %>%
#     dplyr::select(-contains("E4")) %>%
#     dplyr::select(-contains("H1")) %>%
#     dplyr::select(-contains("H2")) %>%
#     dplyr::select(-contains("H3")) %>%
#     dplyr::select(-contains("H4")) %>%
#     dplyr::select(-contains("H5")) %>%
#     dplyr::select(
#       -c(
#         confirmed_cum,
#         date,
#         Country,
#         ISO3,
#         confirmed,
#         death,
#         FullName,
#         recovered,
#         time,
#         R0_lag_2
#       )
#     ) %>%
#     mutate_if(is.factor, as.character) %>%
#     mutate_if(is.character, as.numeric) %>%
#     mutate_if(is.integer, as.numeric)
#   
#   #---VSURF Variable Selection---#########################################################################################################################################################################
#   outcomeVariable <- "R0"
#   mod_formula <- as.formula(paste(outcomeVariable, "~", "."))
#   if (VSURFflag == T) {
#     print(
#       paste0(
#         'Number of cores being used = ',
#         num_cores,
#         ", of possible ",
#         detectCores(),
#         " cores"
#       )
#     )
#     
#     registerDoParallel(num_cores)
#     
#     
#     set.seed(15)
#     
#     glimpse(training_ready_sub2)
#     
#     # Pick the best mtry
#     x <-
#       training_ready_sub2[complete.cases(training_ready_sub2), which(colnames(training_ready_sub2) %ni% outcomeVariable)]
#     y <-
#       training_ready_sub2[complete.cases(training_ready_sub2), which(colnames(training_ready_sub2) %in% outcomeVariable)]
#     # bestMtry <-
#     #   tuneRF(
#     #     x,
#     #     y,
#     #     stepFactor = 1.5,
#     #     improve = 1e-5,
#     #     ntree = number_trees,
#     #     na.action = nasaction
#     #   )
#     # bestMtry <- as.data.frame(bestMtry)
#     # mtry_best <- bestMtry$mtry[which.min(bestMtry$OOBError)]
#     mtry_best <- max(floor(ncol(x)/3), 1)
#     
#     # Run VSURF to get the top variables
#     results.vsurf <- VSURF(
#       x,
#       y,
#       na.action = na.omit,
#       mtry = mtry_best,
#       n_tree = number_trees,
#       parallel = TRUE,
#       verbose = TRUE,
#       ncores = num_cores,
#       nmj = 1
#     )
#     nmj_used = 1
#     results.vsurf.OG <- results.vsurf
#     
#     nVarInterp <-
#       length(colnames(training_ready_sub2[, results.vsurf$varselect.interp]))
#     nVarPred <-
#       length(colnames(training_ready_sub2[, results.vsurf$varselect.pred]))
#     
#     # If the selection process between interpretation step and prediction step essentially eliminated all of the variables,
#     # lets lower the nmj threshold (since there is no natural tuning of this parameter)
#     if (nVarInterp <= 4) {
#       print(
#         "we have less than 4 variables in the interpretation set, so lets predict on all the variables from the interpretation step..."
#       )
#       results.vsurf$varselect.pred <-
#         results.vsurf$varselect.interp
#     } else{
#       if (nVarPred <= 4 && nVarInterp > 4) {
#         print("nmj = 1 gave me less than 4 variables to predict on, lets try nmj = 0.1 ...")
#         results.vsurf_pred_redo <- VSURF_pred(
#           x,
#           y,
#           na.action = na.omit,
#           mtry = mtry_best,
#           n_tree = number_trees,
#           parallel = TRUE,
#           verbose = TRUE,
#           ncores = num_cores,
#           nmj = 0.1,
#           err.interp = results.vsurf$err.interp,
#           varselect.interp = results.vsurf$varselect.interp
#         )
#         results.vsurf$varselect.pred <-
#           results.vsurf_pred_redo$varselect.pred
#         nVarPred <-
#           length(colnames(training_ready_sub2[, results.vsurf$varselect.pred]))
#         nmj_used = 0.1
#       }
#       # If we still have less than 4 variables lets lower nmj again
#       if (nVarPred <= 4 && nVarInterp > 4) {
#         print("nmj = 0.1 gave me less than 4 variables to predict on, lets try nmj = 0.01 ...")
#         results.vsurf_pred_redo <- VSURF_pred(
#           x,
#           y,
#           na.action = na.omit,
#           mtry = mtry_best,
#           n_tree = number_trees,
#           parallel = TRUE,
#           verbose = TRUE,
#           ncores = num_cores,
#           nmj = 0.01,
#           err.interp = results.vsurf$err.interp,
#           varselect.interp = results.vsurf$varselect.interp
#         )
#         results.vsurf$varselect.pred <-
#           results.vsurf_pred_redo$varselect.pred
#         nVarPred <-
#           length(colnames(training_ready_sub2[, results.vsurf$varselect.pred]))
#         nmj_used = 0.01
#       }
#       # If we still have less than 4 variables in the predict set, lets just use the interpretation variable set for the prediction model
#       if (nVarPred <= 4 && nVarInterp > 4) {
#         print("this shouldnt happen very often....")
#         print(
#           "nmj = 0.01 gave me less than 4 variables to predict on, so lets stop trying to adjust nmj and just predict on all the variables from the interpretation step"
#         )
#         results.vsurf$varselect.pred <-
#           results.vsurf$varselect.interp
#         nmj_used = 0
#       }
#     }
#     
#     # look at results of VSURF
#     summary(results.vsurf)
#     plot(results.vsurf)
#     results.vsurf$varselect.thres
#     results.vsurf$varselect.interp
#     results.vsurf$varselect.pred
#     
#     # print the reduced number of variables that should be considered in model
#     colnames(training_ready_sub2[, results.vsurf$varselect.thres])
#     colnames(training_ready_sub2[, results.vsurf$varselect.interp])
#     colnames(training_ready_sub2[, results.vsurf$varselect.pred])    # The final list of variables to be included according to the VSURF methodology.
#     VSURF_thres_keepers <-
#       colnames(training_ready_sub2[, results.vsurf$varselect.thres])
#     VSURF_interp_keepers <-
#       colnames(training_ready_sub2[, results.vsurf$varselect.interp])
#     VSURF_pred_keepers <-
#       colnames(training_ready_sub2[, results.vsurf$varselect.pred])
#     # Save the final list from D4 to be used for D3, D2, and D1
#     # save(nmj_used, VSURF_thres_keepers, VSURF_interp_keepers, VSURF_pred_keepers, results.vsurf, results.vsurf.OG, file = paste0(gwd,"/InputData/",testing_country,"_VSURFkeepers_R0.Rdata"))
#     save(
#       nmj_used,
#       VSURF_thres_keepers,
#       VSURF_interp_keepers,
#       VSURF_pred_keepers,
#       results.vsurf,
#       results.vsurf.OG,
#       file = paste0(
#         gwd,"/InputData/",
#         "All_Countries",
#         "_VSURFkeepers_R0.Rdata"
#       )
#     )
#     
#     
#   }
#   VSURF_thres_keepers <- NULL
#   VSURF_interp_keepers <- NULL
#   VSURF_pred_keepers <- NULL
#   # load(paste0(gwd,"/InputData/",testing_country,"_VSURFkeepers_R0.Rdata"))
#   load(paste0(gwd,"/InputData/", "All_Countries", "_VSURFkeepers_R0_oxford.Rdata"))
#   # VSURF_pred_keepers <- VSURF_pred_keepers
#   # training dataframe with reduced number of variables
#   if (length(VSURF_pred_keepers) > 1) {
#     training_ready_sub_vsurf_result = select(training_ready_sub2,
#                                              c(outcomeVariable, VSURF_pred_keepers))
#     training_ready_sub_vsurf_result_varImp = select(training_ready_sub2,
#                                                     c(outcomeVariable, VSURF_interp_keepers))
#   } else if (length(VSURF_pred_keepers) <= 1 &&
#              length(VSURF_interp_keepers) > 1) {
#     training_ready_sub_vsurf_result = select(training_ready_sub2,
#                                              c(outcomeVariable, VSURF_interp_keepers))
#     training_ready_sub_vsurf_result_varImp = select(training_ready_sub2,
#                                                     c(outcomeVariable, VSURF_interp_keepers))
#   } else if (length(VSURF_pred_keepers) <= 1 &&
#              length(VSURF_interp_keepers) <= 1) {
#     training_ready_sub_vsurf_result = select(training_ready_sub2,
#                                              c(outcomeVariable, VSURF_thres_keepers))
#     training_ready_sub_vsurf_result_varImp = select(training_ready_sub2,
#                                                     c(outcomeVariable, VSURF_thres_keepers))
#   }
#   glimpse(training_ready_sub_vsurf_result)
#   glimpse(training_ready_sub_vsurf_result_varImp)
#   
#   # testing dataframe with reduced number of variables
#   if (length(VSURF_pred_keepers) > 1) {
#     testing_ready_sub_vsurf_result = select(testing_ready_sub2,
#                                             c(outcomeVariable, VSURF_pred_keepers))
#   } else if (length(VSURF_pred_keepers) <= 1 &&
#              length(VSURF_interp_keepers) > 1) {
#     testing_ready_sub_vsurf_result = select(testing_ready_sub2,
#                                             c(outcomeVariable, VSURF_interp_keepers))
#   } else if (length(VSURF_pred_keepers) <= 1 &&
#              length(VSURF_interp_keepers) <= 1) {
#     testing_ready_sub_vsurf_result = select(testing_ready_sub2,
#                                             c(outcomeVariable, VSURF_thres_keepers))
#   }
#   glimpse(testing_ready_sub_vsurf_result)
#   
#   closeAllConnections()
#   
#   #---RF model---#########################################################################################################################################################################
#   print(
#     paste0(
#       'Number of cores being used = ',
#       num_cores,
#       ", of possible ",
#       detectCores(),
#       " cores"
#     )
#   )
#   registerDoParallel(num_cores)
#   
#   # Pick the best mtry
#   x <-
#     training_ready_sub_vsurf_result[complete.cases(training_ready_sub_vsurf_result), which(colnames(training_ready_sub_vsurf_result) %ni% outcomeVariable)]
#   y <-
#     training_ready_sub_vsurf_result[complete.cases(training_ready_sub_vsurf_result), which(colnames(training_ready_sub_vsurf_result) %in% outcomeVariable)]
#   if (length(training_ready_sub_vsurf_result) > 2) {
#     # bestMtry <-
#     #   tuneRF(
#     #     x,
#     #     y,
#     #     stepFactor = 1.5,
#     #     improve = 1e-5,
#     #     ntree = number_trees,
#     #     na.action = nasaction
#     #   )
#     # bestMtry <- as.data.frame(bestMtry)
#     # mtry_best <- bestMtry$mtry[which.min(bestMtry$OOBError)]
#     mtry_best <- max(floor(ncol(x)/3), 1)
#   } else{
#     mtry_best <- 1
#   }
#   
#   
#   tunegrid <- expand.grid(
#     .mtry = mtry_best,
#     .splitrule = c('gini'),
#     .min.node.size = c(5, 10, 20)
#   )
#   
#   control <- trainControl(method = "cv",
#                           number = 3,
#                           # repeats=3,
#                           # verboseIter = T,
#                           # classProbs = T,
#                           allowParallel = TRUE)
#   
#   rf.mod <- caret::train(
#     mod_formula,
#     data = training_ready_sub_vsurf_result,
#     # method = 'ranger',
#     method = 'rf',
#     na.action = nasaction,
#     keep.inbag = TRUE,
#     replace = TRUE,
#     # importance = "permutation", #***
#     trControl = control
#   )
#   
#   qrF <- quantregForest(
#     x = x,
#     y = y,
#     data = training_ready_sub_vsurf_result_varImp,
#     # method = 'ranger',
#     method = 'rf',
#     na.action = nasaction,
#     keep.inbag = TRUE,
#     replace = TRUE,
#     # importance = "permutation", #***
#     trControl = control
#   )
#   
#   # Pick the best mtry
#   x <-
#     training_ready_sub_vsurf_result_varImp[complete.cases(training_ready_sub_vsurf_result_varImp), which(colnames(training_ready_sub_vsurf_result_varImp) %ni% outcomeVariable)]
#   y <-
#     training_ready_sub_vsurf_result_varImp[complete.cases(training_ready_sub_vsurf_result_varImp), which(colnames(training_ready_sub_vsurf_result_varImp) %in% outcomeVariable)]
#   if (length(training_ready_sub_vsurf_result_varImp) > 2) {
#     # bestMtry <-
#     #   tuneRF(
#     #     x,
#     #     y,
#     #     stepFactor = 1.5,
#     #     improve = 1e-5,
#     #     ntree = number_trees,
#     #     na.action = nasaction
#     #   )
#     # bestMtry <- as.data.frame(bestMtry)
#     # mtry_best <- bestMtry$mtry[which.min(bestMtry$OOBError)]
#     mtry_best <- max(floor(ncol(x)/3), 1)
#   } else{
#     mtry_best <- 1
#   }
#   
#   # rf.mod.varImp should be loaded from   file = paste0("./InputData/",
#   #                                                     "All_Countries",
#   #                                                     "_VSURFkeepers_R0.Rdata")
#   # rf.mod.varImp <- caret::train(
#   #   mod_formula,
#   #   data = training_ready_sub_vsurf_result_varImp,
#   #   # method = 'ranger',
#   #   method = 'rf',
#   #   na.action = nasaction,
#   #   keep.inbag = TRUE,
#   #   replace = TRUE,
#   #   # importance = "permutation", #***
#   #   trControl = control
#   # )
#   # varImp(rf.mod.varImp)
#   
#   model_name = paste0("rf.mod")
#   best_model = rf.mod
#   
#   closeAllConnections()
#   #---predictFunction---#########################################################################################################################################################################
#   # the predict function is called when we want to use the tree to make predictions
#   predictFunction <-
#     function(name = best_model,
#              mod_name = model_name,
#              dd = testing_ready_pred,
#              n_trees = (1:30) * 25,
#              nasaction = na.omit) {
#       if ("rf.mod" == mod_name) {
#         predict_tmp <- predict(name, dd, na.action = nasaction)
#         # predict_tmp <- predict(name, dd, na.action = nasaction, type='se', se.method='infjack')
#       }
#       return(predict_tmp)
#     }

for(cc in 1:length(testing_countriesList)){
  testing_country <- testing_countriesList[cc]
  load(paste0("./InputData/",testing_country,"_LOO.RData"))
  
  # #---Hindsight---#########################################################################################################################################################################
  # hindsightAll <- read.csv(gwd,"/InputData/ML_features.csv")
  # hindsightAll$date <- as.Date(hindsightAll$date)
  #   for(i in 1:1){
  #     hindsight_subset <- subset(hindsightAll,ISO3 == testing_country)
  #     start <- which(hindsightAll$confirmed_cum >= 50)[1]
  #     # hindsight_subset <- subset(data_clean,ISO3 == hindsight_country)
  #     hindsight_subset_aligned <- hindsight_subset[start:nrow(hindsight_subset),]
  #
  #     # Smooth out incidence using moving average with a centered window of 7 datapoints (3 to the left, 3 to the right)
  #     # make sure the window is an odd integer
  #     window <- 7
  #     # dim(hindsight_subset_aligned)
  #     # length(rollmean(hindsight_subset_aligned$confirmed, k=window))
  #     hindsight_subset_aligned$movingAverage <- c(hindsight_subset_aligned$confirmed[1:((window-1)/2)],rollmean(hindsight_subset_aligned$confirmed, k=window, align = "center"),hindsight_subset_aligned$confirmed[(nrow(hindsight_subset_aligned)-((window-1)/2)+1):nrow(hindsight_subset_aligned)])
  #     # Plot cases
  #     gg <- ggplot(hindsight_subset_aligned) +
  #       geom_line(position = position_dodge2(width = 0.5, padding = 0.2), aes(x=date, y=confirmed),color="red") +
  #       geom_line(position = position_dodge2(width = 0.5, padding = 0.2), aes(x=date, y=movingAverage),color="blue") +
  #       ggtitle(paste0("Reported cases in ", testing_country))
  #     print(gg)
  #     # Add moving average day lag and one day difference variables
  #     hindsight_subset_aligned[["movingAverage_Lag_1"]] <- lag(hindsight_subset_aligned[["movingAverage"]],1)
  #     hindsight_subset_aligned[["movingAverage_Lag_3"]] <- lag(hindsight_subset_aligned[["movingAverage"]],3)
  #     hindsight_subset_aligned[["movingAverage_Lag_7"]] <- lag(hindsight_subset_aligned[["movingAverage"]],7)
  #     hindsight_subset_aligned[["movingAverage_Lag_14"]] <- lag(hindsight_subset_aligned[["movingAverage"]],14)
  #     hindsight_subset_aligned[["movingAverage_diff_1_3"]] <- hindsight_subset_aligned[["movingAverage_Lag_1"]] - hindsight_subset_aligned[["movingAverage_Lag_3"]]
  #     hindsight_subset_aligned[["movingAverage_diff_1_7"]] <- hindsight_subset_aligned[["movingAverage_Lag_1"]] - hindsight_subset_aligned[["movingAverage_Lag_7"]]
  #     hindsight_subset_aligned[["movingAverage_diff_1_14"]] <- hindsight_subset_aligned[["movingAverage_Lag_1"]] - hindsight_subset_aligned[["movingAverage_Lag_14"]]
  #     hindsight_subset_aligned[["movingAverage_diff_3_7"]] <- hindsight_subset_aligned[["movingAverage_Lag_3"]] - hindsight_subset_aligned[["movingAverage_Lag_7"]]
  #     hindsight_subset_aligned[["movingAverage_diff_7_14"]] <- hindsight_subset_aligned[["movingAverage_Lag_7"]] - hindsight_subset_aligned[["movingAverage_Lag_14"]]
  #
  #     # toCalcR0 <- hindsight_subset_aligned[,c("date","confirmed")]
  #     toCalcR0 <- hindsight_subset_aligned[,c("date","movingAverage")]
  #     colnames(toCalcR0) <- c("dates","I")
  #     toCalcR0$I[toCalcR0$I<0] <- NA
  #     #Get of erroneous negative counts... they sneak throught the API sometimes.
  #     # But if thre is a negative at teh end... are the last one lets just make it equal to the n-1 one
  #     if(is.na(tail(toCalcR0$I,1))){
  #       toCalcR0$I[length(toCalcR0$I)] <- toCalcR0$I[length(toCalcR0$I)-1]
  #     }
  #     # If the NA is not at the end, Lets linearly interpolate them:
  #     toCalcR0$I <- na.approx(toCalcR0$I)
  #     toCalcR0$I <- as.integer(toCalcR0$I)
  #     # res_uncertain_si <- estimate_R(toCalcR0,
  #     #                                method = "uncertain_si",
  #     #                                config = config)
  #
  #     if(RunWT_R_flag == T){
  #       res_uncertain_si <- wallinga_teunis(toCalcR0, method="parametric_si",
  #                                           config = list(si_parametric_distr = "G",
  #                                                         t_start = seq(1, nrow(toCalcR0)-6), t_end = seq(7, nrow(toCalcR0)),
  #                                                         mean_si = wtSIs$mean_si,
  #                                                         std_mean_si = wtSIs$std_mean_si,
  #                                                         min_mean_si = wtSIs$min_mean_si,
  #                                                         max_mean_si = wtSIs$max_mean_si,
  #                                                         std_si = wtSIs$std_si,
  #                                                         std_std_si = wtSIs$std_si,
  #                                                         min_std_si = wtSIs$min_std_si,
  #                                                         max_std_si = wtSIs$max_std_si,
  #                                                         n_sim = 100))
  #       save(res_uncertain_si, file = paste0(gwd,"/InputData/",testing_country,"_WT_R0.Rdata"))
  #     }else{
  #       res_uncertain_si <- NULL
  #       load(paste0(gwd,"/InputData/",testing_country,"_WT_R0.Rdata"))
  #     }
  #
  #     hindsight_subset_aligned$R0 <- NA
  #     hindsight_subset_aligned$R0[head(res_uncertain_si[["R"]]$`t_start`,1):(tail(res_uncertain_si[["R"]]$`t_start`,1))] <- res_uncertain_si[["R"]]$`Mean(R)`
  #     # Autofill beginning R0s with first value
  #     hindsight_subset_aligned$R0[1:head(res_uncertain_si[["R"]]$`t_start`,1)] <- mean(head(res_uncertain_si[["R"]]$`Mean(R)`,1))
  #     # Autofill ending R0s with linear estimation from last week of values
  #     fitFrame <- as.data.frame(cbind(c(1:7),tail(res_uncertain_si[["R"]]$`Mean(R)`,7)))
  #     fit <- lm(V2~V1, data=fitFrame)
  #     fitPred <- as.data.frame(8:(8+length((tail(res_uncertain_si[["R"]]$`t_start`,1)):nrow(hindsight_subset_aligned)))); colnames(fitPred) <- c("V1")
  #     addToBreaker <- nrow(fitPred)
  #     hindsight_subset_aligned$R0[(tail(res_uncertain_si[["R"]]$`t_start`,1)):nrow(hindsight_subset_aligned)] <- NA #predict.lm(fit,fitPred) #mean(tail(res_uncertain_si[["R"]]$`Mean(R)`,5))
  #     listToLag <- c("R0","Google_Retail_recreation","Google_Grocery_pharmacy","Google_Parks","Google_Transit_stations","Google_Workplaces","Google_Residential")
  #     for(npi in 1:length(listToLag)){
  #       # Add 1 day lag factor for R0
  #       hindsight_subset_aligned[[paste0(listToLag[npi],"_Lag_1")]] <- lag(hindsight_subset_aligned[[paste0(listToLag[npi])]],1)
  #       hindsight_subset_aligned[[paste0(listToLag[npi],"_Lag_1")]][1:1] <- mean(hindsight_subset_aligned[[paste0(listToLag[npi])]][1:1])
  #       # Add 3 day lag factor for R0
  #       hindsight_subset_aligned[[paste0(listToLag[npi],"_Lag_3")]] <- lag(hindsight_subset_aligned[[paste0(listToLag[npi])]],3)
  #       hindsight_subset_aligned[[paste0(listToLag[npi],"_Lag_3")]][1:3] <- mean(hindsight_subset_aligned[[paste0(listToLag[npi])]][1:3])
  #       # Add 5 day lag factor for R0
  #       hindsight_subset_aligned[[paste0(listToLag[npi],"_Lag_5")]] <- lag(hindsight_subset_aligned[[paste0(listToLag[npi])]],5)
  #       hindsight_subset_aligned[[paste0(listToLag[npi],"_Lag_5")]][1:5] <- mean(hindsight_subset_aligned[[paste0(listToLag[npi])]][1:5])
  #       # Add 7 day lag factor for R0
  #       hindsight_subset_aligned[[paste0(listToLag[npi],"_Lag_7")]] <- lag(hindsight_subset_aligned[[paste0(listToLag[npi])]],7)
  #       hindsight_subset_aligned[[paste0(listToLag[npi],"_Lag_7")]][1:7] <- mean(hindsight_subset_aligned[[paste0(listToLag[npi])]][1:7])
  #       # Add 10 day lag factor for R0
  #       hindsight_subset_aligned[[paste0(listToLag[npi],"_Lag_10")]] <- lag(hindsight_subset_aligned[[paste0(listToLag[npi])]],10)
  #       hindsight_subset_aligned[[paste0(listToLag[npi],"_Lag_10")]][1:10] <- mean(hindsight_subset_aligned[[paste0(listToLag[npi])]][1:10])
  #       # Add 14 day lag factor for R0
  #       hindsight_subset_aligned[[paste0(listToLag[npi],"_Lag_14")]] <- lag(hindsight_subset_aligned[[paste0(listToLag[npi])]],14)
  #       hindsight_subset_aligned[[paste0(listToLag[npi],"_Lag_14")]][1:14] <- mean(hindsight_subset_aligned[[paste0(listToLag[npi])]][1:14])
  #       # }
  #     }
  #     # Fix lags for non-updated NPIs
  #     hindsight_subset_aligned$Social_Distancing[is.na(hindsight_subset_aligned$Social_Distancing)] <- tail(hindsight_subset_aligned$Social_Distancing[!is.na(hindsight_subset_aligned$Social_Distancing)],1)
  #     hindsight_subset_aligned$Quaranting_Cases[is.na(hindsight_subset_aligned$Quaranting_Cases)] <- tail(hindsight_subset_aligned$Quaranting_Cases[!is.na(hindsight_subset_aligned$Quaranting_Cases)],1)
  #     hindsight_subset_aligned$Close_Border[is.na(hindsight_subset_aligned$Close_Border)] <- tail(hindsight_subset_aligned$Close_Border[!is.na(hindsight_subset_aligned$Close_Border)],1)
  #     listToLag <- c("Social_Distancing","Quaranting_Cases","Close_Border")
  #     for(npi in 1:length(listToLag)){
  #       # Add 3 day lag factor
  #       hindsight_subset_aligned[[paste0(listToLag[npi],"_Lag_03")]] <- lag(hindsight_subset_aligned[[paste0(listToLag[npi])]],3)
  #       hindsight_subset_aligned[[paste0(listToLag[npi],"_Lag_03")]][1:3] <- mean(hindsight_subset_aligned[[paste0(listToLag[npi])]][1:1])
  #       # Add 7 day lag factor
  #       hindsight_subset_aligned[[paste0(listToLag[npi],"_Lag_07")]] <- lag(hindsight_subset_aligned[[paste0(listToLag[npi])]],7)
  #       hindsight_subset_aligned[[paste0(listToLag[npi],"_Lag_07")]][1:7] <- mean(hindsight_subset_aligned[[paste0(listToLag[npi])]][1:1])
  #       # Add 10 day lag factor
  #       hindsight_subset_aligned[[paste0(listToLag[npi],"_Lag_10")]] <- lag(hindsight_subset_aligned[[paste0(listToLag[npi])]],10)
  #       hindsight_subset_aligned[[paste0(listToLag[npi],"_Lag_10")]][1:10] <- mean(hindsight_subset_aligned[[paste0(listToLag[npi])]][1:1])
  #       # Add 14 day lag factor
  #       hindsight_subset_aligned[[paste0(listToLag[npi],"_Lag_14")]] <- lag(hindsight_subset_aligned[[paste0(listToLag[npi])]],14)
  #       hindsight_subset_aligned[[paste0(listToLag[npi],"_Lag_14")]][1:14] <- mean(hindsight_subset_aligned[[paste0(listToLag[npi])]][1:1])
  #     }
  #
  #
  #     plot.new()
  #     plot(res_uncertain_si, legend = T)
  #     mtext(testing_country, outer=TRUE,  cex=1, line=-.5)
  #
  #     tmp <- testing_subset_aligned[rep(1,forecastingTime),]
  #     tmp[,grep("cum|Social_Distancing|Quaranting_Cases|Close_Border|Google|R0|date|confirmed", colnames(tmp))] <- NA
  #     tmp$Social_Distancing <- NA
  #     tmp$Quaranting_Cases <- NA
  #     tmp$Close_Border <- NA
  #     hindsight_subset_aligned_predictNA <- rbind(hindsight_subset_aligned,tmp)
  #     hindsight_subset_aligned_predictNA$time <- c(1:nrow(hindsight_subset_aligned_predictNA))
  #
  #     if(i==1){
  #       hindsight_ready <- hindsight_subset_aligned_predictNA
  #     }else{
  #       hindsight_ready <- as.data.frame(rbind(hindsight_ready,hindsight_subset_aligned_predictNA))
  #     }
  #   }
  
  
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
      # for(j in colnames(DFtoBuildLagsScenario1)){
      #   for(i in breaker:nrow(DFtoBuildLagsScenario1)){
      #     if(length(grep("Google",j)) >= 1){
      #       if(i == breaker){
      #         miny <- mean(DFtoBuildLagsScenario1[[j]][(i-3):(i-1)]) - 5
      #         maxy <- mean(DFtoBuildLagsScenario1[[j]][(i-3):(i-1)]) + 5
      #       }
      #       DFtoBuildLagsScenario1[[j]][i] <- as.integer(runif(1, min = miny, max = maxy))
      #     }else{
      #       DFtoBuildLagsScenario1[[j]][i] <- DFtoBuildLagsScenario1[[j]][i-1]
      #     }
      #   }
      # }
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
    recoveryTime = 14.5
    latencyTime = 5.2
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
    maxDate <- as.character(format.Date(Sys.Date() + timeExtension, "%m-%d-%Y") )
    
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
    
    
    # }
  
    
    #---Correlation between NPI data and google data---#########################################################################################################################################################################
    # look at the correlation matrix betweten the google mobility data and the data collected for NPIs
    # pdf(
    #   paste0(
    #     gwd,"./npicorrPlot_",
    #     "CaseIncidence.pdf"
    #   ),
    #   width = 12,
    #   height = 12
    # )
    # # cerate dataframe
    # NPI_google_df = tibble(data_clean[c(
    #   "date",
    #   "ISO3",
    #   "Google_Residential",
    #   "Google_Workplaces",
    #   "Google_Transit_stations",
    #   "Google_Parks",
    #   "Google_Grocery_pharmacy",
    #   "Google_Retail_recreation",
    #   "Social_Distancing",
    #   "Social_Distancing_Lag_03",
    #   "Social_Distancing_Lag_07",
    #   "Social_Distancing_Lag_10",
    #   "Social_Distancing_Lag_14"
    # )]) #,
    # # "Quaranting_Cases", "Quaranting_Cases_Lag_03", "Quaranting_Cases_Lag_07", "Quaranting_Cases_Lag_010", "Quaranting_Cases_Lag_14",
    # # "Close_Border","Close_Border_Lag_03","Close_Border_Lag_07","Close_Border_Lag_010","Close_Border_Lag_14")])
    # 
    # NPI_google_df <- NPI_google_df %>%
    #   drop_na() %>%
    #   select(-c(date, ISO3))
    # glimpse(NPI_google_df)
    # 
    # NPI_google_cor_mat <- cor(NPI_google_df)
    # NPI_corrplot <-
    #   corrplot(NPI_google_cor_mat, method = "circle", type = "upper")
    # 
    # dev.off()
    
  }
# }

print("SCRIPT HAS FINISHED")
# #---NPI Density Plots---#########################################################################################################################################################################
# training_manipulate <- training_ready_OG
# testing_manipulate <- testing_ready_OG
#
# peek_at_NPIs_training1 <- training_manipulate[,c(c("date","time","Country","ISO3","confirmed","movingAverage"),names(training_manipulate)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google|R0",names(training_manipulate))])]
# NPInames <- names(training_manipulate)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google|R0",names(training_manipulate))]
# # View(training_manipulate[,c(NPInames)])
# counter <- 1
# prevcountry <- training_manipulate$Country[1]
# if(NPIflag1 == "autofill"){
#   for(i in 2:nrow(training_manipulate)){
#     curcountry <- training_manipulate$Country[i]
#     if(curcountry == prevcountry){
#       counter <- counter+1
#     }else{
#       counter <- 1
#     }
#
#     for(j in NPInames){
#       if(is.na(training_manipulate[[j]][i]) && counter > 14){
#         training_manipulate[[j]][i] <- training_manipulate[[j]][i-1]
#       }
#     }
#     prevcountry <- curcountry
#   }
# }
# peek_at_NPIs_training2 <- training_manipulate[,c(c("date","time","Country","ISO3","confirmed","movingAverage"),names(training_manipulate)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google|R0",names(training_manipulate))])]
#
#
# peek_at_NPIs_testing1 <- testing_manipulate[,c(c("date","time","Country","ISO3","confirmed","movingAverage"),names(testing_manipulate)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google|R0",names(testing_manipulate))])]
# NPInames <- names(testing_manipulate)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google|R0",names(testing_manipulate))]
# # counter <- 1
# # prevcountry <- testing_manipulate$Country[1]
# if(NPIflag1 == "autofill"){
#   for(i in 2:nrow(testing_manipulate)){
#     for(j in NPInames){
#       if(is.na(testing_manipulate[[j]][i])){
#         testing_manipulate[[j]][i] <- testing_manipulate[[j]][i-1]
#       }
#     }
#   }
# }
# peek_at_NPIs_testing2 <- testing_manipulate[,c(c("date","time","Country","ISO3","confirmed","movingAverage"),names(testing_manipulate)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google|R0",names(testing_manipulate))])]
#
#
#
# both <- rbind(peek_at_NPIs_training2,peek_at_NPIs_testing2[1:(nrow(peek_at_NPIs_testing2-forecastingTime)),])
# npiDensityPlotData <- both[c("date", "Country","movingAverage","Google_Residential", "Google_Workplaces", "Google_Transit_stations",
#                                    "Google_Parks", "Google_Grocery_pharmacy", "Google_Retail_recreation",
#                                    "Social_Distancing", "Quaranting_Cases", "Close_Border")]
#
# simpleCap <- function(x) {
#   s <- strsplit(x, " ")[[1]]
#   paste(toupper(substring(s, 1,1)), substring(s, 2),
#         sep="", collapse=" ")
# }
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# unlink(paste0(gwd,"/Output_CaseIncidence/npidensPlot_","CaseIncidence.pdf"))
# pdf(paste0(gwd,"/Output_CaseIncidence/npidensPlot_","CaseIncidence.pdf"),width = 28, height = 14)
#
# npiList <- c("Google_Residential", "Google_Workplaces", "Google_Transit_stations",
#               "Google_Parks", "Google_Grocery_pharmacy", "Google_Retail_recreation",
#               "Social_Distancing", "Quaranting_Cases", "Close_Border")
#
# dateRange <- seq(from=min(both$date,na.rm=T), to=max(both$date,na.rm=T), length.out = 23)
# dateSplits <- seq(from=11, to=23, length.out = 4)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# NPIplotter <- function(myNPI = npiList[1]){
#   startDate <- dateRange[1]
#   endDate <- dateRange[dateSplits[1]]
#   bothSub <- subset(both,date<=endDate & date>=startDate)
#   npiDensityPlotData <- bothSub[c("date", "Country","movingAverage",myNPI)]
#   npiDensityPlotDataMelted1 <- melt(npiDensityPlotData, id = c("date", "Country","movingAverage"))
#   npi1 <- ggplot(npiDensityPlotDataMelted1, aes(x = `value`, y = `Country`, fill = ..x..)) +
#     geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 3.) +
#     scale_y_discrete(expand = c(0.0005, 0)) +
#     scale_fill_viridis(name = "", option = "C") +
#     labs(title = paste0(format(startDate, format="%B %d"), " - ",format(endDate, format="%B %d")),
#          subtitle = '')+
#     xlab(simpleCap(paste(unlist(strsplit(myNPI,"_")), sep=" ", collapse=" ")))+
#     theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())
#
#   startDate <- dateRange[dateSplits[1]]
#   endDate <- dateRange[dateSplits[2]]
#   bothSub <- subset(both,date<=endDate & date>startDate)
#   npiDensityPlotData <- bothSub[c("date", "Country","movingAverage",myNPI)]
#   npiDensityPlotDataMelted2 <- melt(npiDensityPlotData, id = c("date", "Country","movingAverage"))
#   npi2 <- ggplot(npiDensityPlotDataMelted2, aes(x = `value`, y = `Country`, fill = ..x..)) +
#     geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 3.) +
#     scale_y_discrete(expand = c(0.0005, 0)) +
#     scale_fill_viridis(name = "", option = "C") +
#     labs(title = paste0(format(startDate, format="%B %d"), " - ",format(endDate, format="%B %d")),
#          subtitle = '')+
#     xlab(simpleCap(paste(unlist(strsplit(myNPI,"_")), sep=" ", collapse=" ")))+
#     theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())
#
#   startDate <- dateRange[dateSplits[2]]
#   endDate <- dateRange[dateSplits[3]]
#   bothSub <- subset(both,date<=endDate & date>startDate)
#   npiDensityPlotData <- bothSub[c("date", "Country","movingAverage",myNPI)]
#   npiDensityPlotDataMelted3 <- melt(npiDensityPlotData, id = c("date", "Country","movingAverage"))
#   npi3 <- ggplot(npiDensityPlotDataMelted3, aes(x = `value`, y = `Country`, fill = ..x..)) +
#     geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 3.) +
#     scale_y_discrete(expand = c(0.0005, 0)) +
#     scale_fill_viridis(name = "", option = "C") +
#     labs(title = paste0(format(startDate, format="%B %d"), " - ",format(endDate, format="%B %d")),
#          subtitle = '')+
#     xlab(simpleCap(paste(unlist(strsplit(myNPI,"_")), sep=" ", collapse=" ")))+
#     theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())
#
#   startDate <- dateRange[dateSplits[3]]
#   endDate <- dateRange[dateSplits[4]]
#   bothSub <- subset(both,date<=endDate & date>=startDate)
#   npiDensityPlotData <- bothSub[c("date", "Country","movingAverage",myNPI)]
#   npiDensityPlotDataMelted4 <- melt(npiDensityPlotData, id = c("date", "Country","movingAverage"))
#   npi4 <- ggplot(npiDensityPlotDataMelted4, aes(x = `value`, y = `Country`, fill = ..x..)) +
#     geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 3.) +
#     scale_y_discrete(expand = c(0.0005, 0)) +
#     scale_fill_viridis(name = "", option = "C") +
#     labs(title = paste0(format(startDate, format="%B %d"), " - ",format(endDate, format="%B %d")),
#          subtitle = '')+
#     xlab(simpleCap(paste(unlist(strsplit(myNPI,"_")), sep=" ", collapse=" ")))+
#     theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())
#
#   # Make commmon axis
#   minx <- min(both[[myNPI]],na.rm=T)
#   maxx <- max(both[[myNPI]],na.rm=T)
#   lowerRange <- minx-(maxx-minx)*.15
#   upperRange <- maxx+(maxx-minx)*.15
#   outerBound <- c(abs(lowerRange),abs(upperRange))[which.max(c(abs(lowerRange),abs(upperRange)))]
#   if((upperRange-lowerRange)>10){
#     npi1 <- npi1 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_viridis(alpha= 1, limits = c(-outerBound, outerBound), oob = scales::squish, name = "", option = "C")
#     npi2 <- npi2 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_viridis(alpha= 1, limits = c(-outerBound, outerBound), oob = scales::squish, name = "", option = "C")
#     npi3 <- npi3 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_viridis(alpha= 1, limits = c(-outerBound, outerBound), oob = scales::squish, name = "", option = "C")
#     npi4 <- npi4 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_viridis(alpha= 1, limits = c(-outerBound, outerBound), oob = scales::squish, name = "", option = "C")
#     }else{
#       if(myNPI == "Social_Distancing"){
#         npi1 <- npi1 + scale_x_continuous(expand = c(0.01, 0), limits = c(-4, 12), breaks=c(0,1,2,3,4,5)) + scale_fill_viridis(alpha= 1, limits = c(lowerRange, upperRange), oob = scales::squish, name = "", option = "D")
#         npi2 <- npi2 + scale_x_continuous(expand = c(0.01, 0), limits = c(-4, 12), breaks=c(0,1,2,3,4,5)) + scale_fill_viridis(alpha= 1, limits = c(lowerRange, upperRange), oob = scales::squish, name = "", option = "D")
#         npi3 <- npi3 + scale_x_continuous(expand = c(0.01, 0), limits = c(-4, 12), breaks=c(0,1,2,3,4,5)) + scale_fill_viridis(alpha= 1, limits = c(lowerRange, upperRange), oob = scales::squish, name = "", option = "D")
#         npi4 <- npi4 + scale_x_continuous(expand = c(0.01, 0), limits = c(-4, 12), breaks=c(0,1,2,3,4,5)) + scale_fill_viridis(alpha= 1, limits = c(lowerRange, upperRange), oob = scales::squish, name = "", option = "D")
#       }else{
#         npi1 <- npi1 + scale_x_continuous(expand = c(0.01, 0), limits = c(-2, 7.5), breaks=c(0,1,2,3,4,5)) + scale_fill_viridis(alpha= 1, limits = c(lowerRange, upperRange), oob = scales::squish, name = "", option = "D")
#         npi2 <- npi2 + scale_x_continuous(expand = c(0.01, 0), limits = c(-2, 7.5), breaks=c(0,1,2,3,4,5)) + scale_fill_viridis(alpha= 1, limits = c(lowerRange, upperRange), oob = scales::squish, name = "", option = "D")
#         npi3 <- npi3 + scale_x_continuous(expand = c(0.01, 0), limits = c(-2, 7.5), breaks=c(0,1,2,3,4,5)) + scale_fill_viridis(alpha= 1, limits = c(lowerRange, upperRange), oob = scales::squish, name = "", option = "D")
#         npi4 <- npi4 + scale_x_continuous(expand = c(0.01, 0), limits = c(-2, 7.5), breaks=c(0,1,2,3,4,5)) + scale_fill_viridis(alpha= 1, limits = c(lowerRange, upperRange), oob = scales::squish, name = "", option = "D")
#
#       }
#     }
#
#   title_paste <- paste0(simpleCap(paste(unlist(strsplit(myNPI,"_")), sep=" ", collapse=" "))," Density Plots by Country")
#
#   gl <- list(npi1,npi2,npi3,npi4)
#   grid.arrange(grobs = gl,
#                top = textGrob(title_paste, gp=gpar(fontsize=18)),
#                layout_matrix = rbind( c(1,2,3,4)),
#                common.legend = TRUE, legend="bottom"
#   )
# }
#
# # print(NPIplotter(myNPI = npiList[1]))
#
# for(n_n in 1:length(npiList)){
# # for(n_n in 7){
#   print(n_n)
#   print(npiList[n_n])
#   print(NPIplotter(myNPI = npiList[n_n]))
# }
#
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# dev.off()
