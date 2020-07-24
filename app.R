
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
library(plotly)
library(data.table)
# Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk-14.0.2') # for 64-bit version
# Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre7') # for 32-bit version
# library(rJava)
# library(RLadyBug)
# Sys.setenv('JAVA_HOME'="C:/Program Files/Java/jdk-14.0.2")
# Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-14.0.2/")
# Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_261')
# Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-14.0.2")
# library(rJava)
# Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-14.0.2")
# https://stackoverflow.com/questions/9120270/how-can-i-install-rjava-for-use-with-64bit-r-on-a-64-bit-windows-computer

clean_plotly_leg <- function(.plotly_x, .extract_str) {
  # Inpects an x$data list in a plotly object, cleans up legend values where appropriate
  if ("legendgroup" %in% names(.plotly_x)) {
    # The list includes a legend group
    
    .plotly_x$legendgroup <- stringr::str_extract(.plotly_x$legendgroup, .extract_str)
    .plotly_x$name <- stringr::str_extract(.plotly_x$name, .extract_str)
    
  }
  .plotly_x
}

# Russia and Hubei/China were removed because they don't have google data
countryString <- "
Pick a 3 letter code corresponding to your country of interest <br/>
Choose testing country from: <br/>
1      AFG     Afghanistan <br/>
2      ARG     Argentina <br/>
3      AUS     Australia <br/>
4      AUT     Austria <br/>
5      BGD     Bangladesh <br/>
6      BLR     Belarus <br/>
7      BEL     Belgium <br/>
8      BOL     Bolivia <br/>
9      BIH     Bosnia and Herzegovina <br/>
10     BRA     Brazil <br/>
11     BGR     Bulgaria <br/>
12     CMR     Cameroon <br/>
13     CAN     Canada <br/>
14     CHL     Chile <br/>
15     COL     Colombia <br/>
16     CRI     Costa Rica <br/>
17     CIV     Cote d'Ivoire <br/>
18     HRV     Croatia <br/>
19     CZE     Czechia <br/>
20     DNK     Denmark <br/>
21     DOM     Dominican Republic <br/>
22     ECU     Ecuador <br/>
23     EGY     Egypt <br/>
24     SLV     El Salvador <br/>
25     EST     Estonia <br/>
26     FIN     Finland <br/>
27     FRA     France <br/>
28     GAB     Gabon <br/>
29     DEU     Germany <br/>
30     GHA     Ghana <br/>
31     GRC     Greece <br/>
32     GTM     Guatemala <br/>
33     HTI     Haiti <br/>
34     HND     Honduras <br/>
35     HUN     Hungary <br/>
36     IND     India <br/>
37     IDN     Indonesia <br/>
38     IRQ     Iraq <br/>
39     IRL     Ireland <br/>
40     ISR     Israel <br/>
41     ITA     Italy <br/>
42     JPN     Japan <br/>
43     JOR     Jordan <br/>
44     KAZ     Kazakhstan <br/>
45     KEN     Kenya <br/>
46     KOR     South Korea <br/>
47     KGZ     Kyrgyzstan <br/>
48     LVA     Latvia <br/>
49     LBN     Lebanon <br/>
50     LTU     Lithuania <br/>
51     LUX     Luxembourg <br/>
52     MYS     Malaysia <br/>
53     MLI     Mali <br/>
54     MEX     Mexico <br/>
55     MDA     Moldova <br/>
56     MAR     Morocco <br/>
57     NPL     Nepal <br/>
58     NLD     Netherlands <br/>
59     NZL     New Zealand <br/>
60     NIC     Nicaragua <br/>
61     NER     Niger <br/>
62     NGA     Nigeria <br/>
63     NOR     Norway <br/>
64     PAK     Pakistan <br/>
65     PAN     Panama <br/>
66     PRY     Paraguay <br/>
67     PER     Peru <br/>
68     PHL     Philippines <br/>
69     POL     Poland <br/>
70     PRT     Portugal <br/>
71     QAT     Qatar <br/>
72     ROU     Romania <br/>
73     RUS     Russia <br/>
74     SAU     Saudi Arabia <br/>
75     SEN     Senegal <br/>
76     SRB     Serbia <br/>
77     SGP     Singapore <br/>
78     SVK     Slovakia <br/>
79     SVN     Slovenia <br/>
80     ZAF     South Africa <br/>
81     ESP     Spain <br/>
82     LKA     Sri Lanka <br/>
83     SWE     Sweden <br/>
84     CHE     Switzerland <br/>
85     TJK     Tajikistan <br/>
86     THA     Thailand <br/>
87     TUR     Turkey <br/>
88     UKR     Ukraine <br/>
89     ARE     United Arab Emirates <br/>
90     GBR     United Kingdom <br/>
91     USA     United States <br/>
92     VEN     Venezuela <br/>
93     ZMB     Zambia <br/>
"

# ** Note you can supply multiple countries.<br/>
# Use commas with no spaces: USA,BEL,GBR,ITA,ESP



# ui <- fluidPage(
#   textInput("testing_countriesList", label=HTML(countryString), value = "USA"),
#   textInput("percPeak", label="Estimate the percent of the peak your country has achieved so far to current date (as a decimal). This is needed account for the fact that
#             most countries have inaccurate reporting toward the beginning of the outbreak -- This value is used to estimate an initial number of 
#             infected individuals in the SEIR model.", value = "0.95"),
#   textInput("dateAlongCurve", label=HTML("Choose a timepoint between 0 and 5 <br/> where 0 = early curve, 5 = late curve"), value = "2"),
#   textInput("NPIprofile", label=HTML("Choose an NPI profile from the 4 Scenarios: <br/> 
#                                      1. Status Quo -- current NPIs are sustained forward in time <br/> 
#                                      2. Pre-COVID-NPI -- NPIs from pre-COVID timeperiod are applied forward in time <br/> 
#                                      3. Extreme-NPI -- the most extreme NPIs (to decrease human contact) are applied forward in time <br/> 
#                                      4. Custom-NPI -- use the options below to build an NPI profile to be applied forward in time"), value = "Custom-NPI"),
#   textInput("Google_Retail_recreation_Custom", label=HTML("If you chose Custom-NPI, specify the metrics for the following Google mobility trends <br/> 
#                                                           In general, % increase/decrease from baseline should be between -80 and +80 <br/> 
#                                                           Retail and Recreation"), value = "-25"),
#   textInput("Google_Grocery_pharmacy_Custom", label="Grocery and Pharmacy", value = "-10"),
#   textInput("Google_Parks_Custom", label="Parks", value = "-5"),
#   textInput("Google_Transit_stations_Custom", label="Transit Stations", value = "-30"),
#   textInput("Google_Workplaces_Custom", label="Workplaces", value = "-30"),
#   textInput("Google_Residential_Custom", label="Residential Areas", value = "+30"),
#   textInput("Social_Distancing_Custom", label=HTML("If you chose Custom-NPI, choose a policy for country-wide social distancing: <br/>
#             0 = No social distancing measures implemented <br/>
#             1 = Large gatherings banned (e.g. concerts, sporting events, conferences) <br/>
#             2 = Mid-sized gatherings are voluntarily closed <br/>
#             3 = Service industries are closed (e.g., restaurants and pubs) <br/>
#             4 = Shelter-in-place orders for non-essential workers <br/>
#             5 = Lockdown except for essentials (e.g. grocery shopping)"), value = "4"),
#   textInput("Quaranting_Cases_Custom", label=HTML("If you chose Custom-NPI, choose a policy for country-wide quarantining: <br/>
#                                                   0 = No action is taken to quarantine cases <br/>
#                                                   1 = Infected individuals are quarantined <br/>
#                                                   2 = Households of infected individuals are quarantined <br/>
#                                                   3 = Others in contact with infected individuals are tracked and then quarantined"), value = "3"),
#   textInput("Close_Border_Custom", label=HTML("If you chose Custom-NPI, choose a policy for country-wide closing the border: <br/>
#                                               0 = No restrictions at the border <br/>
#                                               1 = Closed to Wuhan <br/>
#                                               2 = Closed to multiple highly infected countries <br/>
#                                               3 = Closed except for essential travel <br/>
#                                               4 = Closed completely"), value = "3"),
#   # plot1,
#   # plot2,
#   # plot_predict,
#   # plot_varimp_R,
#   # plot3,
#   # plot4,
#   # plot5,
#   # plot_varimp_nonR,
#   # incidence_gg,
#   # seirScaled_gg
#   actionButton("runScript", "Run"),
#   mainPanel(plotOutput("plot1"),
#             plotOutput("plot2"),
#             plotOutput("plot_predict"),
#             plotOutput("plot3"),
#             plotOutput("plot4"),
#             plotOutput("plot5"),
#             plotOutput("plot_varimp_R"),
#             plotOutput("plot_varimp_nonR"),
#             plotOutput("seirScaled_gg"),
#             plotOutput("incidence_gg")
#   )
# )

appCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"

ui <- fluidPage(
  # textInput("testing_countriesList", label=HTML(countryString), value = "USA"),
  selectInput("testing_countriesList", label=HTML("Select a country of interest for model prediction"),
              c("Afghanistan" = "AFG",
                "Argentina" = "ARG",
                "Australia" = "AUS",
                "Austria" = "AUT",
                "Bangladesh" = "BGD",
                "Belarus" = "BLR",
                "Belgium" = "BEL",
                "Benin" = "BEN",
                "Bolivia" = "BOL",
                "Bosnia and Herzegovina" = "BIH",
                "Brazil" = "BRA",
                "Bulgaria" = "BGR",
                "Burkina Faso" = "BFA",
                "Cameroon" = "CMR",
                "Canada" = "CAN",
                "Chile" = "CHL",
                "Colombia" = "COL",
                "Costa Rica" = "CRI",
                "Croatia" = "HRV",
                "Czechia" = "CZE",
                "Denmark" = "DNK",
                "Dominican Republic" = "DOM",
                "Ecuador" = "ECU",
                "Egypt" = "EGY",
                "El Salvador" = "SLV",
                "Estonia" = "EST",
                "Finland" = "FIN",
                "France" = "FRA",
                "Gabon" = "GAB",
                "Germany" = "DEU",
                "Ghana" = "GHA",
                "Greece" = "GRC",
                "Guatemala" = "GTM",
                "Haiti" = "HTI",
                "Honduras" = "HND",
                "Hungary" = "HUN",
                "India" = "IND",
                "Indonesia" = "IDN",
                "Iraq" = "IRQ",
                "Ireland" = "IRL",
                "Israel" = "ISR",
                "Italy" = "ITA",
                "Japan" = "JPN",
                "Jordan" = "JOR",
                "Kazakhstan" = "KAZ",
                "Kenya" = "KEN",
                "Korea" = "KOR",
                "Kyrgyzstan" = "KGZ",
                "Latvia" = "LVA",
                "Lebanon" = "LBN",
                "Lithuania" = "LTU",
                "Luxembourg" = "LUX",
                "Malaysia" = "MYS",
                "Mali" = "MLI",
                "Mexico" = "MEX",
                "Moldova" = "MDA",
                "Morocco" = "MAR",
                "Mozambique" = "MOZ",
                "Nepal" = "NPL",
                "Netherlands" = "ANT",
                "New Zealand" = "NZL",
                "Nicaragua" = "NIC",
                "Niger" = "NER",
                "Nigeria" = "NGA",
                "Norway" = "NOR",
                "Pakistan" = "PAK",
                "Panama" = "PAN",
                "Paraguay" = "PRY",
                "Peru" = "PER",
                "Philippines" = "PHL",
                "Poland" = "POL",
                "Portugal" = "PRT",
                "Qatar" = "QAT",
                "Romania" = "ROU",
                "Russia" = "RUS",
                "Rwanda" = "RWA",
                "Saudi Arabia" = "SAU",
                "Senegal" = "SEN",
                "Serbia" = "SRB",
                "Singapore" = "SGP",
                "Slovakia" = "SVK",
                "Slovenia" = "SVN",
                "South Africa" = "ZAF",
                "Spain" = "ESP",
                "Sri Lanka" = "LKA",
                "Sweden" = "SWE",
                "Switzerland" = "CHE",
                "Tajikistan" = "TJK",
                "Thailand" = "THA",
                "Turkey" = "TUR",
                "Uganda" = "UGA",
                "Ukraine" = "UKR",
                "United Arab Emirates" = "ARE",
                "United Kingdom" = "GBR",
                "United States" = "USA",
                "Venezuela" = "VEN",
                "Yemen" = "YEM",
                "Zambia" = "ZMB"
              ), selected = "USA"
  ),
  # sliderInput("percPeak", label="Estimate the percent of the peak your country has achieved so far to current date (as a decimal). This is needed account for the fact that
  #           most countries have inaccurate reporting toward the beginning of the outbreak -- This value is used to estimate an initial number of 
  #           infected individuals in the SEIR model.", value = "0.90", min = 0, max = 1),
  sliderInput("chosen_initialInfected", label="Estimate the initial number of infected individuals in the SEIR model.", value = "30000", min = 1, max = 100000),
  textInput(inputId = 'chosen_initialInfected_textValue',value = 30000,label = NULL),
  sliderInput("chosen_initialExposed", label="Estimate the initial number of exposed individuals in the SEIR model.", value = "30000", min = 1, max = 100000),
  textInput(inputId = 'chosen_initialExposed_textValue',value = 30000,label = NULL),
  
  # sliderInput("dateAlongCurve", label=HTML("Choose a timepoint at which new NPIs are implemented <br/> 0 = early-outbreak, 10 = late-outbreak/recent"), value = "8", min = 0, max = 10),
  # Sidebar with a slider input for the number of bins
  sliderInput("dateAlongCurve",
                  label=HTML("Choose a day at which new NPIs are implemented. If the chosen date is too early or too late for the chosen country's available data it will automatically be adjusted to the nearest available endpoint."),
                  min = as.Date("2020-02-01","%Y-%m-%d"),
                  max = as.Date(Sys.Date(),"%Y-%m-%d"),
                  value=as.Date("2020-06-13"),
                  timeFormat="%Y-%m-%d"),
  sliderInput("recoveryTime",
              label=HTML("Select the mean COVID-19 recovery time for the SEIR model."),
              min = 2,
              max = 20,
              value = 14.5, step = 0.1
              ),
  
  sliderInput("latencyTime",
              label=HTML("Select the mean COVID-19 latency time for the SEIR model."),
              min = 0,
              max = 14,
              value = 5.2, step = 0.1
  ),
  
  selectInput("NPIprofile", label=HTML("Choose an NPI profile from the 4 Scenarios. This profile will be caried forward in time from your chosen date in an SEIR model:"),
              c("Status Quo -- current NPIs are sustained forward in time" = "Status Quo",
                "Pre-COVID-NPI -- NPIs from pre-COVID timeperiod are applied forward in time" = "Pre-COVID-NPI",
                "Extreme-NPI -- the most extreme NPIs (to decrease human contact) are applied forward in time" = "Extreme-NPI",
                "Custom-NPI -- use the options below to build an NPI profile to be applied forward in time" = "Custom-NPI"), selected = "Custom-NPI"
              ),
  checkboxInput("Lock_customs", label=HTML("Check here to stop the Custom-NPI sliders from automatically updating"), value = FALSE),
  sliderInput("Google_Retail_recreation_Custom", label=HTML("If you chose Custom-NPI, specify the metrics for the following Google mobility trends. Each number represents the percent change in visits to places compared to baseline (pre-COVID) <br/> <br/> 
                                                             Retail and Recreation: <br/> 
                                                            restaurants, cafes, shopping centers, theme parks, museums, libraries, and movie theaters"), value = "-25", min = -90, max = +50),
  sliderInput("Google_Grocery_pharmacy_Custom", label=HTML("Grocery and Pharmacy: <br/> 
                                                            grocery markets, food warehouses, farmers markets, specialty food shops, drug stores, and pharmacies"), value = "-10", min = -90, max = +90),
  sliderInput("Google_Parks_Custom", label=HTML("Parks: <br/> 
                                                            national parks, public beaches, marinas, dog parks, plazas, and public gardens"), value = "-5", min = -90, max = +90),
  sliderInput("Google_Transit_stations_Custom", label=HTML("Transit Stations: <br/> 
                                                            public transport hubs such as subway, bus, and train stations"), value = "-30", min = -90, max = +50),
  sliderInput("Google_Workplaces_Custom", label=HTML("Workplaces: <br/> 
                                                            places of work"), value = "-31", min = -90, max = +50),
  sliderInput("Google_Residential_Custom", label=HTML("Residential Areas: <br/> 
                                                            places of residence"), value = "+32", min = -20, max = +60),
  
  sliderInput("StringencyIndexForDisplay_Custom", label=HTML("A stringency index (which records the strictness of ‘lockdown style’ policies that primarily restrict people’s behaviour). <br/> 
                                                            "), value = "+70", min = 0, max = 100),
  sliderInput("GovernmentResponseIndexForDisplay_Custom", label=HTML("An overall government response index (which records how the response of governments has varied over all indicators in the database, becoming stronger or weaker over the course of the outbreak <br/> 
                                                            "), value = "+70", min = 0, max = 100),
  sliderInput("ContainmentHealthIndexForDisplay_Custom", label=HTML("A containment and health index (which combines ‘lockdown’ restrictions and closures with measures such as testing policy and contact tracing, short term investment in healthcare, as well investments in vaccine) <br/> 
                                                            "), value = "+70", min = 0, max = 100),
  sliderInput("EconomicSupportIndexForDisplay_Custom", label=HTML("An economic support index (which records measures such as income support and debt relief) <br/> 
                                                            "), value = "+60", min = 0, max = 100),
 
  # selectInput("Social_Distancing_Custom", label=HTML("Choose a country-wide policy for social distancing:"),
  #             c("No social distancing measures implemented" = "0",
  #               "Large gatherings banned (e.g. concerts, sporting events, conferences)" = "1",
  #               "Mid-sized gatherings are voluntarily closed" = "2",
  #               "Service industries are closed (e.g., restaurants and pubs)" = "3",
  #               "Shelter-in-place orders for non-essential workers" = "4",
  #               "Lockdown except for essentials (e.g. grocery shopping)" = "5"), selected = "4"
  # ),
  # selectInput("Quaranting_Cases_Custom", label=HTML("Choose a country-wide policy for quarantining cases"),
  #             c("No action is taken to quarantine cases" = "0",
  #               "Infected individuals are quarantined" = "1",
  #               "Households of infected individuals are quarantined" = "2",
  #               "Others in contact with infected individuals are tracked and then quarantined" = "3"), selected = "3"
  # ),
  # 
  # 
  # selectInput("Close_Border_Custom", label=HTML("Choose a country-wide policy for closing the border"),
  #             c("No restrictions at the border" = "0",
  #               "Closed to Wuhan, China" = "1",
  #               "Closed to multiple highly infected countries" = "2",
  #               "Closed except for essential travel" = "3",
  #               "Closed completely" = "4"), selected = "3"
  # ),

  # plot1,
  # plot2,
  # plot_predict,
  # plot_varimp_R,
  # plot3,
  # plot4,
  # plot5,
  # plot_varimp_nonR,
  # incidence_gg,
  # seirScaled_gg
  actionButton("runScript", "Run"),
  useShinyjs(),
  inlineCSS(appCSS),
  
  # Loading message
  div(
    id = "loading-content",
    h2("Click Run... plots will appear after about 2 minutes")
  ),
  

  sidebarLayout(sidebarPanel( HTML(paste0(
                                    paste0(rep("<br/>",5),collapse=""),"Figure 1: Cumulative cases by country",paste0(rep("<br/>",20),collapse=""),
                                    "Figure 2: R(t) time-series by country",paste0(rep("<br/>",20),collapse=""),
                                    "Figure 3: Google mobility data. Solid lines show the empirical values for percent change from baseline in each sector. Dashed lines illustrate the chosen NPI scenario carried forward after the chosen timepoint.",paste0(rep("<br/>",17),collapse=""),
                                    "Figure 4: OxCGRT data. Solid lines show the empirical values for percent change from baseline in each sector. Dashed lines show values for the chosen NPI scenario carried forward after the chosen timepoint.",paste0(rep("<br/>",17),collapse=""),
                                    "Figure 5: Variable importance for interpretation model.",paste0(rep("<br/>",19),collapse=""),
                                    "Figure 6: R(t) time-series for each scenario. Custom will overlap with one of the others unless Custom NPI profile was chosen. 14 day forecasting is shown beyond the latest data.",paste0(rep("<br/>",18),collapse=""),
                                    "Figure 7: Susceptible (S), Exposed (E), Infected (I), and Recovered (R) curves from the deterministic SEIR compartmental epidemiological models.",paste0(rep("<br/>",18),collapse=""),
                                    "Figure 8: The SEIR model is used to derive the turquoise predicted new infection curve to be compared against the reported epidemic curve (and it's smoothed moving average)",paste0(rep("<br/>",15),collapse="")
                                    # "Plot 9: Cumulative cases by country",paste0(rep("<br/>",21),collapse="")
                                          )
                                  )
                            ),
                mainPanel(
                          plotlyOutput("plot1"),
                          plotlyOutput("plot2"),
                          plotlyOutput("plot3"),
                          plotlyOutput("plot4"),
                          # plotlyOutput("plot5"),
                          plotlyOutput("plot_varimp"),
                          plotlyOutput("plot_predict"),
                          plotlyOutput("seirScaled_gg"),
                          plotlyOutput("incidence_gg")
                          )  
                )
  # mainPanel(
            # ("plot1"),
            # plotOutput("plot2"),
            # plotOutput("plot3"),
            # plotOutput("plot4"),
            # plotOutput("plot5"),
            # plotOutput("plot_predict"),
            # plotOutput("plot_varimp"),
            # # plotOutput("plot_varimp_nonR"),
            # plotOutput("seirScaled_gg"),
            # plotOutput("incidence_gg")
  # )
  
)

# mylist <- list(
#   testing_countriesList = "USA",
#   percPeak = as.numeric("0.95"),
#   dateAlongCurve = as.numeric("2"),
#   NPIprofile = "Custom-NPI",
#   Google_Retail_recreation_Custom = as.numeric("-30"),
#   Google_Grocery_pharmacy_Custom = as.numeric("-30"),
#   Google_Parks_Custom = as.numeric("-30"),
#   Google_Transit_stations_Custom = as.numeric("-30"),
#   Google_Workplaces_Custom = as.numeric("-30"),
#   Google_Residential_Custom = as.numeric("-30"),
#   Social_Distancing_Custom = as.numeric("2"),
#   Quaranting_Cases_Custom = as.numeric("2"),
#   Close_Border_Custom = as.numeric("2")
# )

server <- function(input, output, session) {
  
  mylist <- reactiveVal() # we will store the inputs in a reactive list
  
  observeEvent(input$dateAlongCurve, {
    if(input$Lock_customs == FALSE){
      # myDate <- as.Date("2020-05-21")
      myDate <- as.Date(input$dateAlongCurve)
      updateSliderDF <- fread(paste0("./InputData/ML_features_oxford.csv"),sep=",")
      updateSliderDF_sub <- subset(updateSliderDF, ISO3 == input$testing_countriesList)
      earliestD <- which(updateSliderDF_sub$confirmed_cum >= 50)[1]
      D0 <- as.Date(updateSliderDF_sub$date[earliestD]) + 15 #"2020-03-21"
      D4 <- updateSliderDF_sub$date[which.max(updateSliderDF_sub$confirmed_cum_per_million)]
      if(as.Date(myDate) < as.Date(D0)){
        myDate = as.Date(D0)
      }
      if(as.Date(myDate) > as.Date(D4)){
        myDate = as.Date(D4)
      }
      myDateIndex <- which(updateSliderDF_sub$date == myDate)
      updateSliderInput(session = session,inputId = "Google_Retail_recreation_Custom",
                        value = mean(updateSliderDF_sub$Google_Retail_recreation[(myDateIndex-5):myDateIndex], na.rm = T))
      updateSliderInput(session = session,inputId = "Google_Grocery_pharmacy_Custom",
                        value = mean(updateSliderDF_sub$Google_Grocery_pharmacy[(myDateIndex-5):myDateIndex], na.rm = T))
      updateSliderInput(session = session,inputId = "Google_Parks_Custom",
                        value = mean(updateSliderDF_sub$Google_Parks[(myDateIndex-5):myDateIndex], na.rm = T))
      updateSliderInput(session = session,inputId = "Google_Transit_stations_Custom",
                        value = mean(updateSliderDF_sub$Google_Transit_stations[(myDateIndex-5):myDateIndex], na.rm = T))
      updateSliderInput(session = session,inputId = "Google_Workplaces_Custom",
                        value = mean(updateSliderDF_sub$Google_Workplaces[(myDateIndex-5):myDateIndex], na.rm = T))
      updateSliderInput(session = session,inputId = "Google_Residential_Custom",
                        value = mean(updateSliderDF_sub$Google_Residential[(myDateIndex-5):myDateIndex], na.rm = T))
      updateSliderInput(session = session,inputId = "StringencyIndexForDisplay_Custom",
                        value = mean(updateSliderDF_sub$StringencyIndexForDisplay[myDateIndex], na.rm = T))
      updateSliderInput(session = session,inputId = "GovernmentResponseIndexForDisplay_Custom",
                        value = mean(updateSliderDF_sub$GovernmentResponseIndexForDisplay[myDateIndex], na.rm = T))
      updateSliderInput(session = session,inputId = "ContainmentHealthIndexForDisplay_Custom",
                        value = mean(updateSliderDF_sub$ContainmentHealthIndexForDisplay[myDateIndex], na.rm = T))
      updateSliderInput(session = session,inputId = "EconomicSupportIndexForDisplay_Custom",
                        value = mean(updateSliderDF_sub$EconomicSupportIndexForDisplay[myDateIndex], na.rm = T))
    }
  })
  
  observeEvent(input$testing_countriesList, {
    if(input$Lock_customs == FALSE){
      # myDate <- as.Date("2020-05-21")
      myDate <- as.Date(input$dateAlongCurve)
      updateSliderDF <- fread(paste0("./InputData/ML_features_oxford.csv"),sep=",")
      updateSliderDF_sub <- subset(updateSliderDF, ISO3 == input$testing_countriesList)
      earliestD <- which(updateSliderDF_sub$confirmed_cum >= 50)[1]
      D0 <- as.Date(updateSliderDF_sub$date[earliestD]) + 15 #"2020-03-21"
      D4 <- updateSliderDF_sub$date[which.max(updateSliderDF_sub$confirmed_cum_per_million)]
      if(as.Date(myDate) < as.Date(D0)){
        myDate = as.Date(D0)
      }
      if(as.Date(myDate) > as.Date(D4)){
        myDate = as.Date(D4)
      }
      myDateIndex <- which(updateSliderDF_sub$date == myDate)
      updateSliderInput(session = session,inputId = "Google_Retail_recreation_Custom",
                        value = mean(updateSliderDF_sub$Google_Retail_recreation[(myDateIndex-5):myDateIndex], na.rm = T))
      updateSliderInput(session = session,inputId = "Google_Grocery_pharmacy_Custom",
                        value = mean(updateSliderDF_sub$Google_Grocery_pharmacy[(myDateIndex-5):myDateIndex], na.rm = T))
      updateSliderInput(session = session,inputId = "Google_Parks_Custom",
                        value = mean(updateSliderDF_sub$Google_Parks[(myDateIndex-5):myDateIndex], na.rm = T))
      updateSliderInput(session = session,inputId = "Google_Transit_stations_Custom",
                        value = mean(updateSliderDF_sub$Google_Transit_stations[(myDateIndex-5):myDateIndex], na.rm = T))
      updateSliderInput(session = session,inputId = "Google_Workplaces_Custom",
                        value = mean(updateSliderDF_sub$Google_Workplaces[(myDateIndex-5):myDateIndex], na.rm = T))
      updateSliderInput(session = session,inputId = "Google_Residential_Custom",
                        value = mean(updateSliderDF_sub$Google_Residential[(myDateIndex-5):myDateIndex], na.rm = T))
      updateSliderInput(session = session,inputId = "StringencyIndexForDisplay_Custom",
                        value = mean(updateSliderDF_sub$StringencyIndexForDisplay[myDateIndex], na.rm = T))
      updateSliderInput(session = session,inputId = "GovernmentResponseIndexForDisplay_Custom",
                        value = mean(updateSliderDF_sub$GovernmentResponseIndexForDisplay[myDateIndex], na.rm = T))
      updateSliderInput(session = session,inputId = "ContainmentHealthIndexForDisplay_Custom",
                        value = mean(updateSliderDF_sub$ContainmentHealthIndexForDisplay[myDateIndex], na.rm = T))
      updateSliderInput(session = session,inputId = "EconomicSupportIndexForDisplay_Custom",
                        value = mean(updateSliderDF_sub$EconomicSupportIndexForDisplay[myDateIndex], na.rm = T))
    }
  })
  
  observeEvent(input$chosen_initialInfected_textValue,{
    if(as.numeric(input$chosen_initialInfected_textValue) != input$chosen_initialInfected & as.numeric(input$chosen_initialInfected_textValue) > 0 & as.numeric(input$chosen_initialInfected_textValue) <= 100000)
    {
      updateSliderInput(
        session = session,
        inputId = 'chosen_initialInfected',
        value = input$chosen_initialInfected_textValue
      ) # updateSliderInput
    }#if
    if(as.numeric(input$chosen_initialInfected_textValue) > 100000){
      updateSliderInput(
        session = session,
        inputId = 'chosen_initialInfected',
        value = NULL
      ) # updateSliderInput
    }
  })
  observeEvent(input$chosen_initialInfected,{
    if(as.numeric(input$chosen_initialInfected_textValue) != input$chosen_initialInfected & as.numeric(input$chosen_initialInfected_textValue) > 0)
    {
      updateTextInput(
        session = session,
        inputId = 'chosen_initialInfected_textValue',
        value = input$chosen_initialInfected
      ) # updateTextInput
    }#if
  })
  
  observeEvent(input$chosen_initialExposed_textValue,{
    if(as.numeric(input$chosen_initialExposed_textValue) != input$chosen_initialExposed & input$chosen_initialExposed_textValue > 0 & input$chosen_initialExposed_textValue <= 100000)
    {
      updateSliderInput(
        session = session,
        inputId = 'chosen_initialExposed',
        value = input$chosen_initialExposed_textValue
      ) # updateSliderInput
    }#if
    if(as.numeric(input$chosen_initialExposed_textValue) > 100000){
      updateSliderInput(
        session = session,
        inputId = 'chosen_initialExposed',
        value = NULL
      ) # updateSliderInput
    }
  })
  observeEvent(input$chosen_initialExposed,{
    if(as.numeric(input$chosen_initialExposed_textValue) != input$chosen_initialExposed & input$chosen_initialExposed_textValue > 0)
    {
      updateTextInput(
        session = session,
        inputId = 'chosen_initialExposed_textValue',
        value = input$chosen_initialExposed
      ) # updateTextInput
    }#if
  })
  
  observe({ # create the list
    mylist(list(
      testing_countriesList = input$testing_countriesList,
      # percPeak = as.numeric(input$percPeak),
      chosen_initialInfected = as.numeric(input$chosen_initialInfected_textValue),
      chosen_initialExposed = as.numeric(input$chosen_initialExposed_textValue),
      dateAlongCurve = as.Date(input$dateAlongCurve),
      recoveryTime = as.numeric(input$recoveryTime),
      latencyTime = as.numeric(input$latencyTime),
      NPIprofile = input$NPIprofile,
      Google_Retail_recreation_Custom = as.numeric(input$Google_Retail_recreation_Custom),
      Google_Grocery_pharmacy_Custom = as.numeric(input$Google_Grocery_pharmacy_Custom),
      Google_Parks_Custom = as.numeric(input$Google_Parks_Custom),
      Google_Transit_stations_Custom = as.numeric(input$Google_Transit_stations_Custom),
      Google_Workplaces_Custom = as.numeric(input$Google_Workplaces_Custom),
      Google_Residential_Custom = as.numeric(input$Google_Residential_Custom),
      StringencyIndexForDisplay_Custom = as.numeric(input$StringencyIndexForDisplay_Custom),
      GovernmentResponseIndexForDisplay_Custom = as.numeric(input$GovernmentResponseIndexForDisplay_Custom),
      ContainmentHealthIndexForDisplay_Custom = as.numeric(input$ContainmentHealthIndexForDisplay_Custom),
      EconomicSupportIndexForDisplay_Custom = as.numeric(input$EconomicSupportIndexForDisplay_Custom)))
  })

  
  
  observeEvent(input$runScript, { # "runScript" is an action button
    source("./Scripts/Analysis_2_Model_R0_5_14_WT_SEIR_ribbon_function_oxford.R", local = list2env(mylist()))
    hide(id = "loading-content", anim = TRUE, animType = "fade") 
    # l <- readLines("C:/Users/chris/OneDrive/Desktop/SKOvid19/Scripts/Analysis_2_Model_R0_5_14_WT_SEIR_ribbon_function.R")
    # n <- length(l)
    # withProgress(message = 'Running Model & Making Plots...', value = 0, {
    #   for (i in 1:n) {
    #     eval(parse(text=l[i]))
    #     incProgress(1/n, detail = paste("Doing part", l[i]))
    #   }
    # })

    load(paste0(
      "./InputData/",
      "R0_",
      "Scenarios",
      ".RData"
    ))
    # output$plot1 <- renderPlot({
    #   plot1
    # }, width = "auto", height = "auto")
    # output$plot2 <- renderPlot({
    #   plot2
    # }, width = "auto", height = "auto")
    # output$plot3 <- renderPlot({
    #   plot3
    # }, width = "auto", height = "auto")
    # output$plot4 <- renderPlot({
    #   plot4
    # }, width = "auto", height = "auto")
    # output$plot5 <- renderPlot({
    #   plot5
    # }, width = "auto", height = "auto")
    # output$plot_predict <- renderPlot({
    #   plot_predict
    # }, width = "auto", height = "auto")
    # output$plot_varimp <- renderPlot({
    #   plot_varimp
    # }, width = "auto", height = "auto")
    # # output$plot_varimp_nonR <- renderPlot({
    # #   plot_varimp_nonR
    # # }, width = "auto", height = "auto")
    # output$seirScaled_gg <- renderPlot({
    #   seirScaled_gg
    # }, width = "auto", height = "auto")
    # output$incidence_gg <- renderPlot({
    #   incidence_gg
    # }, width = "auto", height = "auto")
    
    output$plot1 <- renderPlotly({
      ggplotly(plot1)
    })
    output$plot2 <- renderPlotly({
      ggplotly(plot2)
    })
    output$plot3 <- renderPlotly({
      ggplotly(plot3)
    })
    output$plot4 <- renderPlotly({
      ggplotly(plot4)
    })
    # output$plot5 <- renderPlotly({
    #   ggplotly(plot5)
    # })
    output$plot_varimp <- renderPlotly({
      ggplotly(plot_varimp)
    })
    output$plot_predict <- renderPlotly({
      ggplotly(plot_predict)
    })
    output$seirScaled_gg <- renderPlotly({
      seirScaled_gg <- ggplotly(seirScaled_gg)
      seirScaled_ggNames <- c("Susceptible", "Exposed", "Infected", "Recovered") # we need to know the "true" legend values
      for (i in 1:length(seirScaled_gg$x$data)) { # this goes over all places where legend values are stored
        n1 <- seirScaled_gg$x$data[[i]]$name # and this is how the value is stored in plotly
        n2 <- " "
        for (j in 1:length(seirScaled_ggNames)) {
          if (grepl(x = n1, pattern = seirScaled_ggNames[j])) {n2 = seirScaled_ggNames[j]} # if the plotly legend name contains the original value, replace it with the original value
        }
        seirScaled_gg$x$data[[i]]$name <- n2 # now is the time for actual replacement
        if (n2 == " ") {seirScaled_gg$x$data[[i]]$showlegend = FALSE}  # sometimes plotly adds to the legend values that we don't want, this is how to get rid of them, too
      }
      
      ggplotly(seirScaled_gg)
     
    })
    output$incidence_gg <- renderPlotly({
      ggplotly(incidence_gg)
    })

    
  })

}

shinyApp(ui, server)
