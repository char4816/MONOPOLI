# Wrangling the demographics and socioeconomics data

# Written by:  Chris H Arehart
# Written on: March 24th, 2020
# Updated on: June 20th, 2020

library(tidyverse)
library(readxl)
library(reshape2)

HubeiFlag <- T
if(HubeiFlag == T){
  xlFile <- "./InputData/GHSindex_data_static_hub.xlsx"
}else{
  xlFile <- "./InputData/GHSindex_data_static.xlsx"
}


print(getwd())

###########################################################################################################################################

static_variables_list <- c(
  "CountryCodes",
  # "Overall",
  "Prevent",
  "Detect",
  "Respond",
  "Health",
  "Norms",
  "Risk",
  "GDP_bill",
  "GDP_percapita",
  "Population_mill",
  "HumanDevelopmentIndex_2018",
  "EIUDemocracyIndexScore_2019",
  "UNOnlineServicesIndexScore_2018",
  "GlobalPeaceIndex",
  "CorruptionsPerceptionIndex_2018",
  "HumanCapitalIndex_2017",
  "SDGIndexScore_2018"
)

for(i in 1:length(static_variables_list)){
  var_name <- static_variables_list[i]
  if(var_name %in% c("Overall", "Prevent", "Detect", "Respond", "Health", "Norms", "Risk")){
    var_name_with_prefix <- paste0("GHS_",var_name)
  }else{
    var_name_with_prefix <- var_name
  }
  assign(var_name, read_excel(xlFile, sheet = var_name, col_names = F))
  if(var_name == "CountryCodes"){
    assign(var_name, eval(parse(text = var_name)) %>% rename("ISO3" = "...1") )
    assign(var_name, eval(parse(text = var_name)) %>% rename("FullName" = "...2") )
  }else{
    assign(var_name, eval(parse(text = var_name)) %>% rename("FullName" = "...1") )
    assign(var_name, eval(parse(text = var_name)) %>% rename(!!var_name_with_prefix := "...2") )
  }
  
  if(i == 1){
    merge_all <- eval(parse(text = static_variables_list[i]))
  }
  if(i > 1){
    merge_all <- merge(merge_all, eval(parse(text = static_variables_list[i])), by = "FullName")
  }
}

dim(merge_all)

###########################################################################################################################################
# Popultation Structure by gender and age
pop_structure <- read_excel(xlFile, sheet = "PopulationStructure", col_names = T)

pop_structure$AgeGroup[pop_structure$AgeGroup == "0-14 years"] <- "age_0_14"
pop_structure$AgeGroup[pop_structure$AgeGroup == "15-24 years"] <- "age_15_24"
pop_structure$AgeGroup[pop_structure$AgeGroup == "25-54 years"] <- "age_25_54"
pop_structure$AgeGroup[pop_structure$AgeGroup == "55-64 years"] <- "age_55_64"
pop_structure$AgeGroup[pop_structure$AgeGroup == "65 years and over"] <- "age_65_plus"

df_AgeGroupPercent <- melt(pop_structure,id=c("FullName","AgeGroup"),"AgeGroupPercent")
df_MaleCount <- melt(pop_structure,id=c("FullName","AgeGroup"),"MaleCount")
df_FemaleCount <- melt(pop_structure,id=c("FullName","AgeGroup"),"FemaleCount")

df_AgeGroupPercent_unmelt <- dcast(df_AgeGroupPercent, FullName ~ AgeGroup)
colnames(df_AgeGroupPercent_unmelt) <- paste0(colnames(df_AgeGroupPercent_unmelt),"_Percent")
df_MaleCount_unmelt <- dcast(df_MaleCount, FullName ~ AgeGroup)
colnames(df_MaleCount_unmelt) <- paste0(colnames(df_MaleCount_unmelt),"_MaleCount")
df_FemaleCount_unmelt <- dcast(df_FemaleCount, FullName ~ AgeGroup)
colnames(df_FemaleCount_unmelt) <- paste0(colnames(df_FemaleCount_unmelt),"_FemaleCount")

head(df_AgeGroupPercent_unmelt)
head(df_MaleCount_unmelt)
head(df_FemaleCount_unmelt)

'%ni%' <- Negate('%in%')

#check empty so every country matches
merge_all$FullName[which(merge_all$FullName %ni% unique(df_AgeGroupPercent_unmelt$FullName_Percent))]

merge_all <- merge(merge_all, df_AgeGroupPercent_unmelt, by.x="FullName", by.y="FullName_Percent", all.x=T, all.y=F)
merge_all <- merge(merge_all, df_MaleCount_unmelt, by.x="FullName", by.y="FullName_MaleCount", all.x=T, all.y=F)
merge_all <- merge(merge_all, df_FemaleCount_unmelt, by.x="FullName", by.y="FullName_FemaleCount", all.x=T, all.y=F)

dim(merge_all)

###########################################################################################################################################
# Popultation growth rate

pop_growth <- read_excel(xlFile, sheet = "PopulationGrowthRate", col_names = F)
colnames(pop_growth) <- c("FullName","PopulationGrowthRate")
merge_all$FullName[which(merge_all$FullName %ni% unique(pop_growth$FullName))]

pop_growth$FullName[pop_growth$FullName=="Bahamas, The"] <- "Bahamas"
pop_growth$FullName[pop_growth$FullName=="Congo, Republic of the"] <- "Congo (Brazzaville)"
pop_growth$FullName[pop_growth$FullName=="Congo, Democratic Republic of the"] <- "Congo (Democratic Republic)"
pop_growth$FullName[pop_growth$FullName=="Cote d'Ivoire"] <- "Côte d'Ivoire"
pop_growth$FullName[pop_growth$FullName=="Czechia"] <- "Czech Republic"
pop_growth$FullName[pop_growth$FullName=="Swaziland"] <- "eSwatini (Swaziland)"
pop_growth$FullName[pop_growth$FullName=="Gambia, The"] <- "Gambia"
pop_growth$FullName[pop_growth$FullName=="Kyrgyzstan"] <- "Kyrgyz Republic"
pop_growth$FullName[pop_growth$FullName=="Micronesia, Federated States of"] <- "Micronesia"
pop_growth$FullName[pop_growth$FullName=="Burma"] <- "Myanmar"
pop_growth$FullName[pop_growth$FullName=="Korea, North"] <- "North Korea"
pop_growth$FullName[pop_growth$FullName=="Macedonia"] <- "North Macedonia"
pop_growth$FullName[pop_growth$FullName=="Sao Tome and Principe"] <- "São Tomé and Príncipe"
pop_growth$FullName[pop_growth$FullName=="Korea, South"] <- "South Korea"
pop_growth$FullName[pop_growth$FullName=="Saint Kitts and Nevis"] <- "St Kitts and Nevis"
pop_growth$FullName[pop_growth$FullName=="Saint Lucia"] <- "St Lucia"
pop_growth$FullName[pop_growth$FullName=="Saint Vincent and the Grenadines"] <- "St Vincent and The Grenadines"
pop_growth <- rbind(pop_growth,c("Syria",NA))

merge_all$FullName[which(merge_all$FullName %ni% unique(pop_growth$FullName))]

merge_all <- merge(merge_all, pop_growth, by.x="FullName", by.y="FullName", all.x=T, all.y=F)
dim(merge_all)

###########################################################################################################################################
# Popultation smoking

pop_smoke <- read_excel(xlFile, sheet = "PopulationSmoking", col_names = T)
merge_all$PopulationSmoking_male <- NA
merge_all$PopulationSmoking_female <- NA

merge_all$ISO3[which(merge_all$ISO3 %ni% unique(pop_smoke$ISO3))]

for(i in 1:length(merge_all$ISO3)){
  curISO <- merge_all$ISO3[i]
  if(curISO %in% pop_smoke$ISO3){
    cursub <- subset(pop_smoke, pop_smoke$ISO3==curISO & pop_smoke$SEX=="MLE")
    if("FALSE" %in% is.na(cursub$Value)){
      curmax <- max(cursub$Year[which(!is.na(cursub$Value))], na.rm = T)
      if("2020" %in% cursub$Year){
        if("FALSE" %in% is.na(cursub$Value[cursub$Year=="2020"])){
          merge_all$PopulationSmoking_male[i] <- cursub$Value[cursub$Year=="2020" & cursub$SEX=="MLE"]
          # merge_all$PopulationSmoking_female[i] <- cursub$Value[cursub$Year=="2020" & cursub$SEX=="FMLE"]
        }
      }else{
        merge_all$PopulationSmoking_male[i] <- cursub$Value[cursub$Year==curmax & cursub$SEX=="MLE"]
        # merge_all$PopulationSmoking_female[i] <- cursub$Value[cursub$Year=="2020" & cursub$SEX=="FMLE"]
      }
    }
  }
  
  if(curISO %in% pop_smoke$ISO3){
    cursub <- subset(pop_smoke, pop_smoke$ISO3==curISO & pop_smoke$SEX=="FMLE")
    if("FALSE" %in% is.na(cursub$Value)){
      curmax <- max(cursub$Year[which(!is.na(cursub$Value))], na.rm = T)
      print(curmax)
      if("2020" %in% cursub$Year){
        if("FALSE" %in% is.na(cursub$Value[cursub$Year=="2020"])){
          # merge_all$PopulationSmoking_male[i] <- cursub$Value[cursub$Year=="2020" & cursub$SEX=="MLE"]
          merge_all$PopulationSmoking_female[i] <- cursub$Value[cursub$Year=="2020" & cursub$SEX=="FMLE"]
        }
      }else{
        # merge_all$PopulationSmoking_male[i] <- cursub$Value[cursub$Year==curmax & cursub$SEX=="MLE"]
        merge_all$PopulationSmoking_female[i] <- cursub$Value[cursub$Year=="2020" & cursub$SEX=="FMLE"]
      }
    }
  }
  
}


###########################################################################################################################################
# GINI Index

gini <- read_excel(xlFile, sheet = "GINIindex", col_names = F)
colnames(gini) <- c("FullName","GINIindex")
merge_all$FullName[which(merge_all$FullName %ni% unique(gini$FullName))]

gini$FullName[gini$FullName=="Bahamas, The"] <- "Bahamas"
gini$FullName[gini$FullName=="Congo, Republic of the"] <- "Congo (Brazzaville)"
gini$FullName[gini$FullName=="Congo, Democratic Republic of the"] <- "Congo (Democratic Republic)"
gini$FullName[gini$FullName=="Cote d'Ivoire"] <- "Côte d'Ivoire"
gini$FullName[gini$FullName=="Czechia"] <- "Czech Republic"
gini$FullName[gini$FullName=="Swaziland"] <- "eSwatini (Swaziland)"
gini$FullName[gini$FullName=="Gambia, The"] <- "Gambia"
gini$FullName[gini$FullName=="Kyrgyzstan"] <- "Kyrgyz Republic"
gini$FullName[gini$FullName=="Micronesia, Federated States of"] <- "Micronesia"
gini$FullName[gini$FullName=="Burma"] <- "Myanmar"
gini$FullName[gini$FullName=="Korea, North"] <- "North Korea"
gini$FullName[gini$FullName=="Macedonia"] <- "North Macedonia"
gini$FullName[gini$FullName=="Sao Tome and Principe"] <- "São Tomé and Príncipe"
gini$FullName[gini$FullName=="Korea, South"] <- "South Korea"
gini$FullName[gini$FullName=="Saint Kitts and Nevis"] <- "St Kitts and Nevis"
gini$FullName[gini$FullName=="Saint Lucia"] <- "St Lucia"
gini$FullName[gini$FullName=="Saint Vincent and the Grenadines"] <- "St Vincent and The Grenadines"

merge_all$FullName[which(merge_all$FullName %ni% unique(gini$FullName))]

merge_all <- merge(merge_all, gini, by.x="FullName", by.y="FullName", all.x=T, all.y=F)
dim(merge_all)

###########################################################################################################################################
# Urbanization
urb <- read_excel(xlFile, sheet = "Urbanization", col_names = F)

ctys <- c()
percUrb <- c()
rateUrb <- c()
for(i in seq(from = 1, to = nrow(urb), by = 3)){
  ctys <- c(ctys,urb$...1[i])
  percUrb <- c(percUrb,urb$...1[i+1])
  rateUrb <- c(rateUrb,urb$...1[i+2])
}

percUrb <- as.numeric(str_extract(percUrb, "\\-*\\d+\\.*\\d*"))
rateUrb <- as.numeric(str_extract(rateUrb, "\\-*\\d+\\.*\\d*"))

Urbanization <- as_tibble(cbind(ctys,percUrb,rateUrb))
colnames(Urbanization) <- c("FullName","PercentUrban","RateUrbanization")

# colnames(Urbanization) <- c("FullName","GINIindex")
merge_all$FullName[which(merge_all$FullName %ni% unique(Urbanization$FullName))]

Urbanization$FullName[Urbanization$FullName=="Bahamas, The"] <- "Bahamas"
Urbanization$FullName[Urbanization$FullName=="Congo, Republic of the"] <- "Congo (Brazzaville)"
Urbanization$FullName[Urbanization$FullName=="Congo, Democratic Republic of the"] <- "Congo (Democratic Republic)"
Urbanization$FullName[Urbanization$FullName=="Cote d'Ivoire"] <- "Côte d'Ivoire"
Urbanization$FullName[Urbanization$FullName=="Czechia"] <- "Czech Republic"
Urbanization$FullName[Urbanization$FullName=="Eswatini"] <- "eSwatini (Swaziland)"
Urbanization$FullName[Urbanization$FullName=="Gambia, The"] <- "Gambia"
Urbanization$FullName[Urbanization$FullName=="Kyrgyzstan"] <- "Kyrgyz Republic"
Urbanization$FullName[Urbanization$FullName=="Micronesia, Federated States of"] <- "Micronesia"
Urbanization$FullName[Urbanization$FullName=="Burma"] <- "Myanmar"
Urbanization$FullName[Urbanization$FullName=="Korea, North"] <- "North Korea"
Urbanization$FullName[Urbanization$FullName=="Macedonia"] <- "North Macedonia"
Urbanization$FullName[Urbanization$FullName=="Sao Tome and Principe"] <- "São Tomé and Príncipe"
Urbanization$FullName[Urbanization$FullName=="Korea, South"] <- "South Korea"
Urbanization$FullName[Urbanization$FullName=="Saint Kitts and Nevis"] <- "St Kitts and Nevis"
Urbanization$FullName[Urbanization$FullName=="Saint Lucia"] <- "St Lucia"
Urbanization$FullName[Urbanization$FullName=="Saint Vincent and the Grenadines"] <- "St Vincent and The Grenadines"

merge_all$FullName[which(merge_all$FullName %ni% unique(Urbanization$FullName))]

merge_all <- merge(merge_all, Urbanization, by.x="FullName", by.y="FullName", all.x=T, all.y=F)

###########################################################################################################################################
# PhysicianDensity
physdens <- read_excel(xlFile, sheet = "PhysicianDensity", col_names = F)

ctys <- c()
PhysicianDensity <- c()
for(i in seq(from = 1, to = nrow(physdens), by = 2)){
  ctys <- c(ctys,physdens$...1[i])
  PhysicianDensity <- c(PhysicianDensity,physdens$...1[i+1])
}

PhysicianDensity <- as.numeric(str_extract(PhysicianDensity, "\\-*\\d+\\.*\\d*"))

PD <- as_tibble(cbind(ctys,PhysicianDensity))
colnames(PD) <- c("FullName","PhysicianDensity")

# colnames(PD) <- c("FullName","GINIindex")
merge_all$FullName[which(merge_all$FullName %ni% unique(PD$FullName))]

PD$FullName[PD$FullName=="Bahamas, The"] <- "Bahamas"
PD$FullName[PD$FullName=="Congo, Republic of the"] <- "Congo (Brazzaville)"
PD$FullName[PD$FullName=="Congo, Democratic Republic of the"] <- "Congo (Democratic Republic)"
PD$FullName[PD$FullName=="Cote d'Ivoire"] <- "Côte d'Ivoire"
PD$FullName[PD$FullName=="Czechia"] <- "Czech Republic"
PD$FullName[PD$FullName=="Eswatini"] <- "eSwatini (Swaziland)"
PD$FullName[PD$FullName=="Gambia, The"] <- "Gambia"
PD$FullName[PD$FullName=="Kyrgyzstan"] <- "Kyrgyz Republic"
PD$FullName[PD$FullName=="Micronesia, Federated States of"] <- "Micronesia"
PD$FullName[PD$FullName=="Burma"] <- "Myanmar"
PD$FullName[PD$FullName=="Korea, North"] <- "North Korea"
PD$FullName[PD$FullName=="Macedonia"] <- "North Macedonia"
PD$FullName[PD$FullName=="Sao Tome and Principe"] <- "São Tomé and Príncipe"
PD$FullName[PD$FullName=="Korea, South"] <- "South Korea"
PD$FullName[PD$FullName=="Saint Kitts and Nevis"] <- "St Kitts and Nevis"
PD$FullName[PD$FullName=="Saint Lucia"] <- "St Lucia"
PD$FullName[PD$FullName=="Saint Vincent and the Grenadines"] <- "St Vincent and The Grenadines"

merge_all$FullName[which(merge_all$FullName %ni% unique(PD$FullName))]

merge_all <- merge(merge_all, PD, by.x="FullName", by.y="FullName", all.x=T, all.y=F)

###########################################################################################################################################
# PopulationDensity
popdens <- read_excel(xlFile, sheet = "PopulationDensity", col_names = T)

popD <- as_tibble(popdens[,c("FullName","Latest")])
colnames(popD) <- c("FullName","PopulationDensity")

# colnames(popD) <- c("FullName","GINIindex")
merge_all$FullName[which(merge_all$FullName %ni% unique(popD$FullName))]

popD$FullName[popD$FullName=="Bahamas, The"] <- "Bahamas"
popD$FullName[popD$FullName=="Brunei Darussalam"] <- "Brunei"
popD$FullName[popD$FullName=="Congo, Rep."] <- "Congo (Brazzaville)"
popD$FullName[popD$FullName=="Congo, Dem. Rep."] <- "Congo (Democratic Republic)"
popD$FullName[popD$FullName=="Egypt, Arab Rep."] <- "Egypt"
popD$FullName[popD$FullName=="Iran, Islamic Rep."] <- "Iran"
popD$FullName[popD$FullName=="Micronesia, Fed. Sts."] <- "Micronesia"
popD$FullName[popD$FullName=="Korea, Rep."] <- "North Korea"
popD$FullName[popD$FullName=="Korea, Dem. People’s Rep."] <- "South Korea"
popD$FullName[popD$FullName=="Russian Federation"] <- "Russia"
popD$FullName[popD$FullName=="Slovak Republic"] <- "Slovakia"
popD$FullName[popD$FullName=="St. Kitts and Nevis"] <- "St Kitts and Nevis"
popD$FullName[popD$FullName=="St. Lucia"] <- "St Lucia"
popD$FullName[popD$FullName=="St. Vincent and the Grenadines"] <- "St Vincent and The Grenadines"
popD$FullName[popD$FullName=="Syrian Arab Republic"] <- "Syria"
popD$FullName[popD$FullName=="Venezuela, RB"] <- "Venezuela"
popD$FullName[popD$FullName=="Yemen, Rep."] <- "Yemen"
popD$FullName[popD$FullName=="Cote d'Ivoire"] <- "Côte d'Ivoire"
popD$FullName[popD$FullName=="Eswatini"] <- "eSwatini (Swaziland)"
popD$FullName[popD$FullName=="Gambia, The"] <- "Gambia"
popD$FullName[popD$FullName=="Sao Tome and Principe"] <- "São Tomé and Príncipe"

merge_all$FullName[which(merge_all$FullName %ni% unique(popD$FullName))]

merge_all <- merge(merge_all, popD, by.x="FullName", by.y="FullName", all.x=T, all.y=F)

###########################################################################################################################################
# Household Structure
house <- read_excel(xlFile, sheet = "HouseholdStructure", col_names = T)


merge_all$Ave_household_size <- NA
merge_all$Percent_house_Nuclear <- NA
merge_all$Percent_house_Multi_generation <- NA
merge_all$Percent_house_Three_generation <- NA
merge_all$Percent_house_Skip_generation <- NA

merge_all$FullName[which(merge_all$FullName %ni% unique(house$FullName))]

house$FullName[house$FullName=="Bolivia (Plurinational State of)"] <- "Bolivia"
house$FullName[house$FullName=="Brunei Darussalam"] <- "Brunei"
house$FullName[house$FullName=="Dem. Republic of the Congo"] <- "Congo (Democratic Republic)"
house$FullName[house$FullName=="Congo"] <- "Congo (Brazzaville)"
house$FullName[house$FullName=="Czechia"] <- "Czech Republic"
house$FullName[house$FullName=="Dominican Republic"] <- "Dominica"
house$FullName[house$FullName=="Swaziland"] <- "eSwatini (Swaziland)"
house$FullName[house$FullName=="Kyrgyzstan"] <- "Kyrgyz Republic"
house$FullName[house$FullName=="Dem. People's Rep. of Korea"] <- "South Korea"
house$FullName[house$FullName=="Republic of Korea"] <- "North Korea"
house$FullName[house$FullName=="Russian Federation"] <- "Russia"
house$FullName[house$FullName=="Sao Tome and Principe"] <- "São Tomé and Príncipe"
house$FullName[house$FullName=="Saint Kitts and Nevis"] <- "St Kitts and Nevis"
house$FullName[house$FullName=="United Republic of Tanzania"] <- "Tanzania"
house$FullName[house$FullName=="United States of America"] <- "United States"
house$FullName[house$FullName=="Venezuela (Bolivarian Republic of)"] <- "Venezuela"
house$FullName[house$FullName=="Viet Nam"] <- "Vietnam"

for(i in 1:length(merge_all$FullName)){
  curName <- merge_all$FullName[i]
  if(curName %in% house$FullName){
    cursub <- subset(house, house$FullName==curName)
    cursub <- cursub[rev(order(cursub$Date)),]
    rr=1
    while(is.na(merge_all$Ave_household_size[i]) && rr <= nrow(cursub)){
      merge_all$Ave_household_size[i] <- as.numeric(cursub$Average_household_size[rr])
      rr <- rr+1
    }
    rr=1
    while(is.na(merge_all$Percent_house_Nuclear[i]) && rr <= nrow(cursub)){
      merge_all$Percent_house_Nuclear[i] <- as.numeric(cursub$Nuclear[rr])
      rr <- rr+1
    }
    rr=1
    while(is.na(merge_all$Percent_house_Multi_generation[i]) && rr <= nrow(cursub)){
      merge_all$Percent_house_Multi_generation[i] <- as.numeric(cursub$Multi_generation[rr])
      rr <- rr+1
    }
    rr=1
    while(is.na(merge_all$Percent_house_Three_generation[i]) && rr <= nrow(cursub)){
      merge_all$Percent_house_Three_generation[i] <- as.numeric(cursub$Three_generation[rr])
      rr <- rr+1
    }
    rr=1
    while(is.na(merge_all$Percent_house_Skip_generation[i]) && rr <= nrow(cursub)){
      merge_all$Percent_house_Skip_generation[i] <- as.numeric(cursub$Skip_generation[rr])
      rr <- rr+1
    }
  }
}

dim(merge_all)

merge_all$Ave_household_size[which(merge_all$FullName=="Italy")]


###########################################################################################################################################
# Ethnicity Structure
# https://www.cia.gov/library/publications/resources/the-world-factbook/fields/400.html
ethnicGroups <- read_excel(xlFile, sheet = "EFindex", col_names = T)
colnames(ethnicGroups) <- c("FullName","Year","EFindex")

merge_all$FullName[which(merge_all$FullName %ni% unique(ethnicGroups$FullName))]

ethnicGroups$FullName[ethnicGroups$FullName=="Bosnia-Herzegovina"] <- "Bosnia and Herzegovina"
ethnicGroups$FullName[ethnicGroups$FullName=="Cape Verde"] <- "Cabo Verde"
ethnicGroups$FullName[ethnicGroups$FullName=="Congo"] <- "Congo (Brazzaville)"
ethnicGroups$FullName[ethnicGroups$FullName=="Democratic Republic of Congo"] <- "Congo (Democratic Republic)"
ethnicGroups$FullName[ethnicGroups$FullName=="Cote d'Ivoire"] <- "Côte d'Ivoire"
ethnicGroups$FullName[ethnicGroups$FullName=="Dominican Republic"] <- "Dominica"
ethnicGroups$FullName[ethnicGroups$FullName=="Swaziland"] <- "eSwatini (Swaziland)"
ethnicGroups$FullName[ethnicGroups$FullName=="German Federal Republic"] <- "Germany"
ethnicGroups$FullName[ethnicGroups$FullName=="Democratic People's Republic of Korea"] <- "South Korea"
ethnicGroups$FullName[ethnicGroups$FullName=="Republic of Korea"] <- "North Korea"
ethnicGroups$FullName[ethnicGroups$FullName=="Macedonia"] <- "North Macedonia"
# ethnicGroups$FullName[ethnicGroups$FullName=="Sudan"] <- "South Sudan"
ethnicGroups$FullName[ethnicGroups$FullName=="East Timor"] <- "Timor-Leste"
ethnicGroups$FullName[ethnicGroups$FullName=="United States of America"] <- "United States"
ethnicGroups$FullName[ethnicGroups$FullName=="Democratic Republic of Vietnam"] <- "Vietnam"
ethnicGroups$FullName[ethnicGroups$FullName=="Yemen Arab Republic"] <- "Yemen"

merge_all$FullName[which(merge_all$FullName %ni% unique(ethnicGroups$FullName))]

merge_all$EFindex <- NA

for(i in 1:length(merge_all$FullName)){
  curName <- merge_all$FullName[i]
  if(curName %in% ethnicGroups$FullName){
    cursub <- subset(ethnicGroups, ethnicGroups$FullName==curName)
    merge_all$EFindex[i] <- as.numeric(cursub$EFindex[which(cursub$Year == max(cursub$Year,na.rm=T))])
    print(max(cursub$Year,na.rm=T))
  }else{
    merge_all$EFindex[i] <- NA
  }
}

###########################################################################################################################################
# Ethnicity Structure
# https://www.cia.gov/library/publications/resources/the-world-factbook/fields/400.html
# ethnicGroups <- read_excel(xlFile, sheet = "EthnicityStructure", col_names = F)
# colnames(ethnicGroups) <- c("FullName","EthnicityString","Note")
# 
# merge_all$FullName[which(merge_all$FullName %ni% unique(ethnicGroups$FullName))]
# 
# ethnicGroups$FullName[ethnicGroups$FullName=="Bahamas, The"] <- "Bahamas"
# ethnicGroups$FullName[ethnicGroups$FullName=="Congo, Republic of the"] <- "Congo (Brazzaville)"
# ethnicGroups$FullName[ethnicGroups$FullName=="Congo, Democratic Republic of the"] <- "Congo (Democratic Republic)"
# ethnicGroups$FullName[ethnicGroups$FullName=="Cote d'Ivoire"] <- "Côte d'Ivoire"
# ethnicGroups$FullName[ethnicGroups$FullName=="Czechia"] <- "Czech Republic"
# ethnicGroups$FullName[ethnicGroups$FullName=="Eswatini"] <- "eSwatini (Swaziland)"
# ethnicGroups$FullName[ethnicGroups$FullName=="Gambia, The"] <- "Gambia"
# ethnicGroups$FullName[ethnicGroups$FullName=="Kyrgyzstan"] <- "Kyrgyz Republic"
# ethnicGroups$FullName[ethnicGroups$FullName=="Micronesia, Federated States of"] <- "Micronesia"
# ethnicGroups$FullName[ethnicGroups$FullName=="Burma"] <- "Myanmar"
# ethnicGroups$FullName[ethnicGroups$FullName=="Korea, North"] <- "North Korea"
# ethnicGroups$FullName[ethnicGroups$FullName=="Macedonia"] <- "North Macedonia"
# ethnicGroups$FullName[ethnicGroups$FullName=="Sao Tome and Principe"] <- "São Tomé and Príncipe"
# ethnicGroups$FullName[ethnicGroups$FullName=="Korea, South"] <- "South Korea"
# ethnicGroups$FullName[ethnicGroups$FullName=="Saint Kitts and Nevis"] <- "St Kitts and Nevis"
# ethnicGroups$FullName[ethnicGroups$FullName=="Saint Lucia"] <- "St Lucia"
# ethnicGroups$FullName[ethnicGroups$FullName=="Saint Vincent and the Grenadines"] <- "St Vincent and The Grenadines"
# 
# merge_all$FullName[which(merge_all$FullName %ni% unique(ethnicGroups$FullName))]
###########################################################################################################################################
# Tropical Yes/No indicator
AverageTemp <- read_excel(xlFile, sheet = "AverageTemp", col_names = T)
colnames(AverageTemp) <- c("FullName","AverageTemp")

merge_all$FullName[which(merge_all$FullName %ni% unique(AverageTemp$FullName))]
AverageTemp$FullName[which(unique(AverageTemp$FullName) %ni% merge_all$FullName)]

AverageTemp$FullName[AverageTemp$FullName=="Republic of the Congo"] <- "Congo (Brazzaville)"
AverageTemp$FullName[AverageTemp$FullName=="Democratic Republic of the Congo"] <- "Congo (Democratic Republic)"
AverageTemp$FullName[AverageTemp$FullName=="Saint Kitts and Nevis"] <- "St Kitts and Nevis"
AverageTemp$FullName[AverageTemp$FullName=="Ivory Coast"] <- "Côte d'Ivoire"
AverageTemp$FullName[AverageTemp$FullName=="Cape Verde"] <- "Cabo Verde"
AverageTemp$FullName[AverageTemp$FullName=="Saint Lucia"] <- "St Lucia"
AverageTemp$FullName[AverageTemp$FullName=="Eswatini"] <- "eSwatini (Swaziland)"
AverageTemp$FullName[AverageTemp$FullName=="Kyrgyzstan"] <- "Kyrgyz Republic"
AverageTemp$FullName[AverageTemp$FullName=="Federated States of Micronesia"] <- "Micronesia"
AverageTemp$FullName[AverageTemp$FullName=="Saint Vincent and the Grenadines"] <- "St Vincent and The Grenadines"

merge_all$FullName[which(merge_all$FullName %ni% unique(AverageTemp$FullName))]
AverageTemp$FullName[which(unique(AverageTemp$FullName) %ni% merge_all$FullName)]

merge_all <- merge(merge_all, AverageTemp, by.x="FullName", by.y="FullName", all.x=T, all.y=F)
###########################################################################################################################################
# Tropical Yes/No indicator
tropicalIndicator <- read_excel(xlFile, sheet = "TropicalCountries", col_names = T)
colnames(tropicalIndicator) <- c("FullName")

merge_all$FullName[which(merge_all$FullName %ni% unique(tropicalIndicator$FullName))]
tropicalIndicator$FullName[which(unique(tropicalIndicator$FullName) %ni% merge_all$FullName)]

tropicalIndicator$FullName[tropicalIndicator$FullName=="Burma"] <- "Myanmar"
tropicalIndicator$FullName[tropicalIndicator$FullName=="Congo"] <- "Congo (Brazzaville)"
tropicalIndicator$FullName[tropicalIndicator$FullName=="Democratic Republic of Congo"] <- "Congo (Democratic Republic)"
tropicalIndicator$FullName[tropicalIndicator$FullName=="East Timor"] <- "Timor-Leste"
tropicalIndicator$FullName[tropicalIndicator$FullName=="Ivory Coast"] <- "Côte d'Ivoire"
tropicalIndicator$FullName[tropicalIndicator$FullName=="Saint Kitts and Nevis"] <- "St Kitts and Nevis"
tropicalIndicator$FullName[tropicalIndicator$FullName=="Saint Vincent and the Grenadines"] <- "St Vincent and The Grenadines"
tropicalIndicator$FullName[tropicalIndicator$FullName=="Sao Tome and Principe"] <- "São Tomé and Príncipe"
tropicalIndicator$FullName[tropicalIndicator$FullName=="Saint Lucia"] <- "St Lucia"

merge_all$FullName[which(merge_all$FullName %ni% unique(tropicalIndicator$FullName))]
tropicalIndicator$FullName[which(unique(tropicalIndicator$FullName) %ni% merge_all$FullName)]

merge_all$Tropical <- 0
merge_all$Tropical[merge_all$FullName %in% tropicalIndicator$FullName] <- 1
summary(as.factor(merge_all$Tropical))

###########################################################################################################################################
# Latitude
library(measurements)
Latitude <- read_excel(xlFile, sheet = "Latitude", col_names = T)
colnames(Latitude) <- c("FullName","Capital","Lat","Long")

Latitude$Lat <- as.character(Latitude$Lat)
Latitude$Long <- as.character(Latitude$Long)
# change the degree symbol to a space
# Latitude$Lat = gsub('°', ' ', Latitude$Lat)
Latitude$Lat = gsub("'", ' ', Latitude$Lat)
Latitude$Lat[grep("S",Latitude$Lat)] <- paste0("-",Latitude$Lat[grep("S",Latitude$Lat)])
Latitude$Lat = gsub(" S", ' ', Latitude$Lat)
Latitude$Lat = gsub(" N", ' ', Latitude$Lat)
# Latitude$Long = gsub('°', ' ', Latitude$Long)
Latitude$Long = gsub("'", ' ', Latitude$Long)
Latitude$Long[grep("W",Latitude$Long)] <- paste0("-",Latitude$Long[grep("W",Latitude$Long)])
Latitude$Long = gsub(" E", ' ', Latitude$Long)
Latitude$Long = gsub(" W", ' ', Latitude$Long)

# strsplit(Latitude$Lat[1],"")
# Latitude$Lat = gsub(' ', '', Latitude$Lat)
# Latitude$Lat = gsub('?', ' ', Latitude$Lat)
# strsplit(Latitude$Lat[1],"")
# strsplit(Latitude$Long[1],"")
# Latitude$Long = gsub(' ', '', Latitude$Long)
# Latitude$Long = gsub('?', ' ', Latitude$Long)
# strsplit(Latitude$Long[1],"")
# head(Latitude$Long)
# head(Latitude$Long)

# convert from decimal minutes to decimal degrees
Latitude$Lat = measurements::conv_unit(Latitude$Lat, from = 'deg_dec_min', to = 'dec_deg')
Latitude$Long = measurements::conv_unit(Latitude$Long, from = 'deg_dec_min', to = 'dec_deg')

Latitude$Lat
Latitude$Long

Latitude$FullName[which(unique(Latitude$FullName) %ni% merge_all$FullName)]
merge_all$FullName[which(merge_all$FullName %ni% unique(Latitude$FullName))]

Latitude$FullName[Latitude$FullName=="Brunei Darussalam"] <- "Brunei"
Latitude$FullName[Latitude$FullName=="Cape Verde"] <- "Cabo Verde"
Latitude$FullName[Latitude$FullName=="Congo, Democratic Republic of the"] <- "Congo (Democratic Republic)"
Latitude$FullName[Latitude$FullName=="Congo"] <- "Congo (Brazzaville)"
Latitude$FullName[Latitude$FullName=="Democratic Republic of the Congo"] <- "Congo (Democratic Republic)"
Latitude$FullName[Latitude$FullName=="Swaziland"] <- "eSwatini (Swaziland)"
Latitude$FullName[Latitude$FullName=="Iran (Islamic Republic of)"] <- "Iran"
Latitude$FullName[Latitude$FullName=="Kyrgyzstan"] <- "Kyrgyz Republic"
Latitude$FullName[Latitude$FullName=="Lao People's Democratic Republic"] <- "Laos"
Latitude$FullName[Latitude$FullName=="Libyan Arab Jamahiriya"] <- "Libya"
Latitude$FullName[Latitude$FullName=="Micronesia (Federated States of)"] <- "Micronesia"
Latitude$FullName[Latitude$FullName=="Moldova, Republic of"] <- "Moldova"
Latitude$FullName[Latitude$FullName=="Sao Tome and Principe"] <- "São Tomé and Príncipe"
Latitude$FullName[Latitude$FullName=="Russian Federation"] <- "Russia"
Latitude$FullName[Latitude$FullName=="Republic of Korea"] <- "South Korea"
Latitude$FullName[Latitude$FullName=="United States of America"] <- "United States"
Latitude$FullName[Latitude$FullName=="Republic of Korea"] <- "South Korea"
Latitude$FullName[Latitude$FullName=="Cote d'Ivoire"] <- "Côte d'Ivoire"
Latitude$FullName[Latitude$FullName=="United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
Latitude$FullName[Latitude$FullName=="Viet Nam"] <- "Vietnam"
Latitude$FullName[Latitude$FullName=="Saint Kitts and Nevis"] <- "St Kitts and Nevis"
Latitude$FullName[Latitude$FullName=="Saint vincent and the Grenadines"] <- "St Vincent and The Grenadines"
Latitude$FullName[Latitude$FullName=="Saint Lucia"] <- "St Lucia"
Latitude$FullName[Latitude$FullName=="East Timor"] <- "Timor-Leste"
Latitude$FullName[Latitude$FullName=="Dominica Republic"] <- "Dominican Republic"
Latitude$FullName[Latitude$FullName=="Rawanda"] <- "Rwanda"
Latitude$FullName[Latitude$FullName=="Syrian Arab Republic"] <- "Syria"

Latitude$FullName[which(unique(Latitude$FullName) %ni% merge_all$FullName)]
merge_all$FullName[which(merge_all$FullName %ni% unique(Latitude$FullName))]

colnames(Latitude) <- c("FullName","Capital","Latitude","Longitude")
Latitude$Capital <- NULL
merge_all <- merge(merge_all, Latitude, by.x="FullName", by.y="FullName", all.x=T, all.y=F)

# =======
if(HubeiFlag == T){
  merge_all$FullName <- as.character(merge_all$FullName)
  merge_all$FullName[merge_all$FullName=="China"] <- "Hubei"
  merge_all$ISO3 <- as.character(merge_all$ISO3)
  merge_all$ISO3[merge_all$ISO3=="CHN"] <- "HUB"
}

# write_csv(merge_all, './InputData/data_static_vars.csv')














statesAbbrev <- c("USA_AK","USA_AL","USA_AR","USA_AS","USA_AZ","USA_CA","USA_CO","USA_CT","USA_DC","USA_DE","USA_FL","USA_GA","USA_GU","USA_HI","USA_IA","USA_ID","USA_IL","USA_IN","USA_KS","USA_KY","USA_LA","USA_MA","USA_MD","USA_ME","USA_MI","USA_MN","USA_MO","USA_MS","USA_MT","USA_NC","USA_ND","USA_NE","USA_NH","USA_NJ","USA_NM","USA_NV","USA_NY","USA_OH","USA_OK","USA_OR","USA_PA","USA_PR","USA_RI","USA_SC","USA_SD","USA_TN","USA_TX","USA_UT","USA_VA","USA_VI","USA_VT","USA_WA","USA_WI","USA_WV","USA_WY")
correspFips <- c("2","1","5","60","4","6","8","9","11","10","12","13","66","15","19","16","17","18","20","21","22","25","24","23","26","27","29","28","30","37","38","31","33","34","35","32","36","39","40","41","42","72","44","45","46","47","48","49","51","78","50","53","55","54","56")
correspName <- c("Alaska","Alabama","Arkansas","American Samoa","Arizona","California","Colorado","Connecticut","District of Columbia","Delaware","Florida","Georgia","Guam","Hawaii","Iowa","Idaho","Illinois","Indiana","Kansas","Kentucky","Louisiana","Massachusetts","Maryland","Maine","Michigan","Minnesota","Missouri","Mississippi","Montana","North Carolina","North Dakota","Nebraska","New Hampshire","New Jersey","New Mexico","Nevada","New York","Ohio","Oklahoma","Oregon","Pennsylvania","Puerto Rico","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Virginia","Virgin Islands","Vermont","Washington","Wisconsin","West Virginia","Wyoming")
# correspName <- paste0("USA ", c("Alaska","Alabama","Arkansas","American Samoa","Arizona","California","Colorado","Connecticut","District of Columbia","Delaware","Florida","Georgia","Guam","Hawaii","Iowa","Idaho","Illinois","Indiana","Kansas","Kentucky","Louisiana","Massachusetts","Maryland","Maine","Michigan","Minnesota","Missouri","Mississippi","Montana","North Carolina","North Dakota","Nebraska","New Hampshire","New Jersey","New Mexico","Nevada","New York","Ohio","Oklahoma","Oregon","Pennsylvania","Puerto Rico","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Virginia","Virgin Islands","Vermont","Washington","Wisconsin","West Virginia","Wyoming"))
stateIDs <- as.data.frame(cbind(statesAbbrev,correspFips,correspName))
colnames(stateIDs) <- c("statesAbbrev","statesFips","statesName")
states_start <- stateIDs[,c("statesName", "statesAbbrev")]
colnames(states_start) <- c("FullName", "ISO3")

states_start$GHS_Prevent <- merge_all$GHS_Prevent[which(merge_all$ISO3 == "USA")]
states_start$GHS_Detect <- merge_all$GHS_Detect[which(merge_all$ISO3 == "USA")]
states_start$GHS_Respond <- merge_all$GHS_Respond[which(merge_all$ISO3 == "USA")]
states_start$GHS_Health <- merge_all$GHS_Health[which(merge_all$ISO3 == "USA")]
states_start$GHS_Norms <- merge_all$GHS_Norms[which(merge_all$ISO3 == "USA")]
states_start$GHS_Risk <- merge_all$GHS_Risk[which(merge_all$ISO3 == "USA")]


# https://www.bea.gov/system/files/2020-04/qgdpstate0420.pdf
currentVariable <- read_excel("./InputData/Static_demographics_USA.xlsx", sheet = "GDP", col_names = T)
colnames(currentVariable)[3] <- c("GDP_bill")
colnames(currentVariable)[5] <- c("GDP_percapita")
currentVariable <- currentVariable[,c("State", "GDP_bill", "GDP_percapita")]
states_start <- merge(states_start, currentVariable, by.x = "FullName", by.y = "State", all.x = T, all.y = F)

# https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html
currentVariable <- read_excel("./InputData/Static_demographics_USA.xlsx", sheet = "population", col_names = T)
currentVariable <- currentVariable[,c("State", "Population_mill")]
states_start <- merge(states_start, currentVariable, by.x = "FullName", by.y = "State", all.x = T, all.y = F)

states_start$HumanDevelopmentIndex_2018 <- merge_all$HumanDevelopmentIndex_2018[which(merge_all$ISO3 == "USA")]
states_start$EIUDemocracyIndexScore_2019 <- merge_all$EIUDemocracyIndexScore_2019[which(merge_all$ISO3 == "USA")]
states_start$UNOnlineServicesIndexScore_2018 <- merge_all$UNOnlineServicesIndexScore_2018[which(merge_all$ISO3 == "USA")]
states_start$GlobalPeaceIndex <- merge_all$GlobalPeaceIndex[which(merge_all$ISO3 == "USA")]
states_start$CorruptionsPerceptionIndex_2018 <- merge_all$CorruptionsPerceptionIndex_2018[which(merge_all$ISO3 == "USA")]
states_start$HumanCapitalIndex_2017 <- merge_all$HumanCapitalIndex_2017[which(merge_all$ISO3 == "USA")]
states_start$SDGIndexScore_2018 <- merge_all$SDGIndexScore_2018[which(merge_all$ISO3 == "USA")]


states_start$age_0_14_Percent <- NA
states_start$age_15_24_Percent <- NA
states_start$age_25_54_Percent <- NA
states_start$age_55_64_Percent <- NA
states_start$age_65_plus_Percent <- NA
states_start$age_0_14_MaleCount <- NA
states_start$age_15_24_MaleCount <- NA
states_start$age_25_54_MaleCount <- NA
states_start$age_55_64_MaleCount <- NA
states_start$age_65_plus_MaleCount <- NA
states_start$age_0_14_FemaleCount <- NA
states_start$age_15_24_FemaleCount <- NA
states_start$age_25_54_FemaleCount <- NA
states_start$age_55_64_FemaleCount <- NA
states_start$age_65_plus_FemaleCount <- NA
for(i in 1:56){
# for(i in 1:1){ 
  tryCatch({
    twodig <- paste0(sprintf("%02d", i))
    currentVariable <- read_excel(paste0("./InputData/sc-est2019-syasex-",twodig,".xlsx"), col_names = F)
    currentVariable <- as.data.frame(currentVariable)
    colnames(currentVariable) <- currentVariable[1,]
    currentVariable <- currentVariable[3:nrow(currentVariable),]
    myState <- colnames(currentVariable)[1]
    currentVariable[[myState]] <- c(1:86) - 1
    colnames(currentVariable) <- c("myState", "Total Population", "Male",  "Female")
    # currentVariable <- as.character(currentVariable)
    currentVariable$`Total Population` <- as.numeric(currentVariable$`Total Population`)
    currentVariable$`Male` <- as.numeric(currentVariable$`Male`)
    currentVariable$`Female` <- as.numeric(currentVariable$`Female`)
    
    states_start$age_0_14_Percent[which(states_start$FullName == myState)] <- sum(currentVariable$`Total Population`[1:15])/sum(currentVariable$`Total Population`[1:86])
    states_start$age_15_24_Percent[which(states_start$FullName == myState)] <- sum(currentVariable$`Total Population`[16:25])/sum(currentVariable$`Total Population`[1:86])
    states_start$age_25_54_Percent[which(states_start$FullName == myState)] <- sum(currentVariable$`Total Population`[26:55])/sum(currentVariable$`Total Population`[1:86])
    states_start$age_55_64_Percent[which(states_start$FullName == myState)] <- sum(currentVariable$`Total Population`[56:65])/sum(currentVariable$`Total Population`[1:86])
    states_start$age_65_plus_Percent[which(states_start$FullName == myState)] <- sum(currentVariable$`Total Population`[65:86])/sum(currentVariable$`Total Population`[1:86])
    
    states_start$age_0_14_MaleCount[which(states_start$FullName == myState)] <- sum(currentVariable$`Male`[1:15])
    states_start$age_15_24_MaleCount[which(states_start$FullName == myState)] <- sum(currentVariable$`Male`[16:25])
    states_start$age_25_54_MaleCount[which(states_start$FullName == myState)] <- sum(currentVariable$`Male`[26:55])
    states_start$age_55_64_MaleCount[which(states_start$FullName == myState)] <- sum(currentVariable$`Male`[56:65])
    states_start$age_65_plus_MaleCount[which(states_start$FullName == myState)] <- sum(currentVariable$`Male`[65:86])
    
    states_start$age_0_14_FemaleCount[which(states_start$FullName == myState)] <- sum(currentVariable$`Female`[1:15])
    states_start$age_15_24_FemaleCount[which(states_start$FullName == myState)] <- sum(currentVariable$`Female`[16:25])
    states_start$age_25_54_FemaleCount[which(states_start$FullName == myState)] <- sum(currentVariable$`Female`[26:55])
    states_start$age_55_64_FemaleCount[which(states_start$FullName == myState)] <- sum(currentVariable$`Female`[56:65])
    states_start$age_65_plus_FemaleCount[which(states_start$FullName == myState)] <- sum(currentVariable$`Female`[65:86])
  },error=function(e){
  },finally={})
}

states_start$FullName[is.na(states_start[,c(19:33)])]

states_start$PopulationGrowthRate <- merge_all$PopulationGrowthRate[which(merge_all$ISO3 == "USA")]
states_start$PopulationSmoking_male <- merge_all$PopulationSmoking_male[which(merge_all$ISO3 == "USA")]
states_start$PopulationSmoking_female <- merge_all$PopulationSmoking_female[which(merge_all$ISO3 == "USA")]
states_start$GINIindex <- merge_all$GINIindex[which(merge_all$ISO3 == "USA")]

merge_all$PercentUrban[which(merge_all$ISO3 == "USA")]
currentVariable <- read_excel("./InputData/Static_demographics_USA.xlsx", sheet = "PercUrban", col_names = T)
currentVariable <- currentVariable[,c("State", "2010_perc_Urban")]
colnames(currentVariable) <- c("State","PercentUrban")
states_start <- merge(states_start, currentVariable, by.x = "ISO3", by.y = "State", all.x = T, all.y = F)
states_start$RateUrbanization <- merge_all$RateUrbanization[which(merge_all$ISO3 == "USA")]
states_start$PhysicianDensity <- merge_all$PhysicianDensity[which(merge_all$ISO3 == "USA")]

# https://web.archive.org/web/20111028061117/http://2010.census.gov/2010census/data/apportionment-dens-text.php
merge_all$PopulationDensity[which(merge_all$ISO3 == "USA")]
currentVariable <- read_excel("./InputData/Static_demographics_USA.xlsx", sheet = "popDensity", col_names = T)
# currentVariable <- currentVariable[,c("State", "2010_perc_Urban")]
colnames(currentVariable) <- c("State","PopulationDensity")
states_start <- merge(states_start, currentVariable, by.x = "FullName", by.y = "State", all.x = T, all.y = F)


states_start$Ave_household_size <- merge_all$Ave_household_size[which(merge_all$ISO3 == "USA")]
states_start$Percent_house_Nuclear <- merge_all$Percent_house_Nuclear[which(merge_all$ISO3 == "USA")]
states_start$Percent_house_Multi_generation <- merge_all$Percent_house_Multi_generation[which(merge_all$ISO3 == "USA")]
states_start$Percent_house_Three_generation <- merge_all$Percent_house_Three_generation[which(merge_all$ISO3 == "USA")]
states_start$Percent_house_Skip_generation <- merge_all$Percent_house_Skip_generation[which(merge_all$ISO3 == "USA")]
states_start$EFindex <- merge_all$EFindex[which(merge_all$ISO3 == "USA")]

# https://www.ncdc.noaa.gov/
merge_all$AverageTemp[which(merge_all$ISO3 == "USA")]
currentVariable <- read_excel("./InputData/Static_demographics_USA.xlsx", sheet = "AveTemp", col_names = T)
currentVariable <- currentVariable[,c("State", "Mean_C")]
colnames(currentVariable) <- c("State","AverageTemp")
states_start <- merge(states_start, currentVariable, by.x = "FullName", by.y = "State", all.x = T, all.y = F)

states_start$Tropical <- merge_all$Tropical[which(merge_all$ISO3 == "USA")]

# https://developers.google.com/public-data/docs/canonical/states_csv
merge_all$AverageTemp[which(merge_all$ISO3 == "USA")]
currentVariable <- read_excel("./InputData/Static_demographics_USA.xlsx", sheet = "LatAndLong", col_names = T)
currentVariable <- currentVariable[,c("State", "latitude","longitude")]
colnames(currentVariable) <- c("State","Latitude","Longitude")
states_start <- merge(states_start, currentVariable, by.x = "FullName", by.y = "State", all.x = T, all.y = F)

# cat(names(merge_all),sep="\n")

states_start$FullName <- paste0("USA ", states_start$FullName)

cbind(names(merge_all),names(states_start))

finallyDone <- rbind(merge_all,states_start)


write_csv(finallyDone, './InputData/data_static_vars.csv')