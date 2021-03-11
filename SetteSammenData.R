library(tidyverse)
library("Amelia")
AfroBarometer <- readRDS("AfroBarometer.rds")
LatinoBarometer <- readRDS("LatinoBarometer_Mean.rds")
WVS <- readRDS("WVS.rds")
Polity <- readRDS("Data/politySelected.rds")
Conflict <- readRDS("Data/ConflictData.rds")
WB <- readRDS("Data/WB_Data.rds")


SurveyData <- WVS %>%
  full_join(LatinoBarometer, by = c("Country", "year"))




AfroBarometer$Country <- AfroBarometer$country

SurveyData <- SurveyData %>%
  full_join(AfroBarometer, by = c("Country", "year"))

SurveyData <- SurveyData %>%
  left_join(Polity)

SurveyData <- SurveyData %>%
  left_join(Conflict, by = c("Country" = "CountryNum", "year")) %>%
  mutate(Conflict_Binary = ifelse(is.na(Conflict_Binary), 0, Conflict_Binary))



SurveyData <- SurveyData %>%
  select(-Country.y, -ccode, -Year)
  

SurveyData <- SurveyData %>%
  left_join(WB)

SurveyData <- SurveyData %>%
  select(-country, -country_Afro, -Country.y, -ccode)

SurveyData <- SurveyData %>%
  select(-KilSelfDef, -C005, -TradRule_Afro, -OthAdvanScale)


missmap(SurveyData)
saveRDS(SurveyData, "CompleteData.rds")






