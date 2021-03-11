library(tidyverse)
library("Amelia")
AfroBarometer <- readRDS("AfroBarometer.rds")
LatinoBarometer <- readRDS("LatinoBarometer.rds")
WVS <- readRDS("WVS.rds")
Polity <- readRDS("Data/politySelected.rds")
Conflict <- readRDS("Data/ConflictData.rds")
WB <- readRDS("Data/WB_Data.rds")

WVS$Country <- WVS$CcIso
WVS$year <- WVS$Year
LatinoBarometer$Country

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

#saveRDS(SurveyData, "CompleteData.rds")






