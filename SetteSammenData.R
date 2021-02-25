library(tidyverse)
library("Amelia")
AfroBarometer <- readRDS("AfroBarometer.rds")
LatinoBarometer <- readRDS("LatinoBarometer.rds")
WVS <- readRDS("WVS.rds")

WVS$Country <- WVS$CcIso
WVS$year <- WVS$Year
LatinoBarometer$Country


SurveyData <- WVS %>%
  full_join(LatinoBarometer, by = c("Country", "year"))




AfroBarometer$Country <- AfroBarometer$country

SurveyData <- SurveyData %>%
  full_join(AfroBarometer, by = c("Country", "year"))




Imp_Data <- amelia(SurveyData, m = 10, ts = "year", cs = "Country", polytime = 1, intercs = TRUE, incheck = TRUE)






