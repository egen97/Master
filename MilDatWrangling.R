library(tidyverse)
library(countrycode)

MilDat <- read.csv("Data/Country_Stats.csv")

MilDat$majorpower <- ifelse(is.na(MilDat$majorpower), 0, MilDat$majorpower)

MilDat$Country <- countrycode(MilDat$ccode, origin = "cown", "iso3n")



MilDat <- filter(MilDat, !is.na(MilDat$Country))
MilDat$ccode <- NULL

MilDat <- MilDat %>%
  mutate(across(everything(), ~ifelse(.x < 0, NA, .x)))



saveRDS(MilDat, "mildat.rds")
