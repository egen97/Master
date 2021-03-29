library(tidyverse)
library(democracyData)
library(countrycode)
FHI <- download_fh(verbose = FALSE, include_territories = F) 
WGI <- download_wgi_voice_and_accountability()
FHIelec <- download_fh_electoral()

CIRI <- readRDS("ciri.rds")

AfroBarometer <- readRDS("AfroBarometer.rds")
LatinoBarometer <- readRDS("LatinoBarometer_Mean.rds")
WVS <- readRDS("wvs_everything.rds")
Polity <- readRDS("Data/politySelected.rds")
Conflict <- readRDS("Data/ConflictData.rds")
WB <- readRDS("Data/WB_Data.rds")
MilDat <- readRDS("Data/mildat.rds")
Arab <- readRDS("arabMean.rds")
demoData <- readRDS("demoData.rds")
SurveyData <- WVS %>%
  full_join(LatinoBarometer, by = c("Country", "year"))




AfroBarometer$Country <- AfroBarometer$country
AfroBarometer$country <- NULL
AfroBarometer$country_Afro <- NULL
SurveyData <- SurveyData %>%
  full_join(AfroBarometer, by = c("Country", "year"))

SurveyData <- SurveyData %>%
  full_join(Arab, by = c("Country", "year"))

SurveyData <- SurveyData %>%
  full_join(Polity)

SurveyData <- SurveyData %>%
  full_join(Conflict, by = c("Country" = "CountryNum", "year")) %>%
  mutate(Conflict_Binary = ifelse(is.na(Conflict_Binary), 0, Conflict_Binary))



SurveyData <- SurveyData %>%
  select(-Country.y, -ccode, -Year)
  
WB <- WB %>%
  filter(year >= 1980)
SurveyData <- SurveyData %>%
  full_join(WB)

SurveyData <- SurveyData %>%
  select(-country, -country_Afro, -Country.y, -ccode, -n)


SurveyData <- SurveyData %>%
  full_join(MilDat, by = c("Country", "year"))




FHI <- FHI %>%
  select(-GWn, -cown, -in_GW_system)

FHI$Country <- countrycode(FHI$fh_country, origin = "country.name", destination = "iso3n")
FHI <- FHI %>%
  filter(!is.na(Country)) %>%
  select(-fh_country, -extended_country_name)

SurveyData <- SurveyData %>%
  full_join(FHI)



WGI <- WGI %>%
  select(-StdErr, wb_code, -NumSrc, -Lower, -Upper, -'2019', X26, -GWn, -cown, -in_GW_system, -Rank, -X26)

WGI$Country <- countrycode(WGI$wb_country, origin = "country.name", destination = "iso3n")

WGI <- WGI %>%
  select(Country, year, Estimate) %>%
  filter(!is.na(Country))

SurveyData <- SurveyData %>%
  full_join(WGI)

FHIelec <- FHIelec %>%
  select(fh_electoral_country, year, electoral)

FHIelec$Country <- countrycode(FHIelec$fh_electoral_country, origin = "country.name", destination = "iso3n")

FHIelec <- FHIelec %>%
  filter(!is.na(Country)) %>%
  select(-fh_electoral_country)

SurveyData <- SurveyData %>%
  full_join(FHIelec, by = c("Country", "year"))

SurveyData <- SurveyData %>%
  full_join(CIRI, by = c("Country", "year"))

SurveyData <- SurveyData %>%
  filter(year >= 1980)

# 
#missmap(SurveyData)
#saveRDS(SurveyData, "CompleteData.rds")
WVSCountries <- unique(WVS$Country)

SurveyData <- SurveyData %>%
  distinct(Country, year, .keep_all = TRUE) %>%
  filter(Country %in% WVSCountries) %>%
  filter(year >= 1980)
SurveyData <- SurveyData %>%
  select(-D022, -C002)
# 
 
SurveyData$majorpower <- ifelse(!is.na(SurveyData$majorpower), 0, SurveyData$majorpower)
table(is.na(SurveyData$Conflict_Binary))
SurveyData$Conflict_Binary <- ifelse(is.na(SurveyData$Conflict_Binary), 0, SurveyData$Conflict_Binary)

SurveyData <- SurveyData %>%
  select(-ccode.y, -ccode.x, -Country.y)


SurveyData <- SurveyData %>%
  left_join(demoData)



missmap(SurveyData)


# 
saveRDS(SurveyData, "CompleteData.rds")
# 
