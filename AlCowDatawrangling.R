library(tidyverse)
library(Amelia)
library(countrycode)
AlCowDat <- read.csv("Data/COWandAlian.csv")
missmap(AlCowDat)

#Mye koder her som strengt tatt bør kodes om kraftig, og forstås.
#Veldig mange har like mange NA?

AlCowDat %>%
  mutate(Country = countrycode(ccode, origin = "cown", destination = "country.name")) %>%
  filter(!is.na(area_WDI)) %>%
  count(Country)



AlCowDat %>%
  mutate(Country = countrycode(ccode, origin = "cown", destination = "country.name")) %>%
  filter(is.na(area_WDI)) %>%
  count(Country)

#Hente ut WVS land, og filtere dem mut. Se om å fjerne Afghanistan og Nord corea and such forandrer på det

WVS <- readRDS("wvs_everything.rds")

WVSCOuntries <- WVS$Country

AlCowDat$Country <- countrycode(AlCowDat$ccode, origin = "cown", destination = "iso3n")

AlCowDat <- AlCowDat %>%
  filter(Country %in% WVSCOuntries)

missmap(AlCowDat)


#Ok, IPE dataene er ubrukelige, apperantly er det ingen executions i USA? Verken registrert på statlig eller føderalt
#nivå, soo.. Alle variabler har like mange missing, uten noen info om hvorfor.
#Må nesten se om COW har noe bedre? D



grenser <- read.csv("Data/contdirs.csv")


grenser <- grenser %>%
  filter(year > 1980) %>%
  mutate(Country = countrycode(stateno, origin = "cown", destination = "iso3n")) %>%
  filter(Country %in% WVSCOuntries)

Useable <- AlCowDat %>%
  select(Country, year, num_mem)


grenser <- grenser %>%
  select(Country, year, total, land, sea)


NMFC <- read.csv("Data/NMC_5_0.csv")

NMFC <- NMFC %>%
  filter(year > 1980) %>%
  mutate(Country = countrycode(ccode, origin = "cown", destination = "iso3n")) %>%
  filter(Country %in% WVSCOuntries)


NMFC <- NMFC %>%
  select(cinc, Country, year)


MilCAP <- NMFC %>%
  left_join(Useable, by = c("Country", "year")) %>%
  left_join(grenser, by = c("Country", "year"))

saveRDS(MilCAP, "MilCap.rds")
