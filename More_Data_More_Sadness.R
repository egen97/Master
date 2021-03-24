library(tidyverse)
library(countrycode)

CIRI <- read.csv("Data/CIRI Data 1981_2011 2014.04.14.csv")
CIRI$Country <- countrycode(CIRI$CTRY, origin = "country.name", destination = "iso3n")

CIRI_Sel <- CIRI %>%
  select(Country,YEAR, ASSN, DISAP, DOMMOV, FORMOV, KILL, PHYSINT, POLPRIS, TORT) %>%
  filter(YEAR >= 1980) %>%
  filter(Country %in% WVSCountries) %>%
  mutate(across(everything(), ~ifelse(.x < 0, NA, .x)))

missmap(CIRI_Sel)

saveRDS(CIRI_Sel, "ciri.rds")


