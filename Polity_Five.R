library(tidyverse)
library(readxl)
library(countrycode)
#Laste inn polity V

Polity <- read_excel("Data/p5v2018.xls")

PolitySelected <- Polity %>%
  select(ccode, year, democ, polity) %>%
  filter(year > 1979) %>%
  mutate(across(everything(), ~ifelse(.x < -10, NA, .x)))

PolitySelected$Country <- countrycode(PolitySelected$ccode, origin = "cown", destination = "iso3n")

# Warning message:
#   In countrycode(PolitySelected$ccode, origin = "cown", destination = "iso3n") :
#   Some values were not matched unambiguously: 260, 265, 315, 342, 345, 347, 348, 364, 525, 529, 678, 680, 818

PolitySelected <- PolitySelected %>%
  filter(!is.na(Country))

saveRDS(PolitySelected, "Data/politySelected.rds")

CountPol <- PolitySelected %>%
  count(polity) %>%
  mutate(sum = cumsum(n)) 

