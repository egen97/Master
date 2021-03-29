library(tidyverse)
devtools::install_github("vdeminstitute/vdemdata")

library(vdemdata)
WVS <- readRDS("WVS.rds")
Countries <- WVS$Country

VDEM <- vdem


VDEM$Country <- countrycode::countrycode(VDEM$country_name, origin = "country.name", destination = "iso3n")
VDEM <- VDEM %>%
  filter(!is.na(Country))



VDEM <- VDEM %>%
  filter(year >= 1980) 
VDEM <- VDEM %>%
  filter((Country %in% Countries))


VDEM <- VDEM %>%
  select(-starts_with("v3"))

demoData <- VDEM %>%
  select(Country, year, v2x_polyarchy, v2x_libdem, v2x_egaldem, e_total_fuel_income_pc, e_total_resources_income_pc,
         e_radio_n, e_miurbani, e_pefeliex, e_peinfmor, e_pelifeex, e_pematmor, v2xcl_acjst, v2x_corr,
         v2xpe_exlecon, v2xpe_exlgender, v2xpe_exlpol, v2xpe_exlsocgr, v2x_accountability,v2caprotac,
         v2cainsaut,v2caviol,v2mecenefm)



demoData <- demoData %>%
  filter(!is.na(Country)) %>%
  filter(!(Country %in% Countries))


saveRDS(demoData, "demoData.rds")
