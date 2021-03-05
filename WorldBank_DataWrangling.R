library(tidyverse)
library(countrycode)
#Cleane GDP og Population fra World Bank

GDP <- read.csv("Data/gdpCAPITA.csv")
Population <- read.csv("Data/population.csv")

GDP <- rename(GDP, "Country" = "ï..Country.Name")
Population <- rename(Population, "Country" = "ï..Country.Name")


Non_Countries <- c( "Arab World", "Caribbean small states", 
                    "Central Europe and the Baltics", "Channel Islands", "Early-demographic dividend", "East Asia & Pacific",
                    "East Asia & Pacific (excluding high income)"," East Asia & Pacific (IDA & IBRD countries)", "Euro area", 
                    "Europe & Central Asia", "Europe & Central Asia (excluding high income)"," Europe & Central Asia (IDA & IBRD countries)",
                    "European Union", "Fragile and conflict affected situations", "Heavily indebted poor countries (HIPC)",
                    "High income", "IBRD only", "IDA & IBRD total", "IDA blend", "IDA only", "IDA total", "Kosovo", 
                    "Late-demographic dividend", "Latin America & Caribbean", "Latin America & Caribbean (excluding high income)",
                    "Latin America & the Caribbean (IDA & IBRD countries)", "Least developed countries: UN classification", "Low & middle income", "Low income",
                    "Lower middle income", "Middle East & North Africa", "Middle East & North Africa (excluding high income)",
                    "Middle East & North Africa (IDA & IBRD countries)", "Middle income", "North America",
                    "East Asia & Pacific (IDA & IBRD countries)", 
                    "Europe & Central Asia (IDA & IBRD countries)", "Not classified", "OECD members", 
                    "Other small states", "Pacific island small states", "Post-demographic dividend", 
                    "Pre-demographic dividend", "Small states", 
                    "South Asia", "South Asia (IDA & IBRD)", 
                    "Sub-Saharan Africa", "Sub-Saharan Africa (excluding high income)", 
                    "Sub-Saharan Africa (IDA & IBRD countries)", "Upper middle income", "World")


GDP$Country <- ifelse(GDP$Country %in% Non_Countries, NA, GDP$Country)

GDP$Country <- drop_na(GDP$Country)

GDP_Cleaned <- GDP %>%
  select(Country, starts_with("x")) %>%
  filter(!is.na(Country)) %>%
  pivot_longer(starts_with("x"), names_to = "year", values_to = "gdpPRcapita") %>%
  mutate(year = str_remove(year, "X")) %>%
  filter(year != "") %>%
  mutate(year = trimws(year)) %>%
  mutate(year = as.numeric(year))


GDP_Cleaned$Country <- countrycode(GDP_Cleaned$Country, origin = "country.name", destination = "iso3n")


### Population



Population$Country <- ifelse(Population$Country %in% Non_Countries, NA, Population$Country)

Population$Country <- drop_na(Population$Country)

Population_Cleaned <- Population %>%
  select(Country, starts_with("x")) %>%
  filter(!is.na(Country)) %>%
  pivot_longer(starts_with("x"), names_to = "year", values_to = "Population") %>%
  mutate(year = str_remove(year, "X")) %>%
  filter(year != "") %>%
  mutate(year = trimws(year)) %>%
  mutate(year = as.numeric(year))


Population_Cleaned$Country <- countrycode(Population_Cleaned$Country, origin = "country.name", destination = "iso3n")


WB_Data <- GDP_Cleaned %>%
  left_join(Population_Cleaned)

saveRDS(WB_Data, "Data/WB_Data.rds")
