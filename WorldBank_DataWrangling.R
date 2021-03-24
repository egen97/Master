library(tidyverse)
library(countrycode)
#Cleane GDP og Gender fra World Bank

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


Population_Cleaned <- Population %>%
  select(Country, starts_with("x")) %>%
  filter(!is.na(Country)) %>%
  pivot_longer(starts_with("x"), names_to = "year", values_to = "Population") %>%
  mutate(year = str_remove(year, "X")) %>%
  filter(year != "") %>%
  mutate(year = trimws(year)) %>%
  mutate(year = as.numeric(year))


Population_Cleaned$Country <- countrycode(Population_Cleaned$Country, origin = "country.name", destination = "iso3n")





Corruption <- read.csv("Data/CPIACORUPTION.csv")
Corruption <- rename(Corruption, "Country" = "ï..Country.Name")

Gender <- read.csv("Data/CPIAGENDER.csv")
Gender <- rename(Gender, "Country" = "ï..Country.Name")

Law <- read.csv("Data/CPIALAW.csv")
Law <- rename(Law, "Country" = "ï..Country.Name")

SocProt <- read.csv("Data/CPIASOCIALPROTECTION.csv")
SocProt <- rename(SocProt, "Country" = "ï..Country.Name")

Gini <- read.csv("Data/gini.csv")
Gini <- rename(Gini, "Country" = "ï..Country.Name")




Corruption$Country <- ifelse(Corruption$Country %in% Non_Countries, NA, Corruption$Country)

Corruption$Country <- drop_na(Corruption$Country)

Corruption_Cleaned <- Corruption %>%
  select(Country, starts_with("x")) %>%
  filter(!is.na(Country)) %>%
  pivot_longer(starts_with("x"), names_to = "year", values_to = "Corruption") %>%
  mutate(year = str_remove(year, "X")) %>%
  filter(year != "") %>%
  mutate(year = trimws(year)) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year > 2004)


Corruption_Cleaned$Country <- countrycode(Corruption_Cleaned$Country, origin = "country.name", destination = "iso3n")



Gender$Country <- ifelse(Gender$Country %in% Non_Countries, NA, Gender$Country)


Gender_Cleaned <- Gender %>%
  select(Country, starts_with("x")) %>%
  filter(!is.na(Country)) %>%
  pivot_longer(starts_with("x"), names_to = "year", values_to = "Gender") %>%
  mutate(year = str_remove(year, "X")) %>%
  filter(year != "") %>%
  mutate(year = trimws(year)) %>%
  mutate(year = as.numeric(year))


Gender_Cleaned$Country <- countrycode(Gender_Cleaned$Country, origin = "country.name", destination = "iso3n")



Gini$Country <- ifelse(Gini$Country %in% Non_Countries, NA, Gini$Country)


Gini_Cleaned <- Gini %>%
  select(Country, starts_with("x")) %>%
  filter(!is.na(Country)) %>%
  pivot_longer(starts_with("x"), names_to = "year", values_to = "Gini") %>%
  mutate(year = str_remove(year, "X")) %>%
  filter(year != "") %>%
  mutate(year = trimws(year)) %>%
  mutate(year = as.numeric(year))


Gini_Cleaned$Country <- countrycode(Gini_Cleaned$Country, origin = "country.name", destination = "iso3n")








Law$Country <- ifelse(Law$Country %in% Non_Countries, NA, Law$Country)


Law_Cleaned <- Law %>%
  select(Country, starts_with("x")) %>%
  filter(!is.na(Country)) %>%
  pivot_longer(starts_with("x"), names_to = "year", values_to = "Law") %>%
  mutate(year = str_remove(year, "X")) %>%
  filter(year != "") %>%
  mutate(year = trimws(year)) %>%
  mutate(year = as.numeric(year))


Law_Cleaned$Country <- countrycode(Law_Cleaned$Country, origin = "country.name", destination = "iso3n")




SocProt$Country <- ifelse(SocProt$Country %in% Non_Countries, NA, SocProt$Country)


SocProt_Cleaned <- SocProt %>%
  select(Country, starts_with("x")) %>%
  filter(!is.na(Country)) %>%
  pivot_longer(starts_with("x"), names_to = "year", values_to = "SocProt") %>%
  mutate(year = str_remove(year, "X")) %>%
  filter(year != "") %>%
  mutate(year = trimws(year)) %>%
  mutate(year = as.numeric(year))


SocProt_Cleaned$Country <- countrycode(SocProt_Cleaned$Country, origin = "country.name", destination = "iso3n")









WB_Data <- GDP_Cleaned %>%
  left_join(Population_Cleaned)

WB_Data <- WB_Data %>%
  left_join(Corruption_Cleaned)

WB_Data <- WB_Data %>%
  left_join(Gender_Cleaned)

WB_Data <- WB_Data %>%
  left_join(Gini_Cleaned)

WB_Data <- WB_Data %>%
  left_join(Law_Cleaned)

WB_Data <- WB_Data %>%
  left_join(SocProt_Cleaned)

HDI <- read.csv("Data/HDI_18.csv")

HDI <- select(HDI, -HDI.Rank..2017.)



HDI_Cleaned <- HDI %>%
  select(Country, starts_with("x")) %>%
  filter(!is.na(Country)) %>%
  pivot_longer(starts_with("x"), names_to = "year", values_to = "HDI") %>%
  mutate(year = str_remove(year, "X")) %>%
  filter(year != "") %>%
  mutate(year = trimws(year)) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(HDI = ifelse(HDI == "..", NA, HDI))


HDI_Cleaned$Country <- countrycode(HDI_Cleaned$Country, origin = "country.name", destination = "iso3n")

WB_Data <- WB_Data %>%
  left_join(HDI_Cleaned)





MoreDataIGuess <- read.csv("Data/WBSERIES.csv")

MoreDataIGuess <- rename(MoreDataIGuess, "Country" = "ï..Country.Name")

Clean <- MoreDataIGuess %>%
  select(Country, Series.Name, starts_with("x"))%>%
  pivot_longer(starts_with("x"), names_to = "year") 


Cleaner <- Clean %>%
  filter(Series.Name != "") %>%
  #distinct(Country, year, .keep_all = TRUE) %>%
  pivot_wider(id = c("Country", "year"), names_from = "Series.Name") %>%
  mutate(across(everything(), ~ifelse(.x == "..", NA, .x)))



Cleaner <- Cleaner %>%
  mutate(year = str_extract(year, "X\\d*"),
         year = str_remove(year, "X"))


Cleaner$year <- as.numeric(Cleaner$year)

Cleaner <- Cleaner %>%
  rename(
    "Fules_prcap" = "Access to clean fuels and technologies for cooking (% of population)",
    "Elec_prcap" = "Access to electricity (% of population)",
    "fuel_exprprgdp" = "Fuel exports (% of merchandise exports)",
    "expr_prgdp" = "Exports of goods and services (% of GDP)",
    "edu_year" = "Compulsory education, duration (years)",
    "edu_xpns" = "Current education expenditure, primary (% of total expenditure in primary public institutions)",
    "health_xpns" = "Current health expenditure (% of GDP)" 
    
  )

Cleaner$Country <- countrycode(Cleaner$Country, origin = "country.name", destination = "iso3n")
Cleaner <- Cleaner %>%
  filter(!is.na(Country))

WB_Data <- WB_Data %>%
  left_join(Cleaner)


WB_Data <- WB_Data %>%
  filter(year >= 1980)
  
saveRDS(WB_Data, "Data/WB_Data.rds")
