library(tidyverse)
library(haven)
library(countrycode)
Arab1 <- read_sav("Data/ArabBarometer/ABI_English.sav")
Arab1Cont <- as.character(as_factor(Arab1$country, levels = "labels"))
Arab1 <- as_factor(Arab1, levels = "values")
Arab1$country <- Arab1Cont

Arab2 <- read_sav("Data/ArabBarometer/ABII_English.sav")
Arab2Cont <- as.character(as_factor(Arab2$country, levels = "labels"))
Arab2Cont <- str_remove(Arab2Cont, "\\d")
Arab2Cont <- str_remove(Arab2Cont, "[[:punct:]]")

Arab2Cont <- trimws(Arab2Cont)

Arab2 <- as_factor(Arab2, levels = "values")
Arab2$country <- Arab2Cont


Arab3 <- read_sav("Data/ArabBarometer/ABIII_English.sav")
Arab3Cont <- as.character(as_factor(Arab3$country, levels = "labels"))
Arab3 <- as_factor(Arab3, levels = "values")
Arab3$country <- Arab3Cont



Arab4 <- read_sav("Data/ArabBarometer/ABIV_English.sav")
Arab4Cont <- as.character(as_factor(Arab4$country, levels = "labels"))
Arab4 <- as_factor(Arab4, levels = "values")
Arab4$country <- Arab4Cont


Arab5 <- read_sav("Data/ArabBarometer/ABV_Release_Data.sav")
Arab5Cont <- as.character(as_factor(Arab5$country, levels = "labels"))
Arab5 <- as_factor(Arab5, levels = "values")
Arab5$country <- Arab5Cont







Arab1Clean <- Arab1 %>%
  select(country,q2014, q204,q214, q221, q228,
         q234, q2452, q2473, q2474, q2476, q2561,
         q5044, q5045, q5054, q5053, q604, q605) %>%
  rename(
    "trstPoliceArab" = "q2014",
    "trstOtherArab" = "q204", 
    "polSituArab" = "q214",
    "loyGovernArab" = "q221",
    "dispCourtArab" = "q228",
    "humrighSafetyArab" = "q234",
    "strongLeaderArab" = "q2452",
    "impHealthArab" = "q2474",
    "nargapRichPorArab" = "q2473",
    "dealMoralArab" = "q2476",
    "tribeVoteArab" = "q2561",
    "islamGenderUniArab" = "q5044",
    "menPolLeaderArab" = "q5053",
    "armedAntiUsAbarb" = "q604",
    "attackCivArab" = "q605"
    
    
  ) %>%
  mutate(year = 2006)


Arab2Clean <- Arab2 %>%
  select(country, q2014, q103,q216, q215,
         q523, q5183, q2044, q3041,q60104,q60103,
         q706) %>%
  mutate(year = 2010) %>%
  rename(
    "trstPoliceArab" = "q2014",
    "trstOtherArab" = "q103",
    "loyGovernArab" = "q216",
    "dispCourtArab" = "q215",
    "humrighSafetyArab" = "q523",
    "strongLeaderArab" = "q5183",
    "impHealthArab" = "q2044",
    "tribeVoteArab" = "q3041",
    "islamGenderUniArab" = "q60104",
    "menPolLeaderArab" = "q60103",
    "armedAntiUsAbarb" = "q706"
  )



Arab3Clean <- Arab3 %>%
  select(country, q2014, q103, q216, q523,q5183, q2044,
         q6014,q6013,q706) %>%
  rename(
    "trstPoliceArab" = "q2014",
    "trstOtherArab" = "q103",
    "loyGovernArab" = "q216",
    "humrighSafetyArab" = "q523",
    "strongLeaderArab" = "q5183",
    "impHealthArab" = "q2044",
    "islamGenderUniArab" = "q6014",
    "menPolLeaderArab" = "q6013",
    "armedAntiUsAbarb" = "q706"
  ) %>%
  mutate(year = 2013)



Arab4Clean <- Arab4 %>%select(country, q2014, q103, q216, q523,q5183, q2044,
                              q6014,q6013) %>%
  rename(
    "trstPoliceArab" = "q2014",
    "trstOtherArab" = "q103",
    "loyGovernArab" = "q216",
    "humrighSafetyArab" = "q523",
    "strongLeaderArab" = "q5183",
    "impHealthArab" = "q2044",
    "islamGenderUniArab" = "q6014",
    "menPolLeaderArab" = "q6013"
  ) %>%
  mutate(year = 2016)




Arab5Clean <- Arab5 %>%
  select(Q201A_42, Q103, Q216, Q601_4, Q601_3) %>%
  rename(
    "trstPoliceArab" = "Q201A_42",
    "trstOtherArab" = "Q103",
    "loyGovernArab" = "Q216",
    "islamGenderUniArab" = "Q601_4",
    "menPolLeaderArab" = "Q601_3"
  ) %>%
  mutate(year = 2018)






ArabBarometer <- list(Arab1Clean, Arab2Clean, Arab3Clean, Arab4Clean, Arab5Clean) %>%
  reduce(bind_rows) %>%
  mutate(across(trstPoliceArab:attackCivArab,  ~as.numeric(as.character(.x)))) %>%
  mutate(across(trstPoliceArab:attackCivArab, ~ifelse(.x > 10, NA, .x)))


ArabBarometer$Country <- countrycode(ArabBarometer$country,origin = "country.name", destination = "iso3n")
ArabBarometer$country <- NULL

saveRDS(ArabBarometer, "arabclean.rds")

Arab_Mean <- ArabBarometer %>%
  group_by(Country, year) %>%
  summarise(across(everything(), ~mean(.x,na.rm = TRUE))) %>%
  mutate(across(everything(), ~ifelse(is.nan(.x), NA, .x))) %>%
  ungroup() 

saveRDS(Arab_Mean, "arabMean.rds")
