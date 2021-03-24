#Pakker
library(tidyverse)
library(haven)
#Data, World Value Survey timeseries 1981-2020
WVS <- readRDS("Data/WVS_TimeSeries_R_v1_6.rds")
EVS <- read_spss("Data/ESV.sav")

WVS <- as_factor(WVS, levels = "values")
EVS <- as_factor(EVS, levels = "values")

WVSConWave <- WVS$S024
WVSConWave <- as.numeric(as.character(WVSConWave))
EVS$S024 <- as.numeric(as.character(EVS$S024))

EVS <- EVS %>%
  filter(!(S024 %in% WVSConWave))

#164997
WVSSelected <- WVS %>%
  select( S003, S020, S024, S017, S025,A008, A029,A030, A039, A040, A042, A170, 
          A168A, A192, A193, A194, A195, A196, A197, A198, A199, A039, E001, E002, E003, E004, E005, E006,
         C001_01, C002, C005, A029, A030,  A032, A034, A035,  A038, E007, E008, E009, E010, E018, E025, E025B,
          D059, C010, C011, C012, C013, C014, C015, C016, C017, C018, C019, E026, E026B, E027, E028, E045, E061,
         E035, A165, D017, D019, D022, D023, D024, D054, D055, D056, D057, D058, E198, E221B, F063, F120, G007_02,
          F114D, F144, F120, E023, E022,  E025B, E026B, E027, E028, E028B, E029, Y002, Y003,
          E231,  D077, E038, F001, F028, F050, F115, F116, F117, F118, F119,  F121, F121, F123,
          E121, E275, E069_02, E069_06, E069_20, E069_19,
          C059, E012, E018,
          E036, G006, Y022, Y014A, Y014B,
           E232, F144_02,
         A189, A190,A191, Y001) %>%
  rename(
    
    "Country" = "S003",
    "year" = "S020",
    "Weights" = "S017",
    "PostMatInd" = "Y001",
    "PeopAdv" = "A168A",
    "MenJob" = "C001_01",
    "ChildMoney" = "A038",
    "ChildDeter" = "A039",
    "TerrMil" = "E275",
    "TrustOth" = "A165",
    "SecPaidMore" = "C059",
    "FightCountry" = "E012",
    "RespAuth" = "E018",
    "PrivatePublic" = "E038",
    "IncEq" = "E035",
    "ConfMil" = "E069_02",
    "ViolOthJust" = "F114D",
    "KilSelfDef" = "F144",
    "ConfPol" = "E069_06",
    "ConfUN" = "E069_20",
    "ConfNato" = "E069_19",
    "WifeObey" = "D077",
    "Abortion" = "F120",
    "PrdNatWs" = "G006",
    "EquaInd" = "Y022",
    "DemInde" = "E121",
    "TrstArm" = "Y014A",
    "TrstPol" = "Y014B",
    
    
    "StaIncEqu" = "E231",
    
    "DeathPena" = "F144_02",
    "ValueCreat" = "A189",
    "ValueRich" = "A190",
    "ValueSecur" = "A191",
    "ValueGodTim" = "A192",
    "ValueHelp" = "A193",
    "ValueSucses" = "A194",
    "ValueRisk" = "A195",
    "ValueBehav" = "A196",
    "ValueEnvoir" = "A197",
    "ValueTradi" = "A198",
    "ValueGoodSoci" = "A199"
    
    
  ) %>%
  mutate(across(everything(), ~as.numeric(as.character(.x)))) %>%
  mutate(across(everything(), ~ifelse(.x < 0, NA, .x)))

EVSSelected <- EVS %>%
  select( S003, S020, S024, S017, S025,A008, A029,A030, A039, A040, A042, A170, 
          A168A, A192, A193, A194, A195, A196, A197, A198, A199, A039, E001, E002, E003, E004, E005, E006,
           C002, C005, A029, A030,  A032, A034, A035,  A038, E007, E008, E009, E010, E018, E025, E025B,
          D059, C010, C011, C012, C013, C014, C015, C016, C017, C018, C019, E026, E026B, E027, E028, E045, E061,
          E035, A165, D017, D019, D022, D023, D024, D054, D055, D056, D057, D058, E198, E221B, F063, F120, G007_02,
           F144, F120, E023, E022,  E025B, E026B, E027, E028, E028B, E029, Y002, Y003,
          E231,  D077, E038, F001, F028, F050, F115, F116, F117, F118, F119,  F121, F121, F123,
          E121,  E069_02, E069_06, E069_20, E069_19,
          C059, E012, E018,
          E036, G006, Y022,
          E232, F144_02,
          A189, A190,A191, Y001) %>%
  rename(
    
    "Country" = "S003",
    "year" = "S020",
    "Weights" = "S017",
    "PostMatInd" = "Y001",
    "PeopAdv" = "A168A",
    
    "ChildMoney" = "A038",
    "ChildDeter" = "A039",
    
    "TrustOth" = "A165",
    "SecPaidMore" = "C059",
    "FightCountry" = "E012",
    "RespAuth" = "E018",
    "PrivatePublic" = "E038",
    "IncEq" = "E035",
    "ConfMil" = "E069_02",
    
    "KilSelfDef" = "F144",
    "ConfPol" = "E069_06",
    "ConfUN" = "E069_20",
    "ConfNato" = "E069_19",
    "WifeObey" = "D077",
    "Abortion" = "F120",
    "PrdNatWs" = "G006",
    
    "DemInde" = "E121",
    
    
    "StaIncEqu" = "E231",
    
    "DeathPena" = "F144_02",
    "ValueCreat" = "A189",
    "ValueRich" = "A190",
    "ValueSecur" = "A191",
    "ValueGodTim" = "A192",
    "ValueHelp" = "A193",
    "ValueSucses" = "A194",
    "ValueRisk" = "A195",
    "ValueBehav" = "A196",
    "ValueEnvoir" = "A197",
    "ValueTradi" = "A198",
    "ValueGoodSoci" = "A199"
    
    
  ) %>%
  mutate(across(everything(), ~as.numeric(as.character(.x)))) %>%
  mutate(across(everything(), ~ifelse(.x < 0, NA, .x))) 






SurveyData <- WVSSelected %>%
  bind_rows(EVSSelected)


SurveyData <- EVS %>%
  bind_rows(WVS)

#saveRDS(SurveyData, "WVS_EVS.rds")

SurveyData <- readRDS("WVS_EVS.rds")

WVSSelected <- as.data.frame(WVSSelected)

SurveyData <- as.data.frame(SurveyData)






WVS_Mean <- SurveyData %>%
  mutate(across(everything(), ~as.numeric(as.character(.x)))) %>%
  mutate(across(everything(), ~ifelse(.x < 0, NA, .x))) %>%
  group_by(Country, year) %>%
  summarise(across(everything(), ~weighted.mean(.x, w = Weights ,na.rm = TRUE))) %>%
  select(- Weights) %>%
  mutate(across(everything(), ~ifelse(is.nan(.x), NA, .x))) %>%
  ungroup() %>%
  add_count(Country) %>%
  filter(n > 1) 


WVS_Limited <- WVS_Mean %>%
  select(starts_with("Value"), PostMatInd, Country, year, FightCountry, ViolOthJust )

saveRDS(WVS_Mean, "wvs_everything.rds")

saveRDS(WVS_Limited, "WVS.rds")

