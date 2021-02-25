#Pakker
library(tidyverse)

#Data, World Value Survey timeseries 1981-2020
WVS <- readRDS("Data/WVS_TimeSeries_R_v1_6.rds")

WVSSelected <- WVS %>%
  select(S002, S003, S020, S017, 
         A047, A168A, A219,
         C001_01, C002, C005, C038,
         C059, D059, D060, E066,
         E035,
         E198, F114D, F114C, F144,
         F199, E143,  E231, F203,
         starts_with("A124"), D077,
         E108, E109, E120, E121,
         E122, E123, E224, E225,
         E226, E227, E228, E229,
         E230, E231, E232, E233,
         E233A, E233B, F144_02,
         A189, A190,A191, A192, 
         A193, A194, A195, A196,
         A197, A198, A199) %>%
  rename(
    "Wave" = "S002",
    "CcIso" = "S003",
    "Year" = "S020",
    "Weights" = "S017",
    "AbrtHand" = "A047",
    "PeopAdv" = "A168A",
    "FltOth" = "A219",
    "MenJob" = "C001_01",
    "JobNati" = "C002",
    "HanNotJob" = "C005",
    "NtJobLazy" = "C038",
    "SecPaid" = "C059",
    "MenPolLead" = "D059",
    "UniBoy" = "D060",
    "SocEga" = "E066",
    "IncEq" = "E035",
    "ViolPolJust" = "E198",
    "ViolOthJust" = "F114D",
    "BeatKids" = "F114C",
    "KilSelfDef" = "F144",
    "WifeBeat" = "F199",
    "ImgPol" = "E143",
    "DemHarPuni" = "E231",
    "MyRel" = "F203",
    "WifeObey" = "D077",
    "WomMowApprov" = "E108",
    "AntApar" = "E109",
    "DemEcoBad" = "E120",
    "DemInde" = "E121",
    "DemBett" = "E122",
    "DemBaOrd" = "E123",
    "RespHumRigh" = "E224",
    "ArmyCont" = "E225",
    "CivRigh" = "E226",
    "DemEcoProsp" = "E227",
    "DemCrinPun" = "E228",
    "PeoChaLa" = "E229",
    "Womrigh" = "E230",
    "StaIncEqu" = "E231",
    "PeopObey" = "E232",
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
    
    
  )

rm(WVS)

WVS_Mean <- WVSSelected %>%
  group_by(CcIso, Year) %>%
  summarise(across(everything(), ~weighted.mean(.x, w = Weights ,na.rm = TRUE))) %>%
  select(- Weights) %>%
  mutate(across(everything(), ~ifelse(is.nan(.x), NA, .x)))





saveRDS(WVS_Mean, "WVS.rds")
