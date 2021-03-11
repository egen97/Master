#Pakker
library(tidyverse)
library(haven)
#Data, World Value Survey timeseries 1981-2020
WVS <- readRDS("Data/WVS_TimeSeries_R_v1_6.rds")


WVS <- as_factor(WVS, levels = "values")

WVSSelected <- as.data.frame(WVSSelected)

WVSSelected <- WVS %>%
  select(S002, S003, S020, S017, 
         A047, A168A, A192, A193, A194, A195, A196, A197, A198, A199,
         C001_01, C002, C005,
         C059, D059, E066,
         E035,
          F114D, F114C, F144,
         F199, E231, F203, D077,
          E121,
         E122, E123, E225,
         E226, E227,  E229,
         E230,  E232, E233,
         E233A, E233B, F144_02,
         A189, A190,A191) %>%
  rename(
    "Wave" = "S002",
    "CcIso" = "S003",
    "Year" = "S020",
    "Weights" = "S017",
    "AbrtHand" = "A047",
    "PeopAdv" = "A168A",
    "MenJob" = "C001_01",
    "JobNati" = "C002",
    "HanNotJob" = "C005",
    
    "SecPaid" = "C059",
    "MenPolLead" = "D059",
    
    "SocEga" = "E066",
    "IncEq" = "E035",
    
    "ViolOthJust" = "F114D",
    "BeatKids" = "F114C",
    "KilSelfDef" = "F144",
    "WifeBeat" = "F199",
    
    
    "MyRel" = "F203",
    "WifeObey" = "D077",
    
    
    
    "DemInde" = "E121",
    "DemBett" = "E122",
    "DemBaOrd" = "E123",
    
    "ArmyCont" = "E225",
    "CivRigh" = "E226",
    "DemEcoProsp" = "E227",
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
    
    
  ) %>%
  mutate(across(everything(), ~as.numeric(as.character(.x))))


library(Amelia)
library("plm")

WVSSelected <- make.pbalanced(WVSSelected, index = c("CcIso", "Year"))
WVSSelected$CcIso1 <- as.character(WVSSelected$CcIso)

ImputedData <- amelia(WVS_Mean,
                      m = 10,
                      ts = "Year",
                      cs = "CcIso1",
                      idvars = c("Wave",
                                 "CcIso",
                                 "Year"),
                      polytime = 2,
                      intercs = TRUE,
                      paralell = "snow",
                      ncpus = "4")







WVS_Mean <- WVSSelected %>%
  group_by(CcIso1, Year) %>%
  summarise(across(everything(), ~weighted.mean(.x, w = Weights ,na.rm = TRUE))) %>%
  select(- Weights) %>%
  mutate(across(everything(), ~ifelse(is.nan(.x), NA, .x))) %>%
  ungroup()



#### Faktor Analyse (?) #####


Fakt <- factanal(
  WVSSelected %>% select(SecPaid, MenPolLead, SocEga, IncEq, SocEga, starts_with("Value"),
                         starts_with("Dem")) %>%  mutate(across(everything(), ~as.numeric(.x))) %>% View() ,
  2
)







saveRDS(WVS_Mean, "WVS.rds")
