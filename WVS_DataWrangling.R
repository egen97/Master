#Pakker
library(tidyverse)
library(haven)
#Data, World Value Survey timeseries 1981-2020
WVS <- readRDS("Data/WVS_TimeSeries_R_v1_6.rds")


WVS <- as_factor(WVS, levels = "values")



WVSSelected <- WVS %>%
  select( S003, S020, S017, 
          A168A, A192, A193, A194, A195, A196, A197, A198, A199,
         C001_01, C002, C005,
          D059, 
         E035,
          F114D, F144,
          E231,  D077,
          E121, E275,
          
          
           E232, F144_02,
         A189, A190,A191) %>%
  rename(
    
    "Country" = "S003",
    "year" = "S020",
    "Weights" = "S017",
    
    "PeopAdv" = "A168A",
    "MenJob" = "C001_01",
    
    "TerrMil" = "E275",
    
    
    
    
    
    "IncEq" = "E035",
    
    "ViolOthJust" = "F114D",
    "KilSelfDef" = "F144",
    
    
    
    "WifeObey" = "D077",
    
    
    
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
  mutate(across(everything(), ~as.numeric(as.character(.x))))

WVSSelected %>%
  select(starts_with("Value"), IncEq) %>%
  drop_na() %>%
  factanal(
    factors = 4,
    rotation = "varimax"
  )


WVSSelected <- as.data.frame(WVSSelected)

library(Amelia)
library("plm")




WVS_Mean <- WVSSelected %>%
  group_by(Country, year) %>%
  summarise(across(everything(), ~weighted.mean(.x, w = Weights ,na.rm = TRUE))) %>%
  select(- Weights) %>%
  mutate(across(everything(), ~ifelse(is.nan(.x), NA, .x))) %>%
  
  ungroup()


# numCores  = round(parallel::detectCores() * .70)
# cl_par <- parallel::makePSOCKcluster(numCores)
# 
# WVSSelected$Weights <- NULL
# ImputedData <- amelia(WVSSelected,
#                       m = 1,
#                       ts = "year",
#                       cs = "Country",
#                       
#                       polytime = 2,
#                       intercs = TRUE,
#                       
#                       paralell = "snow",
#                       cl = cl_par)
# 
# 
# parallel::stopCluster(cl_par)

saveRDS(WVS_Mean, "WVS.rds")

