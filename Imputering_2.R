library(Amelia)
library(tidyverse)
library(hutilscpp)
CompleteData <- readRDS("CompleteData.rds")


SubSet <- CompleteData %>%
  select(Country, year, polity, gdpPRcapita, Population,
         ValueSucses, ValueRisk, suppDem_Afro, SuppDem,
         v2x_polyarchy, ChildDeter, DOMMOV, v2xpe_exlgender, A008, A029,
         A170, KILL, e_total_resources_income_pc, e_pefeliex, Conflict_Binary,
        FightCountry, milex, milper, majorpower, type_of_conflict, intensity_level, HDI,
        Gini, ValueGodTim, ValueSecur, v2x_accountability, ValueSecur,
        v2xpe_exlgender, PHYSINT, A035, armedAntiUsAbarb, dealMoralArab, DeathPena,
        v2xpe_exlecon, POLPRIS, e_miurbani, e_total_resources_income_pc, e_total_fuel_income_pc,
        v2xpe_exlecon, E006, C019, TrustOth, IncEq, StaIncEqu, attackCivArab,FightCountry,
        dispnum, hiact, hostlev, fatality, length,  fatalpre, land, sea, cinc, num_mem, edu_year, Y002, Y003)



#Fikse at MID dupliserte år
SubSet <- SubSet %>%
  group_by(Country, year) %>%
  summarise(across(everything(), ~max(.x))) 

#Legge til aar siden sist konflikt, og verdier #Tid blir endret i analysen

SubSet <- SubSet %>%
  mutate(ValueScore = (ValueRisk*(0.638)) + (ValueSucses*0.432) + (ValueGodTim*0.420) + (ValueSecur),
         dispnum = ifelse(dispnum == 0, NA, dispnum), 
         MID_Binary = ifelse(!is.na(dispnum) & fatality > 1, 1, 0))

SubSet <- SubSet %>% 
  mutate(
    TimUCDP = cumsum_reset(!(as.logical(Conflict_Binary))),
    TimMID = cumsum_reset(!(as.logical(MID_Binary)))
  ) 



missmap(SubSet)
colnr <- c(4, 5)
low <- c(94.56473, 37500)
up <- c(118823.6, 1397715000)

Bounds <- data.frame(colnr, low, up)
Bounds <- as.matrix(Bounds)

SubSet <- as.data.frame(SubSet)

numCores  = round(parallel::detectCores())
cl_par <- parallel::makePSOCKcluster(numCores)

ImputedData <- amelia(SubSet,
                      m = 10,
                      ts = "year",
                      cs = "Country",
                      idvars = c("type_of_conflict", "intensity_level",
                                 "dispnum"),    
                      polytime = 2,
                      intercs = TRUE,
                      empri = 0.1*nrow(SubSet),
                      #ords = "",
                      bounds = Bounds,
                      logs = c("gdpPRcapita", "Population"),
                      leads = c("polity", "gdpPRcapita", "Population"),
                      lags = c("polity", "gdpPRcapita", "Population"),
                      autopri = 1,
                      paralell = "snow",
                      cl = cl_par)



parallel::stopCluster(cl_par)

save(ImputedData,file = "Data/imputed/PostMatinclude.RData")


overimpute(ImputedData, c("polity"), draws = 1)
compare.density(ImputedData, "ValueSucses")
compare.density(ImputedData, "ValueGodTim")
compare.density(ImputedData, "ValueSecur")

tscsPlot(ImputedData, "ValueGodTim", cs = 344  )

