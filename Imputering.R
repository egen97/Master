library(Amelia)
library(tidyverse)
CompleteData <- readRDS("CompleteData.rds")


#Imp_Data <- amelia(SurveyData, m = 10, ts = "year", cs = "Country", polytime = 1, intercs = TRUE, incheck = TRUE)
CompleteData <- CompleteData %>%
  filter(!is.na(Country)) %>%
  select(-Country.y, -ccode )

CompleteData$milex <- ifelse(CompleteData$milex < 0, NA, CompleteData$milex)
#c(column.number, lower.bound,upper.bound)

# Riddikulus <- CompleteData %>%
#   select(Country, year, ValueRisk, ValueRich, ValueSecur, HDI, Gini, polity, expr_prgdp, health_xpns, gdpPRcapita, Population,
#          fh_total,fuel_exprprgdp, Law, pr, edu_year, Fules_prcap,Conflict_Binary, milex, milper, irst,
#          starts_with("v2"))


Riddikulus <- CompleteData %>%
  select(Country, year, ValueRich, ValueRisk, ValueSecur, expr_prgdp, Population, gdpPRcapita, v2x_polyarchy,
         polity, expr_prgdp, A08, ChildDeter, A040, A170, )

colnr <- c(14, 15)
low <- c(94.56473, 37500)
up <- c(118823.6, 1397715000)

Bounds <- data.frame(colnr, low, up)
Bounds <- as.matrix(Bounds)

CompleteData <- CompleteData %>%
  select(-wb_country, -wb_code, -extended_country_name, -fh_country)


CompleteData$G007_02 <- NULL
CompleteData$Y022 <- NULL
CompleteData$EquaInd <- NULL
CompleteData$AlEduVote_Afro <- NULL
CompleteData$TradRule_Afro <- NULL
CompleteData$PunGuil <- NULL
CompleteData$q5054 <- NULL

CompleteData <- CompleteData %>%
  mutate(across(c("Fules_prcap", "Elec_prcap", "expr_prgdp", "fuel_exprprgdp", "edu_year", "edu_xpns", "health_xpns"),
                  ~as.numeric(.x))) %>%
  select(-fh_country, -extended_country_name)


CompleteData <- CompleteData %>%
  select(-majorpower, -milex, -milper, -irst)


MilDat <- MilDat %>%
  select(-ccode)

CompleteData <- CompleteData %>%
  left_join(MilDat)

CompleteData$majorpower <- ifelse(is.na(CompleteData$majorpower), 0, 1)


CompleteData$HDI <- as.numeric(CompleteData$HDI)




numCores  = round(parallel::detectCores())
cl_par <- parallel::makePSOCKcluster(numCores)

ImputedData <- amelia(Riddikulus,
                      m = 1,
                      ts = "year",
                      cs = "Country",
                      #idvars = c("type_of_conflict", "conflict_id"),    
                      polytime = 2,
                      intercs = TRUE,
                      #ords = "status",
                      #bounds = Bounds,
                      logs = c("gdpPRcapita", "Population"),
                      autopri = 1,
                      paralell = "snow",
                      cl = cl_par)


parallel::stopCluster(cl_par)
