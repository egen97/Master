library(Amelia)
library(tidyverse)
CompleteData <- readRDS("CompleteData.rds")


#Imp_Data <- amelia(SurveyData, m = 10, ts = "year", cs = "Country", polytime = 1, intercs = TRUE, incheck = TRUE)
CompleteData <- CompleteData %>%
  filter(!is.na(Country)) %>%
  select(-Country.y, -ccode )

#c(column.number, lower.bound,upper.bound)

colnr <- c(60, 61,59 )
low <- c(94.56473, 37500,0)
up <- c(118823.6, 1397715000,1)

Bounds <- data.frame(colnr, low, up)
Bounds <- as.matrix(Bounds)

CompleteData <- CompleteData %>%
  select(-wb_country, -wb_code, -extended_country_name, -fh_country)



CompleteData$Y022 <- NULL
CompleteData$EquaInd <- NULL
CompleteData$AlEduVote_Afro <- NULL
CompleteData$TradRule_Afro <- NULL
CompleteData$PunGuil <- NULL
CompleteData$q5054 <- NULL

CompleteData$HDI <- as.numeric(CompleteData$HDI)
numCores  = round(parallel::detectCores())
cl_par <- parallel::makePSOCKcluster(numCores)

ImputedData <- amelia(CompleteData,
                      m = 10,
                      ts = "year",
                      cs = "Country",
                      idvars = c("type_of_conflict", "conflict_id"),    
                      polytime = 2,
                      intercs = TRUE,
                      ords = "status",
                      empri = 10,
                      bounds = Bounds,
                      autopri = 1,
                      paralell = "snow",
                      cl = cl_par)


parallel::stopCluster(cl_par)
