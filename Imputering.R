library(Amelia)

CompleteData <- readRDS("CompleteData.rds")

#Imp_Data <- amelia(SurveyData, m = 10, ts = "year", cs = "Country", polytime = 1, intercs = TRUE, incheck = TRUE)
CompleteData <- CompleteData %>%
    mutate(across(everything(), ~ifelse(.x <0, NA, .x)))


#c(column.number, lower.bound,upper.bound)

colnr <- c(44, 43)
low <- c(min(CompleteData$Population, na.rm = TRUE), min(CompleteData$gdpPRcapita, na.rm = TRUE))
up <- c(max(CompleteData$Population, na.rm = TRUE), max(CompleteData$gdpPRcapita, na.rm = TRUE))

Bounds <- data.frame(colnr, low, up)
Bounds <- as.matrix(Bounds)
# 
# ImputedData <- amelia(CompleteData,
#                       m = 10,
#                       ts = "year",
#                       cs = "Country",
#                       idvars = c("conflict_id", "type_of_conflict"),
#                       polytime = 2,
#                       intercs = TRUE,
#                       bounds = Bounds,
#                       logs = "gdpPRcapita",
#                       paralell = "snow",
#                       ncpus = "6")

CompleteData <- CompleteData %>%
  select(-PeopObey, -GrpBst_Afro,-GrpPrd_Afro, -MenPolLead, -PeopAdv, -PunGuil, -JobNati, -AlEduVote_Afro)


numCores  = round(parallel::detectCores())
cl_par <- parallel::makePSOCKcluster(numCores)

ImputedData <- amelia(CompleteData,
                      m = 1,
                      ts = "year",
                      cs = "Country",
                      idvars = c("type_of_conflict", "conflict_id"),    
                      polytime = 1,
                      intercs = TRUE,
                      bounds = Bounds,

                      paralell = "snow",
                      cl = cl_par)


parallel::stopCluster(cl_par)
