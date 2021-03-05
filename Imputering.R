library(Amelia)

CompleteData <- readRDS("CompleteData.rds")

#Imp_Data <- amelia(SurveyData, m = 10, ts = "year", cs = "Country", polytime = 1, intercs = TRUE, incheck = TRUE)



#c(column.number, lower.bound,upper.bound)

colnr <- c(144, 139, 142, 145)
low <- c(163.6142, 0, 1, 77006)
up <- c(85139.96, 10, 2, 1392730000)

Bounds <- data.frame(colnr, low, up)
Bounds <- as.matrix(Bounds)

ImputedData <- amelia(CompleteData,
                      m = 10,
                      ts = "year",
                      cs = "Country",
                      idvars = c("type_of_conflict", "conflict_id", "Wave", "CcIso",
                                 "country", "country_Afro"),
                      polytime = 2,
                      intercs = TRUE,
                      bounds = Bounds,
                      logs = "gdpPRcapita",
                      paralell = "snow",
                      ncpus = "12")