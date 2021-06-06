AIC_Extract <- function(Model){
  
  q <- lapply(from_zelig_model(Model), AIC)
  
  q <- as.data.frame(q)
  
  q <- round(rowMeans(q),2)
  
  return(q)
  
}


Models_UCDP <- list(UCDP_1, UCDP_2, UCDP_3, UCDP_4, UCDP_5, UCDP_6)
Models_UCDP_FE <- list(UCDP_1_FE, UCDP_2, UCDP_3, UCDP_4, UCDP_5,  UCDP_6)


Models_MID <- list(MID_1, MID_2, MID_3, MID_4, MID_5, MID_6)
Models_MID_FE <- list(MID_1_FE, MID_2_FE, MID_3_FE, MID_4_FE, MID_5_FE, MID_6_FE)

aic_list_UCDP <- lapply(Models_UCDP, AIC_Extract)
aic_list_UCDP_FE <- lapply(Models_UCDP_FE, AIC_Extract)

aic_list_MID <- lapply(Models_MID, AIC_Extract)
aic_list_MID_FE <- lapply(Models_MID_FE, AIC_Extract)


texreg::texreg(l = list(UCDP_1, UCDP_2, UCDP_3, UCDP_1_FE, UCDP_2_FE, UCDP_3_FE),
                  omit.coef = "Country|year|TimUCDP",
                  include.nobs = FALSE,
                  custom.gof.rows = list(
                    "Num. obs" = c(3734, 3734, 3734, 3734, 3734, 3734),
                    "AIC" = c(aic_list_UCDP[1:3], aic_list_UCDP_FE[1:3]),
                    "Fixed Effects" = c("N", "N", "N", "Y", "Y", "Y"),
                    "Time Effects" = c("Y", "Y", "Y", "Y", "Y", "Y")
                  ),
                  custom.model.names = c("UCDP/PRIO 1", "UCDP/PRIO 2", "UCDP/PRIO 3", 
                                         "UCDP/PRIO 4", "UCDP/PRIO 5", "UCDP/PRIO 6"),
                  custom.header = list("Logistic Regression" = 1:6),
                  custom.coef.names = c(
                    "(intercept)",
                    "Self-Enchancement Values",
                    "Polity Score",
                    "Ln GDP/cap"
                  ),
               booktabs = TRUE,
               dcolumn = TRUE,
               use.packages = FALSE,
               center = TRUE,
               scalebox = 0.6,
               caption = "UCDP: Domestic controlls",
               label = "UCDP_1",
               file = "UCDP_1.tex") 




texreg::texreg(l = list(UCDP_4, UCDP_5, UCDP_6, UCDP_4_FE, UCDP_5_FE, UCDP_6_FE),
                  omit.coef = "Country|year|TimUCDP",
                  include.nobs = FALSE,
                  custom.gof.rows = list(
                    "Num. obs" = c(3734, 3734, 3734, 3734, 3734, 3734),
                    "AIC" = c(aic_list_UCDP[4:6], aic_list_UCDP_FE[4:6]),
                    "Fixed Effects" = c("N", "N", "N", "Y", "Y", "Y"),
                    "Time Effects" = c("Y", "Y", "Y", "Y", "Y", "Y")
                  ),
                  custom.model.names = c("UCDP/PRIO 7", "UCDP/PRIO 8", "UCDP/PRIO 9", 
                                         "UCDP/PRIO 10", "UCDP/PRIO 11", "UCDP/PRIO 12"),
                  custom.header = list("Logistic Regression" = 1:6),
                  custom.coef.names = c(
                    "(intercept)",
                    "Self-Enchancement Values",
                    "CINC",
                    "Major Power",
                    "Ln GDP/Cap",
                    "Nr. Allies",
                    "Borders: Sea",
                    "Borders: Land",
                    "Polity Score"
                  ),
               booktabs = TRUE,
               dcolumn = TRUE,
               center = TRUE,
               scalebox = 0.6,
               use.packages = FALSE,
               caption = "UCDP: Military and international controls",
               label = "UCDP_2",
               file = "UCDP_2.tex")








texreg::texreg(l = list(MID_1, MID_2, MID_3, MID_1_FE, MID_2_FE, MID_3_FE),
                  omit.coef = "Country|year|TimMID",
                  include.nobs = FALSE,
                  custom.gof.rows = list(
                    "Num. obs" = c(  3734,   3734, 3734, 3734, 3734, 3734),
                    "AIC" = c(aic_list_MID[1:3], aic_list_MID_FE[1:3]),
                    "Fixed Effects" = c("N", "N", "N", "Y", "Y", "Y"),
                    "Time Effects" = c("Y", "Y", "Y", "Y", "Y", "Y")
                  ),
                  custom.model.names = c("MID 1", "MID 2", "MID 3", 
                                         "MID 4", "MID 5", "MID 6"),
                  custom.header = list("Logistic Regression" = 1:6),
                  custom.coef.names = c(
                    "(intercept)",
                    "Self-Enhancement Values",
                    "Polity Score",
                    "Ln GDP/Cap"
                  ),
               booktabs = TRUE,
               dcolumn = TRUE,
               center = TRUE,
               scalebox = 0.8,
               use.packages = FALSE,
               caption = "MID: Domestic controlls",
               label = "MID_1",
               file = "MID_1.tex")






texreg::texreg(l = list(MID_4, MID_5, MID_6, MID_4_FE, MID_5_FE, MID_6_FE),
                  omit.coef = "Country|year|TimMID",
                  include.nobs = FALSE,
                  custom.gof.rows = list(
                    "Num. obs" = c(3734, 3734, 3734, 3734, 3734, 3734),
                    "AIC" = c(aic_list_MID[4:6], aic_list_MID_FE[4:6]),
                    "Fixed Effects" = c("N", "N", "N", "Y", "Y", "Y"),
                    "Time Effects" = c("Y", "Y", "Y", "Y", "Y", "Y")
                  ),
                  custom.model.names = c("MID 7", "MID 8", "MID 9", 
                                         "MID", "MID 11", "MID 12"),
                  custom.header = list("Logistic Regression" = 1:6),
                  custom.coef.names = c(
                    "(intercept)",
                    "Self-Enchancement Values",
                    "CINC",
                    "Major Power",
                    "Ln GDP/Cap",
                    "Nr. Allies",
                    "Borders: Sea",
                    "Borders: Land",
                    "Polity Score"
                  ),
                  booktabs = TRUE,
                  dcolumn = TRUE,
                  center = TRUE,
                  scalebox = 0.6,
                  use.packages = FALSE,
                  caption = "MID: Military and internationall controls",
                  label = "MID_2",
                  file = "MID_2.tex")

