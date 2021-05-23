
#Texreg klarer ikke hente ut AIC for some reason

AIC_Extract <- function(Model){
  
  q <- lapply(from_zelig_model(Model), AIC)
  
  q <- as.data.frame(q)
  
  q <- rowMeans(q)
  
  return(q)
  
}


Models <- list(UCDP_1, UCDP_1_FE, UCDP_2, UCDP_2_FE, UCDP_3, UCDP_3_FE,
               UCDP_4, UCDP_4_FE, UCDP_5, UCDP_5_FE, UCDP_6, UCDP_6_FE)


Models_UCDP <- list(UCDP_1, UCDP_2, UCDP_3, UCDP_1_FE, UCDP_2_FE, UCDP_3_FE,
                    UCDP_4, UCDP_5, UCDP_4_FE, UCDP_5_FE, UCDP_6, UCDP_6_FE)


aic_list_UCDP <- lapply(Models_UCDP, AIC_Extract)


texreg::screenreg(l = list(UCDP_1, UCDP_2, UCDP_3), omit.coef = "(year)|(Country)",
                   custom.gof.rows = list("Num. obs." = c(3734, 3734, 3734),
                                          AIC = aic_list[c(1,3,5)]),
                   include.nobs = F,
                    custom.note = " %stars \n The time effect containts as well a squared and cubed effect",
                  custom.coef.names = c("(intercept)",
                                        "Value Score",
                                        "Time since conflict change",
                                        "Polity Score",
                                        "Ln(GDP/cap)")
                  )


# Tabel 1: Model 1, 2, 3 domestic variables

texreg::texreg(l = list(UCDP_1, UCDP_2, UCDP_3), omit.coef = "(year)|(Country)",
               custom.gof.rows = list("Num. obs." = c(3734, 3734, 3734),
                                      "AIC" = aic_list_UCDP[1:3]),
               include.nobs = F,
               custom.note = "%stars",
               custom.coef.names = c("(intercept)",
                                     "Value Score",
                                     "Time since conflict change",
                                     "Polity Score",
                                     "Ln(GDP/cap)"),
               booktabs = TRUE,
               use.packages = FALSE,
               caption = "UCDP: Domestic controlls",
               label = "UCDP_1",
               file = "UCDP_1.tex")


# Table 2: Model 1,2,3 Fixed Effects





texreg::screenreg(l = list(UCDP_1_FE, UCDP_2_FE, UCDP_3_FE), omit.coef = "(year)|(Country)",
                  custom.gof.rows = list("Num. obs." = c(3734, 3734, 3734)),
                  include.nobs = F,
                  custom.note = " %stars \n Fixed Effects on year & country",
                  custom.coef.names = c("(intercept)",
                                        "Value Score",
                                        "Time since conflict change",
                                        "Polity Score",
                                        "Ln(GDP/cap)"), custom.model.names = c("Model 1 FE", "Model 2 FE", "Model 3 FE"))


texreg::texreg( l = list(UCDP_1_FE, UCDP_2_FE, UCDP_3_FE), omit.coef = "(year)|(Country)",
                custom.gof.rows = list("Num. obs." = c(3734, 3734, 3734),
                                       "AIC" = aic_list_UCDP[4:6]),
                include.nobs = F,
                custom.note = " %stars \n Fixed Effects on year and country",
                custom.coef.names = c("(intercept)",
                                      "Value Score",
                                      "Time since conflict change",
                                      "Polity Score",
                                      "Ln(GDP/cap)"),
                booktabs = TRUE,
                use.packages = FALSE,
                caption = "UCDP: Domestic Controlls, Fixed Effects",
                label = "UCDP_1_FE",
                custom.model.names = c("Model 1 FE", "Model 2 FE", "Model 3 FE"),
                file = "UCDP_1_FE.tex")



#Table 3: Military Dom et int


texreg::screenreg(l = list(UCDP_4, UCDP_5),
                  custom.coef.names = c(
                    "(intercept)",
                    "Value Score",
                    "Time since conflict change",
                    "CINC",
                    "Major Power",
                    "Ln(GDP/cap",
                    "Nr. Allies",
                    "Borders: Sea",
                    "Borders: Land"
                  ),
                  custom.gof.rows = list("Num. obs." = c(3734, 3734),
                                         "AIC" = aic_list_UCDP[7:8]),
                  include.nobs = F,
                  include.loglik = TRUE,
                  custom.model.names = c("Model 4", "Model 5")
                  )


texreg::texreg(l = list(UCDP_4, UCDP_5),
                  custom.coef.names = c(
                    "(intercept)",
                    "Value Score",
                    "Time since conflict change",
                    "CINC",
                    "Major Power",
                    "Ln(GDP/cap)",
                    "Nr. Allies",
                    "Borders: Sea",
                    "Borders: Land"
                  ),
               custom.note = "%stars",
                  custom.gof.rows = list("Num. obs." = c(3734, 3734),
                                         "AIC" = aic_list_UCDP[7:8]),
                  include.nobs = F,
                  custom.model.names = c("Model 4", "Model 5"),
               booktabs = TRUE,
               use.packages = FALSE,
               caption = "UCDP: Military and international controlls",
               label = "UCDP_2",
               file = "UCDP_2.tex"
)



#Table 4: Mil Inter FE


texreg::screenreg(l = list(UCDP_4_FE, UCDP_5_FE),
                  omit.coef = c("year|Country"),
                  custom.coef.names = c(
                    "(intercept)",
                    "Value Score",
                    "Time since conflict change",
                    "CINC",
                    "Major Power",
                    "Ln(GDP/cap)",
                    "Nr. Allies",
                    "Borders: Sea",
                    "Borders: Land"
                  ),
                  custom.gof.rows = list("Num. obs." = c(3734, 3734),
                                         "AIC" = aic_list_UCDP[9:10]),
                  include.nobs = F,
                  custom.model.names = c("Model 4 FE", "Model 5 FE"), 
                  "%stars 
                  Fixed effects on country and year"
)


texreg::texreg(l = list(UCDP_4_FE, UCDP_5_FE),
               omit.coef = c("year|Country"),
               custom.coef.names = c(
                 "(intercept)",
                 "Value Score",
                 "Time since conflict change",
                 "CINC",
                 "Major Power",
                 "Ln(GDP/cap)",
                 "Nr. Allies",
                 "Borders: Sea",
                 "Borders: Land"
               ),
               custom.note = "%stars \n Fixed effects on country and year",
               custom.gof.rows = list("Num. obs." = c(3734, 3734),
                                      "AIC" = aic_list_UCDP[9:10]),
               include.nobs = F,
               custom.model.names = c("Model 4 FE", "Model 5 FE"),
               booktabs = TRUE,
               use.packages = FALSE,
               caption = "UCDP: Military and international controlls, Fixed Effects",
               label = "UCDP_2_FE",
               file = "UCDP_2_FE.tex"
)


texreg::screenreg(l = list(UCDP_6, UCDP_6_FE), omit.coef = c("Country|year"),
                  custom.coef.names = c(
                    "(intercept)",
                    "Value Score",
                    "Time since conflict change",
                    "Polity Score",
                    "ln(GDP/cap)",
                    "Nr. Allies",
                    "Borders: Sea",
                    "Borders: Land",
                    "CINC",
                    "Major Power"
                    ),
                  custom.gof.rows = list("Num. obs" = c(3734, 3734)),
                  include.nobs = F,
                  custom.model.names = c("Model 6", "Model 6 FE"))


texreg::texreg(l = list(UCDP_6, UCDP_6_FE), omit.coef = c("Country|year"),
               custom.coef.names = c(
                 "(intercept)",
                 "Value Score",
                 "Time since conflict change",
                 "Polity Score",
                 "ln(GDP/cap)",
                 "Nr. Allies",
                 "Borders: Sea",
                 "Borders: Land",
                 "CINC",
                 "Major Power"
               ),
               custom.gof.rows = list("Num. obs" = c(3734, 3734),
                                      "AIC" = aic_list_UCDP[11:12]),
               include.nobs = F,
               custom.model.names = c("Model 6", "Model 6 FE"),
               booktabs = TRUE,
               use.packages = FALSE,
               custom.note = "%stars \n Fixed effects on country and year",
               label = "UCDP_3",
               caption = "UCDP: Complete model",
               file = "UCDP_3.tex")


######## MID ###############

#MID AIC

Models_MID <- list(MID_1, MID_2, MID_3, MID_1_FE, MID_2_FE, MID_3_FE,
                   MID_4, MID_5, MID_4_FE, MID_5_FE, MID_6, MID_6_FE)

aic_list_MID <- lapply(Models_MID, AIC_Extract)





# Tabel 1: Model 1, 2, 3 domestic variables

texreg::texreg(l = list(MID_1, MID_2, MID_3), omit.coef = "(year)|(Country)",
               custom.gof.rows = list("Num. obs." = c(3734, 3734, 3734),
                                      "AIC" = aic_list_MID[1:3]),
               include.nobs = F,
               custom.note = "%stars",
               custom.coef.names = c("(intercept)",
                                     "Value Score",
                                     "Time since conflict change",
                                     "Polity Score",
                                     "Ln(GDP/cap)"),
               booktabs = TRUE,
               use.packages = FALSE,
               caption = "MID: Domestic controlls",
               label = "MID_1",
               file = "MID_1.tex")


# Table 2: Model 1,2,3 Fixed Effects







texreg::texreg( l = list(MID_1_FE, MID_2_FE, MID_3_FE), omit.coef = "(year)|(Country)",
                custom.gof.rows = list("Num. obs." = c(3734, 3734, 3734),
                                       "AIC" = aic_list_MID[4:6]),
                include.nobs = F,
                custom.note = " %stars \n Fixed Effects on year and country",
                custom.coef.names = c("(intercept)",
                                      "Value Score",
                                      "Time since conflict change",
                                      "Polity Score",
                                      "Ln(GDP/cap)"),
                booktabs = TRUE,
                use.packages = FALSE,
                caption = "MID: Domestic Controlls, Fixed Effects",
                label = "MID_1_FE",
                custom.model.names = c("Model 1 FE", "Model 2 FE", "Model 3 FE"),
                file = "MID_1_FE.tex")



#Table 3: Military Dom et int





texreg::texreg(l = list(MID_4, MID_5),
               custom.coef.names = c(
                 "(intercept)",
                 "Value Score",
                 "Time since conflict change",
                 "CINC",
                 "Major Power",
                 "Ln(GDP/cap)",
                 "Nr. Allies",
                 "Borders: Sea",
                 "Borders: Land"
               ),
               custom.note = "%stars",
               custom.gof.rows = list("Num. obs." = c(3734, 3734),
                                      "AIC" = aic_list_MID[7:8]),
               include.nobs = F,
               custom.model.names = c("Model 4", "Model 5"),
               booktabs = TRUE,
               use.packages = FALSE,
               caption = "MID: Military and international controlls",
               label = "MID_2",
               file = "MID_2.tex"
)



#Table 4: Mil Inter FE




texreg::texreg(l = list(MID_4_FE, MID_5_FE),
               omit.coef = c("year|Country"),
               custom.coef.names = c(
                 "(intercept)",
                 "Value Score",
                 "Time since conflict change",
                 "CINC",
                 "Major Power",
                 "Ln(GDP/cap",
                 "Nr. Allies",
                 "Borders: Sea",
                 "Borders: Land"
               ),
               custom.note = "%stars \n Fixed effects on country and year",
               custom.gof.rows = list("Num. obs." = c(3734, 3734),
                                      "AIC" = aic_list_MID[9:10]),
               include.nobs = F,
               custom.model.names = c("Model 4 FE", "Model 5 FE"),
               booktabs = TRUE,
               use.packages = FALSE,
               caption = "MID: Military and international controlls, Fixed Effects",
               label = "MID_2_FE",
               file = "MID_2_FE.tex"
)





texreg::texreg(l = list(MID_6, MID_6_FE), omit.coef = c("Country|year"),
               custom.coef.names = c(
                 "(intercept)",
                 "Value Score",
                 "Time since conflict change",
                 "Polity Score",
                 "ln(GDP/cap)",
                 "Nr. Allies",
                 "Borders: Sea",
                 "Borders: Land",
                 "CINC",
                 "Major Power"
               ),
               custom.gof.rows = list("Num. obs" = c(3734, 3734),
                                      "AIC" = aic_list_MID[11:12]),
               include.nobs = F,
               custom.model.names = c("Model 6", "Model 6 FE"),
               booktabs = TRUE,
               use.packages = FALSE,
               custom.note = "%stars \n Fixed effects on country and year",
               label = "MID_3",
               caption = "MID: Complete model",
               file = "MID_3.tex")

