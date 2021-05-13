#### MID Analysis ####

MIDBinary <- zelig(MID_Binary ~ Y002 +
                     milper + majorpower + cinc + num_mem + land + sea +
                     polity + log(gdpPRcapita) + DeathPena  +
                     as.factor(Country) +  as.factor(year) + as.factor(TimMID), 
                   model = "logit", data = ImputedData)

Highact <- zelig(hiact ~ Y002 +
                   milper + majorpower + cinc + num_mem + land + sea +
                   polity + log(gdpPRcapita) + DeathPena  +
                   as.factor(Country) +  as.factor(year), 
                 model = "ls", data = ImputedData)


MIDLength <- zelig(length ~ Y002 +
                     milper + majorpower + cinc + num_mem + land + sea +
                     polity + log(gdpPRcapita) + DeathPena  +
                     as.factor(Country) +  as.factor(year), 
                   model = "ls", data = ImputedData)


MIDFatal <- zelig(fatalpre ~ Y002 +
                    milper + majorpower + cinc + num_mem + land + sea +
                    DeathPena + polity + log(gdpPRcapita) +
                    as.factor(Country) + as.factor(year),
                  model = "ls", data = ImputedData)




#### UCDP Analysis ##########


UCDPBinary <- zelig(Conflict_Binary ~ Y002 +
                      milper + majorpower + cinc + num_mem + land + sea +
                      DeathPena  + polity + log(gdpPRcapita) +
                      as.factor(Country) + as.factor(year) + as.factor(TimUCDP), 
                    data = ImputedData)


#texreg::screenreg(UCDPBinary,  omit.coef = "(year)|(Country)|(Tim)")


#ImputedData <- transform.amelia(ImputedData, intre = intensity_level -1)

UCDPInten <- zelig(intre ~ Y002 +
                     milper + majorpower + cinc + num_mem + land + sea +
                     DeathPena + polity + log(gdpPRcapita) +
                     as.factor(Country) + as.factor(year),
                   model = "logit", data = ImputedData)




#### UCDP TEX ########

texreg::texreg(l = list(UCDPBinaryPost, UCDPInten), omit.coef = "(year)|(Country)|(Tim)", 
               custom.model.names =  c("UCDP/Prio Binary", "Conflict Intensity"),
               custom.header = list("logistic" = 1:2),
               custom.coef.names = c(
                 "(intercept)",
                 "Post-Materialism Index",
                 "Military Personnel",
                 "Major Power",
                 "COW: CINC",
                 "Nr. Allies",
                 "Borders: Land",
                 "Borders: Sea",
                 "Support capital punishment",
                 "Polity",
                 "ln(GDP/cap)"
               ),
               label = "UCDPIng",
               booktabs = TRUE,
               caption = "Post-Materialism regression: UCDP/Prio",
               use.packages = FALSE,
               file = "UCDPIng.tex"
)



#### MID TEX ######
texreg::texreg(l = list(MIDBinary, Highact, MIDFatal, MIDLength), omit.coef = "(year)|(Country)|(Tim)", 
               custom.model.names =  c("MID Binary", "Level of Conflict", "Fatalities", "Conflict Length"),
               custom.header = list("logistic" = 1, "OLS" = 2:4),
               custom.coef.names = c("(intercept)",
                                     "Post-Materialism Index",
                                     "Military Personnel",
                                     "Major Power",
                                     "COW: CINC",
                                     "Nr. Allies",
                                     "Borders: Land",
                                     "Borders: Sea",
                                     "Polity",
                                     "ln(GDP/cap)",
                                     "Support capital punishment"),
               label = "MIDIng",
               booktabs = TRUE,
               caption = "Post-Materialism regression: Military Interstate Disputes",
               use.packages = FALSE,
               file = "MIDIng.tex")
