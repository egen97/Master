library("Zelig")
library(Amelia)
library("stargazer")
library(tidyverse)
#z5$zelig(tariff ~ polity + pop + gdp.pc + year + country, data = a.out)
#ImputedData <- load("Data/imputed/10imp46var.RData")
#Vel den funka ikke, men det funka å bare klike..god knows why, og gud hater jeg .RData
#a.out <- transform(a.out, lgdp = log(gdp.pc))



stargazer(from_zelig_model(FML)[[1]])

FML <- zelig(Conflict_Binary ~ ValueScore +
               milper + majorpower + DeathPena  + polity +as.factor(Country) + 
               as.factor(year), model = "ls", data = ImputedData)

summary(FML)
t <- from_zelig_model(FML)
summary(t[[1]])
stargazer(
  
)


summary(t[[2]])
stargazer(lm(FML$zelig.out$z.out), type='text', keep = 1:4)

FML
summary(FML$zelig.out$z.out)


FML %>% from_zelig_model() %>% stargazer(type = 'html')


#texreg::texreg(FML)

names(ImputedData[[1]][[1]])


FUBAR <- zelig(Conflict_Binary ~ ValueScore +
               milper + majorpower + DeathPena  + polity + gdpPRcapita +
                cinc + num_mem + land + sea 
               as.factor(Country) + 
               as.factor(year) + as.factor(TimUCDP), model = "logit", data = ImputedData)





SNAFU <- zelig(MID_Binary ~ ValueScore +
                 milper + majorpower + DeathPena  + polity + gdpPRcapita +
                 as.factor(Country) + 
                 as.factor(year) + as.factor(TimMID), model = "logit", data = ImputedData)



SNAFU <- zelig(MID_Binary ~ ValueScore +
                 milper + majorpower + DeathPena  + polity + gdpPRcapita +
                 as.factor(Country) + 
                 as.factor(year), model = "logit", data = ImputedData)



TARFU <- zelig(length ~ ValueScore +
                 milper + majorpower + DeathPena  + polity + gdpPRcapita +
                 as.factor(Country) + 
                 as.factor(year), model = "ls", data = ImputedData)





texreg::texreg(FUBAR)
texreg::texreg(l = list(SNAFU, FUBAR, TARFU), omit.coef = "(year)|(Country)|(Tim)", custom.model.names =  c("MID Binary", "UCDP/PRIO Binary", "Conflict Length"),
                 custom.header = list("logistic regression" = 1:2, "OLS" = 3), file = "BinaryLengthReg.tex")

texreg::screenreg(l = list(SNAFU, FUBAR, TARFU), omit.coef = "(year)|(Country)|(Tim)", custom.model.names =  c("MID Binary", "UCDP/PRIO Binary", "Conflict Length"),
               custom.header = list("logistic regression" = 1:2, "OLS" = 3))

#Labels og sånt må settes her

Df <- ImputedData$imputations$imp1
Df <- as.data.frame(Df)
ggplot(Df ) +
  geom_smooth(aes(year, ValueScore, colour = as.factor(Country)), alpha = .2, size = .01, se = FALSE) +
  geom_smooth(aes(year, ValueScore), size = 2.5, colour = "#FF6666", fill = "#FF6666", alpha = .8) +
  guides(colour = FALSE) +
  theme_classic() +
  labs(y = "Value Score", x = "Year", title = "Timeline: Value Score", subtitle = "All participating countries & World Trend")

#UCDP Variables

FUBAR <- zelig(Conflict_Binary ~ ValueScore +
                 milper + majorpower + cinc + num_mem + land + sea +
                 DeathPena  + polity + gdpPRcapita +
                 as.factor(Country) + as.factor(year) + as.factor(TimUCDP), 
                 model = "logit", data = ImputedData)



#MID Variables



MIDBinary <- zelig(MID_Binary ~ ValueScore +
                 milper + majorpower + cinc + num_mem + land + sea +
                 polity + log(gdpPRcapita) + DeathPena  +
                 as.factor(Country) +  as.factor(year) + as.factor(TimMID), 
                 model = "logit", data = ImputedData)

Highact <- zelig(hiact ~ ValueScore +
                   milper + majorpower + cinc + num_mem + land + sea +
                   polity + log(gdpPRcapita) + DeathPena  +
                   as.factor(Country) +  as.factor(year), 
                 model = "ls", data = ImputedData)


MIDLength <- zelig(length ~ ValueScore +
                   milper + majorpower + cinc + num_mem + land + sea +
                   polity + log(gdpPRcapita) + DeathPena  +
                   as.factor(Country) +  as.factor(year), 
                 model = "ls", data = ImputedData)


MIDFatal <- zelig(fatalpre ~ ValueScore +
                    milper + majorpower + cinc + num_mem + land + sea +
                    DeathPena + polity + log(gdpPRcapita) +
                    as.factor(Country) + as.factor(year),
                  model = "ls", data = ImputedData)



#### To screen ####
texreg::screenreg(l = list(MIDBinary, Highact, MIDLength), omit.coef = "(year)|(Country)|(Tim)", 
                  custom.model.names =  c("MID Binary", "Level of Conflict", "Conflict Length"),
                  custom.header = list("logistic" = 1, "OLS" = 2:3),
                  custom.coef.names = c("(intercept)",
                                        "Value Score",
                                        "Military Personnel",
                                        "Major Power",
                                        "COW: CINC",
                                        "Nr. Allies",
                                        "Borders: Land",
                                        "Borders: Sea",
                                        "Polity",
                                        "ln(GDP/cap)",
                                        "Support capital punishment"))

#### To tex ####


texreg::texreg(l = list(MIDBinary, Highact, MIDFatal, MIDLength), omit.coef = "(year)|(Country)|(Tim)", 
                  custom.model.names =  c("MID Binary", "Level of Conflict", "Fatalities", "Conflict Length"),
                  custom.header = list("logistic" = 1, "OLS" = 2:4),
                  custom.coef.names = c("(intercept)",
                                        "Value Score",
                                        "Military Personnel",
                                        "Major Power",
                                        "COW: CINC",
                                        "Nr. Allies",
                                        "Borders: Land",
                                        "Borders: Sea",
                                        "Polity",
                                        "ln(GDP/cap)",
                                        "Support capital punishment"),
               label = "MIDReg",
               booktabs = TRUE,
               caption = "Regression tables: Military Interstate Disputes",
               use.packages = FALSE,
               file = "MIDreg.tex")




#### UCDP/PRIO ####


UCDPBinary <- zelig(Conflict_Binary ~ ValueScore +
                     milper + majorpower + cinc + num_mem + land + sea +
                     DeathPena  + polity + log(gdpPRcapita) +
                     as.factor(Country) + as.factor(year) + as.factor(TimUCDP), 
                     data = ImputedData)


#texreg::screenreg(UCDPBinary,  omit.coef = "(year)|(Country)|(Tim)")


#ImputedData <- transform.amelia(ImputedData, intre = intensity_level -1)

UCDPInten <- zelig(intre ~ ValueScore +
                     milper + majorpower + cinc + num_mem + land + sea +
                     DeathPena + polity + log(gdpPRcapita) +
                     as.factor(Country) + as.factor(year),
                   model = "logit", data = ImputedData)

texreg::screenreg(UCDPInten,  omit.coef = "(year)|(Country)|(Tim)")



texreg::screenreg(UCDPInten,  omit.coef = "(year)|(Country)|(Tim)")

#### Screenreg UCDP #####

texreg::screenreg(l = list(UCDPBinary,  UCDPInten), omit.coef = "(year)|(Country)|(Tim)", 
               custom.model.names =  c("UCDP/Prio Binary", "Conflict Intensity"),
               custom.header = list("logistic" = 1:2),
               custom.coef.names = c(
                 "(intercept)",
                 "Value Score",
                 "Military Personnel",
                 "Major Power",
                 "COW: CINC",
                 "Nr. Allies",
                 "Borders: Land",
                 "Borders: Sea",
                 "Support capital punishment",
                 "Polity",
                 "GDP/cap"
               )
)


#### Texreg:: UCDP ####

texreg::texreg(l = list(UCDPBinary, UCDPInten), omit.coef = "(year)|(Country)|(Tim)", 
                  custom.model.names =  c("UCDP/Prio Binary", "Conflict Intensity"),
                  custom.header = list("logistic" = 1:2),
                  custom.coef.names = c(
                    "(intercept)",
                    "Value Score",
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
               label = "UCDPreg",
               booktabs = TRUE,
               caption = "Regression tables: UCDP/Prio",
               use.packages = FALSE,
               file = "UCDPreg.tex"
)




#### Robustness Checks ####

UCDPBinaryPost <- zelig(Conflict_Binary ~ Y002 +
                      milper + majorpower + cinc + num_mem + land + sea +
                      DeathPena  + polity + log(gdpPRcapita) +
                      as.factor(Country) + as.factor(year) + as.factor(TimUCDP), 
                    model = "logit",  data = ImputedData)


texreg::screenreg(UCDPBinaryWar,  omit.coef = "(year)|(Country)|(Tim)")




ImputedData <- transform.amelia(ImputedData, MajorWar = ifelse(intensity_level == 2, 1, 0))
ImputedData <- transform.amelia(ImputedData, MajorWar = ifelse(is.na(MajorWar),0, 1))


UCDPBinaryWar <- zelig(MajorWar ~ ValueScore +
                          milper + majorpower + cinc + num_mem + land + sea +
                          DeathPena  + polity + log(gdpPRcapita) +
                          as.factor(Country) + as.factor(year) + as.factor(TimUCDP), 
                        model = "logit",  data = ImputedData)




### War baby ####
texreg::texreg(l = list(UCDPBinaryWar), omit.coef = "(year)|(Country)|(Tim)", 
               custom.model.names =  c("UCDP/Prio War"),
               custom.header = list("logistic" = 1),
               custom.coef.names = c(
                 "(intercept)",
                 "Value Score",
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
               label = "UCDPWar",
               booktabs = TRUE,
               caption = "Regression tables: UCDP/Prio",
               use.packages = FALSE,
               file = "UCDPWar.tex"
)






#### Not imputed <3 ####


#UCDP
SubSet$cnFa <- as.factor(SubSet$Conflict_Binary)
UCDPBinaryNI <- zelig(Conflict_Binary ~ ValueScore +
                      as.factor(Country) + as.factor(year) , 
                    model = "logit",
                    data = SubSet)


MIDBinaryNI <- glm(
  MID_Binary ~ ValueScore + milper + cinc + + num_mem +
    land + sea + polity + log(gdpPRcapita)+
    as.factor(Country) + as.factor(year) + as.factor(TimUCDP),
  family = binomial(link = "logit"),
  na.action = "na.exclude",
  data = SubSet
)
summary(MIDBinaryNI)




texreg::texreg(l = list(MIDBinaryNI), omit.coef = "(year)|(Country)|(Tim)", 
               custom.model.names =  c("MID Binary"),
               custom.header = list("logistic" = 1),
               custom.coef.names = c(
                 "(intercept)",
                 "Value Score",
                 "Military Personnel",
                 "COW: CINC",
                 "Nr. Allies",
                 "Borders: Land",
                 "Borders: Sea",
                 "Polity",
                 "ln(GDP/cap)"
               ),
               label = "MIDni",
               booktabs = TRUE,
               caption = "Regression tables: MID Binary, not imputed",
               use.packages = FALSE,
               file = "MIDni.tex"
)

