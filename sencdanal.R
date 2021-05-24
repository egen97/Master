#install.packages("lme4")
library(lme4)

WVS <- readRDS("WVS_EVS.rds")

SubSet <- WVS %>%
  select(FightCountry, DeathPena, starts_with("Value"), Weights, TrstArm, S024,
         starts_with("X"), Age)






SubSet <- SubSet %>%
  mutate(ValueScore = (ValueRisk*(0.638)) + (ValueSucses*0.432) + (ValueGodTim*0.420) + (ValueSecur))


# "X047"   Income Scale
# "X025R"  Education
# "X003"   Age
# "X001"   Sex





Mod1 <- glm(
  FightCountry ~ ValueScore + X001 + X025R +X047 + Age,
  na.action = "na.exclude",
  family = binomial(link = "logit"),
  #weights = S018,
  data = SubSet
)

summary(Mod1)


Mod2 <- glm(
  FightCountry ~ ValueScore + X001 + X025R +X047 + Age +
    as.factor(S024),
  na.action = "na.exclude",
  family = binomial(link = "logit"),
  #weights = S018,
  data = SubSet
)


Mod3 <- zelig(
  FightCountry ~ ValueScore + X001 + X025R +X047 + Age,
  model = "logit",
  weights = SubSet$S018,
  data = SubSet
)


summary(Mod2)


texreg::texreg(Mod2,
                  omit.coef = ("S024"),
                  custom.model.names = "Fight For Country",
                  custom.coef.names = c(
                    "(intercept)",
                    "Value Score",
                    "Gender",
                    "Education",
                    "Income",
                    "Age"
                  ),
                  label = "FCreg",
                  booktabs = TRUE,
                  caption = "Willigness to fight for own country",
                  use.packages = FALSE,
                  file = "FCreg.tex")


# exreg::texreg(l = list(MID_6, MID_6_FE), omit.coef = c("Country|year"),
#               custom.coef.names = c(
#                 "(intercept)",
#                 "Value Score",
#                 "Time since conflict change",
#                 "Polity Score",
#                 "ln(GDP/cap)",
#                 "Nr. Allies",
#                 "Borders: Sea",
#                 "Borders: Land",
#                 "CINC",
#                 "Major Power"
#               ),
#               custom.gof.rows = list("Num. obs" = c(3734, 3734),
#                                      "AIC" = aic_list_MID[11:12]),
#               include.nobs = F,
#               custom.model.names = c("Model 6", "Model 6 FE"),
#               booktabs = TRUE,
#               use.packages = FALSE,
#               custom.note = "%stars \n Fixed effects on country and year",
#               label = "MID_3",
#               caption = "MID: Complete model",
#               file = "MID_3.tex")

texreg::screenreg(l = list(Mod1, Mod2), omit.coef = "S024",
                  custom.coef.names = 
                    c("(intercept)",
                    "Value Score",
                    "Gender",
                    "Education",
                    "Income",
                    "Age"))
texreg::texreg(l = list(Mod1, Mod2), omit.coef = "S024",
                  custom.coef.names = 
                    c("(intercept)",
                      "Value Score",
                      "Gender",
                      "Education",
                      "Income",
                      "Age"),
               custom.model.names = c("Model 1", "Model 1 FE") ,
               booktabs = TRUE,
               use.packages = FALSE,
               label = "FCREG",
               caption = "Individual level: Fight for Country",
               file = "FCREG.tex")


