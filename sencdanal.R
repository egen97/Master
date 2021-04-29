install.packages("lme4")
library(lme4)

WVS <- readRDS("WVS_EVS.rds")

SubSet <- WVS %>%
  select(FightCountry, DeathPena, starts_with("Value"), Weights, TrstArm, S024)


SubSet <- SubSet %>%
  mutate(ValueScore = ValueRisk + ValueSucses + ValueGodTim + ValueSecur)


Mod1 <- lm(FightCountry ~ ValueScore,  SubSet, na.action = "na.exclude")
Mod2 <- lm(FightCountry ~ ValueScore + TrstArm,  SubSet, na.action = "na.exclude")
summary(Mod2)
summary(Mod1)

