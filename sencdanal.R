#install.packages("lme4")
library(lme4)

WVS <- readRDS("WVS_EVS.rds")

SubSet <- WVS %>%
  select(FightCountry, DeathPena, starts_with("Value"), Weights, TrstArm, S024,
         starts_with("X"), Age)






SubSet <- SubSet %>%
  mutate(ValueScore = ValueRisk + ValueSucses + ValueGodTim + ValueSecur)


# "X047"   Income Scale
# "X025R"  Education
# "X003"   Age
# "X001"   Sex





Mod1 <- glm(
  FightCountry ~ ValueScore + X001 + X025R +X047 + Age,
  na.action = "na.exclude",
  family = binomial(link = "logit"),
  data = SubSet
)

summary(Mod1)


Mod2 <- glm(
  FightCountry ~ ValueScore + X001 + X025R +X047 + Age +
    as.factor(S024),
  na.action = "na.exclude",
  family = binomial(link = "logit"),
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



