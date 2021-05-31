#### Loading and wrangling data #### 
WVS <- readRDS("WVS_EVS.rds")

AnaDat <- WVS %>%
  select(FightCountry, DeathPena, starts_with("Value"), Weights, TrstArm, S024,
         starts_with("X"), Age, S018)


WVSSelected <- WVSSelected %>%
  mutate(ValueScore = (ValueRisk*(0.638)) + (ValueSucses*0.432) + (ValueGodTim*0.420) + (ValueSecur))


# "X047"   Income Scale
# "X025R"  Education
# "X003"   Age
# "X001"   Sex
##### Models ##### 
Mod1 <- glm(
  FightCountry ~ ValueScore,
  na.action = "na.exclude",
  family = binomial(link = "logit"),
  weights = S018,
  data = WVSSelected
)

summary(Mod1)

Mod2 <- glm(
  FightCountry ~ ValueScore +
    X047 +
    X025R +
    X001 +
    Age,
  na.action = "na.exclude",
  family = binomial(link = "logit"),
  weights = S018,
  data = WVSSelected
)

summary(Mod2)

#### Fixed Effects ####

Mod1_FE <- glm(
  FightCountry ~ ValueScore +
    as.factor(S024),
  na.action = "na.exclude",
  family = binomial(link = "logit"),
  weights = S018,
  data = WVSSelected
)

summary(Mod1_FE)

Mod2_FE <- glm(
  FightCountry ~ ValueScore +
    X047 +
    X025R +
    X001 +
    Age +
    as.factor(S024),
  na.action = "na.exclude",
  family = binomial(link = "logit"),
  weights = S018,
  data = WVSSelected
)

summary(Mod2_FE)





##### Tabeller ####
texreg::screenreg(l = list(Mod1, Mod2),
                  custom.coef.names = c(
                    "(intercept)",
                    "Value Score",
                    "Income (deciles)",
                    "Education",
                    "Sex",
                    "Age"
                  ),
                  include.bic = FALSE,
                  include.deviance = FALSE
                  )




