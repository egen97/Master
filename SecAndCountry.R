library(tidyverse)
library(Zelig)
# Mod3 <- zelig(
#   FightCountry ~ ValueScore + X001 + X025R +X047 + Age,
#   model = "logit",
#   weights = SubSet$S018,
#   data = SubSet
# )

Mod1 <- zelig(FightCountry ~ ValueScore,
              model = "ls",
              data = ImputedData)

texreg::screenreg(Mod1)

Mod2 <- zelig(FightCountry ~ ValueScore +
                polity +
                log(gdpPRcapita),
              model = "ls",
              data = ImputedData)

texreg::screenreg(Mod2)


Mod3 <- zelig(FightCountry ~ ValueScore +
                polity +
                log(gdpPRcapita) +
                as.factor(Country) +
                as.factor(year),
              model = "ls",
              data = ImputedData)

texreg::screenreg(Mod3, omit.coef = "year|Country")
