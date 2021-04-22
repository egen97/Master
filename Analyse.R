library("Zelig")
library(Amelia)
library("stargazer")
library(tidyverse)
#z5$zelig(tariff ~ polity + pop + gdp.pc + year + country, data = a.out)
ImputedData <- r

#a.out <- transform(a.out, lgdp = log(gdp.pc))

ImputedData <- transform.amelia(ImputedData, ValueScore =  (ValueRisk + ValueSucses + ValueGodTim + ValueSecur)*-1)


ImputedData <- transform.amelia(ImputedData, MID_Binary = ifelse(!is.na(dispnum) & fatality > 1, 1, 0))


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
                as.factor(Country) + 
               as.factor(year), model = "logit", data = ImputedData)





SNAFU <- zelig(MID_Binary ~ ValueScore +
                 milper + majorpower + DeathPena  + polity + gdpPRcapita +
                 as.factor(Country) + 
                 as.factor(year), model = "logit", data = ImputedData)



SNAFU <- zelig(MID_Binary ~ ValueScore +
                 milper + majorpower + DeathPena  + polity + gdpPRcapita +
                 as.factor(Country) + 
                 as.factor(year), model = "logit", data = ImputedData)



TARFU <- zelig(length ~ ValueScore +
                 milper + majorpower + DeathPena  + polity + gdpPRcapita +
                 as.factor(Country) + 
                 as.factor(year), model = "ls", data = ImputedData)





texreg::texreg(FUBAR)
texreg::screenreg(l = list(SNAFU, FUBAR, TARFU), omit.coef = "(year)|(Country)", custom.model.names =  c("MID Binary", "UCDP/PRIO Binary", "Conflict Length"))



Df <- ImputedData$imputations$imp1
Df <- as.data.frame(Df)
ggplot(Df ) +
  geom_smooth(aes(year, ValueScore, colour = as.factor(Country)), alpha = .2, size = .01, se = FALSE) +
  geom_smooth(aes(year, ValueScore), size = 3, colour = "firebrick2", fill = "firebrick1", alpha = .7) +
  guides(colour = FALSE)


