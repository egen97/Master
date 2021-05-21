library(Amelia)
library(tidyverse)
library(Zelig)
load("Data/Imputed/PostMatinclude.RData")

##### Tidsvariabel ####

ImputedData <- transform.amelia(ImputedData,
                                Change_UCDP =
                                  with(
                                    ImputedData, c(FALSE, Conflict_Binary[-1L] != Conflict_Binary[-length(Conflict_Binary)])
                                  ),
                                Change_MID =
                                  with(
                                    ImputedData, c(FALSE, MID_Binary[-1L] != MID_Binary[-length(MID_Binary)])
                                  )
                                
)

#Lage en for land as well

ImputedData <- transform.amelia(ImputedData,
                                Change_Country =
                                  with(
                                    ImputedData, c(FALSE, Country[-1L] != Country[-length(Country)])
                                  )
                                
                                
)




ImputedData <- transform.amelia(ImputedData,
                                Change_UCDP = ifelse(year == 1980, FALSE, Change_UCDP),
                                Change_MID = ifelse(year == 1980, FALSE, Change_MID)
                                
                                )


#Lager en variabel som viser om konfliktstatus har forandret seg fra året før. Siden dataene er sortert i riktig år-land rekkefølge
#kan en bare basere seg på raden før. Må sette 1980 til FALSE manuelt sånn at de som har konflikt ikke skal bli sammenlignet med
#landet før i 2020 



ImputedData <- transform.amelia(ImputedData,
                                
  TimUCDP = cumsum_reset(!(Change_UCDP | Change_Country)),
  TimMID = cumsum_reset(!(Change_MID | Change_Country))
  
  
)

#Legge til en kumulativ sum som endres når konfliktstatus forandrer seg


ImputedData <- transform.amelia(ImputedData,
  
  TimUCDP = ifelse(Change_UCDP, 1 + lag(TimUCDP, k = 1), TimUCDP),
  TimMID = ifelse(Change_MID , 1 + lag(TimMID, k = 1), TimMID)
  
  
)

ImputedData <- transform.amelia(ImputedData,
                                TimUCDP = ifelse(Change_Country, 1, TimUCDP),
                                TimMID = ifelse(Change_Country , 1 , TimMID)
                                
  
)
#Tok det mindre enn 10 evigheter? Nei. Har vi nå en tidsvariabel? Ja <3 

##### UCDP Modeller #####


UCDP_0 <- zelig(Conflict_Binary ~ ValueScore,
                model = "logit",
                data = ImputedData) #Denne er faktisk signifikant


UCDP_0_LPM <- zelig(Conflict_Binary ~ ValueScore,
                    model = "ls",
                    data = ImputedData)


#####Tid gir complete seperation siden P(Conflict|ln(t) = 0) = 1#####



UCDP_1 <- zelig(Conflict_Binary ~ ValueScore + log(TimUCDP),
                model = "logit",
                data = ImputedData) #Algoritm did not converge: Complete seperation


UCDP_1_LPM <- zelig(Conflict_Binary ~ ValueScore + log(TimUCDP),
                model = "ls",
                data = ImputedData)


#Tid + FE

UCDP_2 <- zelig(Conflict_Binary ~ ValueScore + #Algoritm did not converge: Complete seperation
                log(TimUCDP) +
                as.factor(year) +
                as.factor(Country)  ,
                model = "logit",
                data = ImputedData) 


UCDP_2_LPM <- zelig(Conflict_Binary ~ ValueScore + 
                    log(TimUCDP) +
                    as.factor(year) +
                    as.factor(Country),
                    model = "ls",
                    data = ImputedData)

summary(UCDP_2)
summary(UCDP_2_LPM)





texreg::screenreg(UCDP_0)

#### Fixed Effect

UCDP_1 <- zelig(Conflict_Binary ~ ValueScore +
                as.factor(year) +
                as.factor(country),
                model = "logit",
                data = ImputedData)


texreg::screenreg(UCDP_1, omit.coef = "(year)|(Country)")
#Fixed Effects fjerner effekten




### Alle kontroller men ikke FE

UCDP_2 <- zelig(Conflict_Binary ~ ValueScore +
                  milper + 
                  majorpower +
                  cinc + 
                  num_mem + 
                  land + 
                  sea +
                  DeathPena + 
                  polity + 
                  log(gdpPRcapita),
                  model = "logit",
                  data = ImputedData)


texreg::screenreg(UCDP_2, omit.coef = "(year)|(Country)")
#Endelig er kontrollvariablene signifikante, men ikke value-score

# Alle + FE


UCDP_3 <- zelig(Conflict_Binary ~ ValueScore + #Complete seperation
                  milper + 
                  majorpower +
                  cinc + 
                  num_mem + 
                  land + 
                  sea +
                  DeathPena + 
                  polity + 
                  log(gdpPRcapita) +
                  as.factor(Country) +
                  as.factor(year),
                model = "logit",
                data = ImputedData)


texreg::screenreg(UCDP_3, omit.coef = "(year)|(Country)",
                  custom.gof.rows = list("Num. obs." = 2),
                  include.nobs = F) #For å endre anall observasjoner :D







