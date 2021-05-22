library(Amelia)
library(tidyverse)
library(Zelig)
library(hutilscpp)
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






UCDP_1 <- zelig(Conflict_Binary ~ ValueScore + TimUCDP + (TimUCDP^2) + (TimUCDP^3),
                model = "logit",
                data = ImputedData) 


UCDP_1_LPM <- zelig(Conflict_Binary ~ ValueScore + TimUCDP + (TimUCDP^2) + (TimUCDP^3),
                model = "ls",
                data = ImputedData)


texreg::screenreg(l = list(UCDP_0, UCDP_0_LPM))
texreg::screenreg(l = list(UCDP_1, UCDP_1_LPM))





UCDP_2 <- zelig(Conflict_Binary ~ ValueScore + TimUCDP + (TimUCDP^2) + (TimUCDP^3) +
                  polity,
                model = "logit",
                data = ImputedData) 




UCDP_2_LPM <- zelig(Conflict_Binary ~ ValueScore + log(TimUCDP) +
                      polity,
                    model = "ls",
                    data = ImputedData)



texreg::screenreg(l = list(UCDP_2, UCDP_2_LPM))



UCDP_3 <- zelig(Conflict_Binary ~ ValueScore + TimUCDP + (TimUCDP^2) + (TimUCDP^3) +
                  polity +
                  log(gdpPRcapita),
                model = "logit",
                data = ImputedData) 





#Fixed effects 

UCDP_1_FE <- zelig(Conflict_Binary ~ ValueScore + 
                     log(TimUCDP) + 
                     as.factor(Country)+ 
                     as.factor(year) ,
                model = "logit",
                data = ImputedData) 



UCDP_2_FE <- zelig(Conflict_Binary ~ ValueScore + 
                     log(TimUCDP)  +
                     as.factor(Country) +
                     as.factor(year) +
                  polity,
                model = "logit",
                data = ImputedData) 




UCDP_3_FE <- zelig(Conflict_Binary ~ ValueScore + log(TimUCDP) +
                     as.factor(Country) +
                     as.factor(year) +
                     polity +
                     log(gdpPRcapita),
                model = "logit",
                data = ImputedData) 



# Military Power

UCDP_4 <- zelig(Conflict_Binary ~ ValueScore + TimUCDP + (TimUCDP^2) + (TimUCDP^3) +
                  cinc + majorpower + log(gdpPRcapita),
                model = "logit",
                data = ImputedData)



UCDP_5 <- zelig(Conflict_Binary ~ ValueScore + TimUCDP + (TimUCDP^2) + (TimUCDP^3) +
                  cinc + majorpower + log(gdpPRcapita) + num_mem + sea + land,
                model = "logit",
                data = ImputedData)




UCDP_4_FE <- zelig(Conflict_Binary ~ ValueScore + TimUCDP + (TimUCDP^2) + (TimUCDP^3) +
                  cinc + majorpower + log(gdpPRcapita) +
                    as.factor(Country) + as.factor(year),
                model = "logit",
                data = ImputedData)



UCDP_5_FE <- zelig(Conflict_Binary ~ ValueScore + TimUCDP + (TimUCDP^2) + (TimUCDP^3) +
                  cinc + majorpower + log(gdpPRcapita) + num_mem + sea + land +
                    as.factor(Country) + as.factor(year),
                model = "logit",
                data = ImputedData)


#Complete model!


UCDP_6 <- zelig(Conflict_Binary ~ ValueScore + TimUCDP + (TimUCDP^2) + (TimUCDP^3) +
                  polity + log(gdpPRcapita) + num_mem + sea + land + cinc + majorpower,
                model = "logit",
                data = ImputedData)


UCDP_6_FE <- zelig(Conflict_Binary ~ ValueScore + log(TimUCDP) + 
                  polity + log(gdpPRcapita) + num_mem + sea + land + cinc + majorpower +
                  as.factor(Country) + as.factor(year),
                model = "logit",
                data = ImputedData)

#### MID #####


MID_1 <- zelig(MID_Binary ~ ValueScore + TimMID + (TimMID^2) + (TimMID^3),
               model = "logit",
               data = ImputedData) 





MID_2 <- zelig(MID_Binary ~ ValueScore + TimMID + (TimMID^2) + (TimMID^3) +
                 polity,
               model = "logit",
               data = ImputedData) 










MID_3 <- zelig(MID_Binary ~ ValueScore + TimMID + (TimMID^2) + (TimMID^3) +
                 polity +
                 log(gdpPRcapita),
               model = "logit",
               data = ImputedData) 





#Fixed effects 

MID_1_FE <- zelig(MID_Binary ~ ValueScore + 
                    log(TimMID) + 
                    as.factor(Country)+ 
                    as.factor(year) ,
                  model = "logit",
                  data = ImputedData) 



MID_2_FE <- zelig(MID_Binary ~ ValueScore + 
                    log(TimMID)  +
                    as.factor(Country) +
                    as.factor(year) +
                    polity,
                  model = "logit",
                  data = ImputedData) 




MID_3_FE <- zelig(MID_Binary ~ ValueScore + log(TimMID) +
                    as.factor(Country) +
                    as.factor(year) +
                    polity +
                    log(gdpPRcapita),
                  model = "logit",
                  data = ImputedData) 



# Military Power

MID_4 <- zelig(MID_Binary ~ ValueScore + TimMID + (TimMID^2) + (TimMID^3) +
                 cinc + majorpower + log(gdpPRcapita),
               model = "logit",
               data = ImputedData)



MID_5 <- zelig(MID_Binary ~ ValueScore + TimMID + (TimMID^2) + (TimMID^3) +
                 cinc + majorpower + log(gdpPRcapita) + num_mem + sea + land,
               model = "logit",
               data = ImputedData)




MID_4_FE <- zelig(MID_Binary ~ ValueScore + TimMID + (TimMID^2) + (TimMID^3) +
                    cinc + majorpower + log(gdpPRcapita) +
                    as.factor(Country) + as.factor(year),
                  model = "logit",
                  data = ImputedData)



MID_5_FE <- zelig(MID_Binary ~ ValueScore + TimMID + (TimMID^2) + (TimMID^3) +
                    cinc + majorpower + log(gdpPRcapita) + num_mem + sea + land +
                    as.factor(Country) + as.factor(year),
                  model = "logit",
                  data = ImputedData)


#Complete model!


MID_6 <- zelig(MID_Binary ~ ValueScore + TimMID + (TimMID^2) + (TimMID^3) +
                 polity + log(gdpPRcapita) + num_mem + sea + land + cinc + majorpower,
               model = "logit",
               data = ImputedData)


MID_6_FE <- zelig(MID_Binary ~ ValueScore + log(TimMID) + 
                    polity + log(gdpPRcapita) + num_mem + sea + land + cinc + majorpower +
                    as.factor(Country) + as.factor(year),
                  model = "logit",
                  data = ImputedData)


