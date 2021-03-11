### AfroBarometer ###

####Pakker####
library(tidyverse)
library(haven)
library(car)
library(countrycode)

#####Data####


Afro1 <- read_spss("Data/AfroBarometer/merged_r1_data.sav")
Afro1$country <- as_factor(Afro1$country)
Afro2 <- read_spss("Data/AfroBarometer/merged_r2_data.sav")
Afro2$country <- as_factor(Afro2$country)
Afro3 <- read_spss("Data/AfroBarometer/merged_r3_data.sav")
Afro3$country <- as_factor(Afro3$country)
Afro4 <- read_spss("Data/AfroBarometer/merged_r4_data.sav")
Afro4$COUNTRY <- as_factor(Afro4$COUNTRY)
Afro5 <- read_spss("Data/AfroBarometer/merged_r5_data.sav")
Afro5$COUNTRY <- as_factor(Afro5$COUNTRY)
Afro6 <- read_spss("Data/AfroBarometer/merged_r6_data.sav")
Afro6$COUNTRY <- as_factor(Afro6$COUNTRY)
Afro7 <- read_spss("Data/AfroBarometer/merged_r7_data.sav")
Afro7$COUNTRY <- as_factor(Afro7$COUNTRY)

#Afro1 <- as_factor(Afro1)


#### Velge Variabler ####
#Må bruke spss som kodebok, da den (ikke) gitt fra Afrobarometer
# er veldig vanskelig å tyde/lese


#Afro1
# suppdem
# rejtrd #Traditional Rule
# gidprd #Proud Group
# gidbes #Group is best
# gidstr #Group rather than national
# withinwt #Within country weight
#country
#Må legge til år

Afro1Country <- as_factor(Afro1$country)

Afro1Clean <- Afro1 %>%
  select(supdem, rejtrd, gidprd, gidbes, gidstr, withinwt) %>%
  rename(
    "suppDem_Afro" = "supdem",
    "TradRule_Afro" = "rejtrd",
    "GrpPrd_Afro" = "gidprd",
    "GrpBst_Afro" = "gidbes",
    "GroupNat_Afro" = "gidstr",
    "weight_Afro" = "withinwt"
    
  ) %>%
  mutate(
    suppDem_Afro = recode(suppDem_Afro, 
    "3 = 1;
    2 = 2;
    1 = 3;
    4 = 99"
  ),
    TradRule_Afro = recode(TradRule_Afro,
      "6 = 99"
    ),
  GrpPrd_Afro = recode(GrpPrd_Afro,
    "9 = 98"
    ),
  GrpBst_Afro = recode(GrpBst_Afro,
                  "9 = 98"
                  )
         
         ) %>%
  
  mutate(across(suppDem_Afro:GroupNat_Afro, ~ifelse(.x > 89, NA, .x))) %>%
  mutate(country_Afro = Afro1Country) %>%
  mutate(year = 1999)

#Afro2
# q25e #Violence Political remove 9, <97 >-0
# q38 #SuppDem remove 9 <97 >0
#q75 police/revenge 5, 9 < 97 >0 = NA
# q57 #Group or national 0 national, 1 group
# withinwt

Afro2Countries <- as_factor(Afro2$country)

Afro2Clean <- Afro2 %>%
  select(q25e, q38, q75, q57 ,withinwt) %>%
  mutate(across(everything(), ~car::recode(.x,
    "c(9, 97,98,99,-1) = NA"
  ))) %>%
  mutate(q75 = ifelse(q75 == 5, NA, q75)) %>%
  mutate(country_Afro = Afro2Countries) %>%
  rename(
    "PolViol_Afro" = "q25e",
    "suppDem_Afro" = "q38",
    "PoliRev_Afro" = "q75",
    "GroupNat_Afro" = "q57",
    "weight_Afro" = "withinwt"
  ) %>%
  mutate(year = 2004)




#Afro3
# q24 #Women/Men Polleader 5,9 <90, -1
# q22 #All/Educated vote same as above
# q81 #Etnich unfair -1, 7 <
# withingwght
Afro3Countries <- as_factor(Afro3$country)
Afro3Clean <- Afro3 %>%
  select( q22, q81, withinwt) %>%
  mutate(across(everything(), ~recode(.x,
         "c(9,-1,97,98,99, 998, 997, 998) = NA"))) %>%
  mutate(year = 2005,
        country_Afro = Afro3Countries ) %>%
  rename(
    
    "AlEduVote_Afro" = "q22",
    "EthnUnfair_Afro" = "q81",
    "weight_Afro" = "withinwt"
  )
  
  
  
  
# Afro4
# q30 support democracy
# withingwh
Afro4Country <- as_factor(Afro4$COUNTRY)
Afro4Clean <- Afro4 %>%
  select(Q30, Withinwt) %>%
  mutate(Q30 = ifelse(Q30 > 3 | Q30 < 1, NA, Q30)) %>%
  rename("suppDem_Afro" = "Q30",
          "weight_Afro" = "Withinwt") %>%
  mutate(year = 2008,
         country_Afro = Afro4Country)



#Afro5


# q26e PolViolence -1, 9, 998
# q32 support democracy 
# q78 violence ok/not -1, 5, 9, 998
# q85c proud national-1, , 9, 998
# withinweight
Afro5Country <- as_factor(Afro5$COUNTRY)

Afro5Clean <- Afro5 %>%
  select(Q26E, Q32, Q78, Q85C, withinwt) %>%
  mutate(across(everything(), ~recode(.x,
    "c(-1, 5, 9, 998) = NA"
  ))) %>%
  rename(
    "PolViol_Afro" = "Q26E",
    "suppDem_Afro" = "Q32",
    "VilOK" = "Q78",
    "NatPrd" = "Q85C",
    "weight_Afro" = "withinwt"
  ) %>%
  mutate(country_Afro = Afro5Country,
         year = 2011)
  



#Afro6

# q30 supdem -1, 9, 98
# q88a Ethinic group unfair -1, 7, 9, 98, 99
# q88b Ethinc or national -1, 9, 98, 99, snu
# withinwht
afro6Countries <- as_factor(Afro6$COUNTRY)


Afro6Clean <- Afro6 %>%
  select(Q30, Q88A, Q88B, withinwt) %>%
  mutate(across(everything(), ~recode(.x,
                                      "c(-1, 9,98, 99, 7) = NA"))) %>%
  mutate(Q88B = car::recode(Q88B,
        "1 = 5;
        2 = 4;
        3 = 3;
        4 = 2;
        5 = 1"
  )) %>%
  rename(
    "suppDem_Afro" = "Q30",
    "EthnUnfair_Afro" = "Q88A",
    "EthnNat_Afro" = "Q88B",
    "weight_Afro" = "withinwt"
    
  ) %>%
  mutate(year = 2016,
         country_Afro = afro6Countries)
# Afr07

#q28 suppdem

Afro7Countries <- as_factor(Afro7$COUNTRY)
Afro7Clean <- Afro7 %>%
  select(Q28, withinwt) %>%
  mutate( Q28 = ifelse(Q28 %in% c(-1,8,9), NA, Q28)) %>%
  rename("suppDem_Afro" = "Q28",
         "weight_Afro" = "withinwt") %>%
  mutate(year = 2019, 
         country_Afro = Afro7Countries)


AfroBarometer <- bind_rows(Afro1Clean, Afro2Clean, Afro3Clean,
                           Afro4Clean, Afro5Clean, Afro6Clean, Afro7Clean)



AfroBarometer$country <- countrycode(AfroBarometer$country_Afro, origin = "country.name", destination = "iso3n")


AfroBarometer_Mean <- AfroBarometer %>%
  group_by(country, year) %>%
  summarise(across(everything(), ~weighted.mean(.x, w = weight_Afro ,na.rm = TRUE))) %>%
  select(- weight_Afro) %>%
  mutate(across(everything(), ~ifelse(is.nan(.x), NA, .x)))

saveRDS(AfroBarometer_Mean, "AfroBarometer.rds")
AfroBarometer$weight_Afro <- NULL
