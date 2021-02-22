### AfroBarometer ###

####Pakker####
library(tidyverse)
library(haven)

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

Afro1 <- as_factor(Afro1)


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

Afro1Clean <- Afro1 %>%
  select(supdem, rejtrd, gidprd, gidbes, gidstr, withinwt, country) %>%
  rename(
    "suppDem_Afro" = "supdem",
    "TradRule_Afro" = "rejtrd",
    "GrpPrd_Afro" = "gidprd",
    "GrpBst_Afro" = "gidbes",
    "GrpNat_Afro" = "gidstr",
    "weight_Afro" = "withinwt",
    "country_Afro" = "country"
    
  ) %>%
  mutate(across(suppDem_Afro:GrpNat_Afro, ~ifelse(.x > 89, NA, .x))) %>%
  mutate(
    suppDem_Afro = recode(suppDem_Afro, 
    '3' = 1,
    '2' = 2,
    '1' = 3,
    '4' = 99
  ),
    TradRule_Afro = recode(TradRule_Afro,
      '6' = 99
    ),
  GrpPrd_Afro = recode(GrpPrd_Afro,
    '9' = 98
    ),
  GrpBst_Afro = recode(GrpBst_Afro,
                  '9' = 98
                  ),
  recode(GrpPrd_Afro,
         '9' = 98
         
         )
  )
  

#Afro2
# q25e #Violence Political
# q38 #SuppDem
# q47 #People must obey laws
# q65 #Groupp tratet unfair
# q57 #Group or national
# withinwt

#Afro3
# q24 #Women/Men Polleader
# q22 #All/Educated vote
# q81 #Etnich unfair
# withingwght

# Afro4
# q30 support democracy
# withingwh

#Afro5


# q26e PolViolence
# q32 support democracy
# q78 violence ok/not
# q85c proud national
# withinweight

#Afro6

# q30 supdem
# q88a Ethinic group unfair
# q88b Ethinc or national
# withinwht


# Afr07

#q28 suppdem
