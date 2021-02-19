library(tidyverse)
library(haven) #Latinobarometer kommer kun i .sav

Lat95 <- read_sav("Data/LatinoBarometer/Latinobarometro_1995_data_english_spss_v2014_06_27.sav")

Lat95 <- Lat95 %>%
  select(p20, p11, pais, numero, wt) %>%
  rename("SuppDem" = "p20",
         "PrdNat" = "p11",
         "Country" = "pais",
         "year" = "numero",
         "weight" = "wt")


Lat96 <- read_sav("Data/LatinoBarometer/Latinobarometro_1996_datos_english_spss_v2014_06_27.sav")

Lat96 <- Lat96 %>%
  select(p19, p9, pais,numero,wt) %>%
  rename(
    "SuppDem" = "p19",
    "PrdNat" = "p9",
    "Country" = "pais",
    "year" = "numero",
    "weight" = "wt"
  )


Lat97 <- read_sav("Data/LatinoBarometer/Latinobarometro_1997_datos_english_spss_v2014_06_27.sav")



Lat97 <- Lat97 %>%
  select(sp31, sp22, sp69d,
         idenpa, numinves, wt) %>%
  rename(
    "SuppDem" = "sp31",
    "PrdNat" = "sp22",
    "WomenPubOff" = "sp69d",
    "Country" = "idenpa",
    "year" = "numinves",
    "weight" = "wt"
  )


Lat98 <- read_sav("Data/LatinoBarometer/Latinobarometro_1998_datos_english_v2014_06_27.sav")

Lat98 <- Lat98 %>%
  select(sp28, np14b,
         idenpa, numinves, pondera) %>%
  rename(
   "SuppDem" = "sp28",
   "Privat" = "np14b",
   "Country" = "idenpa",
   "year" = "numinves",
   "weight" = "pondera"
  )


Lat00 <- read_sav("Data/LatinoBarometer/Latinobarometro_2000_datos_eng_v2014_06_27.sav")

Lat00 <- Lat00 %>%
  select(P29ST, P39ST, P70ST.D, P16ST.A, IDENPA,NUMINVES, WT) %>%
  rename(
    "SupppDem" = "P29ST",
    "PrdNat" = "P39ST",
    "WomenPubOff" = "P70ST.D",
    "Privat" = "P16ST.A",
    "Country" = "IDENPA",
    "year" = "NUMINVES",
    "weight" = "WT"
  )

Lat01 <- read_sav("Data/LatinoBarometer/Latinobarometro_2001_datos_english_v2014_06_27.sav")

Lat01 <- Lat01 %>%
  select(p46st, p52nusa,p73st,p15sta, idenpa,
         numinves,wt) %>%
  rename("SuppDem" = "p46st",
         "PunGuil" = "p52nusa",
         "PrdNat" = "p73st",
         "Privat" = "p15sta",
         "Country" = "idenpa",
         "year" = "numinves",
         "weight" = "wt")

Lat02 <- read_sav("Data/LatinoBarometer/Latinobarometro_2002_datos_eng_v2014_06_27.sav")


Lat02 <- Lat02 %>%
  select(p32st,p47st,p22sta,idenpa, numinves,wt) %>%
  rename(
    "SuppDem" = "p32st",
    "PrdNat" = "p47st",
    "Privat" = "p22sta",
    "Country" = "idenpa",
    "year" = "numinves",
    "weight" = "wt"
  )

Lat03 <- read_sav("Data/LatinoBarometer/Latinobarometro_2003_datos_eng_v2014_06_27.sav")

Lat03 <- Lat03 %>%
  select(p14st, p22gb.b ,p64st.b, p18st, p26st, idenpa, numinves,wt) %>%
  rename(
    "SuppDem" = "p14st",
    "NoEduPol" = "p22gb.b",
    "PuinGuil" = "p64st.b",
    "PrdNat" = "p18st",
    "Privat" = "p26st",
    "Country" = "idenpa",
    "year" = "numinves",
    "weight" = "wt"
    )


Lat04 <- read_sav("Data/LatinoBarometer/Latinobarometro_2004_datos_eng_v2014_06_27.sav")


Lat04 <- Lat04 %>%
  select(p13st,p17n,p24wvs, p45st, idenpa, numinves,wt) %>%
  rename(
    "SuppDem" = "p13st",
    "SuppMilGv" = "p17n",
    "CntPwr" = "p24wvs",
    "PrdNat" = "p45st",
    "Country" = "idenpa",
    "year" = "numinves",
    "weight" = "wt"
    
  )

Lat05 <- read_sav("Data/LatinoBarometer/Latinobarometro_2005_datos_eng_v2014_06_27.sav")

Lat05 <- Lat05 %>%
  select(p16st, p75st,p22st, p40sta,p13st, p40stc, idenpa,numinves,wt) %>%
  rename(
    "SuppDem" = "p16st",
    "SuppMilGv" = "p75st",
    "CntPwr" = "p22st",
    "PuinGuil" = "p40sta",
    "PrdNat" = "p13st",
    "Private" = "p40stc",
    "Country" = "idenpa",
    "year" = "numinves",
    "weight" = "wt"
  )

Lat06 <- read_sav("Data/LatinoBarometer/Latinobarometro_2006_datos_eng_v2014_06_27.sav")

Lat06 <- Lat06 %>%
  select(p17st, p20stm, p9st, p19st, p77st.a, idenpa, numinves, wt) %>%
  rename(
    "SuppDem" = "p17st",
    "CntPwr" = "p20stm",
    "PrdNat" = "p9st",
    "WomenPubOff" = "p77st.a",
    "Country" = "idenpa",
    "year" = "numinves",
    "weight" = "wt"
  )

Lat07 <- read_sav("Data/LatinoBarometer/Latinobarometro_2007_datos_eng_v2014_06_27.sav")

Lat07 <- Lat07 %>%
  select(p9st, p25n, p16st, p54sta, idenpa, numinves, wt) %>%
  rename(
    "SuppDem" = "p9st",
    "CntPwr" = "p16st",
    "Private" = "p54sta",
    "Country" = "idenpa",
    "year" = "numinves",
    "weight" = "wt"
  )


Lat08 <- read_sav("Data/LatinoBarometer/Latinobarometro_2008_datos_eng_v2014_06_27.sav")

Lat08 <- Lat08 %>%
  select(p13st, p25st, idenpa, numinves, wt) %>%
  rename(
    "SuppDem" = "p13st",
    "CntPwr" = "p25st",
    "Country" = "idenpa",
    "year" = "numinves",
    "weight" = "wt"
  )



Lat09 <- read_sav("Data/LatinoBarometer/Latinobarometro_2009_datos_eng_v2014_06_27.sav")

Lat09 <- Lat09 %>%
  select(p10st, p21st, p22st, idenpa,
         numinves,wt) %>%
  rename(
    "SuppDem" = "p10st",
    "SuppMilGv" = "p21st",
    "CntPwr" = "p22st",
    "Country" = "idenpa",
    "year" = "numinves",
    "weight" = "wt"
  )

Lat10 <- read_sav("Data/LatinoBarometer/Latinobarometro_2010_datos_eng_v2014_06_27.sav")
