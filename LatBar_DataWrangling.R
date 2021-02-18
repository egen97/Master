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
