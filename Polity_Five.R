library(tidyverse)
library(readxl)
#Laste inn polity V

Polity <- read_excel("Data/p5v2018.xls")

PolitySelected <- Polity %>%
  select(ccode, year, democ, polity) %>%
  filter(year > 1979)
saveRDS(PolitySelected, "politySelected.rds")
