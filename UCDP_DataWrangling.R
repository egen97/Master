library(tidyverse)
library(plm)
library(countrycode)

load("Data/ucdp-prio-acd-201.RData")


#Kan først bare fjerne intrastate krig, og år før 1980

War <- ucdp_prio_acd_201 %>%
  filter(type_of_conflict == 2 & year > 1979| type_of_conflict == 4 & year > 1979)
#Bør kanskje ha med internationalized, og så kode kun side_a2/side_b2

War <- War %>%
  select(conflict_id, side_a, side_b, side_a_2nd, side_b_2nd, starts_with("gwno"), intensity_level, type_of_conflict, year) %>%
  mutate(across(everything(), ~ifelse(.x == -99, NA, .x)))

War <- War %>%
  mutate(side_a = ifelse(type_of_conflict == 4, side_a_2nd, side_a),
         side_b = ifelse(type_of_conflict ==  4, side_b_2nd, side_b)) 

War <- War %>%
  separate_rows(side_a, sep = ",") %>%
  separate_rows(side_b, sep = ",") %>%
  select(side_a, side_b, year, conflict_id, type_of_conflict, intensity_level)

War <- War %>%
  unite(Country, side_a:side_b, sep = "_") %>%
  separate_rows(Country, sep = "_")

War$Conflict_Binary <- 1


War <- make.pbalanced(War, index = c("Country", "year") )

War$Conflict_Binary <- ifelse(is.na(War$Conflict_Binary), 0, War$Conflict_Binary)

War <- War %>%
 mutate(Country = str_remove_all(Country, "Government of "))

War$CountryNum <- countrycode(War$Country, origin = "country.name", destination = "iso3n" )

saveRDS(War, "Data/ConflictData.rds")
