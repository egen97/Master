library(tidyverse)
library(plm)
library(countrycode)
theme_set(theme_classic())

MIL <- read.csv("Data/MID/MIDB 5.0.csv")

MILNew <- MIL %>%
  filter(styear >= 1980)


MILNew %>%
  filter(stabb == "USA") %>%
  View()

#Side A viser hvem som starter


MILNew %>%
  filter(stabb == "USA") %>%
  ggplot(aes(as.factor(sidea))) +
  geom_bar(width = 0.5, fill = "firebrick2") +
  theme_classic() +
  labs(y = "", y = "Intiater", title = "US intiations vs. response to MID") +
  scale_x_discrete(labels = c('Respond','Initiate'), name = "")



MILNew %>%
  filter(stabb == "NOR") %>%
  ggplot(aes(as.factor(sidea))) +
  geom_bar(width = 0.5, fill = "firebrick2") +
  theme_classic() +
  labs(y = "", y = "Intiater", title = "NO intiations vs. response to MID") +
  scale_x_discrete(labels = c('Respond','Initiate'), name = "")




MILNew %>%
  select(sidea, styear) %>%
  group_by(styear) %>%
  summarise(midpr = sum(sidea, nr.rm = TRUE)) %>%
  ggplot(aes(styear, midpr)) +
  geom_line(size = 1, colour = "firebrick2") +
  geom_smooth(se = FALSE, colour = "springgreen1")


MIL %>%
  mutate(nrr = 1) %>%
  select(nrr, styear) %>%
  group_by(styear) %>%
  summarise(nrr = sum(nrr, nr.rm = TRUE)) %>%
  ggplot(aes(styear, nrr)) +
  geom_line(size = 1, colour = "firebrick2") +
  geom_smooth(se = FALSE, colour = "springgreen1")


#Vil en ha hvor mange som er *startet* i ett gitt år, eller hvor mange som er *pågående* i ett år?


# presidents %>%
#   mutate(year = map2(from, to, seq)) %>%
#   unnest(year) %>%
#   select(-from, -to)


MilYear <- MILNew %>%
  mutate(length = endyear - styear,
         length = ifelse(length == 0, 1, length)) %>%
  mutate(year = map2(styear, endyear, seq)) %>%
  unnest(year) 


MilYear %>%
  filter(stabb == "POL") %>%
  filter(styear == 2010) %>%
  View()



MilExpand <- make.pbalanced(MilYear, index = c("ccode", "year"))


MilExpand %>%
  mutate(ConfBiary = ifelse(is.na(styear), 1, 0)) %>%
  group_by(year) %>%
  summarise(nrconf = sum(ConfBiary, na.rm = TRUE) ) %>%
  ggplot(aes(year, nrconf)) +
  geom_col(width = .9, fill = "deepskyblue2") +
  labs(title = "Antall pågående konflitker pr. år", subtitle = "COW MID v5", x = "Year", y = "Nr. of conflicts")


MilExpand$Country <- countrycode(MilExpand$ccode, origin = "cown", destination = "iso3n")


MilDone <- MilExpand %>%
  select(dispnum, Country, year, length, sidea, hiact, hostlev, fatality, fatalpre)

#saveRDS(MilDone, "Data/MID_Data.rds")


MilDonner <- MilDone %>%
  filter(sidea == 1 | is.na(sidea)) %>%
  select(-sidea) %>%
  mutate(across(everything(), ~ifelse(is.na(.x) | .x < 0, 0, .x)))

saveRDS(MilDonner, "Data/MID_Data.rds")
