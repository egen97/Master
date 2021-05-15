library(tidyverse)
library(countrycode)
library(ggthemes)
library(viridis)

DFlist <- ImputedData[[1]]
DFlist <- unclass(DFlist)

Binded <- bind_rows(DFlist)

Binded <- Binded %>%
  group_by(Country, year) %>%
  summarise(across(everything(), ~mean(.x)))




Binded %>%
  ungroup() %>%
  mutate(across(c("year", "ValueScore"), ~as.numeric(.x))) %>%
  mutate(Country = as.factor(Country)) %>%
  ggplot(aes(year, ValueScore, colour = Country)) +
  geom_line()

Binded %>%
  ungroup() %>%
  mutate(Continent = countrycode(Country, origin = "iso3n", destination = "continent")) %>%
  mutate(across(c("year", "ValueScore"), ~as.numeric(.x))) %>%
  mutate(Country = as.factor(Country)) %>%
  filter(!is.na(Continent)) %>%
  ggplot(aes(year, ValueScore, colour = Country)) +
  geom_smooth(show.legend = FALSE, se = FALSE, alpha = .3) +
  facet_wrap(~Continent, scales = "free") +
  ylim(11,19) +
  theme_tufte() +
  scale_color_viridis(option = "C", discrete = TRUE) +
  geom_segment(aes(x = 1980, xend = 2020, y = mean(Binded$ValueScore, na.rm = TRUE),
                   yend = mean(Binded$ValueScore, na.rm = TRUE)),
                   size = 1.5, colour = "#3358DA", alpha = .5) +
  labs(x = "", y = "Value Score", caption = "The blue line marks the world average throughout the period")

