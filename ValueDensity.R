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
  ggplot(aes(ValueScore)) +
  geom_density( fill = ) +
  theme_tufte()

Binded %>%
  ggplot(aes(ValueScore)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white")+
  geom_density(alpha = .2, fill = "#FF6666") +
  labs(y = "", x = "Self-Enhancement values") +
  theme_tufte() 
