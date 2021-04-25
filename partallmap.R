country_data <- SurveyData




country_data <- make.pbalanced(country_data)

country_data$Country <- as.numeric(as.character(country_data$Country))
country_data$CountryChar <- countrycode(country_data$Country, origin = "iso3n", destination = "iso3c")

country_data$part <- ifelse(is.na(country_data$part), 0, country_data$part)


country_data <- country_data %>%
  filter(!(year %in% c(1983,1984,1989,1992,1993))) %>%
  full_join(maps::iso3166, by = c("CountryChar" = "a3")) 






country_data <- country_data %>%
  mutate(year = ifelse(is.na(year), "1981, 1982, 1990, 1991, 1995, 1996, 1997, 1998, 1999,
                       2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
                       2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020", year))


country_data$year <- as.character(country_data$year)
country_data <- country_data %>%
  mutate(year = strsplit(year, ",")) %>%
  unnest(year) %>%
  mutate(year = as.numeric(year))







map_data("world") %>%
  as_tibble() %>%
  filter(region != "Antarctica") %>%
  regex_left_join(country_data, by = c(region = "mapname")) %>%
  filter(!is.na(year)) %>%
  ggplot(aes(long, lat, group = group, fill = part)) +
  geom_polygon(color = "black", size = .05) +
  scale_fill_gradient2(low = "red", high = "green", mid = "red", na.value = "black", midpoint = .5)+
  facet_wrap(~year) +
  theme_map() +
  labs(title = "Participating Countries",  subtitle = "All surveys") +
  guides(fill = FALSE)



















