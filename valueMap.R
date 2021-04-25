?ggplot2::map_data()

ggplot(map_data("world"), aes(lon, lat, group = group)) +
  geom_polygon()

ggplot(mi_counties, aes(lon, lat)) + 
  geom_point(size = .25, show.legend = FALSE) +
  coord_quickmap()


country_data <- ImputedData[[1]][1]
country_data <- as.data.frame(country_data)
library(fuzzyjoin)

country_data$CountryChar <- countrycode(country_data$imp1.Country, origin = "iso3n", destination = "iso3c")

country_data <- country_data %>%
  filter(imp1.year %in% c(1980, 1990,2000, 2010) & !is.na(imp1.year)) %>%
  select(CountryChar, imp1.year, imp1.ValueScore) %>%
  full_join(maps::iso3166, by = c("CountryChar" = "a3")) 


country_data <- country_data %>%
  mutate(imp1.year = ifelse(is.na(imp1.year), "1980, 1990, 2000, 2010", imp1.year))

country_data <- country_data %>%
  mutate(imp1.year = strsplit(imp1.year, ",")) %>%
  unnest(imp1.year) %>%
  mutate(imp1.year = as.numeric(imp1.year))


# > mydf %>% 
#   mutate(V2 = strsplit(as.character(V2), ",")) %>% 
#   unnest(V2)




country_data$imp1.ValueScore <- scales::rescale(country_data$imp1.ValueScore, to = c(1,100))
class(maps::iso3166)
library(ggthemes)

map_data("world") %>%
  as_tibble() %>%
  filter(region != "Antarctica") %>%
  regex_left_join(country_data, by = c(region = "mapname")) %>%
  filter(!is.na(imp1.year)) %>%
  ggplot(aes(long, lat, group = group, fill = imp1.ValueScore)) +
  geom_polygon(color = "black", size = .05) +
  scale_fill_gradient2(low = "blue", high = "red",mid = "green" ,midpoint = 55, na.value = "black") +
  facet_wrap(~imp1.year) +
  theme_map() +
  labs(title = "Self-Enchantement Value Scores", fill = "Value Score")

