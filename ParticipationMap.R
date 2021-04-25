

country_data <- SurveyData %>%
  select(Country, S002) %>%
  distinct() %>%
  mutate(part = 1)



library(plm)

country_data <- make.pbalanced(country_data)

country_data$part <- ifelse(is.na(country_data$part), 0, country_data$part)

country_data$CountryChar <- countrycode(country_data$Country, origin = "iso3n", destination = "iso3c")


country_data <- country_data %>%
  filter(!is.na(CountryChar))




country_data <- country_data %>%
  full_join(maps::iso3166, by = c("CountryChar" = "a3")) 


country_data <- country_data %>%
  mutate(S002 = ifelse(is.na(S002), "1, 2, 3, 4, 5,6 ,7", S002))


country_data$S002 <- as.character(country_data$S002)
country_data <- country_data %>%
  mutate(S002 = strsplit(S002, ",")) %>%
  unnest(S002) %>%
  mutate(S002 = as.numeric(S002))

country_data <- country_data %>%
  filter(!is.na(S002))



map_data("world") %>%
  as_tibble() %>%
  filter(region != "Antarctica") %>%
  regex_left_join(country_data, by = c(region = "mapname")) %>% 
  filter(!is.na(S002)) %>%
ggplot(aes(long, lat, group = group, fill = part)) +
  geom_polygon(color = "black", size = .05) +
  scale_fill_gradient2(low = "blue", high = "green",mid = "red" , na.value = "black") +
  facet_wrap(~S002) +
  theme_map() +
  labs(title = "Participating Countries",  subtitle = "World - and European Value Survey") +
  guides(fill = FALSE)


head(CompleteData)

SurveyOnly <- CompleteData[,1:151] 


SurveyOnly %>%
  filter(across(3:ncol(.), ~is.na(.x))) %>%
  View()

SurveyOnly %>%
  mutate(part = ifelse(is.na(3:ncol(.),1 , 0)))


SurveyData$All_NA <- apply(SurveyData[,3:153], 1, function(x) all(is.na(x)))

SurveyOnly %>%
  filter(All_NA == TRUE) %>%
  View()
  

SurveyData <- SurveyData %>%
  filter(All_NA == FALSE) %>%
  select(Country, year) %>%
  mutate(part = 1)



country_data <- SurveyData
country_data$Country <- as.numeric(as.character(country_data$Country))
country_data$CountryChar <- countrycode(country_data$Country, origin = "iso3n", destination = "iso3c")


country_data <- country_data %>%
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
  regex_full_join(country_data, by = c(region = "mapname")) %>% 
  filter(!is.na(year), !(year %in% c(1983,1984,1989,1992,1993))) %>% 
  ggplot(aes(long, lat, group = group, fill = part)) +
  geom_polygon(color = "black", size = .05) +
  scale_fill_gradient2(low = "blue", high = "green", mid = "red", na.value = "black", midpoint = .5) +
  facet_wrap(~year) +
  theme_map() +
  labs(title = "Participating Countries",  subtitle = "All surveys") +
  guides(fill = FALSE)
