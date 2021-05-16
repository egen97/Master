
#Number of Military Interstate Disputes

Df %>%
  mutate(decade = floor(year/10)*10) %>%
  select(decade, MID_Binary) %>%
  group_by(decade) %>%
  summarise(conflict = sum(MID_Binary)) %>%
  filter(decade != 2020) %>%
  ggplot(aes(decade, conflict)) +
  geom_col(colour = "black", fill = "royalblue", alpha = 1) +
  theme_classic() +
  labs(title = "Military Interstate Disputes", x = "Decade", y = "", caption = "Requires a minimum of 1 fatality")

#MIDcount.png

#Length of MID
#MIDlength.png

Df %>%
  mutate(length = ifelse(length < 0, 1, length)) %>%
  ggplot(aes(length)) +
  geom_density(fill = "royalblue", alpha = .4) +
  xlim(1,13)+
  theme_classic() +
  labs(title = "Length of Conflict", subtitle = "Military Interstate Dispute", x = "Number of years", y = "")

Df %>%
  filter(fatalpre >= 1) %>%
  ggplot(aes(fatalpre)) +
  geom_density(fill = "royalblue", alpha = .4) +
  theme_classic() +
  labs(title = "Fatalities", subtitle = "Military Interstate Dispute", x = "Nr. of Fatalities", y = "")

#fatal.png







#Number and intensity of UCDP/Prio conflicts
#UCDPConf.png
Df %>%
  mutate(decade = floor(year/10)*10) %>%
  select(decade, Conflict_Binary, intensity_level) %>%
  filter(!is.na(intensity_level)) %>%
  mutate(intensity_level = ifelse(intensity_level == 1, "Minor Conflict", "War"),
         intensity_level =  factor(intensity_level, levels = c("Minor Conflict", "War"))) %>%
  group_by(decade) %>%
  #summarise(conflict = sum(Conflict_Binary)) %>%
  filter(decade != 2020) %>%
  ggplot(aes(decade, Conflict_Binary, fill = intensity_level)) +
  geom_col(alpha = .9) +
  theme_classic() +
  labs(title = "UCDP/Prio Conflicts", x = "Decade", y = "", fill = "Intensity Level") +
  scale_fill_manual(values = c("#0CB2D5", "#DA2F1E"))



#Willigness to Fight for country <3

#BarPlot

SubSet %>%
  mutate(FC = ifelse(FightCountry == 1, "Yes", "No"),
         FC = factor(FC, levels = c("No", "Yes"))) %>% 
  filter(!is.na(FC)) %>%
  ggplot(aes(FC)) +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill = "#C00C14", alpha = .8) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "", title = "Willing to Fight for Country", subtitle = "World Value Survey")

#WFper.png

#FC Map

summary(Df$FightCountry) #Noen er >1, prob vektinga

country_data <- Df %>%
  mutate(decade = floor(year/10)*10) %>%
  select(Country, decade, FightCountry) %>%
  group_by(decade, Country) %>%
  summarise(FC = mean(FightCountry)) %>%
  filter(decade != 2020) 


country_data$CountryChar <- countrycode(country_data$Country, origin = "iso3n", destination = "iso3c")


country_data <- country_data %>%
  full_join(maps::iso3166, by = c("CountryChar" = "a3")) 

country_data <- ungroup(country_data)
country_data <- country_data %>%
  mutate(decade = ifelse(is.na(decade), "1980, 1990, 2000, 2010", decade))

country_data <- country_data %>%
  mutate(decade = strsplit(decade, ",")) %>%
  unnest(decade) %>%
  mutate(decade = as.numeric(decade))



  

map_data("world") %>%
  as_tibble() %>%
  filter(region != "Antarctica") %>%
  regex_left_join(country_data, by = c(region = "mapname")) %>%
  filter(!is.na(decade)) %>%
  ggplot(aes(long, lat, group = group, fill = FC)) +
  geom_polygon(color = "black", size = .05) +
  scale_fill_gradient2(low = "blue", high = "red",mid = "green" ,midpoint = .60,
                      na.value = "black", labels = scales::percent) +
  
  facet_wrap(~decade) +
  theme_map() +
  labs(title = "Willigness to Fight for Country", fill = "", subtitle = "World Value Survey") 


#WFMap.png


