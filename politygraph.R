Part <- ImputedData[[1]][[1]]

SurveyData %>%
  select(Country, polity) %>%
  ggplot(aes(polity)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white")+
  geom_density(alpha = .2, fill = "#FF6666") +
  labs(y = "", x = "Polity V Democracy Score", title = "Democracy Scores") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))






ggplot(df, aes(x=weight)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

SurveyData %>%
  select(Country, polity) %>%
  count(polity) %>%
  View()

SurveyData %>%
  group_by(Country) %>%
  filter(sum(!is.na(S025))>4)


#GDP for all and > 4
SurveyData %>%
  select(Country, gdpPRcapita, S025, year) %>%
  group_by(Country) %>%
  filter(sum(!is.na(S025)) >= 4) %>%
  ggplot(aes(gdpPRcapita)) +
  geom_histogram(data = SurveyData, aes(gdpPRcapita, y = ..density..), alpha = 1, fill = "navyblue")+
  geom_density(data = SurveyData, aes(gdpPRcapita, fill = "All"), alpha = .3,)+ # fill = "steelblue2", fill = "All"
  geom_histogram(aes(y = ..density..), colour = "black", fill = "darksalmon", alpha = .4)+
  geom_density(aes(fill = "4 or more"),alpha = .6) + #, fill = "#FF6666"
  labs(y = "", x = "GDP/cap in thousands") +
  theme_classic() +
   scale_fill_manual(
     name = "WVS Waves Participation",
     labels = c( "More than 4 waves", "All participating states"),
     values = c( "#FF6666","steelblue2")
  )



### GDP development

class(SurveyData$Country)
SurveyData$Country <- as.factor(SurveyData$Country)

SurveyData %>%
  ggplot(aes(year, gdpPRcapita, colour = Country)) +
  geom_line() +
  geom_smooth(colour = "black", alpha = .3, size = 2) +
  #geom_hline(yintercept = mean(SurveyData$gdpPRcapita, na.rm = TRUE), colour = "navyblue",
             #size = 1) +
  geom_segment(aes(x=1980,xend= 2019,
                   y=mean(SurveyData$gdpPRcapita, na.rm = TRUE),
                   yend=mean(SurveyData$gdpPRcapita, na.rm = TRUE)),
               colour = "navyblue",
               size = 1.5)+
  guides(colour = FALSE)+
  annotate("text", x=2021, y=13422.1, label= "Average") + 
  annotate("text", x = 2020.5, y=22122.1, label = "Trend") +
  ylab("GDP/cap")+
  xlab("")

































 
 