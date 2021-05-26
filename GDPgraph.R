DFlist <- ImputedData[[1]]
DFlist <- unclass(DFlist)

Binded <- bind_rows(DFlist)

Binded <- Binded %>%
  group_by(Country, year) %>%
  summarise(across(everything(), ~mean(.x)))

#### Gamle Plots #### 

#GDP for all and > 4
Binded %>%
  select(Country, gdpPRcapita, S025, year) %>%
  group_by(Country) %>%
  filter(sum(!is.na(S025)) >= 4) %>%
  ggplot(aes(gdpPRcapita)) +
  geom_histogram(data = Binded, aes(gdpPRcapita, y = ..density..), alpha = 1, fill = "navyblue")+
  geom_density(data = Binded, aes(gdpPRcapita, fill = "All"), alpha = .3,)+ # fill = "steelblue2", fill = "All"
  geom_histogram(aes(y = ..density..), colour = "black", fill = "darksalmon", alpha = .4)+
  geom_density(aes(fill = "4 or more"),alpha = .6) + #, fill = "#FF6666"
  labs(y = "", x = "GDP/cap in thousands") +
  theme_classic() +
  scale_fill_manual(
    name = "WVS Waves Participation",
    labels = c( "More than 4 waves", "All participating states"),
    values = c( "#FF6666","steelblue2")
  )



Binded %>%
  ggplot(aes(year, gdpPRcapita, colour = Country)) +
  geom_line() +
  geom_smooth(colour = "black", alpha = .3, size = 2) +
  #geom_hline(yintercept = mean(Binded$gdpPRcapita, na.rm = TRUE), colour = "navyblue",
  #size = 1) +
  geom_segment(aes(x=1980,xend= 2019,
                   y=mean(Binded$gdpPRcapita, na.rm = TRUE),
                   yend=mean(Binded$gdpPRcapita, na.rm = TRUE)),
               colour = "navyblue",
               size = 1.5)+
  guides(colour = FALSE)+
  annotate("text", x=2021, y=13422.1, label= "Average") + 
  annotate("text", x = 2020.5, y=22122.1, label = "Trend") +
  ylab("GDP/cap")+
  xlab("")


#### GDP all vs. GDP participants ####

GDP <- WB_Data %>%
  select(Country, year, gdpPRcapita)

Participants <- Binded %>%
  select(Country, year) %>%
  mutate(Par = 1, 
         Country = as.numeric(Country))


# 
# AllPol <- PolitySelected %>%
#   left_join(PolImp, by = c("Country", "year")) %>%
#   mutate(Par = ifelse(is.na(Par),0,1)) %>%
#   mutate(Par = ifelse(Par == 1, "Participating", "Not Participating"),
#          Par = factor(Par, levels = c("Not Participating", "Participating")))         


GDP <- GDP %>%
  left_join(Participants, by = c("Country", "year")) %>%
  mutate(Par = ifelse(is.na(Par),0,1)) %>%
  mutate(Par = ifelse(Par == 1, "Participating", "Not Participating"),
  Par = factor(Par, levels = c("Not Participating", "Participating")))         
  



#Not Transformed

GDP %>%
  ggplot(aes(gdpPRcapita, group = Par, fill = Par)) +
  geom_density(alpha = .7) +
  theme_tufte() +
  scale_fill_manual(values = c("#FF6666", "#10CEE6"))+
  theme(legend.title=element_blank()) +
  labs(x = "GDP/cap in thousands", y = "")




#Log-normal
GDP %>%
  mutate(LogGDP = log(gdpPRcapita)) %>%
  ggplot(aes(LogGDP,  fill = Par)) +
  geom_density(alpha = .7) +
  theme_tufte() +
  scale_fill_manual(values = c("#FF6666", "#10CEE6"))+
  theme(legend.title=element_blank()) +
  labs(x = "Log of GDP/cap in thousands", y = "")


GDP <- 
  GDP %>%
  mutate(LogGDP = log(gdpPRcapita))

GDP %>%
  ggplot(aes(LogGDP)) +
  geom_density(aes( fill = "#FF6666"), alpha = .7) +
  geom_density(aes(LogGDP,
                   fill = "#10CEE6"), alpha = .7, data = GDP %>% filter(Par == "Participating")) +
  theme_tufte() +
  theme(legend.title=element_blank()) +
  labs(x = "Log of GDP/cap in thousands", y = "") +
  scale_fill_identity(guide = "legend", labels = c("Participants", "World"))









