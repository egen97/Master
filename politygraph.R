

DFlist <- ImputedData[[1]]
DFlist <- unclass(DFlist)

Binded <- bind_rows(DFlist)

Binded <- Binded %>%
  group_by(Country, year) %>%
  summarise(across(everything(), ~mean(.x)))





Binded %>%
  select(Country, polity) %>%
  ggplot(aes(polity)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white")+
  geom_density(alpha = .2, fill = "#FF6666") +
  labs(y = "", x = "Polity V Democracy Score") +
  theme_classic() 






ggplot(df, aes(x=weight)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

Binded %>%
  select(Country, polity) %>%
  count(polity) %>%
  View()






### GDP development #### 

class(Binded$Country)
Binded$Country <- as.factor(Binded$Country)




# Lage en for sammenligning mot polity real life


PolImp <- Binded %>%
  select(Country, year) %>%
  mutate(Par = 1,
         Country = as.numeric(Country))

PolitySelected <- readRDS("Data/politySelected.rds")

AllPol <- PolitySelected %>%
  left_join(PolImp, by = c("Country", "year")) %>%
  mutate(Par = ifelse(is.na(Par),0,1)) %>%
  mutate(Par = ifelse(Par == 1, "Participating", "Not Participating"),
         Par = factor(Par, levels = c("Not Participating", "Participating")))






AllPol %>%
  ggplot(aes(polity, group = Par, fill = Par)) +
  geom_density(alpha = .7) +
  theme_tufte() +
  scale_fill_manual(values = c("#FF6666", "#10CEE6"))+
  theme(legend.title=element_blank()) +
  labs(x = "Polity V Democracy Score", y = "")
                      



















 
 