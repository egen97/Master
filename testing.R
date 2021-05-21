library(tidyverse)


dat %>%
  group_by(id, grp = cumsum(dat$value == "cool") - (dat$value == "cool")) %>% 
  mutate(newCool = if_else(value == "cool", sum(value == "uncool"), NA_integer_))





SubSet %>%
  group_by(Country, grp = cumsum(Conflict_Binary == 1) - (Conflict_Binary == 1)) %>%
  mutate(Event = ifelse(Conflict_Binary == 1, sum(Conflict_Binary == 0), 0)) %>%
  select(Country, year, Conflict_Binary, Event) %>%
  filter(Country == 400) %>%
  View()


SubSet %>%
  group_by(Country,) %>%
  mutate(Event = 0,
         Event = ifelse(Conflict_Binary == 0, sum(Conflict_Binary == 0), 0)) %>%
  select(Country, year, Conflict_Binary, Event) %>%
  filter(Country == 840) %>%
  View()

install.packages("hutilscpp")
library(hutilscpp)


# ImputedData <- transform.amelia(ImputedData, ValueScore =  (ValueRisk + ValueSucses + ValueGodTim + ValueSecur))
# 
# 
# ImputedData <- transform.amelia(ImputedData, MID_Binary = ifelse(!is.na(dispnum) & fatality > 1, 1, 0))



SubSet <- SubSet %>%
  mutate(ValueScore = ValueRisk + ValueSucses + ValueGodTim + ValueSecur,
        dispnum = ifelse(dispnum == 0, NA, dispnum), 
        MID_Binary = ifelse(!is.na(dispnum) & fatality > 1, 1, 0))

SubSet <- SubSet %>% 
  mutate(
    TimUCDP = cumsum_reset(!(as.logical(Conflict_Binary))),
    TimMID = cumsum_reset(!(as.logical(MID_Binary)))
  ) 





#ind <- with(MyData, c(FALSE, farm[-1L]!= farm[-length(farm)]) & farm!='NULL')

ImputedData <- transform.amelia(ImputedData,
                                Tim_UCDP =
                                with(
                                  ImputedData, c(FALSE, Conflict_Binary[-1L] != Conflict_Binary[-length(Conflict_Binary)])
                                )
                                )


ImputedData[[1]][[1]] %>%
  select(Country, year, Conflict_Binary, Tim_UCDP) %>%
  filter(Country == 8) %>%
  View()




