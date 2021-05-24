UCDP_1 <- zelig(Conflict_Binary ~ Y002 + TimUCDP + (TimUCDP^2) + (TimUCDP^3),
                model = "logit",
                data = ImputedData) 









UCDP_2 <- zelig(Conflict_Binary ~ Y002 + TimUCDP + (TimUCDP^2) + (TimUCDP^3) +
                  polity,
                model = "logit",
                data = ImputedData) 











UCDP_3 <- zelig(Conflict_Binary ~ Y002 + TimUCDP + (TimUCDP^2) + (TimUCDP^3) +
                  polity +
                  log(gdpPRcapita),
                model = "logit",
                data = ImputedData) 





#Fixed effects 

UCDP_1_FE <- zelig(Conflict_Binary ~ Y002 + 
                     TimUCDP + (TimUCDP^2) + (TimUCDP^3) + 
                     as.factor(Country)+ 
                     as.factor(year) ,
                   model = "logit",
                   data = ImputedData) 



UCDP_2_FE <- zelig(Conflict_Binary ~ Y002 + 
                     TimUCDP + (TimUCDP^2) + (TimUCDP^3)  +
                     as.factor(Country) +
                     as.factor(year) +
                     polity,
                   model = "logit",
                   data = ImputedData) 




UCDP_3_FE <- zelig(Conflict_Binary ~ Y002 + 
                     TimUCDP + (TimUCDP^2) + (TimUCDP^3)+
                     as.factor(Country) +
                     as.factor(year) +
                     polity +
                     log(gdpPRcapita),
                   model = "logit",
                   data = ImputedData) 



# Military Power

UCDP_4 <- zelig(Conflict_Binary ~ Y002 + TimUCDP + (TimUCDP^2) + (TimUCDP^3) +
                  cinc + majorpower + log(gdpPRcapita),
                model = "logit",
                data = ImputedData)



UCDP_5 <- zelig(Conflict_Binary ~ Y002 + TimUCDP + (TimUCDP^2) + (TimUCDP^3) +
                  cinc + majorpower + log(gdpPRcapita) + num_mem + sea + land,
                model = "logit",
                data = ImputedData)




UCDP_4_FE <- zelig(Conflict_Binary ~ Y002 + TimUCDP + (TimUCDP^2) + (TimUCDP^3) +
                     cinc + majorpower + log(gdpPRcapita) +
                     as.factor(Country) + as.factor(year),
                   model = "logit",
                   data = ImputedData)



UCDP_5_FE <- zelig(Conflict_Binary ~ Y002 + TimUCDP + (TimUCDP^2) + (TimUCDP^3) +
                     cinc + majorpower + log(gdpPRcapita) + num_mem + sea + land +
                     as.factor(Country) + as.factor(year),
                   model = "logit",
                   data = ImputedData)


#Complete model!


UCDP_6 <- zelig(Conflict_Binary ~ Y002 + TimUCDP + (TimUCDP^2) + (TimUCDP^3) +
                  polity + log(gdpPRcapita) + num_mem + sea + land + cinc + majorpower,
                model = "logit",
                data = ImputedData)


UCDP_6_FE <- zelig(Conflict_Binary ~ Y002 + TimUCDP + (TimUCDP^2) + (TimUCDP^3) + 
                     polity + log(gdpPRcapita) + num_mem + sea + land + cinc + majorpower +
                     as.factor(Country) + as.factor(year),
                   model = "logit",
                   data = ImputedData)

#### MID #####


MID_1 <- zelig(MID_Binary ~ Y002 + TimMID + (TimMID^2) + (TimMID^3),
               model = "logit",
               data = ImputedData) 





MID_2 <- zelig(MID_Binary ~ Y002 + TimMID + (TimMID^2) + (TimMID^3) +
                 polity,
               model = "logit",
               data = ImputedData) 










MID_3 <- zelig(MID_Binary ~ Y002 + TimMID + (TimMID^2) + (TimMID^3) +
                 polity +
                 log(gdpPRcapita),
               model = "logit",
               data = ImputedData) 





#Fixed effects 

MID_1_FE <- zelig(MID_Binary ~ Y002 + 
                    TimMID + (TimMID^2) + (TimMID^3) + 
                    as.factor(Country)+ 
                    as.factor(year) ,
                  model = "logit",
                  data = ImputedData) 



MID_2_FE <- zelig(MID_Binary ~ Y002 + 
                    TimMID + (TimMID^2) + (TimMID^3)  +
                    as.factor(Country) +
                    as.factor(year) +
                    polity,
                  model = "logit",
                  data = ImputedData) 




MID_3_FE <- zelig(MID_Binary ~ Y002 + TimMID + (TimMID^2) + (TimMID^3) +
                    as.factor(Country) +
                    as.factor(year) +
                    polity +
                    log(gdpPRcapita),
                  model = "logit",
                  data = ImputedData) 



# Military Power

MID_4 <- zelig(MID_Binary ~ Y002 + TimMID + (TimMID^2) + (TimMID^3) +
                 cinc + majorpower + log(gdpPRcapita),
               model = "logit",
               data = ImputedData)



MID_5 <- zelig(MID_Binary ~ Y002 + TimMID + (TimMID^2) + (TimMID^3) +
                 cinc + majorpower + log(gdpPRcapita) + num_mem + sea + land,
               model = "logit",
               data = ImputedData)




MID_4_FE <- zelig(MID_Binary ~ Y002 + TimMID + (TimMID^2) + (TimMID^3) +
                    cinc + majorpower + log(gdpPRcapita) +
                    as.factor(Country) + as.factor(year),
                  model = "logit",
                  data = ImputedData)



MID_5_FE <- zelig(MID_Binary ~ Y002 + TimMID + (TimMID^2) + (TimMID^3) +
                    cinc + majorpower + log(gdpPRcapita) + num_mem + sea + land +
                    as.factor(Country) + as.factor(year),
                  model = "logit",
                  data = ImputedData)


#Complete model!


MID_6 <- zelig(MID_Binary ~ Y002 + TimMID + (TimMID^2) + (TimMID^3) +
                 polity + log(gdpPRcapita) + num_mem + sea + land + cinc + majorpower,
               model = "logit",
               data = ImputedData)


MID_6_FE <- zelig(MID_Binary ~ Y002 + TimMID + (TimMID^2) + (TimMID^3) + 
                    polity + log(gdpPRcapita) + num_mem + sea + land + cinc + majorpower +
                    as.factor(Country) + as.factor(year),
                  model = "logit",
                  data = ImputedData)