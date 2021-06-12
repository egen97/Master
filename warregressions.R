CompleteData <- readRDS("Data/CompleteData.rds")


SubSet <- CompleteData %>%
  select(Country, year, polity, gdpPRcapita, Population,
         ValueSucses, ValueRisk, suppDem_Afro, SuppDem,
         v2x_polyarchy, ChildDeter, DOMMOV, v2xpe_exlgender, A008, A029,
         A170, KILL, e_total_resources_income_pc, e_pefeliex, Conflict_Binary,
         FightCountry, milex, milper, majorpower, type_of_conflict, intensity_level, HDI,
         Gini, ValueGodTim, ValueSecur, v2x_accountability, ValueSecur,
         v2xpe_exlgender, PHYSINT, A035, armedAntiUsAbarb, dealMoralArab, DeathPena,
         v2xpe_exlecon, POLPRIS, e_miurbani, e_total_resources_income_pc, e_total_fuel_income_pc,
         v2xpe_exlecon, E006, C019, TrustOth, IncEq, StaIncEqu, attackCivArab,FightCountry,
         dispnum, hiact, hostlev, fatality, length,  fatalpre, land, sea, cinc, num_mem, edu_year, Y002, Y003)



#Fikse at MID dupliserte år
SubSet <- SubSet %>%
  group_by(Country, year) %>%
  summarise(across(everything(), ~max(.x))) 


SubSet <- SubSet %>%
  mutate(ValueScore = (ValueRisk*(0.638)) + (ValueSucses*0.432) + (ValueGodTim*0.420) + (ValueSecur),
         dispnum = ifelse(dispnum == 0, NA, dispnum), 
         Conflict_Binart = ifelse(!is.na(dispnum) & fatality > 1, 1, 0))



SubSet <- transform(SubSet,
                                Change_UCDP =
                                  with(
                                    SubSet, c(FALSE, Conflict_Binary[-1L] != Conflict_Binary[-length(Conflict_Binary)])
                                  ),
                                Change_MID =
                                  with(
                                    SubSet, c(FALSE, Conflict_Binart[-1L] != Conflict_Binart[-length(Conflict_Binart)])
                                  )
                                
)

#Lage en for land as well

SubSet <- transform(SubSet,
                                Change_Country =
                                  with(
                                    SubSet, c(FALSE, Country[-1L] != Country[-length(Country)])
                                  )
                                
                                
)




SubSet <- transform(SubSet,
                                Change_UCDP = ifelse(year == 1980, FALSE, Change_UCDP),
                                Change_MID = ifelse(year == 1980, FALSE, Change_MID)
                                
)


#Lager en variabel som viser om konfliktstatus har forandret seg fra året før. Siden dataene er sortert i riktig år-land rekkefølge
#kan en bare basere seg på raden før. Må sette 1980 til FALSE manuelt sånn at de som har konflikt ikke skal bli sammenlignet med
#landet før i 2020 



SubSet <- transform(SubSet,
                                
                                TimUCDP = cumsum_reset(!(Change_UCDP | Change_Country)),
                                TimMID = cumsum_reset(!(Change_MID | Change_Country))
                                
                                
)

#Legge til en kumulativ sum som endres når konfliktstatus forandrer seg


SubSet <- transform(SubSet,
                                
                                TimUCDP = ifelse(Change_UCDP, 1 + lag(TimUCDP, k = 1), TimUCDP),
                                TimMID = ifelse(Change_MID , 1 + lag(TimMID, k = 1), TimMID)
                                
                                
)

SubSet <- transform(SubSet,
                                TimUCDP = ifelse(Change_Country, 1, TimUCDP),
                                TimMID = ifelse(Change_Country , 1 , TimMID)
                                
                                
)




















Mod0 <- zelig(Conflict_Binary ~ ValueScore,
              model = "logit",
              data = SubSet)


MID_1 <- zelig(Conflict_Binary ~ ValueScore + log(TimUCDP),
               model = "logit",
               data = SubSet) 





MID_2 <- zelig(Conflict_Binary ~ ValueScore + log(TimUCDP) +
                 polity,
               model = "logit",
               data = SubSet) 










MID_3 <- zelig(Conflict_Binary ~ ValueScore + log(TimUCDP) +
                 polity +
                 log(gdpPRcapita),
               model = "logit",
               data = SubSet) 


# Military Power

MID_4 <- zelig(Conflict_Binary ~ ValueScore + log(TimUCDP) +
                 cinc + majorpower + log(gdpPRcapita),
               model = "logit",
               data = SubSet)



MID_5 <- zelig(Conflict_Binary ~ ValueScore + log(TimUCDP) +
                 cinc + majorpower + log(gdpPRcapita) + num_mem + sea + land,
               model = "logit",
               data = SubSet)


#Complete model!


MID_6 <- zelig(Conflict_Binary ~ ValueScore + log(TimUCDP) +
                 polity + log(gdpPRcapita) + num_mem + sea + land + cinc + majorpower,
               model = "logit",
               data = SubSet)



texreg::screenreg(l = list(Mod0, MID_1, MID_2, MID_3, MID_4, MID_5, MID_6),
                  include.bic = FALSE,
                  include.df = TRUE,
                  custom.coef.names = c(
                    "(intercept)",
                    "Self-Enhancement Values",
                    "Ln(Time since conflict)",
                    "Polity",
                    "Ln GDP/cap",
                    "CINC",
                    "Nr. Allies",
                    "Borders: Sea",
                    "Borders: Land"
                  ),
               booktabs = TRUE,
               dcolumn = TRUE,
               use.packages = FALSE,
               center = TRUE,
               scalebox = 0.7,
               caption = "UCDP: Not Imputed",
               label = "UCDP_NI")#,
               file = "UCDP_NI.tex"
                  )


