library(RSQLite)
library(dplyr)
library(lubridate)

setwd('C:\\Users\\Simone\\Documents\\Simone\\Uni\\Aziende')
connection <- dbConnect(SQLite(),'aziende.db')
dati <- dbGetQuery(connection, "SELECT * FROM dati WHERE codicepdv NOT LIKE '4255' AND codicepdv NOT LIKE '2291' AND amg BETWEEN '2012-01-01' AND '2013-12-31';")
dbDisconnect(connection)
rm(connection)
gc()


dati <- na.omit(dati)

classi <- read.csv('dati.csv')
classi$codicearticolo <- as.character(classi$codicearticolo)



dati <- dati %>% left_join(classi,by='codicearticolo')

rm(classi)
gc()


dati <- dati%>%mutate(guadagno = unita_tot*margine)
dati <- dati%>%mutate(week =week(ymd(amg)))
dati <- dati%>%mutate(year =year(ymd(amg)))

# dati3 <- dati %>% 
#   arrange(codicepdv,classe,amg ) %>%
#   mutate(grp = with(rle(paste0(codicepdv,classe,promo)), rep(seq_along(lengths), lengths))) %>%
#   group_by(grp) %>%
#   summarise(
#             codicepdv = unique(codicepdv),
#             codicearticolo= unique(classe),
#             promo = unique(promo),
#             vendite = mean(vendite_tot), 
#             unita = mean(unita_tot), 
#             margine = mean(margine), 
#             scontoatipico = mean(scontoatipico_tot),
#             promo_start = min(amg),
#             promo_end = max(amg),
#             categoria = unique(categoria),
#             citta =unique(citta),
#             residenti=unique(residenti),
#             redditoMedioProCapite=unique(redditoMedioProCapite),
#             spMediaMens=unique(spMediaMens),
#             spMedianaMens=unique(spMedianaMens),
#             percStran=unique(percStran),
#             percDisocc=unique(percDisocc),
#             competitor=unique(competitor),
#             superficieCompetitor=unique(superficieCompetitor),
#             discount=unique(discount)
#   )
# dati


## Category unit sales (c, t)
# vendite totali per una classe c nella settimana t 

medie <- dati %>% select(unita_tot,guadagno,codicepdv,classe) %>% aggregate(as.formula('. ~ codicepdv+classe'),.,mean) %>% rename(unitam = unita_tot,guadagnom=guadagno)

vendite_cl <- dati %>% 
  arrange(codicepdv,classe,year,week) %>% 
  mutate(grp = with(rle(paste0(codicepdv,classe,week,year)), rep(seq_along(lengths), lengths))) %>%
  left_join(medie,by=c('codicepdv','classe')) %>%
  group_by(grp) %>%
  summarise(
    codicepdv = unique(codicepdv),
    week = unique(week),
    year = unique(year),
    classe = unique(classe),
    unitatot = sum(unita_tot),
    guadagni_tot = sum(guadagno),
    season_unita = mean(unita_tot)/unique(unitam),
    season_guadagni = mean(guadagno)/unique(guadagnom)
  )
rm(medie)
gc()


dati <- dati %>% left_join(vendite_cl,by = c('codicepdv','week','year','classe')) 

rm(vendite_cl)
gc()


dati <- dati %>% select(-c(grp,scontoatipico_tot,daylag,categoria))



medie <- dati %>% 
         select(unita_tot,guadagno,codicepdv,codicearticolo,promo) %>% 
         filter(promo==-1) %>%
         select(-promo) %>%
         aggregate(as.formula('. ~ codicepdv+codicearticolo'),.,mean) %>% 
         rename(baseline_unita = unita_tot,baseline_guadagno=guadagno)

dati <- dati %>% left_join(medie,by=c('codicepdv','codicearticolo'))
rm(medie)
gc()

dati <- dati %>% mutate(gross_lift_unita=unita_tot-baseline_unita,gross_lift_guadagno=guadagno-baseline_guadagno) %>% select(-c(baseline_unita,baseline_guadagno))

total_gross_lift <- dati %>%
                    mutate(total_gross_lift_unita = gross_lift_unita,total_gross_lift_guadagno = gross_lift_guadagno)%>%
                    aggregate(as.formula('total_gross_lift_unita ~ codicepdv+classe+week+year'),.,sum)  %>%
                    aggregate(as.formula('total_gross_lift_guadagno ~ codicepdv+classe+week+year'),.,sum)  

