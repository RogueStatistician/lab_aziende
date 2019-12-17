library(RSQLite)
library(dplyr)
library(lubridate)

setwd('C:\\Users\\Simone\\Documents\\Simone\\Uni\\Aziende')
connection <- dbConnect(SQLite(),'aziende.db')
dati <- dbGetQuery(connection, "SELECT * FROM dati WHERE codicepdv NOT LIKE '4255' AND codicepdv NOT LIKE '2291' AND amg BETWEEN '2012-01-01' AND '2013-12-31';")
dati <- na.omit(dati)

classi <- read.csv('dati.csv')
classi$codicearticolo <- as.character(classi$codicearticolo)


dati2 <- dati %>% left_join(classi,by='codicearticolo')
dati2 <- dati2%>%mutate(guadagno = unita_tot*margine)
dati2 <- dati2%>%mutate(week =week(ymd(amg)))
dati2 <- dati2%>%mutate(year =year(ymd(amg)))

# dati3 <- dati2 %>% 
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
# dati2


## Category unit sales (c, t)
# vendite totali per una classe c nella settimana t 
vende_cl <- dati2 %>% 
  arrange(codicepdv,classe,year,week) %>% 
  mutate(grp = with(rle(paste0(codicepdv,classe,week,year)), rep(seq_along(lengths), lengths))) %>%
  mutate(unitmean = mean(unita_tot),guadagnimean = mean(guadagno)) %>%
  group_by(grp) %>%
  summarise(
    codicepdv = unique(codicepdv),
    week = unique(week),
    year = unique(year),
    classe = unique(classe),
    unitatot = sum(unita_tot),
    guadagni_tot = sum(guadagno),
    season_unita = mean(unita_tot)/unitmean,
    season_guadagni = mean(guadagno)/guadagnimean
  )


dati2 <- dati2 %>% left_join(vende_cl,by = c('codicepdv','week','year','classe')) 

dati2 <- dati2 %>% select(-grp)
dati2 <- dati2 %>% select(-scontoatipico_tot)
dati2 <- dati2 %>% select(-c(daylag,categoria))
