# 3. faza: Vizualizacija podatkov
library(ggplot2)
library(dplyr)
source("lib/uvozi.zemljevid.r", encoding = "UTF-8")

imena <- data.frame(c('TR','AT','BE','BG','HR','CY','CZ','DK','EE','FI','MK','FR','DE','EL','HU','IS','IE',
                      'IT','LT','LI','LV','LU','MT','NL','NO','PL','PT','RO','ME','SK','SI','ES','SE','CH','UK'),
                    c('Albania','Austria','Belgium','Bulgaria','Croatia','Cyprus','Czech Republic','Denmark','Estonia',
                      'Finland','Former Yugoslav Republic of Macedonia, the','France','Germany','Greece','Hungary',
                      'Iceland','Ireland','Italy','Latvia','Liechtenstein','Lithuania','Luxembourg','Malta','Netherlands',
                      'Norway','Poland','Portugal','Romania','Serbia','Slovakia','Slovenia','Spain','Sweden','Switzerland',
                      'United Kingdom'))
names(imena) <- c('NUTS_ID', 'ime')
zemljevid <- uvozi.zemljevid("http://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/NUTS_2013_10M_SH.zip",
                             "NUTS_2013_10M_SH/data/NUTS_RG_10M_2013", encoding = "UTF-8") %>%
  pretvori.zemljevid() %>% filter(STAT_LEVL_ == 0) %>% filter(NUTS_ID != 'TR')
zemljevid$NUTS_ID <- gsub("^([^0-9]*)", "\\1", zemljevid$NUTS_ID, ignore.case=TRUE)
#zemljevid$NUTS_ID <- factor(zemljevid$NUTS_ID)
imena$NUTS_ID <- factor(imena$NUTS_ID)
zemljevid <- zemljevid %>% merge(imena)
#zemljevid$imena <- factor(zemljevid$imena)
colnames(zemljevid)[12] <- 'drzava'

zemljevid.evrope <- function(n, tabela){
  drzAve <- tabela %>% filter(leto==2016) %>% mutate(drzava = parse_factor(drzava, levels(zemljevid$drzava)))
  drzAve.norm <- drzAve %>% select(-drzava, -leto) %>% scale()
  rownames(drzAve.norm) <- drzAve$drzava
  k <- kmeans(drzAve.norm, n, nstart = 1000)  
  #k$tot.withinss
  skupine <- data.frame(drzava = drzAve$drzava, skupina = factor(k$cluster))
  #table(k$cluster)
  print(ggplot() + geom_polygon(data = zemljevid %>% left_join(skupine, by = c("drzava" = "drzava")),
                                aes(x = long, y = lat, group = group, fill = skupina)) +
          ggtitle("BDP per capita") + xlab("long") + ylab("lat") +
      coord_quickmap(xlim = c(-25, 40), ylim = c(32, 72)))
  return(k$size)
}
zemljevid1 <- zemljevid.evrope(5, drzave)

zemljevid2 <- ggplot() + geom_polygon(data = zemljevid %>% left_join(velika_tabela %>% filter(leto==2015)),
                             aes(x = long, y = lat, group = group, fill=zaposlenost), color='black') +
  coord_quickmap(xlim = c(-25, 40), ylim = c(32, 72))

#povprecja <- drzave %>% group_by(obcina) %>%
#  summarise(povprecje = sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))

graf1 <- ggplot(drzave %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                                drzava == 'United Kingdom' | drzava == 'Italy' |
                                drzava == 'Slovenia' | drzava == 'Poland' |
                                drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=leto, y=BDPpc, color = drzava) +
  geom_line() + ggtitle("BDP per capita")

graf2 <- ggplot(drzavljani %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                                drzava == 'United Kingdom' | drzava == 'Italy' |
                                drzava == 'Slovenia' | drzava == 'Poland' |
                                drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=leto, y=drzavljani, color = drzava) +
  geom_line() + ggtitle("Število prebivalcev")

graf3 <- ggplot(mladi %>% filter(drzava == 'Hungary'  | drzava == 'Sweden' |
                                    drzava == 'United Kingdom' | drzava == 'Italy' |
                                    drzava == 'Slovenia' | drzava == 'Poland' |
                                    drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=leto, y=mladi, color = drzava) +
  geom_line() + ggtitle("Delež mladih (15-30 let)")

graf4 <- ggplot(izobrazba %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                                    drzava == 'United Kingdom' | drzava == 'Italy' |
                                    drzava == 'Slovenia' | drzava == 'Poland' |
                                    drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=leto, y=izobrazba, color = drzava) +
  geom_line() + ggtitle("delež izobraženih")

graf5 <- ggplot(neformalno %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                               drzava == 'United Kingdom' | drzava == 'Italy' |
                               drzava == 'Slovenia' | drzava == 'Poland' |
                               drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=leto, y=neformalno, color = drzava) +
  geom_line() + ggtitle("Neformalno izobraževanje")

graf6 <- ggplot(zaposlenost %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                               drzava == 'United Kingdom' | drzava == 'Italy' |
                               drzava == 'Slovenia' | drzava == 'Poland' |
                               drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=leto, y=zaposlenost, color = drzava) +
  geom_line() + ggtitle("Zaposlenost mladih")

graf7 <- ggplot(neaktivni %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                                     drzava == 'United Kingdom' | drzava == 'Italy' |
                                     drzava == 'Slovenia' | drzava == 'Poland' |
                                     drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=leto, y=neaktivni, color = drzava) +
  geom_line() + ggtitle("Neaktivni mladi")

graf8 <- ggplot(religija)+# %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                           #          drzava == 'United Kingdom' | drzava == 'Italy' |
                            #         drzava == 'Slovenia' | drzava == 'Poland' |
                             #        drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=drzava, y=religija) +
  geom_bar(stat = "identity") + ggtitle("Udejstvovanje v verskih organizacijah")

graf9 <- ggplot(prostovoljstvo) +# %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                                  #   drzava == 'United Kingdom' | drzava == 'Italy' |
                                   #  drzava == 'Slovenia' | drzava == 'Poland' |
                                    # drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=drzava, y = prostovoljstvo) +
  geom_bar(stat = "identity") + ggtitle("Prostovoljstvo")

graf10 <- ggplot(merge(drzave, drzavljani) %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                                        drzava == 'United Kingdom' | drzava == 'Italy' |
                                        drzava == 'Slovenia' | drzava == 'Poland' |
                                        drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=BDPpc, y = drzavljani, color = drzava, size = leto) +
  geom_point() + ggtitle("BDP per capita in število državljanov")