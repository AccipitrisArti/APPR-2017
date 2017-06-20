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
  evropa <- ggplot() + geom_polygon(data = zemljevid %>% left_join(skupine, by = c("drzava" = "drzava")),
                                aes(x = long, y = lat, group = group, fill = skupina), show.legend=T) +
          ggtitle(paste('Države razdeljene v',as.character(n),  'skupin glede na podatek', colnames(tabela)[3], sep=' ')) + xlab("long") + ylab("lat") +
      coord_quickmap(xlim = c(-25, 40), ylim = c(32, 72))
  return(evropa)
}
zemljevid1 <- zemljevid.evrope(5, drzave)

zemljevid2 <- ggplot() + geom_polygon(data = zemljevid %>% left_join(velika_tabela %>% filter(leto==2015)),
                             aes(x = long, y = lat, group = group, fill=zaposlenost)) +
  coord_quickmap(xlim = c(-25, 40), ylim = c(32, 72))

#povprecja <- drzave %>% group_by(obcina) %>%
#  summarise(povprecje = sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))

vsote1 <- velika_tabela %>% group_by(leto) %>% summarise(prebivalci=sum(drzavljani))
vsote2 <- velika_tabela %>% right_join(vsote1) %>% group_by(leto) %>% summarise(mladina=sum(mladi*drzavljani/prebivalci))
povprecno <- velika_tabela %>% right_join(vsote1) %>% right_join(vsote2) %>%
  group_by(leto) %>% summarise(BDPpc=sum(BDPpc*drzavljani/prebivalci),
                                                        drzavljani=mean(prebivalci),
                                                        mladi=mean(mladina),
                                                        zaposlenost=mean(zaposlenost*mladi*drzavljani/(mladina*prebivalci)),
                                                        izobrazba=mean(izobrazba*mladi*drzavljani/(mladina*prebivalci)),
                                                        neformalno=mean(neformalno*mladi*drzavljani/(mladina*prebivalci)),
                                                        neaktivni=mean(neaktivni*mladi*drzavljani/(mladina*prebivalci)))


lin <- lm(data = povprecno, zaposlenost ~ leto + I(leto^2))
napovej <- data.frame(leto = c(2017, 2018),
                      zaposlenost = predict(lin, data.frame(leto=c(2017, 2018))), napoved= TRUE)
zape <- povprecno[c(1,5)]
zape$napoved <- FALSE
zape <- zape %>% rbind(napovej)
zaposlenost_evropa <- ggplot(zape) + aes(x=leto, y=zaposlenost, color=napoved) +
  geom_line() + geom_point() + ggtitle("Zaposlenost mladih v Evropi")



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

zaposlenost_drzave <- ggplot(zaposlenost %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
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


narisi.graf <- function(tabela1, tabela2, letnica, razdeli=25){
  tabela <- merge(tabela1, tabela2)
  colnames(tabela) <- c('leto', 'drzava', 'prva', 'druga')
  n <- 5
  tabela$skupine <- hclust(dist(scale(tabela$prva))) %>% cutree(n)
  graf <- ggplot(tabela %>% filter(leto==letnica)) +
    aes(x=prva, y=druga, color=skupine) +
    geom_point()
}

sloizb <- izobrazba %>%
  filter(drzava == 'Slovenia')
lin <- lm(data = sloizb, izobrazba ~ leto + I(leto^2) + I(leto^3))
napovej <- data.frame(leto = c(2017, 2018, 2019), drzava = 'Slovenia',
                        izobrazba = predict(lin, data.frame(leto=c(2017, 2018, 2019))), napoved=FALSE)
sloizb$napoved <- TRUE
sloizb <- sloizb %>% left_join(napovej) %>% rbind(sloizb %>% right_join(napovej))
sliz <- ggplot(sloizb) + aes(x=leto, y=izobrazba, color=napoved) +
    geom_line() +
    geom_smooth(method = 'lm', formula = y ~ x + I(x^2) + I(x^3)) +
    geom_point() +
    ggtitle('Izobrazba v Sloveniji skozi leta v in napoved za naslednja 3 leta') +
    xlab('leto') + ylab(izobrazba)

eizb <- izobrazba %>% filter(leto==2008)
eizb1 <- ggplot(eizb) + geom_histogram(binwidth = 4, color='grey', fill='olivedrab3') + aes(x=izobrazba) +
              ggtitle('Porazdelitev deleža izobrazbe v Evropi za 2008') +
              xlab('Delež') + ylab('Ferkvenca')

eizb <- izobrazba %>% filter(leto==2016)
eizb2 <- ggplot(eizb) + geom_histogram(binwidth = 4, color='grey', fill='orchid2') + aes(x=izobrazba) +
  ggtitle('Porazdelitev deleža izobrazbe v Evropi za 2016') +
  xlab('Delež') + ylab('Ferkvenca')


zim <- velika_tabela[c('leto', 'drzava', 'zaposlenost', 'mladi')]
zim$skupine <- hclust(dist(scale(velika_tabela$BDPpc))) %>% cutree(5)
zim.graf <- ggplot(zim %>% filter(leto==2016)) +
            aes(x=zaposlenost, y=mladi, color=as.character(skupine)) + geom_point(size=2, show.legend=F) +
            ggtitle('Primerjava zaposlenosti mladih v primerjavi zdeležem mladih v državi za leto 2016 z razdelitvijo glede na BDP per capita v 5 skupin')

bii <- velika_tabela[c('leto', 'drzava', 'BDPpc', 'izobrazba')]
bii.graf <- ggplot(bii %>% filter(leto==2016)) +
          aes(x=BDPpc, y=izobrazba) + geom_point(size=2) +
          geom_smooth(method = 'lm', formula = y ~ x + I(x^2) + I(x^3)) +
          ggtitle('Primerjava podatkov BDP per capita in izobrazbe za leto 2016')
