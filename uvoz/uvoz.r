# 2. faza: Uvoz podatkov
# drzave, drzavljani, mladi, izobrazba, neformalno, zaposlenost, neaktivni, religija, prostovoljstvo
library(rvest)
library(gsubfn)
library(readr)
library(dplyr)
source("lib/uvozi.zemljevid.r", encoding = "UTF-8")

# Funkcija, ki uvozi podatke o BDP-jih držav (v EUR na osebo)
uvozi.drzave <- function() {
  data <- read_csv("podatki/nama_10_pc_1_Data.csv",
                    locale = locale(encoding = "Windows-1250"))
  names(data) <- c("leto", 'drzava', 'enota', 'BDP', 'BDPpc')
  data <- data %>% filter(! grepl("^Euro", drzava, ignore.case=TRUE),
                          grepl("Current prices, euro per capita", enota, ignore.case=TRUE),
                          grepl("Gross domestic product at market prices", BDP, ignore.case=TRUE)) %>%
    select(-enota, -BDP)
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  data$BDPpc <- parse_number(data$BDPpc, na=c(NA, ":"))
  return(data)
}
#drzave <- uvozi.drzave()
#drzave <- na.omit(drzave)

# Funkcija, ki uvozi podatke o drzavljanih (v tisoč)
uvozi.drzavljani <- function() {
  data <- read_csv("podatki/nama_10_pe_1_Data.csv", locale = locale(encoding = "UTF-8"))
  colnames(data) <- c("leto", 'drzava', 'enota', 'Skupno', 'drzavljani')
  data <- data %>% filter(! grepl("^Euro", drzava, ignore.case=TRUE),
                          grepl("Thousand persons", enota, ignore.case=TRUE)) %>%
    select(-enota, -Skupno)
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  data$drzavljani <- parse_number(data$drzavljani, na=c(NA, ":"))
  return(data)
}
#drzavljani <- uvozi.drzavljani()
#drzavljani <- na.omit(drzavljani)

# Funkcija, ki uvozi podatke o stevilu mladih (stevilo)
uvozi.mladi <- function() {
  data <- read_csv("podatki/yth_demo_010_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"), na=':')
  names(data) <- c("leto", 'drzava', 'Skupno','enota', 'starost', 'mladi', 'komentarji')
  data <- data %>% filter(! grepl("^Euro", drzava, ignore.case=TRUE),
                          grepl("Total", Skupno, ignore.case=TRUE)) %>%
    select(-enota, -Skupno, -komentarji)
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  data$mladi <- parse_number(data$mladi, na=c(NA, ":"))
  data <- na.omit(data)
  data <- data %>% 
    group_by(leto, drzava) %>% 
    summarise(mladi = sum(mladi))
  odstotki <- merge(data, drzavljani)
  odstotki$odstotki <- odstotki$mladi/(odstotki$drzavljani*10)
  data <- merge(data, odstotki)
  data$mladi <- NULL
  data$drzavljani <- NULL
  names(data) <- c('leto', 'drzava', 'mladi')
  return(data)
}
#mladi <- uvozi.mladi()

# Funkcija, ki uvozi podatke o izobrazenosti mladih (v odstotkih, ženske)
uvozi.izobrazba <- function() {
  data <- read_csv("podatki/yth_demo_040_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"), na=':')
  names(data) <- c("leto", 'drzava', 'izobrazenost', 'spol', 'starost', 'enota', 'izobrazba', 'komentarji')
  data <- data %>% filter(! grepl("^Euro", drzava, ignore.case=TRUE)) %>%
    select(-enota, -komentarji, -spol, -starost, -izobrazenost)
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  return(data)
}
#izobrazba <- uvozi.izobrazba()
#izobrazba$izobrazba <- parse_number(izobrazba$izobrazba, na=c(NA, ":"))
#izobrazba <- na.omit(izobrazba)

# Funkcija, ki uvozi podatke o neformalnem izobrazevanju (v odstotkih, stari 15-24)
uvozi.neformalno <- function() {
  data <- read_csv("podatki/yth_educ_060_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"), na=':')
  names(data) <- c("leto", 'drzava', 'enota', 'spol', 'starost', 'neformalno', 'komentarji')
  data <- data %>% filter(! grepl("^Euro", drzava, ignore.case=TRUE),
                          grepl("Total", spol, ignore.case=TRUE)) %>%
    select(-enota, -komentarji, -starost)
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  data$neformalno <- parse_number(data$neformalno, na=c(NA, ":"))
  data <- data %>% 
    group_by(leto, drzava) %>% 
    summarise(neformalno = sum(neformalno))
  return(data)
}
#neformalno <- uvozi.neformalno()
#neformalno <- na.omit(neformalno)

# Funkcija, ki uvozi podatke o zaposlenosti (v odstotkih)
uvozi.zaposlenost <- function() {
  data <- read_csv("podatki/yth_empl_030_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"), na=':')
  names(data) <- c("leto", 'drzava', 'spol', 'starost', 'enota', 'zaposlenost')
  data <- data %>% select(-enota, -spol, -starost)
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  data$zaposlenost <- parse_number(data$zaposlenost, na=c(NA, ":"))
  data <- data %>% 
    group_by(leto, drzava) %>% 
    summarise(zaposlenost = sum(zaposlenost))
  return(data)
}
#zaposlenost <- uvozi.zaposlenost()
#zaposlenost <- na.omit(zaposlenost)

# Funkcija, ki uvozi podatke o neaktivnih (nezaposlenih, nešolajočih se, starih 15-19) (v odstotkih)
uvozi.neaktivni <- function() {
  data <- read_csv("podatki/yth_empl_160_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"), skip=1, na=':')
  names(data) <- c("leto", 'drzava', 'spol', 'starost', 'zaposlen', 'Neaktivnost', 'izobrazba', 'enota', 'neaktivni', 'komentarji')
  data <- data %>% filter(! grepl("^Euro", drzava, ignore.case=TRUE)) %>%
    select(-enota, -komentarji, -starost, -zaposlen, -spol, -Neaktivnost, -izobrazba)
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  return(data)
}
#neaktivni <- uvozi.neaktivni()
#neaktivni$neaktivni <- parse_number(neaktivni$neaktivni, na=c(NA, ":"))
#neaktivni <- na.omit(neaktivni)

# Funkcija, ki uvozi podatke o vkljucenosti v aktivnosti znotraj religij in cerkva (v odstotkih, 2006)
uvozi.religija <- function() {
  data <- read_csv("podatki/yth_part_030_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"), na=':')
  names(data) <- c("starost", 'drzava', 'enota', 'spol', 'leto', 'DA_NE', 'religija')
  data <- data %>% filter(! grepl("^Euro", drzava, ignore.case=TRUE),
                          grepl("Total", spol, ignore.case=TRUE)) %>%
    select(-enota, -DA_NE, -leto)
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  data$enota <- NULL
  data$DA_NE <- NULL
  data$leto <- NULL
  data$religija <- parse_number(data$religija, na=c(NA, ":"))
  data <- data %>% 
    group_by(drzava) %>% 
    summarise(religija = sum(religija))
  return(data)
}
#religija <- uvozi.religija()
#religija <- na.omit(religija)

# Funkcija, ki uvozi podatke o prostovoljstvu (v odstotkih za 2006)
uvozi.prostovoljstvo <- function() {
  stran <- 'podatki/yth_volunt_010.html' %>% read_html()
  data <- stran %>% html_nodes(xpath="//table") %>%
    .[[1]] %>% html_table(dec = ".")
  colnames(data) <- c("drzava", "prostovoljstvo", "od20do29", "od16do19", "od16do24",
                       "od20do24", "od25do29")
  data <- data %>% filter(! grepl("^Euro", drzava, ignore.case=TRUE)) %>%
    select(-c(3, 4, 5, 6, 7))
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  data <- data[-28,]
  data$prostovoljstvo <- parse_double(data$prostovoljstvo)
  #moski <- stran %>% html_nodes(xpath="//table") %>%
  #  .[[2]] %>% html_table(dec = ".")
  #colnames(moski) <- c("drzava", "od16do29", "od20do29", "od16do19", "od16do24",
  #                      "od20do24", "od25do29")
  #izbris <- moski$drzava == moski$drzava[grep("^Euro", moski$drzava, ignore.case=TRUE)]
  #moski <- moski[!izbris,]
  #moski <- moski[-28,]
  #moski$drzava <- gsub(" \\(.*\\)$", "", moski$drzava, ignore.case=TRUE)
  #moski$spol <- 'males'
  #  .[[3]] %>% html_table(dec = ".")
  #  moski$drzava <- gsub(" \\(.*\\)$", "", moski$drzava, ignore.case=TRUE)zenske <- stran %>% html_nodes(xpath="//table") %>%
  #colnames(zenske) <- c("drzava", "od16do29", "od20do29", "od16do19", "od16do24",
  #                      "od20do24", "od25do29")
  #izbris <- zenske$drzava == zenske$drzava[grep("^Euro", zenske$drzava, ignore.case=TRUE)]
  #zenske <- zenske[!izbris,]
  #zenske <- zenske[-28,]
  #zenske$drzava <- gsub(" \\(.*\\)$", "", zenske$drzava, ignore.case=TRUE)
  #zenske$spol <- 'females'
  #moski$od16do29 <- NULL
  #moski$od20do29 <- NULL
  #moski$od16do24 <- NULL
  #zenske$od16do29 <- NULL
  #zenske$od20do29 <- NULL
  #zenske$od16do24 <- NULL
  #tabela <- rbind(moski, zenske)
  #for (col in c("od16do19", "od20do24", "od25do29")) {
  #  tabela[[col]] <- parse_number(tabela[[col]], na = "-")
  #}
  #for (col in c("drzava", "spol")) {
  #  tabela[[col]] <- factor(tabela[[col]])
  #}
  #a <- tabela[c(1,2,5)]
  #names(a)[2] <- 'prostovoljstvo'
  #a$starost <- '16-19'
  #b <- tabela[c(1,3,5)]
  #names(b)[2] <- 'prostovoljstvo'
  #a$starost <- '20-24'
  #c <- tabela[c(1,4,5)]
  #names(c)[2] <- 'prostovoljstvo'
  #c$starost <- '25-29'
  #tabela <- rbind(a, b, c)
  #tabela$prostovoljstvo <- parse_number(tabela$prostovoljstvo, na=c(NA, ":"))
  #tabela <- tabela %>% 
  #  group_by(drzava) %>% 
  #  summarise(prostovoljstvo = sum(prostovoljstvo))
  return(data)
}
#prostovoljstvo <- uvozi.prostovoljstvo()

#write.csv(drzave,"podatki/urejeni_podatki/Drzave.csv",row.names=FALSE)
#write.csv(drzavljani,"podatki/urejeni_podatki/Drzavljani.csv",row.names=FALSE)
#write.csv(mladi,"podatki/urejeni_podatki/Mladi.csv",row.names=FALSE)
#write.csv(izobrazba,"podatki/urejeni_podatki/Izobrazba.csv",row.names=FALSE)
#write.csv(neformalno,"podatki/urejeni_podatki/Neformalno.csv",row.names=FALSE)
#write.csv(zaposlenost,"podatki/urejeni_podatki/Zaposlenost.csv",row.names=FALSE)
#write.csv(neaktivni,"podatki/urejeni_podatki/Neaktivni.csv",row.names=FALSE)
#write.csv(prostovoljstvo,"podatki/urejeni_podatki/Prostovoljstvo.csv",row.names=FALSE)
#write.csv(religija,"podatki/urejeni_podatki/Religija.csv",row.names=FALSE)

drzave <- read_csv("podatki/urejeni_podatki/Drzave.csv",
                   locale = locale(encoding = "Windows-1250"), na=':')
drzavljani <- read_csv("podatki/urejeni_podatki/Drzavljani.csv",
                       locale = locale(encoding = "Windows-1250"), na=':')
mladi <- read_csv("podatki/urejeni_podatki/Mladi.csv",
                  locale = locale(encoding = "Windows-1250"), na=':')
mladi$mladi <- round(mladi$mladi, 1)
izobrazba <- read_csv("podatki/urejeni_podatki/Izobrazba.csv",
                      locale = locale(encoding = "Windows-1250"), na=':')
neformalno <- read_csv("podatki/urejeni_podatki/Neformalno.csv",
                       locale = locale(encoding = "Windows-1250"), na=':')
zaposlenost <- read_csv("podatki/urejeni_podatki/Zaposlenost.csv",
                        locale = locale(encoding = "Windows-1250"), na=':')
neaktivni <- read_csv("podatki/urejeni_podatki/Neaktivni.csv",
                      locale = locale(encoding = "Windows-1250"), na=':')
prostovoljstvo <- read_csv("podatki/urejeni_podatki/Prostovoljstvo.csv",
                           locale = locale(encoding = "Windows-1250"), na=':')
religija <- read_csv("podatki/urejeni_podatki/Religija.csv",
                     locale = locale(encoding = "Windows-1250"), na=':')

# primerjave
# (kar je za vec let)
#velika_tabela <- drzave %>% merge(drzavljani) %>% merge(mladi) %>%
#  merge(izobrazba) %>% merge(zaposlenost) %>% merge(neformalno) %>% merge(neaktivni)
# (kar je samo za 2006)
#mala_tabela <- merge(religija, prostovoljstvo)
#velika_tabela <- velika_tabela %>% 
#  group_by(leto, drzava) %>% 
#  summarise(BDPpc = mean(BDPpc), drzavljani = mean(drzavljani),
#            mladi = mean(mladi), zaposlenost = mean(zaposlenost), izobrazba = mean(izobrazba),
#            neformalno = mean(neformalno), neaktivni = mean(neaktivni))
#write.csv(velika_tabela,"podatki/urejeni_podatki/Velika_tabela.csv",row.names=FALSE)
velika_tabela <- read_csv("podatki/urejeni_podatki/Velika_tabela.csv",
                          locale = locale(encoding = "Windows-1250"), na=':')
#write.csv(mala_tabela,"podatki/urejeni_podatki/Mala_tabela.csv",row.names=FALSE)
#mala_tabela <- read_csv("podatki/urejeni_podatki/Mala_tabela.csv",
#                          locale = locale(encoding = "Windows-1250"), na=':')
