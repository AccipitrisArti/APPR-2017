# Analiza podatkov s programom R, 2016/17

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2016/17

## Tematika
# Aktivnost mladih v Evropi

V tem projektu bom analiziral vključenost mladih v šolski sistem, trg dela, prostovoljstvo ... po letih po evropskih državah.

Države bom razvrščal po deležu posameznih kategorij, BDP/prebivalca ...
Napovedoval bom spremembe v številu zaposlenih, šolajočih se, prostovoljcev za prihodnja leta.

Podatkovne vire bom jemal z:
* Eurostata v csv in html
* ...

V stolpcih 'Tidy data' bodo:
* Država,
* leto,
* BDP per capita, število prebivalcev, delež mladih, delež zaposlenih, delež izobraženih, delež neaktivnih, delež mladih v neformalnem izobraževanju, delež prostovoljcev ...

Države bom razvrščal po 
* deležu posameznih kategorij
* ...

Za prihodnja leta bom napovedoval spremembe v
* številu zaposlenih,
* šolajočih se, 
* ...

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`. Ko ga prevedemo,
se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`. Podatkovni
viri so v mapi `podatki/`. Zemljevidi v obliki SHP, ki jih program pobere, se
shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `maptools` - za uvoz zemljevidov
* `sp` - za delo z zemljevidi
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `reshape2` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)
