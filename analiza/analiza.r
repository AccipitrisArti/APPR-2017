# 4. faza: Analiza podatkov


# Število skupin
n <- 5
skupine <- hclust(dist(scale(drzave$BDPpc))) %>% cutree(n)
