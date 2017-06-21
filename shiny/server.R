library(shiny)
require(stats)
library(mgcv)
#source("../vizualizacija/vizualizacija.r", encoding = "UTF-8")


shinyServer(function(input, output) {
  output$tabele <- DT::renderDataTable({
    dcast(velika_tabela[c('leto', 'drzava', input$sprem1)], drzava ~ leto, value.var = input$sprem1) %>%
      rename(`Država` = drzava)
  })
  
  output$grafi <- renderPlot({
    tabela <- velika_tabela[c('leto', 'drzava', input$sprem2)]
    colnames(tabela) <- c('leto', 'drzava', 'sprem')
    tabela1 <- tabela %>% filter(drzava == input$drzav[1])
    for (i in input$drzav[-1]){
      tabela1 <- tabela1 %>% rbind(tabela %>% filter(drzava == i))}
    tabela <- tabela1
    print(tabela)
    lin <- lm(data = tabela, sprem ~ leto)
    print(ggplot(tabela) +
            aes(x=leto, y=sprem, color=drzava) + geom_line() +
            ggtitle(paste(input$sprem2, 'skozi leta', sep = ' ')) +
            xlab('leto') + ylab(input$sprem2))
  })
  
  output$napovedi <- renderPlot({
    if (input$drzava == 'Evropa'){
    tabela <- povprecno[c('leto', input$sprem3)]
    tabela$drzava <- 'Evropa'
    colnames(tabela) <- c('leto', 'sprem', 'drzava')
    } else {
    tabela <- velika_tabela[c('leto', 'drzava', input$sprem3)] %>%
      filter(drzava == input$drzava)
    colnames(tabela) <- c('leto', 'drzava', 'sprem')
    }
    apr <- input$priblizek1 %>% gsub('y','sprem', ., ignore.case=TRUE) %>% gsub('x','leto', ., ignore.case=TRUE)
    lin <- lm(data = tabela, as.formula(apr))
    print(input$napoved1)
    if (input$napoved1){
      napovej <- data.frame(leto = c(2017, 2018, 2019), drzava = input$drzava,
                            sprem = predict(lin, data.frame(leto=c(2017, 2018, 2019))), napoved=FALSE)
      tabela$napoved <- TRUE
      tabela <- tabela %>% left_join(napovej) %>% rbind(tabela %>% right_join(napovej))
      g <- ggplot(tabela) + aes(x=leto, y=sprem, color=napoved) +
            geom_line() +
            geom_smooth(method = 'lm', formula = as.formula(input$priblizek1)) +
            geom_point() +
            ggtitle(paste(input$sprem3, 'skozi leta v', input$drzava, 'in napoved za naslednja 3 leta', sep = ' ')) +
            xlab('leto') + ylab(input$sprem3)
      } else {
        g <- ggplot(tabela) + aes(x=leto, y=sprem) +
          geom_line() +
          geom_smooth(method = 'lm', formula = as.formula(input$priblizek1)) +
          geom_point() +
          ggtitle(paste(input$sprem3, 'skozi leta v', input$drzava, sep = ' ')) +
          xlab('leto') + ylab(input$sprem3)        
      }
    print(g)
  })
  
  output$primerjava <- renderPlot({
    tabela <- velika_tabela[c('leto', 'drzava', input$spremenljivka1[1], input$spremenljivka2[1])]
    colnames(tabela) <- c('leto', 'drzava', 'prva', 'druga')
    if (input$priblizek2 == 'Izberi metodo'){
    n <- sum(tabela$leto == input$letnica)
    if (input$skup>=n){
      print(ggplot(tabela %>% filter(leto==as.integer(input$letnica))) +
              aes(x=prva, y=druga, color=drzava) + geom_point(size=2) +
              ggtitle(paste('Primerjava podatkov', input$spremenljivka1, 'in', input$spremenljivka2, 'za leto', as.character(input$letnica), sep = ' ')) +
              xlab(input$spremenljivka1) + ylab(input$spremenljivka2))
    } else {
    tabela$skupine <- hclust(dist(scale(velika_tabela$BDPpc))) %>% cutree(input$skup)
    print(ggplot(tabela %>% filter(leto==as.integer(input$letnica))) +
      aes(x=prva, y=druga, color=as.character(skupine)) + geom_point(size=3, show.legend=F) +
        ggtitle(paste('Primerjava podatkov', input$spremenljivka1, 'in', input$spremenljivka2, 'za leto', as.character(input$letnica), sep = ' ')) +
        xlab(input$spremenljivka1) + ylab(input$spremenljivka2))}
    } else {
      print(ggplot(tabela %>% filter(leto==as.integer(input$letnica))) +
              aes(x=prva, y=druga) + geom_point() +
              geom_smooth(method = 'lm', formula = as.formula(input$priblizek2)) +
              ggtitle(paste('Primerjava podatkov', input$spremenljivka1, 'in', input$spremenljivka2, 'za leto', as.character(input$letnica), sep = ' ')) +
              xlab(input$spremenljivka1) + ylab(input$spremenljivka2))
    }
  })
  
  output$porazdelitev <- renderPlot({
    tabela <- velika_tabela[c('leto', 'drzava', input$porazd)]
    colnames(tabela) <- c('leto', 'drzava', 'porazdelji')
    tabela <- tabela %>% filter(leto==as.integer(input$letnik))
    hist(tabela$porazdelji, breaks = input$koraki, col = "gray",
                          main = paste('Porazdelitev', input$porazd, sep = ' '),
                          xlab = 'Število', ylab = 'Ferkvenca'
                          )
    })
  
  output$zemljevidi <- renderPlot({
    drzAve <- velika_tabela[c('leto','drzava', input$sp)] %>% filter(leto==input$le) %>% mutate(drzava = parse_factor(drzava, levels(zemljevid$drzava)))
    colnames(drzAve) <- c('leto','drzava', 'spre')
    if (input$skupin == 'Negrupirano'){
      zemlj <- ggplot() + geom_polygon(data = zemljevid %>% left_join(drzAve),
                                       aes(x = long, y = lat, group = group, fill=spre)) +
        ggtitle(paste('Države obarvane glede na',
                      input$sp, sep=' ')) + xlab("long") + ylab("lat") +
        coord_quickmap(xlim = c(-25, 40), ylim = c(32, 72)) + guides(fill=guide_legend(title=input$sp))
    } else {
      drzAve.norm <- drzAve %>% select(-drzava, -leto) %>% scale()
      rownames(drzAve.norm) <- drzAve$drzava
      k <- kmeans(drzAve.norm, as.integer(input$skupin), nstart = 1000)  
      skupine <- data.frame(drzava = drzAve$drzava, skupina = factor(k$cluster))
      zemlj <- ggplot() + geom_polygon(data = zemljevid %>% left_join(skupine, by = c("drzava" = "drzava")),
                                      aes(x = long, y = lat, group = group, fill = skupina), show.legend=T) +
        ggtitle(paste('Države razdeljene v',as.character(input$skupin),  'skupin glede na podatek', input$sp, sep=' ')) + xlab("long") + ylab("lat") +
        coord_quickmap(xlim = c(-25, 40), ylim = c(32, 72))
    }
    print(zemlj)
  })
})
