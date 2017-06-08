library(shiny)
require(stats)

shinyServer(function(input, output) {
  output$tabele <- DT::renderDataTable({
    dcast(velika_tabela[c('leto', 'drzava', input$sprem1)], drzava ~ leto, value.var = input$sprem1) %>%
      rename(`Država` = drzava)
  })
  
  output$grafi <- renderPlot({
    tabela <- velika_tabela[c('leto', 'drzava', input$sprem2)] %>%
      filter(drzava == 'Hungary' | drzava == 'Spain' | drzava == 'Sweden' |
                    drzava == 'United Kingdom' | drzava == 'Italy' |
                    drzava == 'Slovenia' | drzava == 'Poland' |
                    drzava == 'Austria' | drzava == 'Croatia')
    colnames(tabela) <- c('leto', 'drzava', 'sprem')
    lin <- lm(data = tabela, sprem ~ leto)
    print(predict(lin, data.frame(leto=c(2017, 2018, 2019))))
    print(ggplot(tabela) +
            aes(x=leto, y=sprem, color=drzava) + geom_line() +
            ggtitle(paste(input$sprem2, 'skozi leta', sep = ' ')) +
            xlab('leto') + ylab(input$sprem2))
  })
  
  output$napovedi <- renderPlot({
    tabela <- velika_tabela[c('leto', 'drzava', input$sprem3)] %>%
      filter(drzava == input$drzava)
    colnames(tabela) <- c('leto', 'drzava', 'sprem')
    lin <- lm(data = tabela, sprem ~ leto)
    tabela <- tabela %>% rbind(data.frame(leto = c(2017, 2018, 2019), drzava = input$drzava,
                                         sprem = predict(lin, data.frame(leto=c(2017, 2018, 2019)))))
    print(ggplot(tabela) +
            aes(x=leto, y=sprem) + geom_line() +
            geom_smooth(method = input$priblizek1, formula = y ~ x + I(x^2) + I(x^3)) +
            ggtitle(paste(input$sprem3, 'skozi leta v', input$drzava, sep = ' ')) +
            xlab('leto') + ylab(input$sprem3))
  })
  
  output$primerjava <- renderPlot({
    tabela <- velika_tabela[c('leto', 'drzava', input$spremenljivka1[1], input$spremenljivka2[1])]
    colnames(tabela) <- c('leto', 'drzava', 'prva', 'druga')
    if (input$priblizek2 == 'Izberi metodo'){
    n <- sum(tabela$leto == input$letnica)
    if (input$skup>=n){
      print(ggplot(tabela %>% filter(leto==as.integer(input$letnica))) +
              aes(x=prva, y=druga, color=drzava) + geom_point() +
              ggtitle(paste('Primerjava podatkov', input$spremenljivka1, 'in', input$spremenljivka2, 'za leto', as.character(input$letnica), sep = ' ')) +
              xlab(input$spremenljivka1) + ylab(input$spremenljivka2))
    } else {
    tabela$skupine <- hclust(dist(scale(velika_tabela$BDPpc))) %>% cutree(input$skup)
    print(ggplot(tabela %>% filter(leto==as.integer(input$letnica))) +
      aes(x=prva, y=druga, color=as.character(skupine)) + geom_point(show.legend=F) +
        ggtitle(paste('Primerjava podatkov', input$spremenljivka1, 'in', input$spremenljivka2, 'za leto', as.character(input$letnica), sep = ' ')) +
        xlab(input$spremenljivka1) + ylab(input$spremenljivka2))}
    } else {
      print(ggplot(tabela %>% filter(leto==as.integer(input$letnica))) +
              aes(x=prva, y=druga) + geom_point() +
              geom_smooth(method = input$priblizek2, formula = y ~ x + I(x^2) + I(x^3)) +
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
                          xlab = 'Število', ylab = 'Pogostost'
                          )
    })
})
