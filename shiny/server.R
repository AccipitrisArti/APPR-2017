library(shiny)
require(stats)

shinyServer(function(input, output) {
  output$tabele <- DT::renderDataTable({
    dcast(velika_tabela[c('leto', 'drzava', input$sprem1)], drzava ~ leto, value.var = input$sprem1) %>%
      rename(`Država` = drzava)
  })
  
  output$napovedi <- renderPlot({
    tabela <- velika_tabela[c('leto', 'drzava', input$sprem2)] %>%
      filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                    drzava == 'United Kingdom' | drzava == 'Italy' |
                    drzava == 'Slovenia' | drzava == 'Poland' |
                    drzava == 'Austria' | drzava == 'Croatia')
    colnames(tabela) <- c('leto', 'drzava', 'sprem')
    print(ggplot(tabela) +
            aes(x=leto, y=sprem, color=drzava) + geom_line() +
            ggtitle(paste('Napoved za', input$sprem2, sep = ' ')) +
            xlab('leto') + ylab(input$sprem2))
  })
  
  output$primerjava <- renderPlot({
    tabela <- velika_tabela[c('leto', 'drzava', input$spremenljivka1[1], input$spremenljivka2[1])]
    colnames(tabela) <- c('leto', 'drzava', 'prva', 'druga')
    n <- sum(tabela$leto == input$letnica)
    if (input$skup>=n){
      print(ggplot(tabela %>% filter(leto==as.integer(input$letnica))) +
              aes(x=prva, y=druga, color=drzava) + geom_point() +
              ggtitle(paste('Primerjava podatkov', input$spremenljivka1, 'in', input$spremenljivka2, 'za leto', as.character(input$letnica), sep = ' ')) +
              xlab(input$spremenljivka1) + ylab(input$spremenljivka2))
    } else {
    tabela$skupine <- hclust(dist(scale(tabela$prva))) %>% cutree(input$skup)
    print(ggplot(tabela %>% filter(leto==as.integer(input$letnica))) +
      aes(x=prva, y=druga, color=as.character(skupine)) + geom_point(show.legend=F) +
        ggtitle(paste('Primerjava podatkov', input$spremenljivka1, 'in', input$spremenljivka2, 'za leto', as.character(input$letnica), sep = ' ')) +
        xlab(input$spremenljivka1) + ylab(input$spremenljivka2))}
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
