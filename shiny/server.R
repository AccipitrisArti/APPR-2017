library(shiny)

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
    print(ggplot(tabela %>% filter(leto==as.integer(input$letnica))) +
      aes(x=prva, y=druga, color=drzava) + geom_point() +
        ggtitle(paste('Primerjava podatkov', input$spremenljivka1, 'in', input$spremenljivka2, 'za leto', as.character(input$letnica), sep = ' ')) +
        xlab(input$spremenljivka1) + ylab(input$spremenljivka2))
  })
})
