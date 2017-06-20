library(shiny)

ime <- rbind(data.frame(ime='Evropa', NUTS_ID='EU'), imena)

shinyUI(fluidPage(
  
  titlePanel("Grafični prikaz"),
  
  tabsetPanel(
      tabPanel("Tabela",
               hr(),
               sidebarLayout(
               
                 sidebarPanel(
                 selectInput("sprem1", label="Izberi spremenljivko",
                             choices=colnames(velika_tabela[c(-1,-2)]), selected='BDPpc')),
               mainPanel(DT::dataTableOutput("tabele")))
               ),
      
      tabPanel("Graf",
               sidebarPanel(
                 selectInput("sprem2", label="Izberi spremenljivko",
                             choices=colnames(velika_tabela[c(-1,-2)]), selected='BDPpc'),
                 checkboxGroupInput('drzav', label="Izberi države",
                                    choices=ime$ime, selected=c('Hungary', 'Spain', 'Sweden', 'United Kingdom',
                                                                'Italy', 'Slovenia', 'Poland', 'Austria', 'Croatia'
                                                                ))
               ),
               mainPanel(plotOutput("grafi"))),
      
      tabPanel("Napoved",
               sidebarPanel(
                 selectInput("sprem3", label="Izberi spremenljivko",
                             choices=colnames(velika_tabela[c(-1,-2)]), selected='BDPpc'),
                 selectInput("drzava", label="Izberi drzavo",
                             choices=ime$ime, selected='Slovenia'),
                 checkboxInput('napoved1', label='Napoved'),
                 selectInput("priblizek1", label="Izberi funkcijo linearne regresije",
                             choices=c('y ~ x', 'y ~ x + I(x^2)', 'y ~ x + I(x^2) + I(x^3)'), selected='y ~ x')               ),
               mainPanel(plotOutput("napovedi"))),
      
      tabPanel("Primerjave spremenljivk",
               sidebarPanel(
                   selectInput("spremenljivka1", label="Izberi spremenljivko na x osi",
                               choices=colnames(velika_tabela[c(-1,-2)]), selected='drzavljani'),
                   selectInput("spremenljivka2", label="Izberi spremenljivko na y osi",
                               choices=colnames(velika_tabela[c(-1,-2)]), selected='mladi'),
                   sliderInput('letnica', label='Izberi leto',
                               min=2008, max=2016, step=1, value = 2016),
                   selectInput("priblizek2", label="Izberi metodo aproksimacije",
                               choices=c('Izberi metodo', 'lm'), selected='Izberi metodo'),
                   sliderInput('skup', label='Izberi število skupin glede na BDPpc',
                               min=1, max=25, step=1, value = 5)
                ),
               mainPanel(plotOutput("primerjava"))),
      
      tabPanel("Porazdelitev",
               sidebarPanel(
                 selectInput("porazd", label="Izberi spremenljivko",
                             choices=colnames(velika_tabela[c(-1,-2)]), selected='BDPpc'),
                 sliderInput('letnik', label='Izberi leto',
                             min=2008, max=2016, step=1, value = 2016),
                 sliderInput('koraki', label='Izberi število korakov',
                             min=1, max=30, step=1, value = 10)
               ),
               mainPanel(plotOutput("porazdelitev")))
    )
))
