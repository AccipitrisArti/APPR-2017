library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Grafični prikaz"),
  
  tabsetPanel(
      tabPanel("Tabela",
               sidebarPanel(
                 selectInput("sprem1", label="Izberi spremenljivko",
                             choices=colnames(velika_tabela[c(-1,-2)]), selected='drzavljani')
               ),
               mainPanel(DT::dataTableOutput("tabele"))),
      
      tabPanel("Napoved",
               sidebarPanel(
                 selectInput("sprem2", label="Izberi spremenljivko",
                             choices=colnames(velika_tabela[c(-1,-2)]), selected='drzavljani')
               ),
               mainPanel(plotOutput("napovedi"))),
      
      tabPanel("Primerjave spremenljivk",
               sidebarPanel(
                   selectInput("spremenljivka1", label="Izberi spremenljivko na x osi",
                               choices=colnames(velika_tabela[c(-1,-2)]), selected='drzavljani'),
                   selectInput("spremenljivka2", label="Izberi spremenljivko na y osi",
                               choices=colnames(velika_tabela[c(-1,-2)]), selected='mladi'),
                   sliderInput('letnica', label='Izberi leto',
                               min=2008, max=2016, step=1, value = 2016)
                ),
               mainPanel(plotOutput("primerjava")))
    )
))
