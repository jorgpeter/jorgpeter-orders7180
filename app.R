library(shiny)
library(tidyverse)
library(openxlsx)
library(janitor)
library(bslib)
library(lubridate)
orders <- read.xlsx("data/bestellingen 7180 2025_2026_latest.xlsx" )
orders <- as.data.frame(orders)
#orders$Winkelwagennummer <- as.numeric(orders$Winkelwagennummer)
#orders$Pos. <- as.numeric(orders$Pos.)
orders$Status <- as.factor(orders$Status)
#orders$Nettowaarde <- as.numeric(orders$Nettowaarde)
orders$Valuta <- as.factor(orders$Valuta)
orders$Gecreeerd.door <- as.factor(orders$Gecreeerd.door)
orders$Date <- as.Date(orders$Date, origin="1900-01-01") 
orders$Groep <- as.factor(orders$Groep)
orders$Wie_Wat <- as.factor(orders$Wie_Wat)
orders$Rekening <- as.factor(orders$Rekening)

#neem aleen de echte orders
exotisch <- orders %>% filter(Status != "Verwijderd" & Groep == "Exotisch")
orders <- orders %>% filter(Status != "Verwijderd" & Groep != "Exotisch")
orders <- orders %>% arrange(desc(Nettowaarde))
#shiny ui
ui <- page_sidebar(
  title= "Overzicht uitgaven 7180",
  sidebar = sidebar("Kies eerst een periode",
                    dateRangeInput("datum", "Datum", start = "2026-01-01", end = "2026-12-31" ),
                    selectInput("groep", "Groep", choices = NULL),
                    selectInput("wat", "Wat", choices = NULL) ),
  card(
    card_header("Wat?"),
    plotOutput("plot"))
  ,
  card(
    card_header("Artikelen"),
    DT::dataTableOutput("data")
  )
)


#shiny server
server <- function(input, output, session) {
  datum <- reactive({
    filter(orders, Date >= input$datum[1] & Date <= input$datum[2])
  })
  
  groep <- reactive({
    filter(orders, (Date >= input$datum[1] & Date <= input$datum[2]) & Groep == input$groep)
  })
  observeEvent(datum(), {
    choices <- c("", sort(as.character(datum()$Groep)))
    freezeReactiveValue(input, "groep")
    updateSelectInput(inputId = "groep", choices = choices)
  })
  observeEvent(groep(), {
    choices <- c("", sort(as.character(groep()$Wie_Wat)))
    freezeReactiveValue(input, "wat")
    updateSelectInput(inputId = "wat", choices = choices)
  })
  
  output$plot <- renderPlot (
    {  req(input$groep)
      orders %>% filter(Date >= input$datum[1] & Date <= input$datum[2] ) %>% filter(Groep == input$groep) %>%
        group_by(Wie_Wat) %>% summarise(Totaal = sum(Nettowaarde)) %>%
        ggplot(aes(fct_rev(fct_reorder(Wie_Wat, Totaal)), Totaal, fill=Wie_Wat)) +
        geom_col()+
        labs(x = "Wat",
             title = input$groep)+
        scale_x_discrete(guide = guide_axis(angle = 45))+
        theme_minimal()+
        guides(fill = "none")}
  )
  output$data <- DT::renderDT({
    req(input$groep)
    req(input$wat)
    groep() %>% 
      filter(Date >= input$datum[1] & Date <= input$datum[2] ) %>%
      filter(Wie_Wat == input$wat) %>%
      select(Winkelwagennummer,Naam.winkelwagen,Positienaam,Date, Nettowaarde)
  })
}

#run shiny
shinyApp(ui, server)