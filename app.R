library(leaflet)
library(dplyr)
library(sp)
#library(shiny.semantic)
library(ggplot2)


ui1 <-fluidPage(
  titlePanel("APPSILON"),
  mainPanel(
    tabsetPanel(
      tabPanel("Map", leafletOutput("plot1"
      )),
      tabPanel("Details",
               DT::dataTableOutput("plot2"),
               selectInput("shipname",
                           "ShipNames:",
                           c(names(table(ships$SHIPNAME))))
      ),
      tabPanel("Graphs GGPLOT" ,verbatimTextOutput("a"),
               plotOutput("plotg1"),
               plotOutput("plotg2")
      )
    )
  )
)

server1 <- function(input, output) {
  zo=byship("VENTURA")
  output$plot1 <- renderLeaflet({
    leaflet(data = zo[1:nrow(zo),]) %>%
      addTiles() %>%
      addMarkers(~LON, ~LAT, popup = ~as.character(SPEED), label = ~as.character(SPEED))
  })
  output$plot2 <- DT::renderDataTable(DT::datatable({
    ships
  }))
  output$plotg1 <- renderPlot({
    ggplot(ships, aes(COURSE, HEADING )) + geom_jitter(height = 2, width = 2)
  })
  output$plotg2 <- renderPlot({
    ggplot(ships, aes(diff, SPEED )) + geom_jitter(height = 2, width = 2)
  })
}

shinyApp(ui1, server1)
