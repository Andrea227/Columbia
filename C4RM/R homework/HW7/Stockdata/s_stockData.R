library(shiny)
library(quantmod)

ui <- fluidPage(
  titlePanel("Stock Data"),

  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "symbol",label =  "Symbol",value =  "GOOG"),

      dateRangeInput(inputId = "dates",
                     label = "Date range",
                     start = "2013-01-01",
                     end = as.character(Sys.Date()))
    ),

    mainPanel(plotOutput("plot"))
  )
)

server <- function(input, output) {

  output$plot <- renderPlot({
    x = getSymbols(input$symbol, src = "yahoo",
                   from = input$dates[1],
                   to = input$dates[2],
                   auto.assign = FALSE)
    plot(x)
  })

}

shinyApp(ui, server)
