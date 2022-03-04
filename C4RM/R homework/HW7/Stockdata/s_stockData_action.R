library(shiny)
library(quantmod)

ui <- fluidPage(
  titlePanel("Stock Data"),

  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "symbol",label =  "Symbol",value =  "MSFT"),

      dateRangeInput(inputId = "dates",
                     label = "Date range",
                     start = "2013-01-01",
                     end = as.character(Sys.Date())),
      shiny::actionButton(inputId = "action_getData",label = "Get Data")
    ),

    mainPanel(plotOutput("plot"))
  )
)

server <- function(input, output) {

  obj_function = eventReactive(input$action_getData,{
      getSymbols(input$symbol, 
                 src = "yahoo",
                 from = input$dates[1],
                 to = input$dates[2],
                 auto.assign = FALSE)
  })
  
  output$plot <- renderPlot({
    plot(obj_function())
  })
  
}

shinyApp(ui, server)
