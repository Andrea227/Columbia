library(shiny)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = 'mytext',label = 'Enter Staff',value = 'hi'),
      verbatimTextOutput(outputId = 'mytextout')
    ),
    mainPanel(
      plotOutput(outputId = 'myplot')
    )
  )
)

server <- function(input, output, session) {
  output$myplot = renderPlot({
    plot(cars)
    title(input$mytext)
  })
  
  output$mytextout = renderPrint({
    print(input$mytext)
  })
}

shinyApp(ui, server)