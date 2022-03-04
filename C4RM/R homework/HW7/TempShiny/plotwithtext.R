
library(shiny)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = 'mytext',label = 'Enter Some Text',value = 'hello')
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
}

shinyApp(ui, server)