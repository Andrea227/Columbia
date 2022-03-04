library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      plotOutput(outputId = 'myplot')
      )
  )
)

server <- function(input, output, session) {
  output$myplot = renderPlot({
    plot(cars)
  })
}

shinyApp(ui, server)