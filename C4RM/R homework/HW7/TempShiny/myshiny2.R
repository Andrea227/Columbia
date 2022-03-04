library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel('Hello the side!'),
    mainPanel('hello the main!')
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)