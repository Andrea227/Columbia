library(shiny)

x <- faithful[, 2] 
x2 = x

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for cut
  sidebarLayout(
    sidebarPanel(
      numericInput("lines",
                   "Vertical Line",
                   min = 1,
                   value = 60),
      actionButton("add", "Add?"),
      numericInput("xlimL", "Lower x", value=0),
      numericInput("xlimU", "Upper x", value=50)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output) {
  
  data <- reactiveValues(x2 = x2, lines = NULL)
  
  output$distPlot <- renderPlot({
    plot(data$x2, xlim = c(input$xlimL, input$xlimU))
    abline(v =  data$lines)
  })
  
  observeEvent(input$add,{
    data$lines = c(data$lines, input$lines)
  })
}

shinyApp(ui = ui, server = server)