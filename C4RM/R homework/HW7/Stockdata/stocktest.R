library(shiny)
library(quantmod)

# Return function
getReturns = function(x,lag){
  n = length(x)
  ratiovec = x[(1+lag):n]/x[1:(n-lag)]
  returns =  ratiovec - 1
  return(returns)
}

# Plot hist function

# VaR function
PercentVaR = function(r,alpha){
  quantile(r,1-alpha)
}

# ES function
ES = function(r, alpha){
  var = quantile(r, 1-alpha)
  ESc = r[r < var]
  mean(ESc)
}

# UI

ui <- fluidPage(
  titlePanel("Stock Data"),
  
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "symbol",label =  "Symbol", value = "AAPL"),
      
      dateRangeInput(inputId = "dates",
                     label = "Date range",
                     start = "2013-01-01",
                     end = as.character(Sys.Date())),
      numericInput(inputId = "lag",label = "lag", value = 1),
      numericInput(inputId = "cf",label = "Confidence Level", value = 0.99),
      numericInput(inputId = "bins",label = "Bins", value = 10),
      checkboxInput(inputId = "addvar",
                    label = "Add VaR?",
                    value = FALSE),
      checkboxInput(inputId = "addes",
                    label = "Add Estimated Shortfall?",
                    value = FALSE),
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Price", plotOutput("plot")),
                  tabPanel("Returns, Var and Estimated Shortfall", plotOutput("veplot"))
      )
      )
  )
)

# Server
server <- function(input, output) {

  datainput <- reactive({
    getSymbols(input$symbol,
               src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
  laginput <- reactive(input$lag)
  
  cfinput <- reactive(input$cf)
  
  binput <- reactive(input$bins)
  
 # Print Price

 output$plot <- renderPlot({
   x = datainput()
   price = x[,6]
   plot(price)
 })
  
  # Print Histogram of returns, var and estimated shortfall
  
  output$veplot <- renderPlot({
    x = datainput()
    price = as.numeric(x[,6])
    l = laginput()
    res1 = getReturns(price,l)
    conf = cfinput()
    vr = PercentVaR(res1, conf)
    es = ES(res1, conf)
    bin = binput()
    hist(res1, breaks = bin)
    if(input$addvar){abline(v = vr, col = 'red')}
    if(input$addes){abline(v = es, col = 'blue')}
  })

}

shinyApp(ui, server)
