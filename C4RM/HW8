# Question 1
goToMyPage = function(){
  browseURL(url="https://andreayang227.shinyapps.io/Stock/")
}

# Question 2
goToMyPage_Stock = function(){
  browseURL(url="https://andreayang227.shinyapps.io/StockShiny2/")
}

# Question 3
goToMyPage_MLR = function(){
  browseURL(url="https://andreayang227.shinyapps.io/NewMLR/")
}

# Question 4
goToMyPage_Portfolio= function(){
  browseURL(url="https://andreayang227.shinyapps.io/PortfolioVolatility/")
}

############################ Question 1 code
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
                    value = FALSE)
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

############################ Question 2 Code
library(shiny)
library(quantmod)


# Tools
# VaR
# Return function
getReturns = function(x,lag){
  n = length(x)
  ratiovec = x[(1+lag):n]/x[1:(n-lag)]
  returns =  ratiovec - 1
  return(returns)
}

VaR = function(numvec,alpha = .99, returns = T){
  
  # Returns = T sets the interpreation of numvec as returns or losses
  # If numvec is interpreted as returns, then VaR is on the left tail
  if(returns == T){alpha = 1-alpha}
  percentile = quantile(x = numvec,probs = alpha)
  value = abs(percentile)
  return(value)
}

# Estimated Shortfall
ES = function(numvec,alpha = .99, returns = T, var = NULL, plotit = F){
  
  # plotit = F controls whether to plot a histogram
  # Returns = T sets the interpreation of numvec as returns or losses
  # If numvec is interpreted as returns, then VaR and ES are on the left tail
  
  if(is.null(var)){var = VaR(numvec,alpha,returns)}
  if(returns == T){
    idx_tail = numvec < -var
  } else {
    idx_tail = numvec > var
  }
  out = abs(mean(numvec[idx_tail]))
  
  if(plotit){
    hist(numvec,50)
    if(returns == T){
      abline(v = -var,col='red')  
      abline(v = -out,col='purple')  
    } else {
      abline(v = var,col='red')  
      abline(v = out,col='purple')  
    }}
  
  return(out)
}

# Rolling stats
RollingStatistic = function(datavec,windowSize,fun=sd){
  n = length(datavec)
  outvec = rep(NA,n-windowSize+1)
  idx_window = 1:windowSize
  n_rolls = n - windowSize + 1
  for (i in 1:n_rolls){
    outvec[i] = fun(datavec[idx_window])
    idx_window = idx_window + 1
  }
  return(outvec)
}

# Shiny app UI
ui <- fluidPage(
  
  # Download stats
  titlePanel('Stock Returns & Stats'),
  
  textInput(inputId = "symbol",label =  "Symbol", value = "AAPL"),
  
  dateRangeInput(inputId = "dates",
                 label = "Date range",
                 start = "2013-01-01",
                 end = as.character(Sys.Date())),
  
  shiny::actionButton(inputId = "action_getData",label = "Get Data"),
  
  # First tab for returns
  tabsetPanel(
    tabPanel(type = "tabs", "Returns",
             sidebarLayout(
               sidebarPanel(
                 numericInput(inputId = "lag",label = "lag", value = 1),
                 numericInput(inputId = "cf",label = "Confidence Level", value = 0.99)
               ),
               
               mainPanel(
                 plotOutput("veplot"))
             )
    ),
    # Second tab for stats
    tabPanel(type = 'tabs', 'Stats',
             sidebarLayout(
               sidebarPanel(
                 numericInput(inputId = "wz",label = "Window Size", value = 100),
                 selectInput(inputId = 'stn', label = 'Stats', choice = c('Max', 'Min', 'Var', 'Sd'), selected = 'Max')
               ),
               
               mainPanel(
                 plotOutput("statplot")
               )
             )
    )
  )
)

# Shiny app server
server <- function(input, output, session) {
  
  #Define reative numbers
  datainput <- eventReactive(input$action_getData,{
    getSymbols(input$symbol,
               src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
  laginput <- reactive(input$lag)
  
  cfinput <- reactive(input$cf)
  
  wsinput <- reactive(input$wz)
  
  # Plot returns, VaR and ES
  output$veplot <- renderPlot({
    x = datainput()
    price = as.numeric(x[,6])
    l = laginput()
    res1 = getReturns(price,l)
    conf = cfinput()
    vr = VaR(res1, conf, returns = T)
    es = ES(res1, conf, returns = T, vr, plotit = T)
  }
  )
  
  # User selected stats
  summ <- reactive({
    switch(input$stn,
           'Max' = max,
           'Min' = min,
           'Var' = var,
           'Sd' = sd)
  })
  
  # Plot return stats
  output$statplot <- renderPlot({
    x = datainput()
    price = as.numeric(x[,6])
    l = laginput()
    res1 = getReturns(price,l)
    st = RollingStatistic(res1, wsinput(), summ())
    plot(st)
  }
  )
}

shinyApp(ui, server)

############################ Question 3 Code
library(shiny)
library(car)

# Shiny App UI
ui <- fluidPage(
  titlePanel('Shiny MLR'),
  
  # Upload the file and enter the formula
  fileInput(inputId = 'filein', label= 'Please upload a dataset', accept = 'csv'),
  
  htmlOutput('text'),
  
  textOutput('nameout'),
  
  textInput(inputId = "formula",label =  "Please enter your MLR formula using a~b+c+d format", value = "Formula please"),
  
  # Action button to get the regression
  shiny::actionButton(inputId = "action_reg",label = "Run the regression"),
  
  # tabs
  tabsetPanel(
    # first tab for the table
    tabPanel(type = 'tabs', 'Table',
             tableOutput('tableout')
    ),
    
    # Second tab for the AVplots
    tabPanel(type = 'tabs', 'AVPlots',
             plotOutput('AV')
    ),
    
    # third tab for the VIF
    tabPanel(type = 'tabs', 'Variance Inflation Factors',
             textOutput('VIFn'),
             textOutput('VIF')
    ),
    
    # third tab for the Inf Plots
    tabPanel(type = 'tabs', 'Influence Plots',
             plotOutput('INF'))
  )
)

server <- function(input, output, session) {
  # data
  data <- reactive({
    df = read.csv(file = input$filein$datapath)
  }
  )
  #field names
  field <- reactive({
    names(data())
  })
  
  #Print out the field names
  output$nameout <- renderText({
    name = field()
    name
  }
  )
  
  output$text <- renderText({
    paste('<B>Here are the fields for your choice</B>')
  })
  
  # Print out the data table
  output$tableout <- renderTable({
    d = data()
    d
  }
  )
  
  # Get the selected formula
  fm <- reactive(
    as.formula(input$formula)
  )
  
  # Get the fit
  fit <- eventReactive(input$action_reg,
                       lm(fm(), data = data())
  )
  
  # Print out the AVPlot
  output$AV <- renderPlot(
    car::avPlots(fit())
  )
  
  # Print out Variance Inflation Factors
  output$VIF <- renderText({
    result = car::vif(fit())
    result
  }
  )
  
  output$VIFn <- renderText({
    result = car::vif(fit())
    x = names(result)
    x
  }
  )
  
  # Print out Influence Plots
  output$INF <- renderPlot(
    car::influencePlot(fit())
  )
}

shinyApp(ui, server)

############################ Question 4 Code
library(shiny)
library(quantmod)

# Portfolio volatility
PortfolioVol = function(x,y){
  out = sqrt(t(x) %*% cov(y) %*% x)
  return(out)
}

wvec = c(.33,.33,.33)
w = matrix(wvec,length(wvec),1)

# UI Shiny app

ui <- fluidPage(
  titlePanel('Stock Returns & Stats'),
  
  # Get the symbols
  textInput(inputId = "symbol1",label =  "1st Symbol", value = "AAPL"),
  textInput(inputId = "symbol2",label =  "2nd Symbol", value = "GOOG"),
  textInput(inputId = "symbol3",label =  "3rd Symbol", value = "MSFT"),
  
  dateRangeInput(inputId = "dates",
                 label = "Date range",
                 start = "2017-01-01",
                 end = "2018-01-01"),
  
  # Tabs
  tabsetPanel(
    tabPanel(type = 'tabs', 'Portfolio Return',
             plotOutput('histpr')
    ),
    
    tabPanel(type = 'tabs', 'Portfolio Volatity, weighted average returns and ratio',
             textOutput('text'),
             textOutput('vol'),
             textOutput('avrtext'),
             textOutput('avrout'),
             textOutput('rrtext'),
             textOutput('rr')
    )
  )
)

server <- function(input, output, session) {
  
  #Define reactive numbers
  r1 <- reactive({
    a = getSymbols(input$symbol1,
                   from = input$dates[1],
                   to = input$dates[2],
                   auto.assign = FALSE)
    periodReturn(a[,6],period = 'daily')
  })
  
  r2 <- reactive({
    b = getSymbols(input$symbol2,
                   from = input$dates[1],
                   to = input$dates[2],
                   auto.assign = FALSE)
    periodReturn(b[,6],period = 'daily')
  })
  
  r3 <- reactive({
    c = getSymbols(input$symbol3,
                   from = input$dates[1],
                   to = input$dates[2],
                   auto.assign = FALSE)
    periodReturn(c[,6],period = 'daily')
  })
  
  # Portfolio's Return
  pr <- reactive({
    return = merge.xts(r1(), r2(), r3())
    d = return %*% w
    d
  })
  
  output$histpr <- renderPlot({
    d = pr()
    hist(d, 50)
  })
  
  # Portfolio's Volatility
  v <- reactive({
    return = merge.xts(r1(), r2(), r3())
    vol = round(PortfolioVol(w, return), 4)
    vol
  })
  
  output$text <- renderText({
    paste('Here is the portfolio volatility')
  })
  
  output$vol <- renderText({
    v()
  })
  
  # Average return
  avr <- reactive({
    av1 = mean(r1())
    av2 = mean(r2())
    av3 = mean(r3())
    av = matrix(c(av1, av2, av3), 1, 3)
    wav = av %*% w
    wav
  })
  
  output$avrtext <- renderText({
    paste('Here is the weighted average return')
  })
  
  output$avrout <- renderText({
    avr()
  })
  
  rratio <- reactive({
    a = avr()
    vl = v()
    rr = a/vl
    rr
  })
  
  output$rrtext <- renderText({
    paste('Here is the ratio')
  })
  
  output$rr <- renderText({
    rratio()
  })
}

shinyApp(ui, server)
