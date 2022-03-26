goToMyPage_dplyr = function(){
  browseURL("https://andreayang227.shinyapps.io/yy3137-HW9/")
}


library(dplyr)
library(shiny)

ui <- fluidPage(
  
  titlePanel('Filter Interface'),
  
  sidebarLayout(
    sidebarPanel(
      # upload the dataset
      fileInput(inputId = 'filein', label= 'Please upload a dataset', accept = 'csv'),
      
      # Ask the user for a field
      selectInput(inputId = 'fieldnames',label = 'Please provide the filter field',choices = '',selected = '', multiple = F),
      
      # Ask the user for filter type
      selectInput(inputId = 'filter_type',label = 'Please select a filter type',choices = c('less than','equal to','greater than'),selected = ''),
      
      # Ask the user for filer value
      textInput(inputId = 'filter_number',label = 'Please select a value to filter for',value = 0),
      
      # Ask the user for fields in the result
      selectInput(inputId = 'resultfield',label = 'Please select your result fields', choices = '',selected = '', multiple = T),
      
      # Ask the user for a choice of groups
      selectInput(inputId = 'groupby',label = 'Please select a field to group by', choices = '',selected = '', multiple = F),
      
      # Ask the user for a choice of a stats method
      selectInput(inputId = 'stats',label = 'Please select a stats method', choices = c('max', 'min', 'mean', 'sd'),selected = '', multiple = F)
    ),
    
    mainPanel(
      tabsetPanel(
        type = 'tabs',
        tabPanel('Selected Data', tableOutput('singleout')),
        tabPanel('Summarized stats', tableOutput('groupout'))
      )
    )
  )
)

server <- function(input, output, session) {
  
  Obj_data = reactive({
    df = read.csv(file = input$filein$datapath)
  })
  
  Obj_fieldnames = reactive({
    names(Obj_data())
  })
  
  observe({
    req(input$filein$datapath)
    updateSelectInput(session = session, inputId = 'fieldnames', choices = Obj_fieldnames(),  selected = Obj_fieldnames()[1])
  })
  
  observe({
    req(input$filein$datapath)
    updateSelectInput(session = session, inputId = 'resultfield', choices = Obj_fieldnames(),  selected = '')
  })
  
  observe({
    req(input$filein$datapath)
    updateSelectInput(session = session, inputId = 'groupby', choices = Obj_fieldnames(),  selected = '')
  })
  
  summ = reactive({
    switch(input$stats,
           'max' = max,
           'min' = min,
           'mean' = mean,
           'sd' = sd)
  })
  
  Obj_data1 = reactive({
    req(input$fieldnames != '')
    
    if(input$filter_type == 'less than'){
      Obj_data() %>%
        dplyr::filter(get(input$fieldnames) < input$filter_number)
    } else if (input$filter_type == 'equal to'){
      Obj_data() %>% 
        dplyr::filter(get(input$fieldnames) == input$filter_number)
    } else {
      Obj_data() %>% 
        dplyr::filter(get(input$fieldnames) > input$filter_number)
    }
    
  })
  
  Obj_data2 = reactive({
    req(input$fieldnames != '')
    Obj_data1() %>% group_by_(input$groupby) %>% summarise_if(is.numeric,summ(),na.rm = TRUE)
  })
  
  output$singleout = renderTable({
    d = Obj_data1()
    d[, input$resultfield]
  })
  
  output$groupout = renderTable({
    d2 = Obj_data2()
    d2[, input$resultfield]
  })
}

shinyApp(ui, server)
