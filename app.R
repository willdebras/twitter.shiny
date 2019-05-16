library(shiny)
library(shinyjs)


ui <- pageWithSidebar(
  
  headerPanel("Twitter rules drafter"),
  
  sidebarPanel(
    shinyjs::useShinyjs(),
    id = "side-panel",
    textInput("mytext", "Enter a search term", placeholder = "votecast"),
    actionButton("add" ,"Add"),
    tags$hr(),
    br(),
    textInput("mytext2", "Enter a set of URL keywords", placeholder = "stellar debut votecast"),
    actionButton("add2" ,"Add"),
    tags$hr(),
    br(),
    textInput("mytext3", "Enter a URL", placeholder = "apnorc.org"),
    actionButton("add3" ,"Add")
    #                 class = "btn btn-primary")
  ),
  
  mainPanel(
    h4("Terms"),
    verbatimTextOutput("summary"),
    br(),
    br(),
    br(),
    br(),
    h4("URL terms"),
    verbatimTextOutput("summary_urls"),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    h4("URLs"),
    verbatimTextOutput("summary_url_key")
  )
  
)

server <- function(input, output, session) {
  
  ###First set of values for first input  
  
  values <- reactiveVal("AP-NORC")
  
  # update values table on button click
  observeEvent(input$add, {
    
    old_values <- values()
    
    new_values <- input$mytext
    
    
    # paste these together:
    new_string <- paste(old_values, new_values, sep = ", ")
    
    #store the result in values variable
    values(new_string)
    
  })
  
  
  ###Second set of values for second input
  
  values2 <- reactiveVal(NULL)
  
  
  observeEvent(input$add2,{
    
    old_values2 <- values2()
    
    new_values2 <- input$mytext2
    
    
    ifelse(!is.null(values2()), 
           new_string2 <- paste(old_values2, new_values2, sep = ", "), 
           new_string2 <- paste(new_values2))
    
    
    values2(new_string2)
    
  })
  
  
  ###Third set of values for third input
  
  values3 <- reactiveVal(NULL)
  
  
  observeEvent(input$add3,{
    
    old_values3 <- values3()
    
    new_values3 <- input$mytext3
    
    
    ifelse(!is.null(values3()), 
           new_string3 <- paste(old_values3, new_values3, sep = ", "), 
           new_string3 <- paste(new_values3))
    
    
    values3(new_string3)
    
  })  
  
  
  ###Generate outputs that get displayed in verbatimTextOutput above
  
  output$summary <- renderText({
    return(values())
  })
  
  output$summary_urls <- renderText({
    return(values2())
  })
  
  output$summary_url_key <- renderText({
    return(values3())
  })
  
  
  ###Add JS functions to reset the input when you click the add button
  
  observeEvent(input$add, {
    shinyjs::reset("side-panel")
  })
  
  observeEvent(input$add2, {
    shinyjs::reset("side-panel")
  })
  
  observeEvent(input$add3, {
    shinyjs::reset("side-panel")
  })
}

shinyApp(ui, server)