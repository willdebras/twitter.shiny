library(shiny)
library(shinyjs)


ui <- fluidPage(
  
  headerPanel("Twitter rules drafter"),
  
  fluidRow(
    
    column(3, 
           shinyjs::useShinyjs(),
           br(),
           id = "side-panel",
           textInput("mytext", "Enter a search term", placeholder = "votecast"),
           actionButton("add" ,"Add")),
    column(8,
           h4("Terms"),
           verbatimTextOutput("text_term")
           )
    
    
  ),
  
  fluidRow(
    
    column(3, 
           shinyjs::useShinyjs(),
           br(),
           id = "side-panel",
           textInput("mytext2", "Enter a set of URL keywords", placeholder = "stellar debut votecast"),
           actionButton("add2" ,"Add")),
    column(8,
           h4("URL terms"),
           verbatimTextOutput("text_urlterm"))
    
  ),
  
  fluidRow(
    
    column(3, 
           shinyjs::useShinyjs(),
           br(),
           id = "side-panel",
           textInput("mytext3", "Enter a URL", placeholder = "apnorc.org"),
           actionButton("add3" ,"Add")),
    column(8,
           h4("URLs"),
           verbatimTextOutput("text_url"))
    
    
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
  
  output$text_term <- renderText({
    return(values())
  })
  
  output$text_urlterm <- renderText({
    return(values2())
  })
  
  output$text_url <- renderText({
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