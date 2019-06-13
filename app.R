library(shiny)
library(shinyjs)


ui <- fluidPage(
  
  headerPanel("Twitter rules drafter"),
  
  tabsetPanel(
    
    tabPanel("Rules",
             
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
                      id = "side-panel2",
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
                      id = "side-panel3",
                      textInput("mytext3", "Enter a URL", placeholder = "apnorc.org"),
                      actionButton("add3" ,"Add")),
               column(8,
                      h4("URLs"),
                      verbatimTextOutput("text_url"))
               
               
             ),
             
             fluidRow(
               column(11,
                      br(),
                      br(),
                      h3("Twitter Syntax"),
                      br(),
                      verbatimTextOutput("rules")
                      
               )
             )  
    ),
    tabPanel("Explanation",
             br(),
             "This app exists for the purpose of generating JSON syntax for search terms for the twitter firehose API. You simply add in search terms related to the release, add the phrases associated with URLs, and then finally add relevant URLs. The app will output syntax associated with the terms displayed in the right hand column."
             
    )
    
  )
)

server <- function(input, output, session) {
  
  ###First set of values for first input  
  
  values <- reactiveVal(NULL)
  
  # update values table on button click
  observeEvent(input$add, {
    
    old_values <- values()
    
    new_values <- input$mytext
    
    
    # paste these together after first input:
    ifelse(!is.null(values()), 
           new_string <- paste(old_values, new_values, sep = ", "), 
           new_string <- paste(new_values))
    
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
  
  
  output$rules <- renderText({
    return(paste("(\\\"ap-norc\\\" OR \\\"AP NORC\\\" OR apnorc OR \\\"Associated Press-NORC\\\") (poll OR survey)",
           values(),
           values2(),
           values3()))
  })
  
  ###Add JS functions to reset the input when you click the add button
  
  observeEvent(input$add, {
    shinyjs::reset("side-panel")
  })
  
  observeEvent(input$add2, {
    shinyjs::reset("side-panel2")
  })
  
  observeEvent(input$add3, {
    shinyjs::reset("side-panel3")
  })
}

shinyApp(ui, server)