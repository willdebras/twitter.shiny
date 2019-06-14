library(shiny)
library(shinyjs)
library(stringr)
library(rclipboard)




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
                      verbatimTextOutput("text_term"),
                      tags$style(type="text/css", "#text_term {white-space: pre-wrap;}")
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
                      verbatimTextOutput("text_urlterm"),
                      tags$style(type="text/css", "#text_urlterm {white-space: pre-wrap;}"))
               
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
                      verbatimTextOutput("text_url"),
                      tags$style(type="text/css", "#text_url {white-space: pre-wrap;}"))
               
               
             ),
             
             fluidRow(
               column(11,
                      br(),
                      br(),
                      h3("Twitter Syntax"),
                      br(),
                      verbatimTextOutput("rules"),
                      tags$style(type="text/css", "#rules {white-space: pre-wrap;}")
                      
               )
             ),
             fluidRow(
               rclipboardSetup(),
               column(11,
                      br(),
                      uiOutput("clip"))
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
  
  values_json <- reactiveVal(NULL)
  
  observeEvent(input$add, {
    
    first_json <- values()
    
    json1 <- strsplit(first_json, ",") 
    
    json2 <- as.vector(json1[[1]]) %>% trimws(which = "both")
    
    json3 <- sapply(json2, function(x) ifelse(str_detect(x, "[:blank:]"), paste("\\\"", x, "\\\"", sep = ""), paste(x)))
    
    
    values_json(paste("(", paste(json3, sep = " ", collapse = " OR "), ")", sep = ""))
    
    
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
  
  values_json2 <- reactiveVal(NULL)
  
  observeEvent(input$add2, {
    
    first_json2 <- values2()
    
    json2_1 <- strsplit(first_json2, ",") 
    
    json2_2 <- as.vector(json2_1[[1]]) %>% trimws(which = "both") %>%
      strsplit(" ")
    
    
    json2_3 <- sapply(json2_2, function(x) paste(paste("url_contains:", x, sep = ""), "OR", collapse = " "))
    
    json2_4 <- sapply(json2_3, function(x) str_sub(x, 1, str_length(x)-3))
    
    json2_5 <- sapply(json2_4, function(x) paste("(", x, ")", sep = ""))
    
    json2_6 <- paste(json2_5, sep = " ", collapse = " OR ")
    
    
    values_json2(json2_6)
    
    
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
  
  
  values_json3 <- reactiveVal(NULL)
  
  observeEvent(input$add3, {
    
    first_json3 <- values3()
    
    json3_1 <- strsplit(first_json3, ",")
    
    json3_2 <- as.vector(json3_1[[1]]) %>% trimws(which = "both")
    
    json3_3 <- test4 <- sapply(json3_2, function(x) paste("url_contains:", "\\\"", x, "\\\"", sep = ""))
    
    json3_4 <- paste(json3_3, sep = " ", collapse = " OR ")
    
    
    values_json3(json3_4)
    
    
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
                 values_json(),
                 " OR ",
                 values_json2(),
                 " OR ",
                 values_json3()))
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
  
  ###We want to allow a user to copy and paste these rules
  
  ##Make these data reactive since we can't read objects from the output above
  
  copyabledata <- reactive({
    paste("(\\\"ap-norc\\\" OR \\\"AP NORC\\\" OR apnorc OR \\\"Associated Press-NORC\\\") (poll OR survey)",
          values_json(),
          "OR",
          values_json2(),
          "OR",
          values_json3())
  })
  
  ##Render the copy button
  
  output$clip <- renderUI({
    rclipButton("clipbtn", "Copy rules", input$copytext, icon("clipboard"))
  })
  
  ##observe the input and write to clipboard the copy-able data
  
  # Workaround for execution within RStudio
  observeEvent(input$clipbtn, clipr::write_clip(copyabledata()))
}

shinyApp(ui, server)