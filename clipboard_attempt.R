library(rclipboard)
library(shiny)

# The UI
ui <- bootstrapPage(
  
  rclipboardSetup(),
  
  # Add a text input
  textInput("copytext", "Copy this:", "Zlika!"),
  
  # UI ouputs for the copy-to-clipboard buttons
  uiOutput("clip"),
  
  # A text input for testing the clipboard content.
  textInput("paste", "Paste here:")
  
)

# The server
server <- function(input, output) {
  
  # Add clipboard buttons
  output$clip <- renderUI({
    rclipButton("clipbtn", "rclipButton Copy", input$copytext, icon("clipboard"))
  })
  
  # Workaround for execution within RStudio
  observeEvent(input$clipbtn, clipr::write_clip(input$copytext))
  
}

shinyApp(ui = ui, server = server)