connectUI <- fluidPage(
  
  p("Hello."),
  textAreaInput("message", "Message"),
  actionButton("submit", "Submit"),
  textOutput("messages"),
  actionButton("reload", "Reload")
  
)



connectServer <- function(session, input, output) {
  
  observeEvent(input$submit, {
    if (!file.exists("messages.log")) {
      cat("\n", file = "messages.log")
    }
    
    cat(input$message, "\n", file = "messages.log", append = TRUE)
    updateTextInput(session, "message", value = "")
    
  })
  
  output$messages <- renderText({
    input$reload
    if (file.exists("messages.log")) {
      readLines("messages.log")
    }
  })
  
}