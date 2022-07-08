library(shiny)
# pkgload::load_all(".")

ui <- fluidPage(

  p("Hello."),
  textInput("message", label = "Message"),
  actionButton("submit", "Submit"),
  textOutput("status"),
  actionButton("update", "Update")
    
)

server <- function(session, input, output) {

  if (!file.exists("messages.log")) {
    cat("Messages:\n", file = "messages.log")
  }
  
  observeEvent(input$submit, {
    cat(paste0(input$message, "\n"), file = "messages.log", append = TRUE)
    updateTextInput(session, "message", value = "")
  })
  
  output$status <- renderText({
    input$update
    print(readLines("messages.log"))
    paste(readLines("messages.log"), sep = "\n")
  })

}

shinyApp(ui = ui, server = server)
