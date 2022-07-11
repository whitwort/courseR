connectUI <- fluidPage(
  
  p("Hello."),
  textOutput("files")
  
)

connectServer <- function(session, input, output) {
  
  output$files <- renderText({
    list.files()
  })
  
}