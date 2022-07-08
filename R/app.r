library(shiny)
# pkgload::load_all(".")

ui <- fluidPage(

  p("Hello.")
    
)

server <- function(session, input, output) {

}

shinyApp(ui = ui, server = server)
