library(shiny)
pkgload::load_all(".")

shinyApp(ui = connectUI, server = connectServer)
