#' @import shiny 
NULL

instructorServer <- function(pkg) {
  
  function(input, output, session) {
    
  }
  
}

instructorUI <- function(pkg) {
  
  pkgPath <- file.path(normalizePath(pkg), "data")
  siteyml <- yaml::yaml.load_file(file.path(pkgPath, "_site.yml"))
  config  <- yaml::yaml.load_file(file.path(pkgPath, "courseR.yml"))
  
  navbarPage( id       = 'navpage'
            , selected = 'overview'
            , title    = config$build$package$name
            , collapsible = TRUE
            , header   = tagList( includeCSS(file.path(pkgPath, "site_libs", "bootstrap-3.3.5", "css", paste0(siteyml$output$html_document$theme, ".min.css")))
                                , includeCSS(file.path(pkgPath, "site_libs", "highlightjs-1.1", "default.css"))
                                , includeScript(file.path(pkgPath, "site_libs", "highlightjs-1.1", "highlight.js"))
                                )
            # , footer = tags$script("hljs.initHighlightingOnLoad();")
            , footer = tags$script("function rehighlight() {
                                    $('pre code').each(function(i, block) {
                                      hljs.highlightBlock(block)
                                     })
                                  }
                                  $(document).on('shiny:value', function(event) { 
                                    window.setTimeout(rehighlight, 500) 
                                  })
                                 "
                                  )
            , tabPanel( "Overview", value = "overview"
                      , h1("Overview")
                      , p("This table show the current status of progress
                      on assignments in the course.")
                      , br()
                      , tableOutput('status')
                      )
            , do.call( navbarMenu
                     , args = c( list(title = "Submissions")
                               , lapply( names(listSubmitted(pkg))
                                       , function(name) {
                                           tabPanel( title = splitext(name)
                                                   , value = paste0("submit-", splitext(name))
                                                   , uiOutput(paste0('submit', "-", splitext(name)))
                                                   )
                                           }
                                       )
                               )
                     )
            )
  
}

launchInstructorUI <- function(pkg) {
  
  app <- shinyApp( ui     = instructorUI(pkg)
                 , server = instructorServer(pkg)
                 )
  runApp(app)
  
}
