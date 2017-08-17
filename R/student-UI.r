#' @import shiny 
NULL

knitAssignment <- function(name, sourcePath) {
  message("Knitting updated assignment file: ", name)
  showNotification( paste("Re-Knitting assignment source file:", name, "...")
                  , duration = 10
                  , type     = "warning"
                  , id       = "knitting"
                  )
  
  result <- try({ rmarkdown::render(sourcePath, envir = new.env()) })
  if (class(result) == "try-error") {
    showNotification( "Looks like re-knitting your source file didn't go well; this page was not updated.  See the console for error messages."
                    , type     = "error"
                    , duration = 10
                    )
  }
  
  removeNotification("knitting")
}

studentServer <- function(pkg, autoknit, wd) {
  
  pkgPath <- file.path(normalizePath(pkg), "data")
  config  <- yaml::yaml.load_file(file.path(pkgPath, "courseR.yml"))
  
  function(input, output, session) {
    interval <- 500  # in ms
    
    checked   <- do.call(reactiveValues, args = as.list(listChecked(pkg)))
    submitted <- do.call(reactiveValues, args = as.list(listSubmitted(pkg)))
    
    # watch rds files for new checks and submissions
    observe({
      invalidateLater(interval)

      newChecked <- listChecked(pkg)
      for (name in names(checked)) {
        if (!identical(newChecked[[name]], checked[[name]])) {
          checked[[name]] <- newChecked[[name]]
        }
      }

      newSubmitted <- listSubmitted(pkg)
      for (name in names(submitted)) {
        if (!identical(newSubmitted[[name]], submitted[[name]])) {
          submitted[[name]] <- newSubmitted[[name]]
        }
      }

    })
    
    sources <- do.call(reactiveValues, args = as.list(listSources(pkg, path = wd)))

    # auto re-knit when sources change
    observe({
      if (autoknit) {
        invalidateLater(interval)
        newSources <- listSources(pkg, path = wd)
        for (name in names(sources)) {
          if (!identical(newSources[[name]], sources[[name]])) {
            knitAssignment(name, getRMDFile(name, path = wd))
            sources[[name]] <- newSources[[name]]
          }
        }
      }
    })

    renderView <- function(studentRDS, solutionRDS) {
      s <- studentRDS$html
      s <- gsub("\\{\\{task-\\d+-before\\}\\}", as.character(tags$b("Your answer:")), s)
      for (i in 1:length(solutionRDS$html)) {
        p <- paste0("{{task-", i, "-after}}")
        r <- paste( tags$b("Answer key output:")
                  , solutionRDS$html[[i]]
                  , sep = "\n"
                  )

        s <- gsub(p, r, s, fixed = TRUE)
      }

      s <- gsub("<body>", "", s, fixed = TRUE)
      s <- gsub("</body>", "", s, fixed = TRUE)

      s <- paste0( as.character(fluidRow(column(width = 3, p("Version: ", substring(studentRDS$sourceHASH, 1, 7)))))
                 , s
                 # , as.character(tags$script("$('pre code').each(function(i, block) { hljs.highlightBlock(block) })"))
                 )

      HTML(s)
    }

    renderCheck <- function(name) {
      renderUI({
        if (input$navpage != 'overview') {
          # showNotification("A fresh copy of your answers was loaded!", type = "message")

          rmdPath        <- getRMDFile(input$navpage, path = wd, exists = FALSE)
          studentRDSPath <- rdsPath(input$navpage, path = studentPath(pkg))

          if (!file.exists(rmdPath)) {
            return(
              div(p( "It looks like you haven't started this assignment yet.  Stop this app and use "
                   , code(paste0(config$build$package$name, '::startAssignment("'
                                , input$navpage , '")'
                                )
                         )
                   , " to do so."
                   )
                 )
            )
          }

          if (!file.exists(studentRDSPath)) {
            knitAssignment(input$navpage, rmdPath)

            if (!file.exists(studentRDSPath)) {
              return(
                div(p( "It looks like your current solutions to this assignment won't knit.  Stop this app and use "
                     , code(paste0(config$build$package$name, '::checkAssignment("'
                                  , input$navpage
                                  , '")'
                                  )
                           )
                     , " to get more information about the problem(s)."
                     )
                   )
              )
            }
          }

          version     <- checked[[name]]  # here to dirty output when a new version is available
          studentRDS  <- readRDS(studentRDSPath)
          solutionRDS <- readRDS(rdsPath(input$navpage, file.path(pkg, "data") , tag = "-solutions"))

          renderView(studentRDS, solutionRDS)
        }
      })
    }
    for (name in .listAssignments(pkg)) { output[[paste0('check', "-", name)]] <- renderCheck(name) }

    renderSubmit <- function(name) {
      renderUI({
        if (input$navpage != 'overview') {
          # showNotification("A fresh copy of your submission was loaded!", type = "message")

          name <- substring(input$navpage, first = 8)

          studentRDSPath <- rdsPath(name, path = file.path(studentPath(pkg), "submitted"))
          if (!file.exists(studentRDSPath)) {
            return(
              div(p( "It looks like you haven't submitted this assignment yet.  Use "
                   , code(paste0(config$build$package$name, '::submitAssignment("'
                                , input$navpage
                                , '")'
                                )
                         )
                   , " to do so."
                   )
                 )
            )
          }

          version     <- submitted[[name]]  # here to dirty output when a new version is available
          studentRDS  <- readRDS(studentRDSPath)
          solutionRDS <- readRDS(rdsPath(name, file.path(pkg, "data") , tag = "-solutions"))

          renderView(studentRDS, solutionRDS)
        }
      })
    }
    for (name in .listAssignments(pkg)) { output[[paste0('submit', "-", name)]] <- renderSubmit(name) }
    
    output$status <- shiny::renderTable({
      data.frame( Assignment            = .listAssignments(pkg)
                , `In progress version` = substring(as.character(reactiveValuesToList(checked)), 1, 7)
                , `Submitted version`   = substring(as.character(reactiveValuesToList(submitted)), 1, 7)
                , Status                = "" # TODO
                , check.names = FALSE
                )
    })
   
  }
}

studentUI <- function(pkg, page) {
  
  pkgPath <- file.path(normalizePath(pkg), "data")
  siteyml <- yaml::yaml.load_file(file.path(pkgPath, "_site.yml"))
  config  <- yaml::yaml.load_file(file.path(pkgPath, "courseR.yml"))
  
  navbarPage( id       = 'navpage'
            , selected = page
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
                      , p("This table show the current status of your progress
                          on assignments in the course.  You can start a new
                          assignment using the ", code(paste0(config$build$package$name,
                          "::startAssignment")), "function.  You can submit the
                          current version of your answers with ",
                          code(paste0(config$build$package$name, "::submitAssignment")),
                          ". An ", code("NA"), " means that you haven't started
                          or submitted the assignment yet.")
                      , br()
                      , tableOutput('status')
                      )
            , do.call( navbarMenu
                     , args = c( list(title = "In progress")
                               , lapply( nonames(.listAssignments(pkg))
                                       , function(name) {
                                           tabPanel( title = splitext(name)
                                                   , value = splitext(name)
                                                   , uiOutput(paste0('check', "-", splitext(name)))
                                                   )
                                         }
                                       )
                               )
                     )
            , do.call( navbarMenu
                     , args = c( list(title = "Submissions")
                               , lapply( nonames(.listAssignments(pkg))
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

launchStudentUI <- function(pkg, page, autoknit, wd) {

  app <- shinyApp( ui     = studentUI(pkg, page)
                 , server = studentServer(pkg, autoknit, wd)
                 )
  runGadget(app)
  
}
