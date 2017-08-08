#' @import shiny 
NULL

studentServer <- function(pkg, autoknit) {
  
  pkgPath <- file.path(normalizePath(pkg), "data")
  config <- yaml::yaml.load_file(file.path(pkgPath, "courseR.yml"))
  
  function(input, output, session) {
    interval <- 500  # in ms

    changed <- function(before, after) {
      if (is.null(before) || is.null(after)) { return(FALSE) }
      # if NA changed to a time or...
      (is.na(before) && !is.na(after)) ||
      # new time > old time
      (!identical(after, before))
    }

    sources <- do.call(reactiveValues, args = listSources(pkg))
    answers <- do.call(reactiveValues, args = listCheck(pkg))
    submits <- do.call(reactiveValues, args = listSubmitted(pkg))

    # auto re-knit when sources change
    observe({
      if (autoknit) {
        invalidateLater(interval)

        newSources <- listSources(pkg)
        for (name in names(sources)) {
          isolate({ before <- sources[[name]] })
          if (changed(before, newSources[[name]])) {
            sources[[name]] <- newSources[[name]]
            rds <- readRDS(rdsPath(name, path = studentPath(pkg)))

            message("Knitting updated assignment file: ", name)
            showNotification( paste("Re-Knitting assignment source file:", name, "...")
                            , duration = 10
                            , type     = "warning"
                            , id       = "knitting"
                            )
            
            handler <- function(e) {
              
              message(e)
            }

            result <- try({ rmarkdown::render(rds$sourceRMD, envir = new.env()) })
            if (class(result) == "try-error") {
              showNotification( "Looks like re-knitting your source file didn't go well; this page was not updated.  See the console for error messages."
                              , type     = "error"
                              , duration = 10
                              )
            }
            
            removeNotification("knitting")
          }
        }
      }
    })

    # watch rds files for checks and submitted
    observe({
      invalidateLater(interval)

      newAnswers <- listCheck(pkg)
      for (name in names(answers)) {
        isolate({ before <- answers[[name]] })
        if (changed(before, newAnswers[[name]])) {
          answers[[name]] <- newAnswers[[name]]
        }
      }
      
      newSubmits <- listSubmitted(pkg)
      for (name in names(submits)) {
        isolate({ before <- submits[[name]] })
        if (changed(before, newSubmits[[name]])) {
          submits[[name]] <- newSubmits[[name]]
        }
      }

    })

    renderView <- function(studentRDS, solutionRDS, version) {
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
      
      s <- paste0( s
                 , as.character(fluidRow(column(width = 3, p("Version: ", substring(version, 1, 7)))))
                 # , as.character(tags$script("$('pre code').each(function(i, block) { hljs.highlightBlock(block) })"))
                 )
      
      HTML(s)
    }
    
    renderCheck <- function(name) {
      renderUI({
        if (input$navpage != 'overview') {
          showNotification("A fresh copy of your answers loaded!", type = "message")
          
          studentRDSPath <- rdsPath(input$navpage, path = studentPath(pkg))
          if (!file.exists(studentRDSPath)) {
            return(div(p("It looks like you haven't started this assignment yet.  Use the", code(paste0(config$build$package$name, "::startAssignment()")), "function to do so!")))
          }
          
          studentRDS  <- readRDS(studentRDSPath)
          solutionRDS <- readRDS(rdsPath(input$navpage, file.path(pkg, "data") , tag = "-solutions"))
          
          renderView(studentRDS, solutionRDS, version = answers[[input$navpage]]) # get dirty
        }
      })
    }
    for (name in names(listCheck(pkg))) { output[[paste0('check', "-", name)]] <- renderCheck(name) }

    renderSubmit <- function(name) {
      renderUI({
        if (input$navpage != 'overview') {
          showNotification("A fresh copy of your submission was loaded!", type = "message")
          
          name <- substring(input$navpage, first = 8)
          
          studentRDSPath <- rdsPath(name, path = file.path(studentPath(pkg), "submitted"))
          if (!file.exists(studentRDSPath)) {
            return(div(p("It looks like you haven't submitted this assignment yet.  Use the", code(paste0(config$build$package$name, "::submitAssignment()")), "function to do so!")))
          }
          
          studentRDS  <- readRDS(studentRDSPath)
          solutionRDS <- readRDS(rdsPath(name, file.path(pkg, "data") , tag = "-solutions"))
          
          renderView(studentRDS, solutionRDS, version = submits[[name]]) # get dirty
        }
      })
    }
    for (name in names(listSubmitted(pkg))) { output[[paste0('submit', "-", name)]] <- renderSubmit(name) }
    
    output$status <- shiny::renderTable({
      data.frame( Assignment            = .listAssignments(pkg)
                , `In progress version` = substring(as.character(listCheck(pkg)), 1, 7)
                , `Submitted version`   = substring(as.character(listSubmitted(pkg)), 1, 7)
                , Feedback              = "" # TODO
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
                               , lapply( names(listCheck(pkg))
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

launchStudentUI <- function(pkg, page, autoknit) {

  app <- shinyApp( ui     = studentUI(pkg, page)
                 , server = studentServer(pkg, autoknit)
                 )
  runGadget(app)
  
}
