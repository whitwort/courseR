#' @import shiny 
NULL

studentServer <- function(pkg, autoknit) {
  function(input, output, session) {
    
    interval <- 500  # in ms

    changed <- function(before, after) {
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
      s <- gsub("\\{\\{task-\\d-before\\}\\}", as.character(h4("Your answer:")), s)
      for (i in 1:length(solutionRDS$html)) {
        p <- paste0("{{task-", i, "-after}}")
        r <- paste( h4("Answer key output:")
                  , solutionRDS$html[[i]]
                  , sep = "\n"
                  )
        
        s <- gsub(p, r, s, fixed = TRUE)
      }
      
      s <- gsub("<body>", "", s, fixed = TRUE)
      s <- gsub("</body>", "", s, fixed = TRUE)
      
      s <- paste0( s
                 , as.character(fluidRow(column(width = 3, p("Version: ", substring(version, 1, 7)))))
                 )
      
      HTML(s)
    }
    
    output$renderCheck  <- renderUI({
      if (input$navpage != 'overview') {
        showNotification("A fresh copy of your answers loaded!", type = "message")
        
        studentRDS  <- readRDS(rdsPath(input$navpage, path = studentPath(pkg)))
        solutionRDS <- readRDS(rdsPath(input$navpage, file.path(pkg, "data") , tag = "-solutions"))
        
        renderView(studentRDS, solutionRDS, version = answers[[input$navpage]]) # get dirty
        
      }
    })

    output$renderSubmit <- renderUI({
      if (input$navpage != 'overview') {
        showNotification("A fresh copy of your submission was loaded!", type = "message")
        
        studentRDS  <- readRDS(rdsPath(input$navpage, path = file.path(studentPath(pkg), "submitted")))
        solutionRDS <- readRDS(rdsPath(input$navpage, file.path(pkg, "data") , tag = "-solutions"))
        
        renderView(studentRDS, solutionRDS, version = submits[[input$navpage]]) # get dirty
      }
    })
    
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
  print(page)
  navbarPage( id       = 'navpage'
            , selected = page
            , title    = config$build$package$name
            , theme    = siteyml$output$html_document$theme
            , collapsible = TRUE
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
                                                   , uiOutput('renderCheck')
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
                                                   , value = splitext(name)
                                                   , uiOutput('renderSubmit')
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
