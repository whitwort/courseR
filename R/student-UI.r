#' @import shiny
NULL

knitAssignment <- function(name, sourcePath) {
  message("Knitting updated assignment file: ", name)
  showNotification( paste("Re-Knitting assignment source file:", name, "...")
                  , duration = 120
                  , type     = "warning"
                  , id       = "knitting"
                  )
  
  result <- try({ rmarkdown::render(sourcePath, envir = new.env()) })
  removeNotification("knitting")
  
  if (class(result) == "try-error") {
    showNotification( "Looks like re-knitting your source file didn't go well; this page was not updated.  See the console for error messages."
                    , type     = "error"
                    , duration = 10
                    )
  } else {
    showNotification("Your assignment source file was successfully reloaded.", type = "message")
  }
  
}

panel <- function(heading, body, footer = NA, class = "default") {
  div( class = paste0("panel panel-", class)
     , div(class = "panel-heading", heading)
     , div(class = "panel-body", body)
     , if (!identical(footer, NA)) { div(class = "panel-footer", footer) }
     )
}

studentServer <- function(pkg, autoknit, wd) {
  
  pkgPath <- file.path(normalizePath(pkg), "data")
  config  <- yaml::yaml.load_file(file.path(pkgPath, "courseR.yml"))
  
  function(input, output, session) {
    interval <- 500  # in ms
    
    checked   <- do.call(reactiveValues, args = as.list(listChecked(pkg)))
    submitted <- do.call(reactiveValues, args = as.list(listSubmitted(pkg)))
    
    gradePath <- file.path(getGradePath(pkgPath), paste0(getUser(),".rds"))
    grades    <- do.call(reactiveValues, args = if (file.exists(gradePath)) readRDS(gradePath) else list())
    lastGrade <- file.mtime(gradePath)
    
    # watch rds files for new checks, submissions and grades
    observe({
      invalidateLater(interval)

      newChecked <- listChecked(pkg)
      for (name in names(checked)) {
        if (!identical(newChecked[[name]], checked[[name]])) {
          checked[[name]] <<- newChecked[[name]]
        }
      }

      newSubmitted <- listSubmitted(pkg)
      for (name in names(submitted)) {
        if (!identical(newSubmitted[[name]], submitted[[name]])) {
          submitted[[name]] <<- newSubmitted[[name]]
        }
      }
      
      currGrade <- file.mtime(gradePath) # TODO this isn't invalidating assignment pages or the summary table
      if (!identical(currGrade, lastGrade)) {
        #grades    <<- do.call(reactiveValues, readRDS(gradePath))
        newGrades <- readRDS(gradePath)
        for (name in names(newGrades)) {
          grades[[name]] <<- newGrades[[name]]
        }
        
        lastGrade <<- currGrade
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
            knitAssignment(name, getAssignmentRMD(name, path = wd, pkg))
            sources[[name]] <- newSources[[name]]
          }
        }
      }
    })
    
    # editBtnObservers <- ""
    renderView <- function(name, studentRDS, solutionRDS, editButton = FALSE) {
      s <- studentRDS$html
      s <- gsub("<body>",  "", s, fixed = TRUE)
      s <- gsub("</body>", "", s, fixed = TRUE)
      
      status        <- getStatus(name, studentRDS, grades)
      names(status) <- names(studentRDS$answers)
      
      for (i in 1:length(solutionRDS$html)) {
        
        editBtn <- if (editButton && require(rstudioapi) && rstudioapi::isAvailable()) {
          btnName <- paste("edit", name, i, sep = "-")
          actionButton(inputId = btnName, label = "Edit")
        }
        
        pattern <- paste0("\\{\\{task-", i, "-before\\}\\}\\n(.*?)\\n\\{\\{task-", i, "-after\\}\\}")
        stat    <- status[[as.character(i)]]
        replace <- as.character(panel( heading = fluidRow( column(6, h4(stat$heading))
                                                         , column(6, span(class = "pull-right", editBtn))
                                                         )
                                     , body    = div( p(tags$b("Your answer:"))
                                                    , "\\1"
                                                    , p(tags$b("Answer key:"))
                                                    , HTML(solutionRDS$html[[i]])
                                                    )
                                     , footer  = stat$footer
                                     , class   = stat$class
                                     )
                               )
        s <- sub(pattern = pattern, replacement = replace, x = s)
      }
      
      s
    }

    renderCheck <- function(name) {
      force(name)
      renderUI({
        if (input$navpage != 'overview') {

          rmdPath        <- getAssignmentRMD(input$navpage, path = wd, pkg, exists = FALSE)
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

          version     <- checked[[paste0(name, ".html")]]  # here to dirty output when a new version is available
          studentRDS  <- readRDS(studentRDSPath)
          solutionRDS <- readRDS(rdsPath(input$navpage, file.path(pkg, "data") , tag = "-solutions"))
          
          if (length(studentRDS$taskHTML) != length(solutionRDS$html)) {
            return(
              div(p( "It looks like your R markdown file for this assignment is invalid:  it contains "
                   , code(length(studentRDS$taskHTML))
                   , " task chunks but should contain "
                   , code(length(solutionRDS$html))
                   , ".  You can get a fresh copy of the assignment markdown by temporarily renaming your current file and then running "
                   , code(paste0(config$build$package$name, '::startAssignment("'
                                 , input$navpage , '")'
                                 )
                         )
                   )
                 )
            )
          }
          
          # note: student-UI logic is robust enough to handle an out of order
          # task chunk, but this seems a recipe for confusion and complicates
          # the grading implementation.
          len   <- length(studentRDS$answers)
          names <- names(studentRDS$answers)
          if (!identical(as.character(1:len), names)) {
            return(
              div(p( "Your task chunks appear to be out of order in this assignment markdown.
                     Task chunks must be correctly ordered to be reviewed and graded.
                     Your current order is:"
                   , code(paste(names(studentRDS$answers), collapse = " "))
                   )
                 )
            )
          }
          
          subButton <- if (!identical(version, submitted[[paste0(name, ".html")]])) {
            actionButton( inputId = paste0('submit-', name)
                        , label   = "Submit"
                        , class   = "btn btn-default action-button btn-primary"
                        )
          }
          
          tagList( fluidRow( column(width = 3, p("Version:", substring(version, 1, 7)))
                           , column(width = 3, offset = 6, span(class = "pull-right", subButton))
                           )
                 , HTML(renderView(name, studentRDS, solutionRDS, editButton = TRUE))
                 )
        }
      })
    }

    for (name in .listAssignments(pkg)) {
      # assignment pages
      output[[paste0("check-", name)]]  <- renderCheck(name)
    }

    # submit button observers
    sub <- lapply( .listAssignments(pkg)
                 , function(name) {
                   # submit button handler
                     btnName <- paste0("submit-", name)
                     observeEvent(force(input[[btnName]]), {
                       submitAssignment(name, path = wd, pkg = pkg)
                     })
                   }
                 )
    
    # edit button observers    
    obs <- lapply( names(.listAssignments(pkg))
                 , function(name) {
                     solutionRDS <- readRDS(rdsPath(name, file.path(pkg, "data") , tag = "-solutions"))
                     lapply( 1:length(solutionRDS$html)
                           , function(i) {
                               btnName <- paste("edit", splitext(name), i, sep = "-")
                               observeEvent(force(input[[btnName]]), {
                                 if (require(rstudioapi) && rstudioapi::isAvailable()) {
                                   
                                   studentRDSPath <- rdsPath(splitext(name), path = studentPath(pkg))
                                   studentRDS     <- readRDS(studentRDSPath)
                                   if (file.exists(studentRDS$sourceRMD)){ 
                                     rmd <- readLines(studentRDS$sourceRMD)
                                     taskLine <- grep(paste0("task=", i), rmd)
                                     rstudioapi::navigateToFile( studentRDS$sourceRMD
                                                               , line = if (length(taskLine) > 0) {
                                                                   taskLine[1] + 1
                                                                 } else {
                                                                   1  
                                                                 }
                                                               ) 
                                   } else {
                                     warning("The source file for this assignment appears to have been deleted.")
                                   }
                                  
                                 }
                               })
                             }
                           )
                   }
                 )
    
    renderSubmit <- function(name) {
      force(name)
      renderUI({
        if (input$navpage != 'overview') {
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

          version     <- submitted[[paste0(name, ".html")]]  # here to dirty output when a new version is available
          studentRDS  <- readRDS(studentRDSPath)
          solutionRDS <- readRDS(rdsPath(name, file.path(pkg, "data") , tag = "-solutions"))

          tagList( fluidRow(column(width = 3, p("Version:", substring(version, 1, 7))))
                 , HTML(renderView(name, studentRDS, solutionRDS))
                 )
        }
      })
    }
    for (name in .listAssignments(pkg)) { output[[paste0('submit', "-", name)]] <- renderSubmit(name) }
    
    output$status <- renderTable({
      
      status <- vapply( .listAssignments(pkg)
                      , function(assignment) {
                        
                          submitPanelId <- paste("submit", assignment, sep = "-")
                          actionLinkId  <- paste("showTab", submitPanelId, sep = "-")

                          observeEvent(input[[actionLinkId]], {
                            #showTab(inputId = "navpage", target = submitPanelId, select = TRUE)
                            updateTabsetPanel(session, inputId = 'navpage', selected = submitPanelId)
                          })
                          
                          # if there's no submission for this assignment yet
                          if (is.na(submitted[[paste0(assignment, ".html")]])) {
                            return("")
                          }
                          
                          # if grade entry is null or empty
                          if ( is.null(grades[[assignment]]) || 
                               (length(grades[[assignment]]) == 0) 
                             ) {
                            return("Not yet graded")
                          }
                          
                          # if there are fewer task grades than tasks, or the grade and submission taskHASH's don't match
                          solutionRDS    <- readRDS(rdsPath(assignment, file.path(pkg, "data") , tag = "-solutions"))
                          studentRDSPath <- rdsPath(assignment, path = file.path(studentPath(pkg), "submitted"))
                          studentRDS     <- readRDS(studentRDSPath)
                          
                          gradeHashes <- vapply(grades[[assignment]], function(x) x$taskHASH, FUN.VALUE = "", USE.NAMES = FALSE)

                          if ( length(grades[[assignment]]) < length(solutionRDS$html) ||
                               !all(gradeHashes == studentRDS$taskHASH)
                             ) {
                            return(
                              HTML(as.character(tags$a( id    = actionLinkId
                                                      , href  ="#"
                                                      , class = "action-button"
                                                      , style = 'color: #333;'
                                                      , "Partially graded"
                                                      )
                                               )
                                  )
                            )
                          }
                          
                          # if all aren't marked as complete
                          if (!all(vapply(grades[[assignment]], function(x) x$completed, FUN.VALUE = TRUE))) {
                            HTML(as.character(tags$a( id    = actionLinkId
                                                    , href  ="#"
                                                    , class = "action-button"
                                                    , style = 'color: #333;'
                                                    ,"Incomplete"
                                                    )
                                             )
                                )
                            
                          # if all are
                          } else {
                            HTML(as.character(tags$a( id    = actionLinkId
                                                    , href  ="#"
                                                    , class = "action-button"
                                                    , style = 'color: #333;'
                                                    , "Complete"
                                                    )
                                             )
                                )
                          }
                        }
                      , FUN.VALUE = ""
                      )
      
      subNames    <- paste0(.listAssignments(pkg), ".html")
      checkedVers <- as.character(reactiveValuesToList(checked)[subNames])
      subVers     <- as.character(reactiveValuesToList(submitted)[subNames])
      
      data.frame( Assignment            = .listAssignments(pkg)
                , `In progress version` = substring(checkedVers, 1, 7)
                , `Submitted version`   = substring(subVers, 1, 7)
                , Status                = status
                , check.names           = FALSE
                )
      
    }, sanitize.text.function = function(x) x )
    
  }
}

studentUI <- function(pkg, page) {
  
  pkgPath <- file.path(normalizePath(pkg), "data")
  siteyml <- yaml::yaml.load_file(file.path(pkgPath, "_site.yml"))
  config  <- yaml::yaml.load_file(file.path(pkgPath, "courseR.yml"))
  
  navbarPage( id          = 'navpage'
            , selected    = page
            , title       = config$build$package$name
            , collapsible = TRUE
            , theme       = shinythemes::shinytheme(siteyml$output$html_document$theme)
            , header = tagList( tags$link(href=paste0(config$build$site$url, "/site_libs/highlightjs-1.1/default.css"), rel = "stylesheet")
                              , tags$script(src=paste0(config$build$site$url, "/site_libs/highlightjs-1.1/highlight.js"))
                              , includeScript(file.path(pkgPath, "site_libs", "navigation-1.1", "tabsets.js"))
                              )
            # , footer = tags$script("hljs.initHighlightingOnLoad();")
            , footer = tags$script("function rehighlight() {
                                      $('pre code').each(function(i, block) {
                                        hljs.highlightBlock(block)
                                       })
                                      $(\"td:contains('Incomplete')\").removeClass().addClass(\"danger\")
                                      $(\"td:contains('Complete')\").removeClass().addClass(\"success\")
                                    }
                                    $(document).on('shiny:value', function(event) {
                                      window.setTimeout(rehighlight, 1.00)
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
                                                   , uiOutput(paste0('check-', splitext(name)))
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
                                                   , uiOutput(paste0('submit-', splitext(name)))
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
