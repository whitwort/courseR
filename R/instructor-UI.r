#' @import shiny
#' @import shinydashboard 
NULL
library(shiny)

styledButton <- function (..., class = "btn btn-default action-button") {
  b <- actionButton(...)
  b$attribs$class <- class
  b
}

instructorServer <- function(pkg, initGrades, submissions) {
  
  pkgPath <- file.path(normalizePath(pkg), "data")
  config  <- yaml::yaml.load_file(file.path(pkgPath, "courseR.yml"))
  
  function(input, output, session) {
    
    grades <- do.call(reactiveValues, args = initGrades)

    createGradeBinding <- function(assnName, task, student) {
      if (is.null(grades[[student]])) {
        grades[[student]] <- list()
      }
      if (is.null(grades[[student]][[assnName]])) {
        grades[[student]][[assnName]] <- list()
      }
      if (is.null(grades[[student]][[assnName]][[task]])) {
        grades[[student]][[assnName]][[task]] <- list()
      }
    }
    
    renderGrade <- function(assnName, task, student) {
      force(c(assnName, task, student))
      
      isolate({
        if (!identical(config$build$package$autograde, FALSE)) {
          if (submissions[[assnName]][[task]][[student]]$matches) {
            createGradeBinding(assnName, task, student)
            grades[[student]][[assnName]][[task]]$completed <- TRUE
            grades[[student]][[assnName]][[task]]$feedback  <- config$build$package$autograde$pass
            grades[[student]][[assnName]][[task]]$grade     <- config$build$package$autograde$pass
            grades[[student]][[assnName]][[task]]$taskHASH  <- submissions[[assnName]][[task]][[student]]$taskHASH
          }
          if (!is.na(submissions[[assnName]][[task]][[student]]$check)) {
            createGradeBinding(assnName, task, student)
            grades[[student]][[assnName]][[task]]$completed <- FALSE
            grades[[student]][[assnName]][[task]]$feedback  <- submissions[[assnName]][[task]][[student]]$check
            grades[[student]][[assnName]][[task]]$grade     <- config$build$package$autograde$fail
            grades[[student]][[assnName]][[task]]$taskHASH  <- submissions[[assnName]][[task]][[student]]$taskHASH
          }
        }
      })
      
      feedbackBinding <- paste("feedback", assnName, task, student, sep = "-")
      gradeButtons <- do.call( div
                             , lapply( names(config$build$package$grades)
                                     , function(gradeName) {
                                         completes     <- config$build$package$grades[[gradeName]]
                                         buttonBinding <- paste("grade", assnName, task, student, gradeName, sep = "-")

                                         observeEvent(input[[buttonBinding]], {
                                           
                                           createGradeBinding(assnName, task, student)
                                           grades[[student]][[assnName]][[task]]$completed <- completes
                                           grades[[student]][[assnName]][[task]]$feedback  <- input[[feedbackBinding]]
                                           grades[[student]][[assnName]][[task]]$grade     <- gradeName
                                           grades[[student]][[assnName]][[task]]$taskHASH  <- submissions[[assnName]][[task]][[student]]$taskHASH

                                         })

                                         styledButton( inputId = buttonBinding
                                                     , label   = gradeName
                                                     , class   = if (completes) "btn btn-success action-button" else "btn btn-warning action-button" # -warning is more accessible than -danger for R/G
                                                     )
                                       }
                                     )
                             )

      renderUI({
        subm  <- submissions[[assnName]][[task]][[student]]
        grade <- grades[[student]][[assnName]][[task]]

        disp  <- if (is.null(grade)) {
                   if(!is.na(subm$check)) {
                     list(footer = subm$check, class = "warning")
                   } else if (subm$matches) {
                     list(footer = "No problems; output matches answer key.", class = "info")
                   } else {
                     list(footer = "", class = "default")
                   }
                 } else {
                   if (grade$completed) {
                     list(footer = grade$feedback, class = "success")
                   } else {
                     list(footer = grade$feedback, class = "danger")
                   }
                 }

        panel( heading = fluidRow( column(6, p(student))
                                          , column(6, span(class = "pull-right", substring(subm$taskHASH, 1, 7)))
                                          )
             , body    = HTML(subm$taskHTML)
             , footer  = div( textAreaInput( inputId = feedbackBinding
                                           , label  = "Feedback"
                                           , value  = disp$footer
                                           )
                            , gradeButtons
                            )
             , class   = disp$class
             )

      })

    }
    for (assnName in names(submissions)) {
      for (task in names(submissions[[assnName]])) {
        for (student in names(submissions[[assnName]][[task]])) {
          binding <- paste("renderGrade"
                          , assnName
                          , task
                          , student
                          , sep = "-"
                          )
          output[[binding]] <- renderGrade(assnName, task, student)
        }
      }
    }
    
    output$assignmentMenu <- renderMenu({
      do.call( sidebarMenu
             , lapply( names(submissions)
                     , function(assnName) {
                       badgeName <- paste0("badge-", assnName)
                       
                       output[[badgeName]] <- renderUI({
                        as.character(
                          
                          # grades left to do (=)
                          
                          # the number of submissions (-)
                          sum(vapply(submissions[[assnName]], length, FUN.VALUE = 0)) - 
                          
                          # the number of grades assigned since we launched the app (=)
                            
                          # the number of grades currently in grades (-)
                          (sum(vapply( reactiveValuesToList(grades)     
                                     , function(studentGrades) {
                                         length(studentGrades[[assnName]])
                                       }
                                     , FUN.VALUE = 0
                                     )
                              ) -
                             
                          # the number of grades in initGrades
                          sum(vapply( initGrades    
                                    , function(studentGrades) {
                                        length(studentGrades[[assnName]])
                                      }
                                    , FUN.VALUE = 0
                                    )
                             )
                          )
                        )
                         
                       })
                       
                       menuItem( text       = assnName
                               , tabName    = assnName
                               , badgeLabel = uiOutput(badgeName)
                               )

                       }
                     )
             )
    })
    
    output$overview <- renderTable({
      
      d <- data.frame(student = names(reactiveValuesToList(grades)), stringsAsFactors = FALSE)
      
      for (assnName in splitext(.listAssignments(pkg))) {
        d[[assnName]] <- vapply( d$student
                               , function(student) {
                                   currGrades <- grades[[student]][[assnName]]
                                   if (is.null(currGrades)) {
                                     if (is.null(submissions[[assnName]][["1"]][[student]])) {
                                       "No submission"
                                     } else {
                                       "Waiting for grading"
                                     }
                                   } else {
                                     taskGrades <- vapply( currGrades
                                                         , function (x) x$completed
                                                         , FUN.VALUE = TRUE
                                                         )
                                     if (all(taskGrades)) {
                                       "Done"
                                     } else {
                                       "Errors"
                                     }
                                   }
                                 }
                               , FUN.VALUE = ""
                               )
      }
      
      d
    })
  
    # hack: for some reason apps disconnects after 60s of inactivity when
    # launched in Rstudio server
    output$heartbeat <- renderUI({
      invalidateLater(58 * 1000, session)
      p(Sys.time(), style = "visibility: hidden;")
    })
    
    
    onStop(function() {
      isolate({
        message("Saving grades...")
        g <- reactiveValuesToList(grades)
        saveGrades(pkgPath = pkgPath, grades = g)
        message("Saved.")
      })
    }, session = session)
  }
  
}

instructorUI <- function(pkg, submissions, solutions) {
  
  pkgPath <- file.path(normalizePath(pkg), "data")
  config  <- yaml::yaml.load_file(file.path(pkgPath, "courseR.yml"))
  
  items <- c( list(tabItem( tabName = "overview"
                          , fluidRow(box(title = "Overview", tableOutput("overview"), width = 12))
                          )
                  )
            , lapply( names(submissions)
                    , function(assnName) {
                        a <- lapply( names(submissions[[assnName]])
                                   , function(task) { 
                                       t <- c( list( title = paste0("Task-", task)
                                                   , p(tags$b("Answer key:"))
                                                   , HTML(solutions[[assnName]]$html[[as.numeric(task)]])
                                                   , br()
                                                   )
                                             , lapply( names(submissions[[assnName]][[task]])
                                                     , function(student) {
                                                         uiOutput(paste("renderGrade"
                                                                       , assnName
                                                                       , task
                                                                       , student
                                                                       , sep = "-"
                                                                       )
                                                                 )
                                                       }
                                                     )
                                             )
                                       do.call(tabPanel, t)
                                     }
                                   )
                        a$title <- assnName
                        a$width <- 12
                        tabItem(tabName = assnName, fluidRow(do.call(tabBox, a)))
                      }
                    )
            )
  
  dashboardPage( header  = dashboardHeader( title = config$build$package$name
                                          , uiOutput('heartbeat')
                                          )
               , sidebar = dashboardSidebar( sidebarMenu( menuItem("Overview"
                                                                  , tabName = "overview"
                                                                  , icon    = icon("th")
                                                                  )
                                                        )
                                           , sidebarMenuOutput("assignmentMenu")
                                           , tags$head(tagList( includeCSS(file.path(pkgPath, "site_libs", "highlightjs-1.1", "default.css"))
                                                              , includeScript(file.path(pkgPath, "site_libs", "highlightjs-1.1", "highlight.js"))
                                                              )
                                                      )
                                           )
               , body    = dashboardBody( do.call(tabItems, items)
                                        , tags$footer( tags$script("function rehighlight() {
                                                                      $('pre code').each(function(i, block) {
                                                                        hljs.highlightBlock(block)
                                                                       })
                                                                    }
                                                                    $(document).on('shiny:value', function(event) {
                                                                      window.setTimeout(rehighlight, 1.00) 
                                                                      $(\"td:contains('Errors')\").removeClass().addClass(\"danger\")
                                                                      $(\"td:contains('Done')\").removeClass().addClass(\"success\")
                                                                    })
                                                                   "
                                                                  )
                                          
                                                     )
                                        )
               , title   = config$build$package$name
               , skin    = "black"
               )
}

launchInstructorUI <- function(pkg) {
  
  message("Loading course data...")
  pkgPath   <- file.path(normalizePath(pkg), "data")
  courseRDS <- readRDS(file.path(pkgPath, "course.rds"))
  assnNames <- splitext(names(courseRDS$assignments))
  solutions <- lapply( assnNames
                     , function(name) {
                         readRDS(file.path(pkgPath, paste0(name, "-solutions.rds")))
                       }
                     )
  names(solutions) <- assnNames
  
  message("Loading new submissions...")
  ret         <- collectSubmissions(pkgPath, assnNames)
  submissions <- ret$submissions
  grades      <- ret$grades
  
  app <- shinyApp( ui     = instructorUI(pkg, submissions, solutions)
                 , server = instructorServer(pkg, grades, submissions)
                 )
  runApp(app)
  
}
