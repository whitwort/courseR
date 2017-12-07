#' @import shiny
#' @import shinydashboard 
NULL
library(shiny)

styledButton <- function (..., class = "btn btn-default action-button") {
  b <- actionButton(...)
  b$attribs$class <- class
  b
}

splitDropdownButton <- function( inputId
                               , label
                               , actionLinkIds  # Label = id; character vector
                               , width
                               , class = "btn btn-default action-button"
                               ) {
  
  div( class = "btn-group"
     , styledButton(inputId = inputId, label = label, class = class)
     , tags$button( type            = "button"
                  , class           = paste(class, "dropdown-toggle")
                  , `data-toggle`   = "dropdown" 
                  , `aria-haspopup` = "true" 
                  , `aria-expanded` = "false"
                  , tags$span(class = "carret")
                  , tags$span(class = "sr-only", "Toggle Dropdown")
                  )
     , tags$ul( class = "dropdown-menu"
              , tag( "ul"
                   , lapply( names(actionLinkIds)
                           , function(label) {
                               id <- actionLinkIds[label]
                               tags$li(actionLink(inputId = id, label = label))
                             }
                           )
                   )
              )
     )
  
}

instructorServer <- function(pkg, initGrades, submissions) {
  
  pkgPath <- file.path(normalizePath(pkg), "data")
  config  <- yaml::yaml.load_file(file.path(pkgPath, "courseR.yml"))
  
  function(input, output, session) {
    
    grades   <- do.call(reactiveValues, args = initGrades)

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
      
      clearButtonBinding <- paste("clear", assnName, task, student, sep = "-")
      gradeButtonIds     <- character(0)
      
      gradeButtons <- tag( "div"
                         , c( lapply( names(config$build$package$grades)
                                    , function(gradeName) {
                                        completes     <- config$build$package$grades[[gradeName]]
                                        buttonBinding <- paste("grade", assnName, task, student, gradeName, sep = "-")
                                        gradeButtonIds <<- c(gradeButtonIds, buttonBinding)
                                        
                                        observeEvent(input[[buttonBinding]], {
                                          
                                          createGradeBinding(assnName, task, student)
                                          grades[[student]][[assnName]][[task]]$completed <- completes
                                          grades[[student]][[assnName]][[task]]$grade     <- gradeName
                                          grades[[student]][[assnName]][[task]]$taskHASH  <- submissions[[assnName]][[task]][[student]]$taskHASH
                                            
                                        }, priority = 100)
                                          
                                        styledButton( inputId = buttonBinding
                                                    , label   = gradeName
                                                    , class   = if (completes) "btn btn-success action-button" else "btn btn-warning action-button" # -warning is more accessible than -danger for R/G
                                                    , width   = "100px"
                                                    )
                                     }
                                    )
                            , list(actionButton( inputId = clearButtonBinding
                                               , label   = "Clear feedback"
                                               )
                                  )
                            )
                         )

      feedbackBinding <- paste("feedback", assnName, task, student, sep = "-")
      observeEvent(input[[feedbackBinding]], {
        grades[[student]][[assnName]][[task]]$feedback <- input[[feedbackBinding]]
      })
      
      observeEvent(input[[clearButtonBinding]], {
        updateTextInput(session, feedbackBinding, value = "")
      })
      
      renderUI({
        for (id in gradeButtonIds) { input[[id]] }
        
        isolate({
          subm  <- submissions[[assnName]][[task]][[student]]
          grade <- grades[[student]][[assnName]][[task]]

                  # if this task hasn't been graded (either manually or by autograde)
          
          disp <- if (is.null(grade) || length(grade) < 2) {
                    list(footer = "", class = "default")
            
                  # if it has been graded before
                  } else {
                    
                    # if this submission is the same version that was graded
                    if (grade$taskHASH == subm$taskHASH) {
                      
                      # if the grade is completed show "success"
                      if (grade$completed) {
                        list(footer = grade$feedback, class = "success")
                      
                      # if the grade is not completed show "danger"  
                      } else {
                        list(footer = grade$feedback, class = "danger")
                      }
                      
                    # if this submission is a different version than was graded
                    } else {
                      list(footer = "", class = "info")
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
                                                         , function (x) identical(x$completed, TRUE)
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
        # message("Saving standard feedback...")
        # f <- reactiveValuesToList(feedback)
        # saveStandardFeedback(pkgPath = pkgPath, feedback = f)
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
                                           , tags$head(tagList( tags$link(href=paste0(config$build$site$url, "/site_libs/highlightjs-1.1/default.css"), rel = "stylesheet")
                                                              , tags$script(src=paste0(config$build$site$url, "/site_libs/highlightjs-1.1/highlight.js"))
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
