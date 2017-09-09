#' Short hand for checkr test final_() followed by check_value().
#'
#' @param ... argunents passed along to \code{\link{checkr::check_value}}
#'
#' @return test function that takes a capture
#' @export
check_final <- function(...) {
  function(capture) {
    checkr::check_value(...)(checkr::final_(capture))
  }
}

#' Code checking function that ensures the last value in a set of expressions is
#' a table with the same columns as a target
#' 
#' @param x the target table
#'   
#' @return test function that takes a capture
#' @export
check_cols <- function(x) {
  check_final(checkr::match_data_frame(x, names_match = TRUE), "Your table doesn't have the right set columns.")
}

#' Code checking function that ensures the last value in a set of expressions is
#' a table with the same number of rows as a target
#' 
#' @param x the target table
#'   
#' @return test function that takes a capture
#' @export
check_rows <- function(x) {
  check_final(checkr::match_data_frame(x, nrow = TRUE), "Your table doesn't have the right number of rows.")
}

# for each answer returns NA if no checks or all checks passed; message string if not.
check <- function(answers, checks) {
  checkNames <- names(checks)
  vapply( names(answers)
        , function(name) {
            checkFuns <- checks[grepl(paste0("^", name, ".\\d"), x = checkNames)]
            capture   <- answers[[name]]
            if (length(capture$returns) == 0) {
              return("Your code does not appear to do anything.")
            }
            for (f in checkFuns) {
              res <- f(capture)
              if (!res$passed) { return(res$message) }
            }
            as.character(NA)
          }
        , FUN.VALUE = ""
        , USE.NAMES = TRUE
        )
}

# grade RDS layout:  
# assignment:
#  "1":
#    completed: TRUE|FALSE
#    feedback:  ""
#    grade:     ""
#    taskHASH:  md5 hash

getStatus <- function(assignment, studentRDS, gradeRDS) {
  
  lapply( names(studentRDS$answers)
        , function(taskName) {
            grade <- if (!is.null(gradeRDS[[assignment]][[taskName]]) &&
                         gradeRDS[[assignment]][[taskName]]$taskHASH == studentRDS$taskHASH[[as.numeric(taskName)]]
                        ) {
                          gradeRDS[[assignment]][[taskName]]
                        } else {
                          NA
                        }
          
            if (!identical(grade, NA)) {
              list( heading = grade$grade
                  , class   = if (grade$completed) "success" else "danger"
                  , footer  = grade$feedback
                  )
            } else {
              message <- studentRDS$checks[[as.character(taskName)]]
              if (!is.na(message)) {
                list(heading = "Problems found", footer = message, class = "warning") 
              } else if (studentRDS$matchesKey[[as.numeric(taskName)]]) {
                list(heading = "Not yet graded", footer = "Your output matches the answer key exactly.", class = "info")
              } else {
                list(heading = "Not yet graded", footer = NA, class = "default")
              }
           }
         }
       )
  
}

getGradePath <- function(pkgPath, create = FALSE) {
  config <- loadConfig(pkgPath)
  
  p1 <- file.path( config$`home-directory`
                 , config$`instructor-user`
                 , ".courseR"
                 )
  if (create && !dir.exists(p1)) { dir.create(p1) }
  
  p2 <- file.path(p1, config$build$package$name)
  if (create && !dir.exists(p2)) { dir.create(p2) }
  
  p3 <- file.path(p2, "feedback")
  if (create && !dir.exists(p3)) { dir.create(p3) }
  
  p3
  
}

# Grades layout
# student:
  # assignment:
  #  "1":
  #    completed: TRUE|FALSE
  #    feedback:  ""
  #    grade:     ""
  #    taskHASH:  md5 hash

loadGrades <- function(pkgPath) {
  usernames <- getUsernames(pkgPath)
  gradePath <- getGradePath(pkgPath)
  grades    <- lapply( usernames
                     , function(username) {
                         rdsPath <- file.path(gradePath, paste0(username, ".rds"))
                         if (file.exists(rdsPath)) {
                           readRDS(rdsPath)
                         }
                       }
                     )
  names(grades) <- usernames
  grades
}

saveGrades <- function(pkgPath, grades) {
  gradePath <- getGradePath(pkgPath, create = TRUE)
  
  for (username in names(grades)) {
    
    studentFile <- file.path(gradePath, paste0(username, ".rds"))
    if (file.exists(studentFile)) {
      file.copy(from = studentFile, to = paste0(studentFile, ".bak"), overwrite = TRUE)
    }
    if (!is.null(grades[[username]])) {
      saveRDS( object = grades[[username]]
             , file   = studentFile
             ) 
    }
  }
}

# loadStandardFeedback <- function(pkgPath) {
#   gradePath <- getGradePath(pkgPath)
#   filePath  <- file.path(gradePath, "standard-feedback.rds")
#   feedback  <- if(file.exists(filePath)) { 
#                  readRDS(filePath)
#                } else {
#                  list(global = c(pasteSolution = "Paste solution"))
#                }
#   feedback
# }
# 
# saveStandardFeedback <- function(pkgPath, feedback) {
#   gradePath <- getGradePath(pkgPath)
#   saveRDS(feedback, file = file.path(gradePath, "standard-feedback.rds"))
# }

# submissions layout:
# assignment:
#   task:
#     student:
#       taskHTML:
#       taskHASH:
#       check:
#       matches:
#       lastGrade:
collectSubmissions <- function(pkgPath, assignments) {
  
  grades    <- loadGrades(pkgPath)
  usernames <- getUsernames(pkgPath)
  config    <- loadConfig(pkgPath)
  
  subs <- lapply( assignments
                , function(assignment) {
                    solutionsPath      <- file.path(pkgPath, paste0(assignment, "-solutions.rds"))
                    solutionsRDS       <- readRDS(solutionsPath)
                    tasks              <- 1:length(solutionsRDS$html)
                    submissions        <- lapply(tasks, function(t) list())
                    names(submissions) <- as.character(tasks)
                    
                    
                    for (username in usernames) {
                      answerPath <- file.path( config$`home-directory`
                                             , username
                                             , ".courseR"
                                             , config$build$package$name
                                             , "submitted"
                                             , paste0(assignment, "-answers.rds")
                                             )
                      if (file.exists(answerPath)) {
                        answerRDS <- readRDS(answerPath)
                        for (taskN in tasks) {
                          
                          # if it's not been graded, or the answer has been
                          # updated since it was last graded, include this task
                          # among the submissions
                          if (is.null(grades[[username]][[assignment]][[as.character(taskN)]]) ||
                            grades[[username]][[assignment]][[as.character(taskN)]]$taskHASH != answerRDS$taskHASH[taskN]
                          ) {
                            
                            submissions[[as.character(taskN)]][[username]] <- list( taskHTML  = answerRDS$taskHTML[[taskN]]
                                                                                  , taskHASH  = answerRDS$taskHASH[taskN]
                                                                                  , check     = answerRDS$checks[taskN]
                                                                                  , matches   = answerRDS$matchesKey[taskN]
                                                                                  , lastGrade = grades[[username]][[assignment]][[as.character(taskN)]]
                                                                                  , changed   = grades[[username]][[assignment]][[as.character(taskN)]]$taskHASH != answerRDS$taskHASH[taskN]
                                                                                  )
                            
                          }
                          
                        }
                      }
                      
                    }
                  
                  submissions 
                  }
                )
                  
  names(subs) <- assignments
  
  list(submissions = subs, grades = grades)
}

# returns only users with submissions for the course
getUsernames <- function(pkgPath) {
  config    <- loadConfig(pkgPath)
  subPath   <- file.path(".courseR", config$build$package$name)
  usernames <- if (identical(config$`student-users`, "*")) { 
                 list.dirs(config$`home-directory`, recursive = FALSE, full.names = FALSE)
               } else {
                 config$`student-users`
               }
  
  Filter( function(name) { 
            dir.exists(file.path(config$`home-directory`, name, subPath)) 
          }
        , x = usernames
        )
}
