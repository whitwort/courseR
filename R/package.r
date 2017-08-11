#' List available assignments
#'
#' @param pkg path to course package
#'
#' @return character vector with assignment names
#' @export
listAssignments <- function(pkg) {
  .listAssignments(pkg)
}

#' Start an assignment
#' 
#' This function will create an RMD file that you can use to start working on
#' solutions for an assignment.  It also refreshes the data files in your data/
#' directory.
#' 
#' @param name name of the assignment to start
#' @param overwrite if TRUE this function will overwrite any existing work 
#'   you've done on this asignment already
#' @param path optional path to your course project folder
#' @param pkg path to course package
#'   
#' @return new file path
#'   
#' @export
startAssignment <- function(name, overwrite = FALSE, path = getwd(), pkg) {

  taskPath <- file.path(pkg, "data", "assignments")
  tryCatch( source <- getRMDFile(name, taskPath, exists = TRUE)
          , error = function(e) {
              stop(e$message, "\n\nThere is no assignment with that name. Try `listAssignments()`.")
            }
          )

  dest <- getRMDFile(name, path, exists = FALSE)
  if (file.exists(dest) && !overwrite) {
    stop("An excercise of that name already exists; use overwrite = TRUE if you'd like to erase your current copy.")
  }

  message("Copying assignment file to: ", dest)
  file.copy(from = source, to = dest, overwrite = overwrite)
  
  message("Refreshing `data/` folder")
  file.copy( from = file.path(taskPath, "data/")
           , to   = file.path(path)
           , recursive = TRUE
           )
  
  dest
}

#' Check an assignment
#' 
#' Allows you to check your current progress on an assignment against a 
#' reference solution.
#' 
#' @param name name of the assignment to check
#' @param path optional path to your course project folder
#' @param autoknit if TRUE your assignment Rmd files will automatically be
#'   re-knitted when you save changes
#' @param pkg path to course package
#'   
#' @return true if we don't find anything dodgy
#' @export
checkAssignment <- function(name, path = getwd(), autoknit = TRUE, pkg) {

  file <- getRMDFile(name, path)
  
  message("Knitting ", name)
  rmarkdown::render(file, envir = new.env())
  
  if (grepl(".[Rr]md$", name)) {
    name <- substring(name, 1, nchar(name) - 4)
  }
  
  launchStudentUI(pkg = pkg, page = name, autoknit = autoknit)
  
}

#' Submit an assignment
#' 
#' Submitting an assignment flags the current version of your work as complete 
#' and/or ready for grading.  You can resubmit assignments; doing so will reset
#' the current feedback information.
#' 
#' @param name name of the assignment to submit
#' @param path optional path to your course project folder
#' @param pkg path to course package
#'   
#' @export
submitAssignment <- function(name, path = getwd(), pkg) {
  
  file <- getRMDFile(name, path)

  message("Knitting; the assignment will only be submitted if this succeeds...")
  rmarkdown::render(file, envir = new.env())
  
  checks    <- listCheck(pkg)
  shortHash <- substring(checks[[splitext(name)]], first = 1, last = 7) # git style

  source <- rdsPath(name, studentPath(pkg))
  dest   <- file.path(studentPath(pkg), "submitted", basename(source))
  
  file.copy(from = source, to = dest, overwrite = FALSE)
  
  message("Assignment submitted as version: ", shortHash)
  
}

#' Check assignments
#' 
#' This function behaves differently when called from a student versus 
#' instructor account.  For students, it shows your current progress on all
#' assignments; for instructors a grading interface.
#' 
#' @param path optional path to your course project folder
#' @param autoknit if TRUE your assignment Rmd files will automatically be
#'   re-knitted when you save changes; ignored in instructor mode.
#' @param pkg path to course package
#'   
#' @export
checkAssignments <- function(path = getwd(), autoknit = TRUE, pkg) {
  config <- loadConfig(file.path(pkg, "data"))
  user   <- Sys.info()["user"]
  
  if (!is.null(config$`instructor-user`) && user %in% config$`instructor-user`) {
    launchInstructorUI(pkg = pkg)
  } else if (config$`student-users` == "*" || user %in% config$`student-users`) {
    launchStudentUI(pkg = pkg, page = 'overview', autoknit = autoknit)
  } else {
    message("Your username does not appear to be an instructor or student in this course.")
  }
  
}

#' Open course website
#' 
#' This convenience function will open the course website in the RStudio Viewer
#' panel.  Only works if run from within RStudio.
#' 
#' @param pkg path to course package
#'   
#' @export
website <- function(pkg) {
  config <- loadConfig(file.path(pkg, "data"))
  rstudio::viewer(config$build$site$url)
}


