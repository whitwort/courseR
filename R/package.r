#' Start an assignment
#' 
#' @param name name of the assignment to start
#' @param overwrite if true this function will overwrite any existing work
#'   you've done on this asignment already
#' @param path path to your course project folder
#' @param pkg path to course package
#'   
#' @export
startAssignment <- function(name, overwrite = FALSE, path = getwd(), pkg) {
  
  taskPath <- file.path(pkg, "data", "assignments")
  source   <- file.path(taskPath, paste0(name, ".Rmd"))
  if (!file.exists(source)) { 
    stop("There is not assignment with that name") 
  }
  
  dest <- file.path(path, paste0(name, ".Rmd"))
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
}

#' Check an assignment
#' 
#' Allows you to check your current progress on an assignment against a
#' reference solution.
#' 
#' @param name name of the assignment to check
#' @param path path to your course project folder
#' @param pkg path to course package
#'   
#' @return true if we don't find anything dodgy
#' @export
checkAssignment <- function(name, path = getwd(), pkg) {
  
}

#' Submit an assignment
#' 
#' Submitting an assignment flags the current version of your work as complete 
#' and/or ready for grading.  You can resubmit assignments; doing so will reset
#' the current grading information.
#' 
#' @param name name of the assignment to submit
#' @param path path to your course project folder
#' @param pkg path to course package
#'   
#' @export
submitAssignment <- function(name, path = getwd(), pkg) {
  
  if (!dir.exists(path, "assignments")) {
    dir.create(path, "assignments")
  }
  
  source <- file.path(path, paste0(name, ".Rmd"))
  if (!file.exists(source)) {
    stop("You don't seem to have a solution file for this assignment: ", source)
  }
  
  message("Here let me Knit that for you; the assignment will only be submitted if this succeeds...")
  tryCatch( { rmarkdown::render(source, envir = new.env()) }
          , error = function(e) stop("Well, that didn't go well.  Assignment not submitted.")
          )
  
  
  
}

#' Check assignments
#' 
#' This function behaves differently when called from a student versus 
#' instructor account.  For students, it shows your current progress on all
#' assignments; for instructors a grading interface.
#' 
#' @param path path to your course project folder
#' @param pkg path to course package
#'   
#' @export
checkAssignments <- function(path = getwd(), pkg) {
  
}
