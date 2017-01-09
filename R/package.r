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
