#' Start an assignment
#' 
#' @param name name of the assignment to start
#' @param overwrite if true this function will overwrite any existing work
#'   you've done on this asignment already
#' @param path path to your course project folder
#'   
#' @export
startAssignment <- function(name, overwrite = FALSE, path = getwd()) {
  courseR::startAssignment(name = name, overwrite = overwrite, path = path, pgk = system.file(package = "{{name}}"))
}

#' Check an assignment
#' 
#' Allows you to check your current progress on an assignment against a
#' reference solution.
#' 
#' @param name name of the assignment to check
#' @param path path to your course project folder
#'   
#' @return true if we don't find anything dodgy
#' @export
checkAssignment <- function(name, path = getwd()) {
  courseR::checkAssignment(name = name, path = path, pkg = system.file(package = "{{name}}"))
}

#' Submit an assignment
#' 
#' Submitting an assignment flags the current version of your work as complete 
#' and/or ready for grading.  You can resubmit assignments; doing so will reset
#' the current grading information.
#' 
#' @param name name of the assignment to submit
#' @param path path to your course project folder
#'   
#' @export
submitAssignment <- function(name, path = getwd()) {
  courseR::submitAssignment(name = name, path = path, pkg = system.file(package = "{{name}}"))
}

#' Check assignments
#' 
#' This function behaves differently when called from a student versus 
#' instructor account.  For students, it shows your current progress on all
#' assignments; for instructors a grading interface.
#' 
#' @param path
#'   
#' @export
checkAssignments <- function(path = getwd()) {
  courseR::checkAssignments(path = path, pkg = system.file(package = "{{name}}"))
}
