#' List available assignments
#'
#' @inheritParams courseR::listAssignment
#' 
#' @export
listAssignments <- function() {
  courseR::listAssignments(pkg = system.file(package = "{{name}}"))
}

#' Start an assignment
#' 
#' @inheritParams courseR::startAssignment
#' 
#' @export
startAssignment <- function(name, overwrite = FALSE, path = getwd()) {
  courseR::startAssignment(name = name, overwrite = overwrite, path = path, pkg = system.file(package = "{{name}}"))
}

#' Check an assignment
#' 
#' Allows you to check your current progress on an assignment against a
#' reference solution.
#' 
#' @inheritParams courseR::checkAssignment
#' 
#' @export
checkAssignment <- function(name, path = getwd(), autoknit = TRUE) {
  courseR::checkAssignment(name = name, path = path, autoknit = autoknit, pkg = system.file(package = "{{name}}"))
}

#' Submit an assignment
#' 
#' Submitting an assignment flags the current version of your work as complete 
#' and/or ready for grading.  You can resubmit assignments; doing so will reset
#' the current grading information.
#' 
#' @inheritParams courseR::submitAssignment
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
#' @inheritParams courseR::checkAssignments
#'   
#' @export
checkAssignments <- function(path = getwd(), autoknit = TRUE) {
  courseR::checkAssignments(path = path, autoknit = autoknit, pkg = system.file(package = "{{name}}"))
}

#' Custom RMarkdown document type that parses student assignments
#' 
#' Flag an Rmd file as an assignment by setting its output type to this
#' function; not meant to be called directly.
#' 
#' @inheritParams courseR::assignment
#' 
#' @export
assignment <- function(...) {
  courseR::assignment(pkg = system.file(package = "{{name}}"), ...)
}


#' Open course website
#' 
#' This convenience function will open the course website in the RStudio Viewer
#' panel.  Only works if run from within RStudio.
#' 
#' @inheritParams courseR::website
#'   
#' @export
website <- function() {
  courseR::website(pkg = system.file(package = "{{name}}"))
}

#' Publishes the current version of your Shiny project to a public server
#' 
#' Publish the current version of a Shiny app project hosted on the course's 
#' Bio-185 GitHub organization to the RNA Shiny server.
#' 
#' @inheritParams courseR::publishApp
#'   
#' @export
publishApp <- function(projectName, remove = TRUE) {
  courseR::publishApp(projectName = projectName, remove = remove, pkg = system.file(package = "{{name}}"))
}

#' Copies any current error logs for your published shiny app into your current
#' working directory. Note the logs are only available while your app is still
#' running; once you close the window they will be erased.
#' 
#' @inheritParams courseR::copyErrorLogs
#'   
#' @export
copyErrorLogs <- function(projectName) {
  courseR::copyErrorLogs(projectName, pkg = system.file(package = "{{name}}"))
}


