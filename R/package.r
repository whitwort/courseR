#' Start an assignment
#' 
#' @param name name of the assignment to start
#' @param overwrite if true this function will overwrite any existing work
#'   you've done on this asignment already
#' @param path path to your course project folder
#' @param pkg path to course package
#' 
#' @return new file path
#'   
#' @export
startAssignment <- function(name, overwrite = FALSE, path = getwd(), pkg) {

  taskPath <- file.path(pkg, "data", "assignments")
  if (!grepl(".Rmd", name)) {
    name <- paste0(name, ".Rmd")
  }
  source   <- file.path(taskPath, name)
  if (!file.exists(source)) { 
    stop("There is not assignment with that name") 
  }
  
  dest <- file.path(path, name)
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

#' Custom RMarkdown document type that parses student assignments
#' 
#' Flag an Rmd file as an assignment by setting its output type to this
#' function; not meant to be called directly.
#' 
#' @param assignment name of the assignment
#' @param submit bool is this a submission render?
#' @param pkg path to course package for this assignment
#' @param ...
#'   
#' @return an R Markdown output format definition
#' @export
assignment <- function(assignment, submit = FALSE, pkg, ...) {
  print("assignment called")
  path <- studentPath()
  if (submit) {
    warning("not yet implemented")
  } else {
    config  <- loadConfig(file.path(pkg, "data"))
    pkgPath <- file.path(path, config$build$package$name)
    if (!dir.exists(pkgPath)) { dir.create(pkgPath) }
    rdsPath <- file.path(pkgPath, "check") #update to handle submission runs
    if (!dir.exists(rdsPath)) { dir.create(rdsPath) }
  }

  doc <- rmarkdown::html_document()
  doc$keep_md <- TRUE
  doc$clean_supporting <- FALSE
  
  doc$knitr$knit_hooks <- list(task = function(before, options, envirs, ...) { 
      print(names(options))
      print(names(list(...)))
      if (before) { 
        paste0("start-", options$task) 
      } else { 
        paste0("end-", options$task) 
      }
    })
  
  doc
}
