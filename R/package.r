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
  file.copy( from = file.path(taskPath, "data/") # TODO check hashes
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
  
  launchStudentUI(pkg = pkg, page = name, autoknit = autoknit, wd = path)
  
}

#' Submit an assignment
#' 
#' Submits the last version of the assignment that was checked with
#' \code{\link{checkAssignment}} or \code{\link{checkAssignments}}.
#' 
#' @param name name of the assignment to submit
#' @param path optional path to your course project folder
#' @param pkg path to course package
#'   
#' @export
submitAssignment <- function(name, path = getwd(), pkg) {
  
  source  <- rdsPath(name, studentPath(pkg))
  if (!file.exists(source)) {
    stop("It doesn't look like you've ever checked your answers to this assignment.  Please do so with the `checkAssignment` function.")
  }
  
  file <- getRMDFile(name, path)
  data <- readRDS(source)
  
  if (data$sourceHASH != hash(file)) {
    warning("It looks like you've changed your source file for this assignment since the last time you checked it.  You are currently submitting the last CHECKED version.  You may want to run `checkAssignment` to check your current source file and then resubmit.")
  }
  
  dest <- rdsPath(name, file.path(studentPath(pkg), "submitted"))
  file.copy(from = source, to = dest, overwrite = TRUE)
  
  message("Assignment submitted (Version: ", substring(data$sourceHASH, 1, 7), ")")
  
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
    launchStudentUI(pkg = pkg, page = 'overview', autoknit = autoknit, wd = path)
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

#' Publish the current version of a Shiny app project hosted on the course's 
#' Bio-185 GitHub organization to the RNA Shiny server.
#' 
#' @param projectName string with your group's project name.  This must match
#'   your group's repository name on GitHub.
#' @param remove if TRUE will remove the old version of your app and update.
#' @param pkg path to course package
#'   
#' @export
publishApp <- function(projectName, remove = TRUE, pkg) {
  config <- loadConfig(file.path(pkg, "data"))
  
  basePath <- config$projects$publishPath
  path     <- file.path(basePath, projectName)
  
  if (dir.exists(path) && remove) {
    message("Removing old version...")
    unlink(path, recursive = TRUE, force = TRUE)
  } else if (dir.exists(path) && !remove) {
    message("Can't publish; an app with that name already exists.  Try 'remove = TRUE'.")
  }
  
  gitURL <- paste0("https://github.com/", config$build$projects$githubOrg, "/", projectName, ".git")
  message("Trying to clone: ", gitURL)
  
  system(paste0( "git clone "
               , gitURL
               , " "
               , path
               )
        )
  
  system(paste0("chmod -R g+w ", path))
  
  message("If there were no errors, your app was published to:")
  message(paste0(config$projects$shinyServer, projectName, "/"))
  
}

