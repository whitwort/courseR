#' Initialize a new project
#'
#' Creates the needed file structure at the given path
#'
#' @param path path to use to create a new project
#' @param name name of the new project
#' @param overwrite overwrite existing files or not
#' @param examples should example content and assignment files be created
#'
#' @export
init <- function( path      = getwd()
                , name      = basename(path)
                , overwrite = TRUE
                , examples  = TRUE
                ) {
  
  message("Initializing courseR project: ", name)
  
  sourcePath <- system.file(package = "courseR")
  file.copy( from      = file.path(sourcePath, "project-template")
           , to        = path
           , overwrite = overwrite
           , recursive = TRUE
           )
  
  lapply( list.files(file.path(path, "templates", "init"))
        , function(file) {
            renderTemplate( template = file.path(path, "templates", "init", file) 
                          , data     = list(name = name)
                          , file = file.path(path, file)
                          )
          }
        )
  
  if (examples) {
    newContent(file = "01-sample.rmd", title = "Sample 1", data = "sample.txt")
    cat("sample data", file = file.path(path, "data", "sample1.txt"))
    
    newContent(file = "02-sample.rmd", title = "Sample 2")
    
    newAssignment(file = "03-assignment.rmd", title = "Assignment 1")
  }

}

#' Update a project
#' 
#' Updates files that depend on the current contents.  This is called
#' automatically from \code{\link{newContent}} and \code{\link{newAssignment}}.
#' 
#' @param path the path for the project to update
#' 
#' @export
update <- function(path = getwd()) {
  
  rmds <- lapply( list.files(path, pattern = "*.rmd")
                , function(file) { 
                    h <- getHeader(file.path(path, file))
                    list( type  = h$type
                        , file  = paste0(splitext(file), ".html")
                        , title = h$title
                        )                
                  }
                )
  types  <- sapply(rmds, function(x) x$type)
  
  data <- loadConfig(path)$templates$data
  data$content     <- rmds[types == 'content'] 
  data$assignments <- rmds[types == 'assignment']
  
  # _navbar
  renderTemplate( template = file.path(path, "templates", "site", "_navbar.html")
                , data     = data
                , file     = file.path(path, "_navbar.html")
                , partials = loadPartials(file.path(path, "templates", "site", "partials"))
                )
  
}

#' Build a project
#' 
#' @param cleanBuild should intermediate build files be deleted when the build 
#'   is done
#' @param cleanPreviews should .html files created in the main project directory
#'   by making ad hoc previews in RStudio be deleted
#' @param path path for the project to build
#'   
#' @export
build <- function(cleanBuild = TRUE, cleanPreviews = TRUE, path = getwd()) {
  update(path)
}

#' Create a new assignment file
#' 
#' @param file the name of the new file to create
#' @param title the title of the new assignment
#' @param data files in the data/ folder that this assignment relies upon
#' @param path path to the project to create a new assignment in
#' @param ... additional arguments are available as keys when the template is
#'   rendered to create the new file
#'   
#' @return
#' @export
#' 
#' @examples
newAssignment <- function(file, title, data = "", path = getwd(), ...) {

  newSource( tmpl  = "assignment.rmd"
           , dest  = file
           , title = title
           , data  = data
           , ...
           )
  update(path)
  
}

#' Create a new course content file
#' 
#' @param file the name of the new file to create
#' @param title the title of the new piece of course content
#' @param data files in the data/ folder that this content relies upon
#' @param path path to the project to create a new content file in
#' @param ... additional arguments are available as keys when the template is
#'   rendered to create the new file
#'   
#' @return
#' @export
#' 
#' @examples
newContent <- function(file, title, data = "", path = getwd(), ...) {
  
  newSource( tmpl  = "content.rmd"
           , dest  = file
           , title = title
           , data  = data
           , ...
           )
  update(path)
  
}


