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
  
  if (!dir.exists(path)) { dir.create(path) }
  
  sourcePath <- system.file(package = "courseR")
  file.copy( from      = list.files( file.path(sourcePath, "project-template")
                                   , full.names   = TRUE
                                   , include.dirs = TRUE
                                   )
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
    newContent(file = "01-sample.Rmd", title = "Sample 1", data = "sample.txt", path = path)
    cat("sample data", file = file.path(path, "data", "sample1.txt"))
    
    newContent(file = "02-sample.Rmd", title = "Sample 2", path = path)
    
    newAssignment(file = "03-assignment.Rmd", title = "Assignment 1", path = path)
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
  
  rmds <- lapply( list.files(path, pattern = "*.Rmd")
                , function(file) { 
                    h <- getHeader(file.path(path, file))
                    list( type  = h$type
                        , rmd   = file
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
  
  invisible(list(rmds = rmds, types = types))
  
}

#' Build a project
#' 
#' @param cleanBuild should intermediate build files be deleted when the build 
#'   is done.  Note this will trounce any knitr caching
#' @param cleanPreviews should .html files created in the main project directory
#'   by making ad hoc previews in RStudio be deleted
#' @param path path for the project to build
#'   
#' @export
build <- function(cleanBuild = FALSE, cleanPreviews = TRUE, path = getwd()) {
  
  update <- update(path)
  rmds   <- update$rmds
  types  <- update$types
  
  config <- loadConfig(path)
  
  if (!is.logical(config$build$site)) {
    buildPath <- file.path(path, config$build$site$build)
    
    if (cleanBuild && dir.exists(buildPath)) {
      unlink(buildPath)
    }
    
    if (!dir.exists(buildPath)) {
      dir.create(buildPath)
    }
    
    file.copy( from = file.path(path, c("_site.yml", "_navbar.html", "footer.md", "index.Rmd", "data")) # index.Rmd can't be spelled .rmd currently
             , to   = buildPath
             , recursive = TRUE
             )
    
    partials = loadPartials(file.path(path, "templates", "site", "partials"))
    
    content <- rmds[types == 'content']
    lapply( content
          , function(x) {
              rmd    <- getRMD(file.path(path, x$rmd))
              
              pagedata   <- c( config$templates$data
                             , x
                             , list( content = collapseRMD(rmd)
                                   , slides  = paste0(splitext(x$file), "-slides.html")
                                   )
                             )
              renderTemplate( template = file.path(path, "templates", "site", "content-page.Rmd")
                            , data     = pagedata
                            , file     = file.path(buildPath, x$rmd)
                            , partials = partials
                            )
              
              slidedata <- c( config$templates$data
                            , x
                            , list(content = slideRMD(rmd, config))
                            )
              
              renderTemplate( template = file.path(path, "templates", "site", "content-slides.Rmd")
                            , data     = slidedata
                            , file     = file.path(buildPath, paste0(splitext(x$file), "-slides.Rmd"))
                            , partials = partials
                            )
             
              
              if (cleanPreviews) {
                unlink(x$file)
              }
            }
          )
    
    assignments <- rmds[types == 'assignment']
    lapply( assignments
          , function(x) {
              rmd  <- getRMD(file.path(path, x$rmd))
              data <- c( config$templates$data
                       , x
                       , list( solution   = solutionRMD(rmd)
                             , assignment = splitext(x$rmd)
                             )
                       )
              
              instPath <- file.path(path, "templates", "site", "assignment-instructions.md")
              
              renderTemplate( template = file.path(path, "templates", "site", "assignment-page.Rmd")
                            , data     = data
                            , file     = file.path(buildPath, x$rmd)
                            , partials = partials
                            )
            
              if (cleanPreviews) {
                unlink(x$file)
              }
              
            }
          )
    
    rmarkdown::render_site(input = buildPath, env = new.env())
  }
  
  if (!is.logical(config$build$package)) {
    
  }
  
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
#' @export
newAssignment <- function(file, title, data = "", path = getwd(), ...) {

  newSource( tmpl  = "assignment.Rmd"
           , dest  = file
           , title = title
           , data  = data
           , path  = path
           , ...
           )
  
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
#' @export
newContent <- function(file, title, data = "", path = getwd(), ...) {
  
  newSource( tmpl  = "content.Rmd"
           , dest  = file
           , title = title
           , data  = data
           , path  = path
           , ...
           )
  
}


