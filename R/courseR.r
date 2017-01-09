#' Initialize a new project
#'
#' Creates the needed file structure at the given path
#'
#' @param path path to use to create a new project
#' @param name name of the new project
#' @param overwrite overwrite existing files or not
#' @param examples should example content and assignment files be created
#' @param examples should the package be installed in the distribution directory
#'
#' @export
init <- function( path      = getwd()
                , name      = basename(path)
                , overwrite = TRUE
                , examples  = TRUE
                , install   = TRUE
                ) {
  
  message("Initializing courseR project: ", name)
  
  if (!dir.exists(path)) { dir.create(path) }
  
  data <- list(name = name)
  
  # website
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
                          , data     = data
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

  # course package
  file.copy( from = file.path(path, "templates", "package/")
           , to   = file.path(path)
           , recursive = TRUE
           , overwrite = overwrite
           )
  
  renderTemplate( template = file.path(path, "package", "DESCRIPTION")
                , data = data
                , file = file.path(path, "package", "DESCRIPTION")
                )
  
  renderTemplate( template = file.path(path, "package", "R", "course.R")
                , data = data
                , file = file.path(path, "package", "R", "course.R")
                )
  
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
  
  distPath <- file.path(path, config$paths$dist)
  unlink(distPath, recursive = TRUE)
  dir.create(distPath)
  
  if (!is.logical(config$build$package)) {
    message("Building course package...")
    
    roxygen2::roxygenize(file.path(path, "package"), roclets=c('rd', 'collate', 'namespace'))
    file <- devtools::build(pkg = file.path(path, "package"), path = distPath, binary = TRUE, manual = TRUE)
    system(paste("tar -C", distPath, "-zxf", file))
   
    file.copy( from = file.path(path, "courseR.yml")
             , to   = file.path(distPath, config$build$package$dist, "data")
             )
    
    taskPath <- file.path(distPath, config$build$package$dist, "data", "assignments")
    if (!dir.exists(taskPath)) { dir.create(taskPath) }
    
    file.copy( from = file.path(path, "data/")
             , to   = file.path(taskPath)
             , recursive = TRUE
             )
        
    
  } else {
    # hacktacular
    dir.create(file.path(distPath, config$build$package$dist, "data"))
  }
  
  if (!is.logical(config$build$site)) {
    message("Building website...")
    
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
    
    # content pages and slides
    pagePath  <- file.path(path, "templates", "site", "content-page.Rmd")
    pageHead  <- getHeader(pagePath)
    pageTmpl  <- subHeader(pagePath, "{{header}}")
    
    slidePath <- file.path(path, "templates", "site", "content-slides.Rmd")
    slideHead <- getHeader(slidePath)
    slideTmpl <- subHeader(slidePath, "{{header}}")
    
    content <- rmds[types == 'content']
    lapply( content
          , function(x) {
              rmd  <- getRMD(file.path(path, x$rmd))
              head <- getHeader(file.path(path, x$rmd))
              
              pagedata   <- c( config$templates$data
                             , x
                             , list( content = collapseRMD(rmd)
                                   , slides  = paste0(splitext(x$file), "-slides.html")
                                   )
                             )
              pagedata$header <- renderTemplate( template = mergeHeader(pageHead, head)
                                               , pagedata
                                               , partials = partials
                                               )
              
              renderTemplate( template = pageTmpl
                            , data     = pagedata
                            , file     = file.path(buildPath, x$rmd)
                            , partials = partials
                            )
              
              slidedata <- c( config$templates$data
                            , x
                            , list(content = slideRMD(rmd, config))
                            )
              slidedata$header <- renderTemplate( template = mergeHeader(slideHead, head)
                                                , slidedata
                                                , partials = partials
                                                )
              
              renderTemplate( template = slideTmpl
                            , data     = slidedata
                            , file     = file.path(buildPath, paste0(splitext(x$file), "-slides.Rmd"))
                            , partials = partials
                            )
             
              
              if (cleanPreviews) {
                unlink(x$file)
              }
            }
          )
    
    # assignments
    assnPath  <- file.path(path, "templates", "site", "assignment-page.Rmd")
    assnHead  <- getHeader(assnPath)
    assnTmpl  <- subHeader(assnPath, "{{header}}")
    
    assignments <- rmds[types == 'assignment']
    lapply( assignments
          , function(x) {
              rmd  <- getRMD(file.path(path, x$rmd))
              head <- getHeader(file.path(path, x$rmd))
                
              data <- c( config$templates$data
                       , x
                       , list( solution   = solutionRMD(rmd)
                             , tasks      = taskRMD(rmd)
                             , assignment = splitext(x$rmd)
                             )
                       )
              
              data$header <- renderTemplate( template = mergeHeader(assnHead, head)
                                           , data
                                           , partials = partials
                                           )
              
              # knitr's working path is in build/
              pkgData <- file.path("..", "..", distPath, config$build$package$dist, "data")
              data$taskcollector <- renderTemplate( template = file.path(path, "templates", "site", "task-collector.Rmd")
                                                  , data = list( chunkrds   = file.path(pkgData, "solution-chunk.rds")
                                                               , assignment = data$assignment
                                                               )
                                                  )
              
              instPath <- file.path(path, "templates", "site", "assignment-instructions.md")
              
              # solution htmls
              renderTemplate( template = assnTmpl
                            , data     = data
                            , file     = file.path(buildPath, x$rmd)
                            , partials = partials
                            )
            
              # task htmls
              renderTemplate( template = file.path(path, "templates", "site", "assignment-tasks.Rmd")
                            , data     = data
                            , file     = file.path(taskPath, x$rmd)
                            , partials = partials
                            )
              
              if (cleanPreviews) {
                unlink(x$file)
              }
              
            }
          )
    
    rmarkdown::render_site(input = buildPath, env = new.env())
    file.copy( from      = file.path(buildPath, "_site/")
             , to        = distPath
             , recursive = TRUE
             )
    file.rename( from = file.path(distPath, "_site")
               , to   = file.path(distPath, config$build$site$dist)
               )
  }
  
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

