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
            renderTemplate( template  = file.path(path, "templates", "init", file) 
                          , data      = data
                          , file      = file.path(path, file)
                          , overwrite = overwrite
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
  
  renderTemplate( template  = file.path(path, "package", "DESCRIPTION")
                , data      = data
                , file      = file.path(path, "package", "DESCRIPTION")
                , overwrite = overwrite
                )
  
  renderTemplate( template  = file.path(path, "package", "R", "course.R")
                , data      = data
                , file      = file.path(path, "package", "R", "course.R")
                , overwrite = overwrite
                )
  
  # to avoid confusion remove init from templates
  unlink(file.path(path, "templates", "init"), recursive = TRUE)
  
  path
}

update <- function(path = getwd()) {
  
  rmds <- lapply( list.files(path, pattern = "*.[Rr]md")
                , function(file) { 
                    h <- getHeader(file.path(path, file))
                    list( type     = h$type
                        , rmd      = file
                        , file     = paste0(splitext(file), ".html")
                        , title    = h$title
                        , data     = h$data
                        , filehash = hash(file.path(path, file))
                        , datahash = hash(file.path(path, "data", h$data))
                        )      
                  }
                )
  names(rmds) <- sapply(rmds, function(x) x$file )
  
  types  <- sapply(rmds, function(x) x$type)
  
  config <- loadConfig(path)
  data   <- config$templates$data
  
  data$contents    <- nonames(rmds)[types == 'content'] 
  data$assignments <- nonames(rmds)[types == 'assignment']
  data$projects    <- if (identical(config$build$projects, FALSE)) FALSE else TRUE
  
  invisible(list( rmds        = rmds
                , types       = types
                , contents    = rmds[types == 'content'] 
                , assignments = rmds[types == 'assignment']
                , data        = data
                , config      = config
                )
           )
  
}

buildPackage <- function(config) {
  if (!identical(config$build$package, FALSE)) TRUE else FALSE
}

buildSite <- function(config) {
  if (!identical(config$build$site, FALSE)) TRUE else FALSE
}

buildProjects <- function(config) {
  if (!identical(config$build$projects, FALSE)) {
    if (!buildSite(config)) {
      stop("Cannot build a projects page if the website is disabled.  See `Build` section of `courseR.yml`")
    }
    TRUE 
  } else { 
    FALSE
  }
}

#' Build a project
#' 
#' @param path path for the project to build.
#' @param cleanBuild if TRUE completely rebuilds the project; erases knitr 
#'   caches and the contents of 'build/' and 'dist/'.  Set to TRUE if you have
#'   changed any of the website templates since the last build.
#' @param cleanPreviews should .html files created in the main project directory
#'   by making ad hoc previews in RStudio be deleted.
#'   
#' @export
build <- function(path = getwd(), cleanBuild = FALSE, cleanPreviews = TRUE) {
  
  update <- update(path)
  config <- update$config
  
  if (!is.null(config$build$hooks$before)) {
    lapply(config$build$hooks$before, function(f) do.call(f, args = list(update = update)))
  }
  
  distPath <- file.path(path, config$paths$dist)
  if (!dir.exists(distPath)) { dir.create(distPath) }
  
  # package
  if (buildPackage(config)) {
    message("Building course package...")
    pkgPath <- file.path(distPath, config$build$package$name)
    
    descData         <- config$templates$data
    descData$version <- as.character(packageVersion("courseR"))
    descData$date    <- as.character(date())
    renderTemplate( template  = file.path(path, "templates", "package", "DESCRIPTION")
                  , data      = descData
                  , file      = file.path(path, "package", "DESCRIPTION")
                  , overwrite = TRUE
                  )
    
    roxygen2::roxygenize(file.path(path, "package"), roclets=c('rd', 'collate', 'namespace'))
    file <- devtools::build(pkg = file.path(path, "package"), path = distPath, binary = TRUE, manual = TRUE)
    system(paste("tar -C", distPath, "-zxf", file))
   
    file.copy( from = file.path(path, "courseR.yml")
             , to   = file.path(pkgPath, "data")
             , overwrite = TRUE
             )
    file.copy( from = file.path(path, "_site.yml")
             , to   = file.path(pkgPath, "data")
             , overwrite = TRUE
             )
    
    taskPath <- file.path(pkgPath, "data", "assignments")
    if (!dir.exists(taskPath)) { dir.create(taskPath) }
    gradePath <- file.path(pkgPath, "data", "feedback")
    if (!dir.exists(gradePath)) { dir.create(gradePath) }
    
    file.copy( from = file.path(path, "data/")
             , to   = file.path(taskPath)
             , recursive = TRUE
             , overwrite = TRUE
             )
    
    saveRDS( update
           , file = file.path(pkgPath, "data", "course.rds")
           )
  }
  
  # website
  if (buildSite(config)) {
    message("Building website...")
    
    buildPath <- file.path(path, config$build$site$build)
  
    if (cleanBuild && dir.exists(buildPath)) {
      unlink(buildPath, recursive = TRUE)
    }
    
    if (!dir.exists(buildPath)) { dir.create(buildPath) }

    pkgDataPath <- file.path(buildPath, ".pkgData")
    if (buildPackage(config) && !dir.exists(pkgDataPath)) { dir.create(pkgDataPath) }
    
    # copy core rmarkdown website files
    file.copy( from = file.path(path, c("_site.yml", "_navbar.html", "footer.html", "index.Rmd", "data")) # index.Rmd can't be spelled .rmd currently
             , to   = buildPath
             , recursive = TRUE
             )
    
    # load partials for templating
    partials = loadPartials(file.path(path, "templates", "site", "partials"))
    
    # figure out which content and assignment pages need to be built and rendered
    lastBuild <- if (file.exists(file.path(buildPath, "last-build.rds"))) {
                   readRDS(file.path(buildPath, "last-build.rds"))
                 } else {
                   list(rmds = list(), files = "")
                 }
    
    lapply(names(lastBuild$rmds), function(name) {
      if (!name %in% names(update$rmds)) {
        unlink(file.path(buildPath, paste0(splitext(name), ".Rmd")))
        unlink(file.path(distPath, config$build$site$dist, name))
        unlink(file.path(distPath, config$build$site$dist, paste0(splitext(name), "-slides.html")))
      }
    })
    
    rebuild <- function(rmd) {
      if (rmd$file %in% names(lastBuild$rmds)) {
        lastRMD <- lastBuild$rmds[[rmd$file]]
        
        # if dependant data have changed, purge the knitr cache and build
        if (!identical(lastRMD$datahash, rmd$datahash)) {
          knitrCache <- file.path(buildPath, splitext(rmd$file), "_cache")
          if (dir.exists(knitrCache)) { unlink(knitrCache, recursive = TRUE) }
          return(TRUE)
        }
        
        # if only the source file has changed, leave knitr cache in place and build
        if (lastRMD$filehash != rmd$filehash) {
          return(TRUE)
        }
        
        # if nothing has changed remove the source from the build path, and skip it this time
        unlink(file.path(buildPath, rmd$rmd))
        unlink(file.path(buildPath, paste0(splitext(rmd$file), "-slides.Rmd")))
        FALSE
        
      } else {
        
        # if the source didn't exist before, build it
        TRUE
        
      }
    }
    
    contents    <- Filter(rebuild, update$contents)
    assignments <- Filter(rebuild, update$assignments)
    
    # build content page and slide Rmds to build/
    pagePath  <- file.path(path, "templates", "site", "content-page.Rmd")
    pageHead  <- getHeader(pagePath)
    pageTmpl  <- subHeader(pagePath, "{{header}}")
    
    slidePath <- file.path(path, "templates", "site", "content-slides.Rmd")
    slideHead <- getHeader(slidePath)
    slideTmpl <- subHeader(slidePath, "{{header}}")
    
    lapply( contents
          , function(x) {
              rmd  <- getRMD(file.path(path, x$rmd))
              head <- getHeader(file.path(path, x$rmd))
              
              pagedata   <- c( update$data
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
                            , post     = stripEmptyH
                            )
              
              slidedata <- c( update$data
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
    
    # build assignment Rmds to build/
    assnPath  <- file.path(path, "templates", "site", "assignment-solution.Rmd")
    assnTmpl  <- subHeader(assnPath, "{{{header}}}")
    
    lapply( assignments
          , function(x) {
              rmd  <- getRMD(file.path(path, x$rmd))
              head <- getHeader(file.path(path, x$rmd))
              
              assndata <- c( update$data
                           , x
                           , list( solution   = solutionRMD(rmd)
                                 , tasks      = taskRMD(rmd)
                                 , assignment = splitext(x$rmd)
                                 , rdsPath    = normalizePath(pkgDataPath)
                                 )
                           )
              
              assndata$header <- renderTemplate( template = getHeaderString(assnPath) # with this implementation there is no merge of headers on assignments
                                               , assndata
                                               , partials = partials
                                               )
  
              # solution Rmds
              renderTemplate( template = assnTmpl
                            , data     = assndata
                            , file     = file.path(buildPath, x$rmd)
                            , partials = partials
                            )
              
              # task Rmds
              taskPath <- file.path(pkgDataPath, "assignments")
              if (!dir.exists(taskPath)) { dir.create(taskPath) }
              renderTemplate( template = file.path(path, "templates", "site", "assignment-tasks.Rmd")
                            , data     = assndata
                            , file     = file.path(taskPath, x$rmd)
                            , partials = partials
                            )
              
              if (cleanPreviews) {
                unlink(x$file)
              }
              
            }
          )

    
    # build shiny projects page Rmd to build/
    if (buildProjects(config)) {
      message("Updating projects page...")
      
      url <- paste0("https://api.github.com/orgs/", config$build$projects$githubOrg, "/repos")
      ps  <- httr::content( httr::GET(url), as = "parsed")
      
      if (!is.null(ps$message) && ps$message == "Not Found") {
        stop("Incorrect configuration: github can't find group `", config$build$projects$githubOrg, "` at API URL `", url, "`")
      }
      
      rateLimit <- httr::content(httr::GET("https://api.github.com/rate_limit"), as = "parsed")
      if (rateLimit$resources$core$remaining < length(ps)) {
        warning("Can't update project page because the github API rate limit had been exceeded.")
      } else {
        pushed_at <- sapply(ps, function(l) { l$pushed_at } )
        
        projects <- ps[order( strptime(pushed_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "GMT")
                            , decreasing = TRUE
                            )
                      ]
        
        projects <- lapply( projects
                          , function(p) {
                              p$contributors <- httr::content(httr::GET(p$contributors_url), as = "parsed")
                              p
                            }
                          )
        
        renderTemplate( file.path(path, "templates", "site", "projects-page.Rmd")
                      , data = c( config$templates$data
                                , list(projects = projects)
                                )
                      , file = file.path(buildPath, "projects-page.Rmd")
                      )  
      }
      
    }
        
    # render the site with rmarkdown to build/_site; supress rmarkdown warnings
    message("Rendering website...")
    smartSuppress({
      rmarkdown::render_site(input = buildPath, env = new.env())
    }, "cannot rename file")
    
    message("Copying additional files...")
    
    # remove placeholder markup from assignment files
    for (l in assignments) {
      s <- readFile(file.path(buildPath, "_site", l$file))
      s <- gsub("\\{\\{.*?\\}\\}", "", s)
      cat(s, file = file.path(buildPath, "_site", l$file))
    }
    
    # copy img to _site/img
    file.copy( from      = file.path(path, config$build$site$img, "")
             , to        = file.path(buildPath, "_site")
             , recursive = TRUE
             )
    
    # copy img to build/img
    file.copy( from      = file.path(path, config$build$site$img, "")
             , to        = buildPath
             , recursive = TRUE
             )
    
    # copy rendered files to the dist path
    sitePath <- file.path(distPath, config$build$site$dist)
    if (!dir.exists(sitePath)) { dir.create(sitePath) }
    file.copy( from      = list.files(file.path(buildPath, "_site/"), full.names = TRUE)
             , to        = sitePath
             , recursive = TRUE
             , overwrite = TRUE
             )
    
    # render includes.html containing navbar and footer content
    renderTemplate( template = file.path(path, "templates", "site", "includes.html")
                  , data     = update$data
                  , file     = file.path(sitePath, "includes.html")
                  , partials = partials
                  )
    
    # the templating data for site_libs and scripts is the project configuration
    configTemplate <- c(config, config$templates$data)
    configTemplate$`student-usernames` <- lapply( config$`student-users`
                                                , function(name) list(name = name)
                                                )
    
    # overwrite any custom site_libs files
    overwriteLibs  <- file.path(path, "templates", "site_libs")
    overwriteFiles <- list.files(overwriteLibs, recursive = TRUE)
    lapply( overwriteFiles
          , function(f) {
              renderTemplate( template  = file.path(overwriteLibs, f)
                            , data      = configTemplate
                            , file      = file.path(sitePath, "site_libs", f)
                            , overwrite = TRUE
                            )
            }
          )

    if (identical(config$build$scripts, TRUE)) {
      scriptPath <- file.path(path, "scripts")
      if (!dir.exists(scriptPath)) { dir.create(scriptPath) }
      scripts <- list.files(file.path(path, "templates", "scripts"))
      lapply( scripts
            , function(f) {
              renderTemplate( template  = file.path(path, "templates", "scripts", f)
                            , data      = configTemplate
                            , file      = file.path(path, "scripts", f)
                            , overwrite = TRUE
                            )
              }
            )
    }
    
    if (buildPackage(config)) {
      # Copy .js and .css files to project package if it's included in the build
      file.copy( from      = file.path(sitePath, "site_libs/")
               , to        = file.path(distPath, config$build$package$name, "data")
               , recursive = TRUE
               )
      
      # copy contents of build/.pkgData to the package's data folder
      file.copy( from      = list.files(pkgDataPath, full.names = TRUE)
               , to        = file.path(distPath, config$build$package$name, "data")
               , recursive = TRUE
               , overwrite = TRUE
               )
      
    }

  }

  # save build information
  saveRDS(update, file = file.path(buildPath, "last-build.rds"))
  
  if (!is.null(config$build$hooks$after)) {
    lapply(config$build$hooks$after, function(f) do.call(f, args = list(update = update)))
  }
  
  TRUE
}


#' Publish the latest build of the project
#' 
#' Copies the lastest build of (optionally) the website and package to paths 
#' specified in in the `publish` section of the `courseR.yml` configuration
#' file.  The current user obviously needs write permissions for the specified
#' path.
#' 
#' @param path  path to the project to publish
#' @param build should the project be rebuilt before publishing
#' @param www   should the website be published
#' @param pkg   should the package be published
#'   
#' @export
publish <- function(path = getwd(), build = TRUE, www = buildSite(config), pkg = buildPackage(config)) {
  config   <- loadConfig(path)
  distPath <- file.path(path, config$paths$dist)
  
  if (!build && !file.exists(distPath)) {
    stop("The target project has not been built.  Please use `courseR::build` or specify `build = TRUE`.")
  }
  
  if (build) { build(path = path) }
  if (www) {
    wwwPath <- file.path(path, config$publish$site, "/")
    if (!dir.exists(wwwPath)) {
      dir.create(wwwPath)
    }
    file.copy( from = file.path(distPath, config$build$site$dist)
             , to   = wwwPath
             , recursive = TRUE
             )
    message("Website published to: ", wwwPath)
  }
  if (pkg) {
    pkgPath <- file.path(path, config$publish$package, "/")
    if (!dir.exists(pkgPath)) {
      dir.create(pkgPath)
    }
    file.copy( from = file.path(distPath, config$build$package$name)
             , to   = pkgPath
             , recursive = TRUE
             )
    message("Course package published to: ", wwwPath)
  }
  
  TRUE
}


#' Delete intermediate and final build files from the project
#'
#' @param path project path
#'
#' @export
clean <- function(path = getwd()) {
  unlink(x = file.path(path, c("_site", "build", "dist", "scripts")), recursive = TRUE)
  TRUE
}

newSource <- function(tmpl, dest, path, ...) {
  config  <- loadConfig(path)
  renderTemplate( template = file.path(path, "templates", "site", tmpl)
                , data     = c(config$templates$data, list(...))
                , file     = file.path(path, dest)
                , partials = loadPartials(file.path(path, "templates", "site", "partials"))
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

