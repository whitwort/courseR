#' Custom RMarkdown document type that parses assignment solution files
#' 
#' Flag an Rmd file as containing assignment solutions by setting its output
#' type to this function; not meant to be called directly.
#' 
#' @param ... document parameters
#'   
#' @return an R Markdown output format definition
#' @export
solution <- function(...) {
 
  siteyml <- yaml::yaml.load_file("_site.yml")
  taskCollector(type = "solutions",  siteyml = siteyml, ...)
  
}

#' Custom RMarkdown document type that parses student answers to assignments
#' 
#' Flag an Rmd file as containing a set of answers to an assignment by setting
#' its output type to this function; not meant to be called directly.
#' 
#' @param pkg path to course package for this assignment
#' @param ... document parameters
#'   
#' @return an R Markdown output format definition
#' @export
assignment <- function(pkg, ...) {
  
  config  <- loadConfig(file.path(pkg, "data"))
  siteyml <- yaml::yaml.load_file(file.path(pkg, "data", "_site.yml"))
  taskCollector(type = config$build$package$name, siteyml = siteyml, pkg = pkg)
  
}

# herein lies some serious knitr/rmarkdown hackery
taskCollector <- function(type, siteyml, pkg = NULL, ...) {
  
  doc <- rmarkdown::html_document(..., theme = siteyml$output$html_document$theme)
  
  # custom hooks
  task     <- function(before, options, envir) {
    if (before) { 
      paste0("{{start-task-", options$task, "}}") 
    } else { 
      paste0("{{end-task-", options$task, "}}") 
    }
  }
  
  # and here there be mutation of document-wide globals; avert your eyes.
  checkrs  <- list()
  checkr   <- function(before, options, envir) {
    if (!before) {
      code  <- paste(options$code, collapse = "\n")
      value <- capture.code.envir(code_text = code, envir = envir)
      
      captureFunc <- function(f) { is.function(f) && !is.null(formals(f)$capture) }
      funcs       <- Filter(captureFunc, value$returns)
      
      checkrs[[paste(options$task, options$checkr, sep = ".")]] <<- reducer(funcs = funcs)
    }
  }
  
  solutions <- list()
  solution  <- function(before, options, envir) {
    if (!before) {
      code  <- paste(options$code, collapse = "\n")
      value <- capture.code.envir(code_text = code, envir = envir)
      solutions[[as.character(options$task)]] <<- value
    }
  }
  
  answers <- list()
  answer  <- function(before, options, envir) {
    if (!before) {
      code  <- paste(options$code, collapse = "\n")
      value <- capture.code.envir(code_text = code, envir = envir)
      answers[[as.character(options$task)]] <<- value
    }
  }
  
  doc$knitr$knit_hooks <- list( task     = task
                              , checkr   = checkr
                              , solution = solution
                              , answer   = answer
                              )
  
  pre <- doc$pre_processor
  doc$pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) { 
    
    s <- readFile(input_file)
    s <- gsub( "\\{\\{start-task-(\\d+)\\}\\}"
             , '<div>{{task-\\1-before}}</div><div class="assignment-task" id="task-\\1">' 
             , s
             )
    s <- gsub( "\\{\\{end-task-(\\d+)\\}\\}"
             , '</div><div>{{task-\\1-after}}</div>'
             , s
             )
    
    cat(s, file = input_file)

    pre(metadata, input_file, runtime, knit_meta, files_dir, output_dir) 
  }
  
  post <- doc$post_processor
  doc$post_processor <- function(metadata, input_file, output_file, clean, verbose) { 

    s  <- readFile(output_file)
    h  <- xml2::read_html(s)
    
    # just in case for some extremely odd reason they re-arrange their task blocks
    ns <- rvest::html_nodes(h, ".assignment-task")
    d  <- lapply( 1:(length(ns))
                , function(i) {
                    as.character(rvest::html_node(h, paste0("#task-", i)))
                  }
                )
    
    if (type == "solutions") {
      # we don't seem to be able to force self_contained during a site build; so
      # we URI encode manually
      imgs <- sapply( list.files( file.path(paste0(metadata$assignment, "_files"), "figure-html")
                                , full.names = TRUE
                                )
                    , function(file) { knitr::image_uri(file) }
                    )
      
      embeded <- lapply( d
                       , function(html) {
                           for (name in names(imgs)) {
                             html <- gsub(name, imgs[name], html, fixed = TRUE)
                           }
                           html
                         } 
                       )
      
      # check that all solutions actually pass their own checking code
      checkMessages <- check(solutions, checkrs)
      for (taskName in names(checkMessages)) {
        if (!is.na(checkMessages[[taskName]])) {
          warning( "In assignment `", input_file, "` the solution for task `"
                 , taskName, "` failed to pass checking code.  Message: '"
                 , checkMessages[[taskName]], "'"
                 )
        }
      }
      
      saveRDS( list(html = embeded, checkrs = checkrs)
             , file.path(metadata$rdsPath, paste0(metadata$assignment, "-solutions.rds"))
             )
      
    } else {
      # create a template for the check-UI
      b    <- rvest::html_node(h, "body")
      body <- as.character(b)
      
      # save the source file path
      sourceRMD  <- getRMDFile( path = normalizePath(dirname(output_file))
                              , name = splitext(basename(output_file))
                              )
      sourceHTML <- normalizePath(output_file)
      
      # check answers against the checkrs in the package and answer keys
      solutionrds   <- readRDS(file.path(pkg, "data", paste0(metadata$assignment, "-solutions.rds")))
      checkMessages <- check(answers, solutionrds$checkrs)
      
      inner <- function(x) {
        if (!is.na(x)) {
          as.character(rvest::html_children(rvest::html_children(rvest::html_children(xml2::read_html(x)))))  
        }
      }
      matchesKey    <- vapply( 1:(length(d))
                             , function(n) {
                                 sol <- inner(solutionrds$html[[n]])
                                 ans <- inner(d[[n]])
                                 identical(sol, ans[2])
                               }
                             , FUN.VALUE = TRUE
                             )
      
      saveRDS( list( html       = body
                   , sourceRMD  = sourceRMD
                   , sourceHTML = sourceHTML
                   , sourceHASH = hash(sourceRMD)
                   , answers    = answers
                   , taskHTML   = d
                   , taskHASH   = vapply(d, openssl::md5, FUN.VALUE = "")
                   , checks     = checkMessages
                   , matchesKey = matchesKey
                   )
             , file = file.path( studentPath(type)
                               , paste0(metadata$assignment, "-answers.rds")
                               )
             )
    }
    
    post(metadata, input_file, output_file, clean, verbose) 
  }
  
  doc
}

# Rmd = title
.listAssignments <- function(pkg) {
  data <- readRDS(file.path(pkg, "data", "course.rds"))
  sapply(data$assignments, function(l) { splitext(l$rmd) } )
}

rdsPath <- function(name, path, tag = "-answers") {
  file.path(path, paste0(splitext(name), tag, ".rds"))
}

# $filename = hash | NA if missing
listChecked <- function(pkg, path = studentPath(pkg)) {
  vapply( .listAssignments(pkg)
        , function(name) {
            filePath <- rdsPath(name, path)
            if (!file.exists(filePath)) {
              as.character(NA)
            } else {
              rds <- readRDS(filePath)
              rds$sourceHASH
            }
          }
        , FUN.VALUE = ""
        , USE.NAMES = TRUE
        )
}

listSubmitted <- function(pkg, path = studentPath(pkg)) {
  listChecked(pkg, path = file.path(path, "submitted"))
}

listSources <- function(pkg, path) {
  vapply( splitext(.listAssignments(pkg))
        , function(name) {
            filePath <- getRMDFile(name, path, exists = FALSE)
            if (!file.exists(filePath)) {
              as.character(NA)
            } else {
              hash(filePath)
            }
          }
        , FUN.VALUE = ""
        , USE.NAMES = TRUE
        )
}

studentPath <- function(pkg, home = "~", create = TRUE) {
  path <- file.path(home, ".courseR")
  pkg  <- basename(pkg)
  pkgPath <- file.path(path, pkg)
  
  if (create) {
    if (!dir.exists(path)) { dir.create(path) }
    if (!dir.exists(pkgPath)) { dir.create(pkgPath) }
    if (!dir.exists(file.path(pkgPath, "submitted"))) { dir.create(file.path(pkgPath, "submitted")) }
  }

  pkgPath
}