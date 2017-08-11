#' Custom RMarkdown document type that parses assignment solution files
#' 
#' Flag an Rmd file as an assignment by setting its output type to this
#' function; not meant to be called directly.
#' 
#' @param ... document parameters
#'   
#' @return an R Markdown output format definition
#' @export
solution <- function(...) {
 
  siteyml <- yaml::yaml.load_file("_site.yml")
  taskCollector(type = "solutions",  siteyml = siteyml, ...)
  
}

#' Custom RMarkdown document type that parses student assignments
#' 
#' Flag an Rmd file as an assignment by setting its output type to this
#' function; not meant to be called directly.
#' 
#' @param pkg path to course package for this assignment
#' @param ... document parameters
#'   
#' @return an R Markdown output format definition
#' @export
assignment <- function(pkg, ...) {
  
  config  <- loadConfig(file.path(pkg, "data"))
  siteyml <- yaml::yaml.load_file(file.path(pkg, "data", "_site.yml"))
  taskCollector(type = config$build$package$name, siteyml = siteyml, ...)
  
}

# herein lies some serious hackery
taskCollector <- function(type, siteyml, ...) {
  
  doc <- rmarkdown::html_document(..., theme = siteyml$output$html_document$theme)
  
  doc$knitr$knit_hooks <- list(task = function(before, options, ...) { 
    if (before) { 
      paste0("{{start-task-", options$task, "}}") 
    } else { 
      paste0("{{end-task-", options$task, "}}") 
    }
  })
  
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
      # we have URI encode manually
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
      
      saveRDS( list(html = embeded)
             , file.path(metadata$rdsPath, paste0(metadata$assignment, "-solutions.rds"))
             )
    } else {
      # create a template for the check-UI
      b      <- rvest::html_node(h, "body")
      d$html <- as.character(b)
      
      # save the source file path
      d$sourceRMD  <- getRMDFile( path = normalizePath(dirname(output_file))
                                , name = splitext(basename(output_file))
                                )
      d$sourceHTML <- normalizePath(output_file)
      
      saveRDS( d
             , file = file.path( studentPath(type)
                               , paste(metadata$assignment, "answers.rds", sep = "-")
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

# $filename = hash | NA if missing
hash <-function(filePath) {
  paste(as.character(openssl::md5(file(filePath))), collapse = "")
}
listCheck <- function(pkg, path = studentPath(pkg)) {
  l <- lapply( .listAssignments(pkg)
             , function(name) { 
                 filePath <- rdsPath(name, path)
                 if (!file.exists(filePath)) {
                   NA
                 } else {
                   hash(filePath)
                 }
               }
             )
  names(l) <- .listAssignments(pkg)
  l
}

rdsPath <- function(name, path, tag = "-answers") {
  file.path(path, paste0(splitext(name), tag, ".rds"))
}

listSubmitted <- function(pkg, path = studentPath(pkg)) {
  listCheck(pkg, path = file.path(path, "submitted"))
}

listSources <- function(pkg, path = studentPath(pkg)) {
  checks <- listCheck(pkg, path)
  l <- lapply( names(checks)
             , function(name) {
                 if (!is.na(checks[[name]])) {
                   filePath <- readRDS(rdsPath(name, path))$sourceRMD
                   if (!file.exists(filePath)) {
                     NA
                   } else {
                     hash(filePath)
                   }
                 }
               }
             )
    
  names(l) <- names(checks)
  l
}

studentPath <- function(pkg) {
  path <- file.path("~", ".courseR")
  pkg  <- basename(pkg)
  
  if (!dir.exists(path)) { dir.create(path) }
  pkgPath <- file.path(path, pkg)
  if (!dir.exists(pkgPath)) { dir.create(pkgPath) }
  if (!dir.exists(file.path(pkgPath, "submitted"))) { dir.create(file.path(pkgPath, "submitted")) }
  pkgPath
}