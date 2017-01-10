#' Custom RMarkdown document type that parses assignment solution files
#' 
#' Flag an Rmd file as an assignment by setting its output type to this
#' function; not meant to be called directly.
#' 
#' @param ...
#'   
#' @return an R Markdown output format definition
#' @export
solution <- function(...) {
 
  taskCollector(type = "solutions", ...)
  
}

#' Custom RMarkdown document type that parses student assignments
#' 
#' Flag an Rmd file as an assignment by setting its output type to this
#' function; not meant to be called directly.
#' 
#' @param pkg path to course package for this assignment
#' @param ...
#'   
#' @return an R Markdown output format definition
#' @export
assignment <- function(pkg, ...) {

  config  <- loadConfig(file.path(pkg, "data"))
  taskCollector(type = config$build$package$name, ...)
  
}

# herein lies some serious hackery
taskCollector <- function(type, ...) {
  
  doc <- rmarkdown::html_document(...)
  
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
    s <- gsub("\\{\\{start-task-(\\d)\\}\\}", '<div class="assignment-task" id="task-\\1">' , s)
    s <- gsub("\\{\\{end-task-(\\d)\\}\\}", "</div>", s)
    
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
      rdsPath <- file.path(metadata$rdsPath, paste0(metadata$assignment, "-solutions.rds") )  
    } else {
      # type is package/course name
      rdsPath <- file.path(studentPath(), paste(type, metadata$assignment, "answers.rds", sep = "-") )
    }
    
    saveRDS(d, file = rdsPath)
    
    post(metadata, input_file, output_file, clean, verbose) 
  }
  
  #doc$keep_md <- TRUE
  
  doc
  
}

studentPath <- function() {
  path <- file.path("~", ".courseR")
  if (!dir.exists(path)) { dir.create(path) }
  path
}