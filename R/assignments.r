#' Collect task chunks/source code blocks
#' 
#' knitr hook function to collect source and result chunks when assignments are
#' knit.  Not meant to be used directly.
#' 
#' @param callback tail call callback
#' @param assignment name of the current assignment
#' @param rds target data file
#'   
#' @return function to set as knitr hook
#' @export
taskCollector <- function(callback, assignment, rds) {
  
  function(x, options) {
    
    if (!is.null(options$task)) {
      if (file.exists(rds)) {
        db <- readRDS(rds)
      } else {
        db <- list()
      }
      
      if (is.null(db[[assignment]])) { db[[assignment]] <- list() }
      db[[assignment]][[options$task]] <- x

      saveRDS(db, rds)
    }
    
    callback(x, options)
  }
  
}
