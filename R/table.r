#' Create a table source file
#' 
#' The resulting YAML file can be easily hand editted and included as a table in
#' an Rmd file with the code{\link{renderTable}} function.
#' 
#' @param nrows number of rows
#' @param cols column names
#' @param row.col name of column for row numbers; set to false to skip
#' @param file output YAML file
#'   
#' @return the YAML string invisibly
#' @export
createTable <- function(nrows, cols, row.col = "#", file) {
  l <- lapply( 1:nrows
             , function(n) { 
                  r <- list()
                  if (!row.col == FALSE) {
                    r[[row.col]] <- as.character(n)
                  }
                  c( r
                   , sapply(cols, function(n) { "" }, simplify = FALSE)
                   ) 
               }
             )
  
  s <- yaml::as.yaml(l)
  #s <- gsub(pattern = "''", replacement = '\n    ""', x = s, fixed = TRUE)
  
  cat(s, file = file)
  invisible(s)
}

#' Render a table in an Rmd chunk
#' 
#' Use this function with the file created by code{\link{createTable}}
#' 
#' @param file source YAML file
#' @param markdown if TRUE text values in the YAML file are preprocessed as
#'   markdown
#' @param template table template file
#' @param widths if not null gives the grid system widths of the columns
#'   
#' @return the HTML table taged as HTML tools tags so that it's rendered as HTML
#'   with the rmd is knit
#' @export
renderTable <- function(file, markdown = TRUE, template = "templates/site/table.html", widths = NULL) {
  
  d <- yaml::yaml.load_file(file)

  colnames <- lapply( 1:length(d[[1]])
                    , function(i) {
                        n <- names(d[[1]])[i]  
                        if (!is.null(widths)) {
                          class = paste0('class="col-sm-', widths[i], '"')
                        } else {
                          class = NULL
                        }
                        list(name = n, class = class)
                      }
                    )
  
  rows     <- lapply( d
                    , function(l) { 
                        list(cols = lapply(names(l), function(n) list(name = n, value = l[[n]]))) 
                      } 
                    )
  
  data <- list( colnames = colnames
              , rows = if (markdown) markdownify(rows) else rows 
              )
  
  s <- whisker::whisker.render( template = readFile(template)
                              , data     = data
                              )
  
  s <- whisker.unescape(s)
  
  htmltools::HTML(s)
  
}
