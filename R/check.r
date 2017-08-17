# Utilities for code checking with checkr

#' Short hand for checkr test final() followed by check_value().
#'
#' @param ... argunents passed along to \code{\link{checkr::check_value}}
#'
#' @return test function that takes a capture
#' @export
check_final <- function(...) {
  function(capture) {
    checkr::check_value(...)(checkr::final_(capture))
  }
}

#' Code checking function that ensures the last value in a set of expressions is
#' a table with the same columns as a target
#' 
#' @param x the target table
#'   
#' @return test function that takes a capture
#' @export
check_cols <- function(x) {
  check_final(checkr::match_data_frame(x, names_match = TRUE), "Your table doesn't have the right set columns.")
}

#' Code checking function that ensures the last value in a set of expressions is
#' a table with the same number of rows as a target
#' 
#' @param x the target table
#'   
#' @return test function that takes a capture
#' @export
check_rows <- function(x) {
  check_final(checkr::match_data_frame(x, nrow = TRUE), "Your table doesn't have the right number of rows.")
}

# for each answer returns NA if no checks or all checks passed; message string if not.
check <- function(answers, checks) {
  checkNames <- names(checks)
  vapply( names(answers)
        , function(name) {
            checkFuns <- checks[grepl(paste0(name, ".\\d"), x = checkNames)]
            capture   <- answers[[name]]
            for (f in checkFuns) {
              res <- f(capture)
              if (!res$passed) { return(res$message) }
            }
            as.character(NA)
          }
        , FUN.VALUE = ""
        , USE.NAMES = TRUE
        )
}
