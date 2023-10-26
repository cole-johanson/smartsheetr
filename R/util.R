#' Convert list to a JSON character vector
#'
#' The default method of jsonlite::toJSON() returns values wrapped as vectors, e.g. `{"x":[1]}`. The
#' smartsheets API complains about this, so we must remove it by using the auto_unbox parameter.
#'
#' @param ... Passed on to \link[jsonlite]{toJSON}
#'
#' @keywords internal
to_json <- function(...) {
  jsonlite::toJSON(..., auto_unbox = T)
}

#' Get a random sheet name
#'
#' Randomly selects letters for a Smartsheet sheet name
#'
#' @param n The number of characters to generate
#'
#' @examples
#' random_sheet_name()
#'
#' @return A character vector
#'
#' @export
random_sheet_name <- function(n=10) {
  paste0(sample(c(LETTERS,letters), size=n, replace=TRUE), collapse="")
}
