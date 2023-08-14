#' @export
to_json <- function(...) {
  jsonlite::toJSON(..., auto_unbox = T)
}
