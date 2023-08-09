#' Return the Smartsheet Column Type that aligns with the R class
#'
#' @details
#' See [https://smartsheet.redoc.ly/tag/columnsRelated/#section/Column-Types]
#'
#' @param r_class A character vector (returned from a call to `base::class()`)
#'
#' @return A character vector
#'
ss_column_type <- function(r_class) {
  if(inherits(r_class, c("Date"))) {
    return("DATE")
  } else if(inherits(r_class, c("POSIXct","POSIXt"))) {
    return("DATETIME")
  } else return("TEXT_NUMBER")
}

ss_write_columns <- function(.data) {
  columns = data.frame(title = colnames(.data))
  columns$index = (1:nrow(columns))-1
  columns$primary = columns$index==0
  columns$type = purrr::map(purrr::map(.data,class), ss_column_type)

  jsonlite::toJSON(columns)
}

