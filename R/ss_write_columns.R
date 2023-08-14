#' Create a Smartsheets API Columns Object from an R data frame
#'
#' See [Smartsheets API Columns Object Reference](https://smartsheet.redoc.ly/tag/columnsObjects#section/Column-Object).
#' This function outputs a data frame that can be used as a `columns` argument in `ss_write_sheet()`.
#'
#' @return a data frame
#'
#' @export
ss_columns <- function(df) {
  cols_ = data.frame(title = colnames(df))
  cols_$primary = c(T,rep(F,ncol(df)-1))
  cols_$type = purrr::map(purrr::map(df,class), ss_column_type)
  return(cols_)
}

#' Return the Smartsheet Column Type that aligns with the R class
#'
#' @details
#' See [https://smartsheet.redoc.ly/tag/columnsRelated/#section/Column-Types]
#'
#' @param r_class A character vector (returned from a call to `base::class()`)
#'
#' @return A character vector
#'
#' @export
ss_column_type <- function(r_class) {
  if(inherits(r_class, c("Date"))) {
    return("DATE")
  } else if(inherits(r_class, c("POSIXct","POSIXt"))) {
    return("DATETIME")
  } else return("TEXT_NUMBER")
}
