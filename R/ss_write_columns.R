#' Write the initial columns for the a sheet
#'
#' @param sheet_name A character vector
#' @param cols A ss_columns object returned from \link{ss_columns()}
#'
#' @return A `ss_addcolumns_resp` object
#'
#' @export
ss_add_columns <- function(sheet_name, cols) {
  if(!inherits(cols, 'ss_columns')) rlang::abort('cols must be a ss_columns object')
  resp = ss_post(path='sheets', body = to_json(list(name = sheet_name, columns = cols$columns)))
  class(resp) = c("ss_addcolumns_resp", class(resp))
  return(resp)
}

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
  return(structure(list(columns = cols_), class = 'ss_columns'))
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
