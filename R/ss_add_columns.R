#' Add columns to an existing sheet
#'
#' @param ss_id The sheetId, permalink, or name of the Smartsheet sheet to read
#' @param data A data frame of columns to be added
#' @param index The index location where the columns should be added
#'
#' @examples
#' ss_id = ss_sheetid(ss_write_sheet(paste0("smartsheetr-example-",random_sheet_name())))
#' ss_add_columns(ss_id, data.frame("FK"=character()), index=1)
#' ss_read_sheet(ss_id)
#' # clean up
#' ss_delete_sheet(ss_id)
#'
#' @return A `ss_addcolumns_resp` object
#'
#' @export
ss_add_columns <- function(ss_id, data, index = 0) {
  ss_id = validate_ss_id(ss_id)

  cols_ = data.frame(title = colnames(data))
  cols_$type = purrr::map(purrr::map(data,class), ss_column_type)
  cols_$index = index

  resp = ss_post(
    path=paste0('sheets/',ss_id,'/columns'),
    body = to_json(cols_)
  )
  class(resp) = c("ss_addcolumns_resp", class(resp))
  return(resp)
}

#' Return the Smartsheet Column Type that aligns with the R class
#'
#' @details
#' See \url{https://smartsheet.redoc.ly/tag/columnsRelated/#section/Column-Types}
#'
#' @param r_class A character vector (returned from a call to `base::class()`)
#'
#' @return A character vector
ss_column_type <- function(r_class) {
  if(inherits(r_class, c("Date"))) {
    return("DATE")
  } else if(inherits(r_class, c("POSIXct","POSIXt","POSIXlt"))) {
    return("DATETIME")
  } else return("TEXT_NUMBER")
}

