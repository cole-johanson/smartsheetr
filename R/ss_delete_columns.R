#' Delete non-primary columns from a given sheet.
#'
#' The primary column(s) cannot be deleted.
#'
#' @param ss_id The sheetId, permalink, or name of the Smartsheet sheet to read
#' @param column_ids A vector of the smartsheet rowIds, or NULL to delete all non-primary columns
#'
#' @examples
#' df = data.frame(PK=c(1,2), FK=c("a","b"))
#' ss_id = ss_sheetid(ss_write_sheet(paste0("smartsheetr-example-",random_sheet_name()), data=df))
#' col_ids = ss_column_ids(ss_id)
#' ss_delete_columns(ss_id, col_ids[2])
#' ss_read_sheet(ss_id)
#' # clean up
#' ss_delete_sheet(ss_id)
#'
#' @return A list of ss_resp objects
#'
#' @export
ss_delete_columns <- function(ss_id, column_ids = NULL) {
  ss_id = validate_ss_id(ss_id)
  if(is.null(column_ids)) {
    resp_sheet = ss_get(path = paste0('sheets/',ss_id))
    columns_data = ss_resp_data_to_dataframe(resp_sheet$content$columns)
    column_ids = columns_data[!columns_data$primary | is.na(columns_data$primary),"id"]
  }
  purrr::map(column_ids, ~ss_delete(path=paste0('sheets/',ss_id,'/columns/',.x)))
}
