#' List column ids for a given sheet
#'
#' Returns a vector of the Smartsheet internal column ids for a given sheet
#'
#' @param ss_id The sheetId, permalink, or name of the Smartsheet sheet to read
#'
#' @return A numeric vector
#'
#' @examples
#' ss_id = ss_sheetid(ss_write_sheet(paste0("smartsheetr-example-",random_sheet_name())))
#' col_names = colnames(ss_read_sheet(ss_id))
#' col_ids = ss_column_ids(ss_id)
#' setNames(col_ids, col_names)
#' # clean up
#' ss_delete_sheet(ss_id)
#'
#' @export
ss_column_ids <- function(ss_id) {
  ss_id = validate_ss_id(ss_id)
  resp_sheet = ss_get(path = paste0('sheets/',ss_id))
  ss_resp_data_to_dataframe(resp_sheet$content$columns)$id
}
