#' List share data for a given sheet
#'
#' @param ss_id The sheetId, permalink, or name of the Smartsheet sheet to read
#'
#' @examples
#' ss_id = ss_sheetid(ss_write_sheet(paste0("smartsheetr-example-",random_sheet_name())))
#' ss_list_sheet_shares(ss_id)
#' # clean up
#' ss_delete_sheet(ss_id)
#'
#' @return A dataframe
#'
#' @export
ss_list_sheet_shares <- function(ss_id) {
  ss_id = validate_ss_id(ss_id)
  resp = ss_get(path = paste0('sheets/',ss_id,'/shares'))
  ss_resp_data_to_dataframe(resp$content$data)
}
