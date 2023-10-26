#' List row ids for a given sheet
#'
#' Returns a vector of the Smartsheet internal row ids for a given sheet
#'
#' @param ss_id The sheetId, permalink, or name of the Smartsheet sheet to read
#'
#' @examples
#' \dontrun{
#' df = data.frame(PK=c(1,2), FK=c("a","b"))
#' ss_id = ss_sheetid(ss_write_sheet(paste0("smartsheetr-example-",random_sheet_name()), data=df))
#' ss_row_ids(ss_id)
#' # clean up
#' ss_delete_sheet(ss_id)
#' }
#'
#' @return A numeric vector
#'
#' @export
ss_row_ids <- function(ss_id) {
  ss_id = validate_ss_id(ss_id)
  resp_sheet = ss_get(path = paste0('sheets/',ss_id))
  ss_resp_data_to_dataframe(resp_sheet$content$rows)$id
}
