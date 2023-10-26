#' Delete a smartsheet
#'
#' @param ss_id The sheetId, permalink, or name of the Smartsheet sheet to read
#'
#' @examples
#' \dontrun{
#' ss_id = ss_sheetid(ss_write_sheet(paste0("smartsheetr-example-",random_sheet_name())))
#' ss_read_sheet(ss_id)
#' ss_delete_sheet(ss_id)
#' }
#'
#' @return A `ss_resp` object
#'
#' @export
ss_delete_sheet <- function(ss_id) {
  ss_id = validate_ss_id(ss_id)
  ss_delete(path = paste0('sheets/',ss_id))
}
