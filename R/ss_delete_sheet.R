#' Delete a smartsheet
#'
#' @param ss_id The sheetId (or permalink) of the table to read
#'
#' @return An ss_resp object
#'
#' @export
ss_delete_sheet <- function(ss_id) {
  ss_id = validate_ss_id(ss_id)
  ss_delete(path = paste0('sheets/',ss_id))
}
