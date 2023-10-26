#' Delete rows from a given sheet
#'
#' @param ss_id The sheetId, permalink, or name of the Smartsheet sheet to read
#' @param row_ids A vector of the smartsheet rowIds, or NULL to delete all
#'
#' @examples
#' \dontrun{
#' df = data.frame(PK=c(1,2), FK=c("a","b"))
#' ss_id = ss_sheetid(ss_write_sheet(paste0("smartsheetr-example-",random_sheet_name()), data=df))
#' row_ids = ss_row_ids(ss_id)
#' ss_delete_rows(ss_id, row_ids[2])
#' ss_read_sheet(ss_id)
#' # clean up
#' ss_delete_sheet(ss_id)
#' }
#'
#' @return A list of ss_resp objects
#'
#' @export
ss_delete_rows <- function(ss_id, row_ids = NULL) {
  ss_id = validate_ss_id(ss_id)
  if(is.null(row_ids)) {
    resp_sheet = ss_get(path = paste0('sheets/',ss_id))
    row_ids = ss_resp_data_to_dataframe(resp_sheet$content$rows)$id
  }
  if(is.null(row_ids)) return(NULL)

  str_row_ids = paste0(row_ids,collapse=',')
  ss_delete(path=paste0('sheets/',ss_id,'/rows?ids=',str_row_ids))
}
