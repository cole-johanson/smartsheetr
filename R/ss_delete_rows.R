#' Delete rows from a given sheet
#'
#' @param ss_id The sheetId (or permalink) of the table to read
#' @param row_ids A vector of the smartsheet rowIds, or NULL to delete all
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
