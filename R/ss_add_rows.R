#' Add rows to a sheet.
#'
#' @param ss_id The sheetId, permalink, or name of the Smartsheet sheet to read
#' @param data A data frame of rows to be added
#' @param column_ids A vector of the columnIds of the smartsheets sheetId. If `NULL`, this will be obtained.
#'
#' @examples
#' ss_id = ss_sheetid(ss_write_sheet(paste0("smartsheetr-example-",random_sheet_name())))
#' ss_add_rows(ss_id, data.frame("PK"="1"))
#' ss_read_sheet(ss_id)
#' # clean up
#' ss_delete_sheet(ss_id)
#'
#' @return A `ss_addrows_resp` object
#'
#' @export
ss_add_rows <- function(ss_id, data, column_ids = NULL) {
  ss_id = validate_ss_id(ss_id)

  if(is.null(column_ids)) {
    col_resp = ss_get(path = paste0('sheets/',ss_id))
    column_ids = ss_resp_data_to_dataframe(col_resp$content$columns)$id
  }

  row_data = list()
  for(i in 1:nrow(data)) {
    cell_data = list()
    for(j in 1:ncol(data)) {
      cell_data[[j]] = list(columnId = unlist(column_ids[j]), value = unlist(data[i,j]))
    }
    row_data[[i]] = list(toTop = T, cells = cell_data)
  }

  path = paste0('sheets/',ss_id,'/rows')
  resp = ss_post(path = path, body = to_json(row_data))
  class(resp) = c("ss_addrows_resp", class(resp))
  return(resp)
}
