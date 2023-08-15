#' Add rows to a sheet.
#'
#' @param ss_id The smartsheets sheetId
#' @param df A data frame of rows to add
#' @param column_ids A vector of the columnIds of the smartsheets sheetId. If `NULL`, this will be obtained.
#'
#' @export
ss_add_rows <- function(ss_id, df, column_ids = NULL) {
  ss_id = validate_ss_id(ss_id)

  if(is.null(column_ids)) {
    col_resp = resp = ss_get(path = paste0('sheets/',ss_id))
  }

  row_data = list()
  for(i in 1:nrow(df)) {
    cell_data = list()
    for(j in 1:ncol(df)) {
      cell_data[[j]] = list(columnId = unlist(column_ids[j]), value = df[i,j])
    }
    row_data[[i]] = list(toTop = T, cells = cell_data)
  }

  path = paste0('sheets/',ss_id,'/rows')
  resp = ss_post(path = path, body = to_json(row_data))
  class(resp) = c("ss_addrows_resp", class(resp))
  return(resp)
}
