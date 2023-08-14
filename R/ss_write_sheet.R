#' Create a sheet
#'
#' Creating a sheet requires either a template or a set of columns
#' (see https://smartsheet.redoc.ly/tag/sheets#operation/create-sheet-in-sheets-folder). This function only
#' allows for the columns option.
#'
#' @param sheet_name A character vector
#' @param columns A data.frame containing at least a `title` column. Other available options are described
#' in the [Smartsheet API Columns reference](https://smartsheet.redoc.ly/tag/columnsObjects#section/Column-Object).
#' @param df
#'
#' @return A smartsheetsr response object
#'
#' @export
ss_write_sheet <- function(sheet_name, df = data.frame("PK" = character())) {
  if(length(sheet_name) != 1) {
    rlang::abort('sheet_name must have length 1.')
  }
  if(!inherits(sheet_name,'character')) {
    rlang::abort('sheet_name must be a character vector.')
  }
  cols = ss_columns(df)
  path='sheets'
  resp = ss_post(path=path, body = to_json(list(name = sheet_name, columns = cols)))

  if(nrow(df) == 0) return(resp)

  sheet_info = ss_resp_get_sheet_info(resp)
  sheetid = sheet_info$sheet_id
  body = ss_write_rows_json(df, column_ids = sheet_info$column_ids)

  # Add rows
  path = paste0('sheets/',sheetid,'/rows')
  ss_post(
    path = path,
    body = body
  )
}

ss_resp_get_sheet_info <- function(resp) {
  result = ss_response_parse(resp)$content$result

  list(
    sheet_id = result$id,
    column_ids = ss_resp_data_to_dataframe(result$columns)[,'id']
  )
}

ss_write_rows_json <- function(df, column_ids) {
  row_data = list()
  for(i in 1:nrow(df)) {
    cell_data = list()
    for(j in 1:ncol(df)) {
      cell_data[[j]] = list(columnId = unlist(column_ids[j]), value = df[i,j])
    }
    row_data[[i]] = list(toTop = T, cells = cell_data)
  }

  to_json(row_data)
}
