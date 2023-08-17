#' Create a sheet
#'
#' Creating a sheet requires either a template or a set of columns
#' (see https://smartsheet.redoc.ly/tag/sheets#operation/create-sheet-in-sheets-folder). This function only
#' allows for the columns option.
#'
#' @param sheet_name A character vector
#' @param df A data frame
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
  if(!inherits(df,'data.frame')) {
    rlang::abort('df must be a data frame.')
  }
  col_resp = ss_create_sheet_with_columns(sheet_name, df)

  if(nrow(df) == 0) return(col_resp)

  ss_id = ss_sheetid(col_resp)
  column_ids = ss_resp_data_to_dataframe(col_resp$content$result$columns)$id
  row_resp = ss_add_rows(ss_id, df, column_ids = column_ids)

  resp = structure(
    list(
      content = list(
        id = col_resp$content$result$id,
        name = col_resp$content$result$name,
        permalink = col_resp$content$result$permalink,
        columns = col_resp$content$result$columns,
        rows = row_resp$content$result
      ),
      responses = list(
        ss_createsheet_resp = col_resp,
        ss_addrows_resp = row_resp
      )
    ),
    class = 'ss_writesheet_resp'
  )

  return(resp)
}

#' Write the initial columns for the a sheet
#'
#' @param sheet_name A character vector
#' @param df A data frame of columns to be added
#'
#' @details See [Smartsheets API Columns Object Reference](https://smartsheet.redoc.ly/tag/columnsObjects#section/Column-Object).
#'
#' @return A `ss_addcolumns_resp` object
#'
#' @export
ss_create_sheet_with_columns <- function(sheet_name, df) {
  cols_ = data.frame(title = colnames(df))
  cols_$primary = c(T,rep(F,ncol(df)-1)) # Assume first column is the primary
  cols_$type = purrr::map(purrr::map(df,class), ss_column_type)

  resp = ss_post(path='sheets', body = to_json(list(
    name = sheet_name,
    columns = cols_
  )))
  class(resp) = c("ss_createsheet_resp", class(resp))
  return(resp)
}
