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
  cols = ss_columns(df)
  col_resp = ss_add_columns(sheet_name, cols)

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
        ss_addcolumns_resp = col_resp,
        ss_addrows_resp = row_resp
      )
    ),
    class = 'ss_writesheet_resp'
  )

  return(resp)
}
