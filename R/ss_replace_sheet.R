#' Replace the contents of a sheet with a new data frame
#'
#' @param ss_id The sheetId (or permalink) of the table to read
#' @param df The new sheet contents
#'
#' @return A named list of ss_resp objects
#'
#' @export
ss_replace_sheet <- function(ss_id, df) {
  rows_resp_delete = ss_delete_rows(ss_id)

  new_col_names = colnames(df)
  cols_resp_rename_pk = ss_rename_columns(ss_id, new_names = new_col_names[1], column_locs = 1) # Renames PK
  cols_resp_delete_rest = ss_delete_columns(ss_id) # Deletes all but PK
  cols_resp_add_rest = ss_add_columns(ss_id, subset(df, select = -1), index = 1) # Adds rest, starting at (0-indexed) pos 1
  rows_resp_add = ss_add_rows(ss_id, df)

  return(list(
    cols_resp_rename_pk = cols_resp_rename_pk,
    cols_resp_delete_rest = cols_resp_delete_rest,
    cols_resp_add_rest = cols_resp_add_rest,
    rows_resp_add = rows_resp_add
  ))
}
