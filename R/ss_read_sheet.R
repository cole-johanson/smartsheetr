#' Require either a sheet name or a link,
#'
#' @param ss_id The sheetId (or permalink) of the table to read
#'
#' @return A tibble::tbl_df object
#'
#' @export
ss_read_sheet <- function(ss_id) {
  ss_id = validate_ss_id(ss_id)
  resp = ss_get(path = paste0('sheets/',ss_id))

  # resp$content is a list containing
  #   - columns: A data frame with id, version and title
  #   - rows: A list of lists containing id, rowNumber, and a data frame 'cells' of collumnId and value for
  #           each column.

  # Record the column names
  colnames = ss_resp_data_to_dataframe(resp$content$columns)$title

  # 1. Parse the row cells (one row in the data frame per cell in the table). The "value" column is a list
  #    of lists to prevent the column types from mixing (i.e. list(list("a"),list(1))) is valid but c("a",1)
  #    is not.
  # 2. The values are in column order, so group by the rowNumber and use the position of the cell to add the
  #    colname.
  # 3. Pivot the colnames up.
  # 4. Remove the rowNumber and unlist each cell (taking care to treat NULL as NA).
  x = tibble::tibble(row = resp$content$rows) |>
    tidyr::unnest_wider(row) |>
    tidyr::unnest_longer(cells) |>
    tidyr::unnest_wider(cells) |>
    dplyr::group_by(rowNumber) |>
    dplyr::mutate(
      colNumber = dplyr::row_number(),
      colname = colnames[colNumber]
    ) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(
      id_cols = rowNumber,
      names_from = colname,
      values_from = value
    ) |>
    dplyr::select(-rowNumber) |>
    dplyr::mutate_all(unlist_and_replace_null)

  return(x)
}

# Helper function for nested lists
unlist_and_replace_null <- function(l) {
  unlist(purrr::map(l, ~if(is.null(.x)) {NA} else {.x}))
}
