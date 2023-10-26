#' Reads a Smartsheet sheet into an R data frame
#'
#' @param ss_id The sheetId, permalink, or name of the Smartsheet sheet to read
#'
#' @examples
#' \dontrun{
#' df = mtcars
#' ss_id = ss_sheetid(ss_write_sheet(paste0("smartsheetr-example-",random_sheet_name()), data=df))
#' ss_read_sheet(ss_id)
#' # clean up
#' ss_delete_sheet(ss_id)
#' }
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
  rows = tibble::tibble(row = resp$content$rows)

  if(nrow(rows) == 0) {
    ss_cols_data = ss_resp_data_to_dataframe(resp$content$columns)
    return(tibble::as_tibble(ss_cols_to_dataframe(ss_cols_data)))
  }

  x = rows |>
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

#' Helper function to replace NULL values with NA, and unlist, which is useful in converting nested lists
#' to data frames
#'
#' @param l A list
#'
unlist_and_replace_null <- function(l) {
  unlist(purrr::map(l, ~if(is.null(.x)) {NA} else {.x}))
}

#' Helper function to take columns data and create a data frame.
#'
#' @param ss_cols_data A data frame
#'
ss_cols_to_dataframe <- function(ss_cols_data) {
  purrr::map(ss_cols_data$type, ss_column_type_to_class) |>
    rlang::set_names(ss_cols_data$title) |>
    as.data.frame()
}

#' Return an empty vector of the correct class from the smartsheet Column Type
#'
#' The opposite of \code{\link{ss_column_type}}
#'
#' @param ss_column_type A character vector representing a smartsheet Column Type
#'
#' @details
#' See \url{https://smartsheet.redoc.ly/tag/columnsRelated/#section/Column-Types}
#'
#' @param ss_column_type A character vector
#'
#' @return A character vector
ss_column_type_to_class <- function(ss_column_type) {
  if(ss_column_type == "DATE") {
    as.Date(x = integer(0))
  } else if(ss_column_type == "DATETIME") {
    as.POSIXlt(x = integer(0))
  } else if(ss_column_type == "TEXT_NUMBER") {
    character()
  }
}
