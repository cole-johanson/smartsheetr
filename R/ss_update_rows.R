#' Update rows in a sheet.
#'
#' @param ss_id The sheetId, permalink, or name of the Smartsheet sheet to update
#' @param data A data frame of rows to be updated. Column names should match Smartsheet column names if use_column_names is TRUE.
#' @param row_ids Optional; a vector of the rowIds corresponding to the rows in the data frame.
#'                If `NULL`, the row IDs will be obtained from the sheet.
#' @param row_numbers Optional; a vector of the row numbers corresponding to the rows in the data frame.
#' @param column_ids Optional; a vector of the columnIds of the smartsheets sheetId.
#'                   If `NULL` and use_column_names is TRUE, the column IDs will be matched based on the column names in `data`.
#' @param row_offset The number of rows to skip from the beginning for updating.
#'                   Defaults to 0 (start at the first row).
#' @param column_offset The number of columns to skip from the beginning for updating.
#'                      Defaults to 0 (start at the first column).
#' @param use_column_names Logical; if TRUE, column names in `data` are used to match column IDs in the sheet.
#'                         Defaults to TRUE.
#'
#' @examples
#' \dontrun{
#' ss_id = ss_sheetid(ss_write_sheet(paste0("smartsheetr-example-",random_sheet_name())))
#' # Assuming existing rows and columns
#' ss_add_rows(ss_id, data.frame("PK"="Value"))
#' ss_update_rows(ss_id, data.frame("PK"="Updated Value"))
#' ss_read_sheet(ss_id)
#' # clean up
#' ss_delete_sheet(ss_id)
#' }
#'
#' @return A `ss_updaterows_resp` object
#'
#' @export
ss_update_rows <- function(ss_id, data, row_ids = NULL, row_numbers = NULL, column_ids = NULL, row_offset = 0, column_offset = 0, use_column_names = TRUE) {
  ss_id = validate_ss_id(ss_id)
  if (!is.data.frame(data) || nrow(data) == 0) {
    stop("data must be a non-empty data frame.")
  }
  if (!is.null(row_numbers) && (!is.numeric(row_numbers) || any(row_numbers < 1))) {
    stop("row_numbers must be a vector of positive numbers.")
  }
  if (length(row_numbers) != nrow(data)) {
    stop("row_numbers must be the same length as the data.")
  }
  if (!is.null(row_offset) && (!is.numeric(row_offset) || row_offset < 0)) {
    stop("row_offset must be a non-negative number.")
  }
  if (!is.null(column_offset) && (!is.numeric(column_offset) || column_offset < 0)) {
    stop("column_offset must be a non-negative number.")
  }

  # Fetching Column IDs
  if(is.null(column_ids)) {
    col_resp = ss_get(path = paste0('sheets/', ss_id, '/columns'))
    # Match column names to column IDs
    if (use_column_names) {
      # Create dataframe of column IDs and titles
      sheet_columns = purrr::map(col_resp$content$data, ~data.frame(id=.x$id, title=.x$title)) |>
        purrr::list_rbind()
      # Check if all column names are matched and filter out unmatched columns
      data_column_names = names(data)
      unmatched_columns = setdiff(data_column_names, sheet_columns$title)
      if (length(unmatched_columns) > 0) {
        stop("No match found for column names: ", paste(unmatched_columns, collapse = ", "))
      }
      column_ids = sheet_columns |>
        dplyr::filter(title %in% data_column_names) |>
        dplyr::pull(id)
    } else {
      # Use all column IDs from the sheet
      column_ids = purrr::map(col_resp$content$data, ~.x$id) |> purrr::list_c()
    }
  }

  # Fetching Row IDs
  if(is.null(row_ids)) {
    # Fetching the number of rows in the data
    if (is.null(row_numbers)) {
      row_seq = 1:nrow(data)
      row_numbers = row_seq+row_offset
    }
    # Convert row numbers to comma separated string
    row_numbers = paste(row_numbers, collapse = ",")
    if (nchar(row_numbers) >= 5000) {
      warning("The number of rows to update is too large. Please use row_ids instead.")
      row_query = list()
    } else {
      row_query = list(rowNumbers=row_numbers)
    }
    row_resp = ss_get(path = paste0('sheets/', ss_id), query = row_query)
    row_ids = purrr::map(row_resp$content$rows, ~.x$id) |> purrr::list_c()
    # Check if enough row IDs are available
    if (length(row_ids) == 0) {
      stop("No row IDs available to update. Please add rows to the sheet.")
    }
    if (length(row_ids) < nrow(data)) {
      stop("Not enough row IDs available to update all rows.")
    }
  }

  if(length(row_ids) != nrow(data)) {
    stop("The number of row_ids must match the number of rows in the data frame.")
  }

  if(column_offset >= ncol(data)) {
    stop("The column_offset must be less than the number of columns in the data frame.")
  }

  # Format row data for API payload
  row_data = list()
  for(i in 1:nrow(data)) {
    cell_data = list()
    for(j in (1 + column_offset):ncol(data)) {
      cell_value = unlist(data[i, j])
      if (is.na(cell_value)) {
        # Convert NaNs to NA
        cell_value = NA
      }
      cell_data[[j - column_offset]] = list(columnId = unlist(column_ids[j]), value = cell_value)
    }
    row_data[[i]] = list(id = row_ids[i], cells = cell_data)
  }

  # Update rows with JSON payload
  path = paste0('sheets/', ss_id, '/rows')
  resp = ss_put(path = path, body = to_json(row_data))
  class(resp) = c("ss_updaterows_resp", class(resp))
  return(resp)
}
