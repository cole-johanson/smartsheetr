#' Rename columns
#'
#' Rename a set of columns. One of the following must be true:
#' \itemize{
#' \item{column_names is not NULL}
#' \item{column_locs is not NULL, or}
#' \item{new_names is the same length as the number of columns of the ss_id sheet}
#' }
#'
#' @param ss_id The sheetId, permalink, or name of the Smartsheet sheet to read
#' @param new_names A character vector of new names for the chosen columns
#' @param column_names A vector of names of columns within the sheet to be replaced
#' @param column_locs A vector of locations of columns within the sheet to be replaced
#'
#' @examples
#' \dontrun{
#' df = data.frame("PK"=character(), "temp"=character())
#' ss_id = ss_sheetid(ss_write_sheet(paste0("smartsheetr-example-",random_sheet_name()), data=df))
#' ss_rename_columns(ss_id, new_names="FK", column_names="temp")
#' ss_read_sheet(ss_id)
#' # clean up
#' ss_delete_sheet(ss_id)
#' }
#'
#' @return A list of ss_resp objects
#'
#' @export
ss_rename_columns <- function(ss_id, new_names, column_names = NULL, column_locs = NULL) {
  ss_id = validate_ss_id(ss_id)

  resp_sheet = ss_get(path = paste0('sheets/',ss_id))
  columns_data = ss_resp_data_to_dataframe(resp_sheet$content$columns)

  if(!is.null(column_names) & !is.null(column_locs)) {
    rlang::abort("column_names or column_locs must not both be present")
  }

  column_ids = NULL
  if(!is.null(column_names)) {
    column_names_missing = column_names[!column_names %in% columns_data$title]
    if(length(column_names_missing) > 0) {
      rlang::abort(paste0(
        "column_names must be present in data. The following column_names are missing: ",
        paste0(column_names_missing, collapse = ', '),
        ". As a reminder, column_names are case-sensitive."
      ))
    }
    column_ids = columns_data[columns_data$title %in% column_names,]$id
  } else if(!is.null(column_locs)) {
    column_locs_missing = column_locs[column_locs > nrow(columns_data)]
    if(length(column_locs_missing) > 0) {
      rlang::abort(paste0(
        "column_locs must be present in data. The following column_locs are missing: ",
        paste0(column_locs_missing, collapse = ', '),
        ". As a reminder, column_locs are indexed at 1, not 0."
      ))
    }
    column_ids = columns_data[column_locs,]$id
  } else {
    if(length(new_names) != nrow(columns_data)) {
      rlang::abort(paste0(
        "column_names or column_locs must be non-NULL, or new_names is the same length as the number of ",
        "columns of the ss_id sheet (",nrow(columns_data),")."
      ))
    }
    column_ids = columns_data$id
  }

  purrr::map2(column_ids, new_names, ~ss_put(path = paste0('sheets/',ss_id,'/columns/',.x), body = to_json(list(title = .y))))
}
