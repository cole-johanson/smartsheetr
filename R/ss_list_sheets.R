#' Get a data frame describing the smartsheets available
#'
#' @details
#' Note that the environment variable SMARTSHEET_API_TOKEN should be defined in order to run this or any
#' other `smarsheetr` functions.
#'
#' @export
ss_list_sheets <- function() {
  resp = ss_get(path='sheets')
  # TODO: Handle pagination
  ss_resp_data_to_dataframe(resp$content$data)
}
