#' Get a data frame describing the Smartsheets available
#'
#' @details
#' Note that the environment variable SMARTSHEET_API_TOKEN should be defined in order to run this or any
#' other `smarsheetr` functions.
#'
#' @export
ss_get_sheets <- function() {
  resp = ss_get(path='sheets')
  parsed = ss_response_parse(resp)
  # TODO: Handle pagination
  ss_resp_data_to_dataframe(parsed$content$data)
}
