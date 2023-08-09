#' Get a data frame describing the Smartsheets available
#'
#' @details
#' Note that the environment variable SMARTSHEET_API_TOKEN should be defined in order to run this or any
#' other `smarsheetr` function.
#'
#' @export
ss_get_sheets <- function() {
  path = 'sheets'
  resp = ss_get(path)
  parsed = ss_response_parse(resp)
  # TODO: Handle pagination
  ss_resp_data_to_dataframe(parsed$content$data)
}
