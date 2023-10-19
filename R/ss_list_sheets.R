#' Get a data frame describing the smartsheets/reports available
#'
#' @details
#' Note that the environment variable SMARTSHEET_API_TOKEN should be defined in order to run this or any
#' other `smarsheetr` functions.
#' @param endpoint either the "sheets" or "reports" API endpoint
#'
#' @export
ss_list_sheets <- function(endpoint = c("sheets", "reports")) {
  path = match.arg(endpoint)
  resp = ss_get(path = path)
  # TODO: Handle pagination
  ss_resp_data_to_dataframe(resp$content$data)
}

#' @rdname ss_list_sheets
#' @export
ss_list_reports <- function() {
  ss_list_sheets(endpoint = "reports")
}
