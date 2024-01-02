#' Get a data frame describing the smartsheets available
#'
#' @details
#' Note that the environment variable SMARTSHEET_API_TOKEN should be defined in order to run this or any
#' other `smarsheetr` functions.
#'
#' @examples
#' \dontrun{
#' ss_list_sheets()
#' }
#'
#' @return A dataframe
#'
#' @export
ss_list_sheets <- function() {
  path = paste0('sheets?', url_query(list(includeAll="true")))
  resp = ss_get(path=path)

  ss_resp_data_to_dataframe(resp$content$data)
}
