#' List smartsheet users
#'
#' @return A dataframe
#'
#' @export
ss_get_users <- function() {
  resp = ss_get('users')
  ss_resp_data_to_dataframe(resp$content$data)
}
