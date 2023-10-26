#' List smartsheet users
#'
#' @examples
#' \dontrun{
#' ss_list_users()
#' }
#'
#' @return A dataframe
#'
#' @export
ss_list_users <- function() {
  resp = ss_get('users')
  ss_resp_data_to_dataframe(resp$content$data)
}
