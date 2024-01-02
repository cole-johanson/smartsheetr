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
  path = paste0('users?', url_query(list(includeAll="true")))
  resp = ss_get(path=path)
  ss_resp_data_to_dataframe(resp$content$data)
}
