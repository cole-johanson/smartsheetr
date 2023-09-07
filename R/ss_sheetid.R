#' Get a smartsheet sheetId from a response
#'
#' @param resp An ss_resp object
#'
#' @return A numeric sheetId
#'
#' @export
ss_sheetid <- function(resp) {
  UseMethod("ss_sheetid")
}

#' @export
ss_sheetid.ss_createsheet_resp <- function(resp) {
  resp$content$result$id
}

#' @export
ss_sheetid.ss_addrows_resp <- function(resp) {
  # Interesting that sheetId is returned for every row. Take the first
  ss_resp_data_to_dataframe(resp$content$result)$sheetId[1]
}

#' @export
ss_sheetid.ss_writesheet_resp <- function(resp) {
  resp$content$id
}
