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
ss_sheetid.ss_createsheet_resp <- function(ss_createsheet_resp) {
  ss_createsheet_resp$content$result$id
}

#' @export
ss_sheetid.ss_addrows_resp <- function(ss_addrows_resp) {
  # Interesting that sheetId is returned for every row. Take the first
  ss_resp_data_to_dataframe(row_resp$content$result)$sheetId[1]
}

#' @export
ss_sheetid.ss_writesheet_resp <- function(ss_writesheet_resp) {
  ss_writesheet_resp$content$id
}
