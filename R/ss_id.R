#' Extract and detect a Smartsheet sheetId
#'
#' These functions determine what are SheetIds and extracts sheetIds from URLs.
#'
#' @param ss_id A character vector of sheetId(s) or URL(s)
#'
#' @export
is_ss_id <- function(ss_id) {
  grepl('^[A-Za-z0-9]{40}$', ss_id)
}

#' @rdname is_ss_id
extract_ss_id <- function(ss_id) {
  split_id = strsplit(unlist(ss_id),'[/\\?]')

  ss_id_find = purrr::map(split_id, ~which(is_ss_id(.x)))
  if(any(purrr::map(ss_id_find, length) != 1)) {
    rlang::abort('ss_id must be valid.')
  }
  return(unlist(purrr::map2(split_id,ss_id_find,~.x[[.y]])))
}
