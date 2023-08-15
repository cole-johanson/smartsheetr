#' Validate or get the sheetID from a numeric/character vector
#'
#' This function validates a single ss_id is passed in and returns a smartsheets sheetId
#'
#' @param x A smartsheet sheet name, permalink, of sheetId
#'
#' @return A smartsheets sheetId
validate_ss_id <- function(ss_id) {
  if(length(ss_id) != 1) {
    rlang::abort('ss_id must have length 1.')
  }

  # If numeric, assume it's a sheetId
  if(inherits(ss_id, "numeric")) return(ss_id)

  # Some undocumented "magic"
  if(inherits(ss_id, c("ss_addcolumns_resp", "ss_addrows_resp"))) {
    return(ss_sheetid(ss_id))
  }

  if(!inherits(ss_id, "character")) rlang::abort('ss_id must be a numeric or character vector')

  ss_sheets = ss_get_sheets()
  if(grepl('^http',ss_id)) {
    sheetId = ss_sheets[which(ss_sheets$permalink == ss_id),]$id
    if(length(sheetId) == 0) {
      rlang::abort(paste('Permalink',ss_id,'not found'))
    }
  } else {
    sheetId = ss_sheets[which(ss_sheets$name == ss_id),]$id
    if(length(sheetId) == 0) {
      rlang::abort(paste('Sheet name',ss_id,'not found'))
    }
    if(length(sheetId) > 1) {
      rlang::abort(paste('Multiple sheets with name',ss_id,'found'))
    }
  }

  return(sheetId)
}
