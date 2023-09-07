#' Share a sheet with a user
#'
#' @param ss_id The sheetId (or permalink) of the table
#' @param email The email address of the user to share to, i.e. a value in ss_get_users()$email
#' @param access_level A character object. See \url{https://smartsheet.redoc.ly/#section/Security/Access-Levels}
#'
#' @return An ss_resp object
#'
#' @export
ss_sheet_share <- function(ss_id, email, access_level = c("VIEWER","EDITOR","COMMENTER","EDITOR_SHARE","OWNER","ADMIN")) {
  access_level = rlang::arg_match(access_level)
  ss_id = validate_ss_id(ss_id)

  resp = ss_post(
    path = paste0('sheets/',ss_id,'/shares'),
    body = to_json(list(email = email, accessLevel = access_level))
  )
}
