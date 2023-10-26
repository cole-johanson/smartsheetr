#' Share a sheet with a user
#'
#' @param ss_id The sheetId (or permalink) of the table
#' @param email The email address of the user to share to, i.e. a value in ss_list_users()$email
#' @param access_level A character object. See \url{https://smartsheet.redoc.ly/#section/Security/Access-Levels}
#'
#' @examples
#' ss_id = ss_sheetid(ss_write_sheet(paste0("smartsheetr-example-",random_sheet_name())))
#' users = ss_list_users()
#' user = users[1,'email']
#' ss_sheet_share(ss_id, user)
#' # clean up
#' ss_delete_sheet(ss_id)
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
