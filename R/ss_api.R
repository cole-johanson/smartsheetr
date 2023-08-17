#' Smartsheet GET
#'
#' This is a simple wrapper around the GET function in the [Smartsheet API 2.0](https://smartsheet.redoc.ly/)
#'
#' @param path A character vector to add to the API url. See (https://smartsheet.redoc.ly/#section/Introduction)
#' for more information.
#'
#' @details
#' Note that the environment variable SMARTSHEET_API_TOKEN should be defined in order to run this or any
#' other `smarsheetr` functions.
#'
#' @return An httr::response object
#'
ss_get <- function(path, ...) {
  ss_api(httr::GET, path=path, ...)
}

#' @describeIn ss_get SmartSheet POST
#'
#' @param body A list of objects
#'
#' @export
ss_post <- function(path, body, ...) {
  ss_api(httr::POST, path=path, body=body, encode='json', ...)
}

#' @describeIn ss_get SmartSheet POST
#'
#' @param body A list of objects
#'
#' @export
ss_delete <- function(path, ...) {
  ss_api(httr::DELETE, path=path, encode='json', ...)
}

#' @describeIn ss_get SmartSheet POST
#'
#' @param body A list of objects
#'
#' @export
ss_put <- function(path, ...) {
  ss_api(httr::PUT, path=path, encode='json', ...)
}

ss_api <- function(FUN, ...) {
  args = list(...)
  token = Sys.getenv("SMARTSHEET_API_TOKEN")

  args = append(
    args,
    list(
      httr::add_headers("Authorization" = paste("Bearer",token)),
      httr::user_agent('https://github.com/cole-johanson/smartsheetr')
    )
  )

  if(!'path' %in% names(args)) {
    rlang::abort('Calls to the smartsheetr API must contain a path argument')
  }
  path = args[['path']]
  args[['url']] = httr::modify_url("https://api.smartsheet.com/", path = paste0('2.0/', path))
  args[['path']] = NULL

  resp = do.call(FUN, args)

  if(httr::http_type(resp) == "application/json") {
    parsed = jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

    if (httr::status_code(resp) != 200) {
      rlang::abort(
        sprintf(
          "SmartSheet API request failed [%s]\n%s\n<%s>",
          httr::status_code(resp),
          parsed$message,
          parsed$refId
        )
      )
    }

    ss_resp = structure(
      list(
        content = parsed,
        path = path,
        response = resp
      ),
      class = c("ss_resp",class(resp))
    )
  } else {
    rlang::abort('Unknown content type')
  }

  return(ss_resp)
}

#' Helper to rbind lists in a list into a data frame
ss_resp_data_to_dataframe <- function(resp_data) {
  purrr::map(resp_data, as.data.frame) |> purrr::list_rbind()
}
