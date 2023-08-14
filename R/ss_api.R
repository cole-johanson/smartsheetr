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
  args[['url']] = httr::modify_url("https://api.smartsheet.com/", path = paste0('2.0/',args[['path']]))
  args[['path']] = NULL

  resp = do.call(FUN, args)

  if(httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  if (httr::status_code(resp) != 200) {
    parsed = ss_response_parse(resp, args[['url']])
    stop(
      sprintf(
        "SmartSheet API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        parsed$content$message,
        parsed$content$refId
      ),
      call. = FALSE
    )
  }

  return(resp)
}

#' @describeIn ss_get Parse the response of an ss_get() call.
#' @export
ss_response_parse <- function(resp, url = NULL) {
  if(httr::http_type(resp) == "application/json") {
    parsed = jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
    structure(
      list(
        content = parsed,
        url = url,
        response = resp
      ),
      class = "smartsheets_api"
    )
  } else {
    stop('Unknown content type')
  }
}

#' Helper to rbind lists in a list into a data frame
ss_resp_data_to_dataframe <- function(resp_data) {
  purrr::map(resp_data, as.data.frame) |> purrr::list_rbind()
}
