#' Execute curl commands for the Smartsheet API
#'
#' `ss_get()` wraps the `httr::GET()` function
#' `ss_post()` wraps the `httr::POST()` function
#' `ss_put()` wraps the `httr::PUT()` function
#' `ss_delete()` wraps the `httr::DELETE()` function
#'
#' @rdname ss_get
#'
#' @param path A character vector to add to the API url. See (https://smartsheet.redoc.ly/#section/Introduction)
#' for more information.
#' @param body A list of objects
#' @param ... Further arguments passed to \code{\link{ss_api}}
#'
#' @details
#' Note that the environment variable SMARTSHEET_API_TOKEN should be defined in order to run this or any
#' other `smarsheetr` functions.
#'
#' @return An httr::response object
ss_get <- function(path, ...) {
  ss_api(httr::GET, path=path, ...)
}

#' @rdname ss_get
ss_post <- function(path, body, ...) {
  ss_api(httr::POST, path=path, body=body, encode='json', ...)
}

#' @rdname ss_get
ss_delete <- function(path, ...) {
  ss_api(httr::DELETE, path=path, encode='json', ...)
}

#' @rdname ss_get
ss_put <- function(path, ...) {
  ss_api(httr::PUT, path=path, encode='json', ...)
}

#' The workhorse function that performs each call to the Smartsheet API
#'
#' @param FUN An http verb function, typically from the `httr` package
#' @param ... Further parameters passed to the http verb function
#'
ss_api <- function(FUN, ...) {
  args = list(...)
  token = Sys.getenv("SMARTSHEET_API_TOKEN")
  if(nchar(token) == 0) {
    rlang::abort("Environment variable `SMARTSHEET_API_TOKEN` must be set. See https://github.com/cole-johanson/smartsheetr#installation")
  }

  # Check if args are supplied for JSON parsing and assign defaults if not
  simplifyVector = ifelse('simplifyVector' %in% names(args), args[['simplifyVector']], FALSE)
  flatten = ifelse('flatten' %in% names(args), args[['flatten']], FALSE)
  # Remove these arguments from the list before passing to the http verb function
  args = args[!names(args) %in% c('simplifyVector', 'flatten')]

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
    parsed = jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = simplifyVector, flatten = flatten)

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
#'
#' @param resp_data A list of lists
#'
ss_resp_data_to_dataframe <- function(resp_data) {
  purrr::map(resp_data, as.data.frame) |> purrr::list_rbind()
}
