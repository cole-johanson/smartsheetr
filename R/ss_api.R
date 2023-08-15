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
          parsed$content$message,
          parsed$content$refId
        ),
        call. = FALSE
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

#' @export
print.ss_resp <- function(ss_resp_, max.lines = 10, width = getOption("width")) {
  cat("Response [", ss_resp_$response$url, "]\n", sep = "")
  cat("  Status: ", ss_resp_$response$status_code, "\n", sep = "")

  print_content(ss_resp_$response)
}

#' @export
print.ss_writesheet_resp <- function(ss_writesheet_resp_, max.lines = 10, width = getOption("width")) {
  col_resp = ss_writesheet_resp_$responses$ss_addcolumns_resp$response
  row_resp = ss_writesheet_resp_$responses$ss_addrows_resp$response

  cat("Response (columns): ", col_resp$url, "\n", sep = "")
  cat("Response (rows): ", row_resp$url, "\n", sep = "")
  cat("Status (columns): ", col_resp$status_code, "\n", sep = "")
  cat("Status (rows): ", row_resp$status_code, "\n", sep = "")
  cat("Content (columns): ")
  print(col_resp)
  cat("Content (rows): ")
  print(row_resp)
}

# Helper taken from https://github.com/r-lib/httr/blob/main/R/response.r
print_content <- function(x, max.lines = 10, width = getOption("width")) {
  if(!inherits(x, "response")) rlang::abort('x must be a response object')

  suppressMessages(text <- httr::content(x, "text"))

  breaks <- gregexpr("\n", text, fixed = TRUE)[[1]]
  last_line <- breaks[min(length(breaks), max.lines)]
  lines <- strsplit(substr(text, 1, last_line), "\n")[[1]]

  too_wide <- nchar(lines) > width
  lines[too_wide] <- paste0(substr(lines[too_wide], 1, width - 3), "...")

  cat(lines, sep = "\n")
  if (max.lines < length(breaks)) cat("...\n")

  invisible(x)
}

#' Helper to rbind lists in a list into a data frame
ss_resp_data_to_dataframe <- function(resp_data) {
  purrr::map(resp_data, as.data.frame) |> purrr::list_rbind()
}
