#' @export
print.ss_resp <- function(ss_resp_, max.lines = 10, width = getOption("width")) {
  cat("Response [", ss_resp_$response$url, "]\n", sep = "")
  cat("  Status: ", ss_resp_$response$status_code, "\n", sep = "")

  print_content(ss_resp_$response)
}

#' @export
print.ss_writesheet_resp <- function(ss_writesheet_resp_, max.lines = 10, width = getOption("width")) {
  col_resp = ss_writesheet_resp_$responses$ss_createsheet_resp$response
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
