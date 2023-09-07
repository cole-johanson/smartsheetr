print <- function(x, ...) UseMethod("print")

print.default <- function(x, ...) base::print(x, ...)

print.ss_resp <- function(x, ...) {
  cat("Response [", x$response$url, "]\n", sep = "")
  cat("  Status: ", x$response$status_code, "\n", sep = "")

  print_content(x$response, ...)
  NextMethod("print")
}

print.ss_writesheet_resp <- function(x, ...) {
  col_resp = x$responses$ss_createsheet_resp$response
  row_resp = x$responses$ss_addrows_resp$response

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
