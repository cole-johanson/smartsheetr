#' Smartsheet GET
#'
#' This is a simple wrapper around the GET function in the [Smartsheet API 2.0](https://smartsheet.redoc.ly/)
#'
#' @param path A character vector to add to the API url. See (https://smartsheet.redoc.ly/#section/Introduction)
#' for more information.
#' @param resp An httr::response object
#'
#' @details
#' Note that the environment variable SMARTSHEET_API_TOKEN should be defined in order to run this or any
#' other `smarsheetr` function.
#'
#' @return An httr::response object
#'
ss_get <- function(path) {
  url = httr::modify_url("https://api.smartsheet.com/", path = paste0('2.0/',path))
  token = Sys.getenv("SMARTSHEET_API_TOKEN")
  resp = httr::GET(
    url,
    httr::add_headers("Authorization" = paste("Bearer",token)),
    httr::user_agent('https://github.com/cole-johanson/smartsheetr')
  )

  if(httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  if (httr::status_code(resp) != 200) {
    parsed = ss_response_parse(resp)
    stop(
      sprintf(
        "GitHub API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        parsed$message,
        parsed$refId
      ),
      call. = FALSE
    )
  }

  return(resp)
}

#' @describeIn ss_get Parse the response of an ss_get() call.
ss_response_parse <- function(resp) {
  if(httr::http_type(resp) == "application/json") {
    parsed = jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
    structure(
      list(
        content = parsed,
        path = path,
        response = resp
      ),
      class = "smartsheets_api"
    )
  } else {
    stop('Unknown content type')
  }
}

ss_get_sheets <- function() {
  path = 'sheets'
  resp = ss_get(path)
  parsed = ss_response_parse(resp)
  # TODO: Handle pagination
  ss_resp_data_to_dataframe(parsed$content$data)
}

#' Require either a sheet name or a link,
#'
#' @param ss_id The id (or permalink) of the table to read
#'
#' @export
ss_read_sheet <- function(ss_id) {
  if(length(ss_id) != 1) {
    rlang::abort('ss_id must have length 1.')
  }
  if(!inherits(ss_id,'character')) {
    rlang::abort('ss_id must be a character vector.')
  }
  ss_id = as_ss_id(ss_id)
  path = paste0('sheets/',ss_id)
  resp = ss_get(path)
  parsed = ss_response_parse(resp)

  # parsed$content structure:
  # {
  #   "id", "name", "version", ...,
  #   "columns": {
  #     [[1]] { "id", "version", ..., "title", ...},
  #     ...
  #   }
  #   "rows": {
  #     [[[1]] {
  #       "id", "rowNumber", ...,
  #       "cells": {
  #         [[1]] { "columnId", "value", ...},
  #         [[2]] { "columnId", "value", ...}
  #       }
  #     },
  #     ...
  #   },
  #   ...
  # }

  # Record the column names
  colnames = ss_resp_data_to_dataframe(parsed$content$columns)$title

  # 1. Parse the row cells (one row in the data frame per cell in the table). The "value" column is a list
  #    of lists to prevent the column types from mixing (i.e. list(list("a"),list(1))) is valid but c("a",1)
  #    is not.
  # 2. The values are in column order, so group by the rowNumber and use the position of the cell to add the
  #    colname.
  # 3. Pivot the colnames up.
  # 4. Remove the rowNumber and unlist each cell (taking care to treat NULL as NA).
  x = tibble::tibble(row = parsed$content$rows) |>
    unnest_wider(row) |>
    unnest_longer(cells) |>
    unnest_wider(cells) |>
    group_by(rowNumber) |>
    mutate(
      colNumber = row_number(),
      colname = colnames[colNumber]
    ) |>
    ungroup() |>
    pivot_wider(
      id_cols = rowNumber,
      names_from = colname,
      values_from = value
    ) |>
    select(-rowNumber) |>
    mutate_all(unlist_and_replace_null)

  return(x)
}

unlist_and_replace_null <- function(l) {
  unlist(purrr::map(l, ~if(is.null(.x)) {NA} else {.x}))
}

#' Helper to rbind lists in a list into a data frame
ss_resp_data_to_dataframe <- function(resp_data) {
  purrr::map(resp_data, as.data.frame) |> purrr::list_rbind()
}

ss_resp_data_to_tibble <- function(resp_data) {
  resp_data |> map_df(flatten_df)
}

#' https://smartsheet.redoc.ly/tag/sheetsObjects
ss_write_sheet <- function(.data, ss_id = NULL) {
  # First, create a sheet: https://smartsheet.redoc.ly/tag/sheets#operation/create-sheet-in-sheets-folder
  # Next, Add columns: https://smartsheet.redoc.ly/tag/columns#operation/columns-addToSheet
  # Finally, add rows: https://smartsheet.redoc.ly/tag/rows#operation/rows-addToSheet
}

ss_column_type <- function(r_class) {
  if(inherits(r_class, c("Date"))) {
    return("DATE")
  } else if(inherits(r_class, c("POSIXct","POSIXt"))) {
    return("DATETIME")
  } else return("TEXT_NUMBER")
}

ss_write_columns <- function(.data) {
  columns = data.frame(title = colnames(.data))
  columns$index = (1:nrow(columns))-1
  columns$primary = columns$index==0
  columns$type = purrr::map(purrr::map(.data,class), ss_column_type)

  jsonlite::toJSON(columns)
}

ss_write_column <- function(column_name, column_data) {
  list(
    format =
  )
}
