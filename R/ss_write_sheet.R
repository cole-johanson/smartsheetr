#' Create a sheet
#'
#' Creating a sheet requires either a template or a set of columns
#' (see https://smartsheet.redoc.ly/tag/sheets#operation/create-sheet-in-sheets-folder). This function only
#' allows for the columns option.
#'
#' @details
#' The [Smartsheet API 2.0](https://smartsheet.redoc.ly) uses two calls for creating a sheet with data.
#' The first is a call to create a sheet and populate the columns (analogous to
#' [smartsheetr::ss_write_sheet_columns]). The second is to add rows (analogous to
#' [smartsheetr::ss_add_rows]). [smartsheetr::ss_write_sheet] accomplishes both of these steps.
#'
#' @param sheet_name A character vector
#' @param data A data frame
#' @param use_rownames Logical; whether to use the rownames as the Primary Column
#'
#' @examples
#' \dontrun{
#' ss_id = ss_sheetid(ss_write_sheet(paste0("smartsheetr-example-",random_sheet_name()), data=mtcars))
#' ss_read_sheet(ss_id)
#' # clean up
#' ss_delete_sheet(ss_id)
#' }
#'
#' @return A smartsheetr response object
#'
#' @export
ss_write_sheet <- function(sheet_name, data = data.frame("PK" = character()), use_rownames=F) {
  if(length(sheet_name) != 1) {
    rlang::abort('sheet_name must have length 1.')
  }
  if(!inherits(sheet_name,'character')) {
    rlang::abort('sheet_name must be a character vector.')
  }
  if(!inherits(data,'data.frame')) {
    rlang::abort('data must be a data frame.')
  }
  if(use_rownames) {
    data[["PK"]] = rownames(data)
    data = data[, c("PK", setdiff(colnames(data),"PK"))]
  }
  col_resp = ss_write_sheet_columns(sheet_name, data)

  ss_id = ss_sheetid(col_resp)
  column_ids = ss_resp_data_to_dataframe(col_resp$content$result$columns)$id
  row_resp = {if(nrow(data) > 0) ss_add_rows(ss_id, data, column_ids = column_ids) else NULL}

  resp = structure(
    list(
      content = list(
        id = col_resp$content$result$id,
        name = col_resp$content$result$name,
        permalink = col_resp$content$result$permalink,
        columns = col_resp$content$result$columns,
        rows = row_resp$content$result
      ),
      responses = list(
        ss_createsheet_resp = col_resp,
        ss_addrows_resp = row_resp
      )
    ),
    class = 'ss_writesheet_resp'
  )

  return(resp)
}

#' Write the initial columns for the a sheet
#'
#' @details
#' The [Smartsheet API 2.0](https://smartsheet.redoc.ly) uses two calls for creating a sheet with data.
#' The first is a call to create a sheet and populate the columns (analogous to
#' [smartsheetr::ss_write_sheet_columns]). The second is to add rows (analogous to
#' [smartsheetr::ss_add_rows]). [smartsheetr::ss_write_sheet] accomplishes both of these steps.
#'
#' @param sheet_name A character vector
#' @param data A data frame of columns to be added
#'
#' @examples
#' \dontrun{
#' temp_sheet_name = paste0("smartsheetr-example-",random_sheet_name())
#' ss_id = ss_sheetid(ss_write_sheet_columns(temp_sheet_name, data=mtcars))
#' ss_read_sheet(ss_id) # No rows. Use ss_write_sheet() to write the full data frame
#' # clean up
#' ss_delete_sheet(ss_id)
#' }
#'
#' @return A `ss_createsheet_resp` object
#'
#' @export
ss_write_sheet_columns <- function(sheet_name, data=data.frame("PK" = character())) {
  cols_ = data.frame(title = colnames(data))
  cols_$primary = c(T,rep(F,ncol(data)-1)) # Assume first column is the primary
  cols_$type = purrr::map(purrr::map(data,class), ss_column_type)

  resp = ss_post(path='sheets', body = to_json(list(
    name = sheet_name,
    columns = cols_
  )))
  class(resp) = c("ss_createsheet_resp", class(resp))
  return(resp)
}
