#' Get Data from a Smartsheet
#'
#' @description Gets data from a specified Smartsheet using the Smartsheet API and returns it as a tibble.
#' @param ss_id string, The ID of the Smartsheet from which data is to be fetched.
#' @return A `tibble` containing the data from the specified Smartsheet.
#' @examples
#' \dontrun{
#' df = mtcars
#' ss_id = ss_sheetid(ss_write_sheet(paste0("smartsheetr-example-",random_sheet_name()), data=df))
#' ss_get_data(ss_id)
#' # clean up
#' ss_delete_sheet(ss_id)
#' }
#' @export
ss_get_data <- function(ss_id) {
  # Validate the Smartsheet ID
  ss_id = validate_ss_id(ss_id)

  # Call ss_get function to fetch data from the sheet and extract the 'data' part of the content
  data = ss_get(path = paste0('sheets/', ss_id), simplifyVector = TRUE, flatten = TRUE)$content

  # Extract column IDs, types, and titles for mapping data
  column_ids <- as.character(data$columns$id)
  types <- purrr::set_names(data$columns$type, column_ids)
  titles <- purrr::set_names(data$columns$title, column_ids)
  options <- purrr::set_names(data$columns$options, column_ids)

  # Transform the data into a tidy format
  df <- purrr::map(data$rows$cells, ~ tibble::as_tibble(t(purrr::set_names(.x$value, .x$columnId)))) |>
    purrr::list_rbind() |>
    dplyr::mutate(dplyr::across(dplyr::all_of(names(types)), ~ convert_column_type(.x, types[dplyr::cur_column()], options[[dplyr::cur_column()]]))) |>
    dplyr::rename_with(~ titles[.x])

  # Return the transformed data frame
  return(df)
}

#' Convert Smartsheet Column Type to R Data Type
#'
#' @description Internal helper function to convert Smartsheet column types to R data types.
#' @param value The value to be converted.
#' @param type The type of Smartsheet column.
#' @return The converted value in the appropriate R data type.
#' @noRd
convert_column_type <- function(value, type, levels = NULL) {
  switch(type,
         "CHECKBOX" = as.logical(value),
         "CONTACT_LIST" = as.character(value),
         "MULTI_CONTACT_LIST" = as.character(value),
         "DATE" = as.Date(value),
         "ABSTRACT_DATETIME" = as.POSIXct(value),
         "DATETIME" = as.POSIXct(value),
         "PICKLIST" = factor(value, levels = levels),
         "MULTI_PICKLIST" = strsplit(value, ", "),
         "DURATION" = as.numeric(value),
         "PREDECESSOR" = as.character(value),
         "TEXT_NUMBER" = utils::type.convert(value, as.is = TRUE),
         rlang::abort("Unknown column type")
  )
}
