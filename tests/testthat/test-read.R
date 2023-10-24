.first_unique_sheet <- function() {
  all_sheets = ss_list_sheets()
  all_sheets |> dplyr::group_by(name) |> dplyr::filter(dplyr::n() == 1) |> dplyr::ungroup()
  if(nrow(all_sheets) == 0) rlang::abort(
    "Testing requires at least one unique report on Smartsheet. \
    You can add one with smartsheetr::ss_write_sheet(smartsheetr::random_sheet_name(), mtcars)"
  )
  first_sheet = all_sheets[1,]
}

first_unique_sheet <- memoise::memoise(.first_unique_sheet)

test_that("Reading via sheet name", {
  skip_if_offline()
  first_sheet = first_unique_sheet()
  first_sheet_name = first_sheet$name
  expect_s3_class(ss_read_sheet(first_sheet_name),"data.frame")
})

test_that("Reading via sheet id", {
  first_sheet = first_unique_sheet()
  first_sheet_id = first_sheet$id
  expect_s3_class(ss_read_sheet(first_sheet_id),"data.frame")
})

test_that("Reading via sheet permalink", {
  first_sheet = first_unique_sheet()
  first_sheet_id = first_sheet$permalink
  expect_s3_class(ss_read_sheet(first_sheet_id),"data.frame")
})

