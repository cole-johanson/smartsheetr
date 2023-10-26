test_that("deleting columns and rows works", {
  skip_if_offline()
  test_df = data.frame("PK" = c(1,2), "FK"=c("a","b"))
  ss_writesheet_resp = ss_write_sheet(paste0("smartsheetr-test-",random_sheet_name()), test_df)
  ss_id = ss_sheetid(ss_writesheet_resp)

  # drop the 2nd row
  row_to_drop = ss_row_ids(ss_id)[2]
  ss_delete_rows(ss_id, row_to_drop)
  expect_equal( ss_read_sheet(ss_id), tibble::tibble("PK" = c(1), "FK"=c("a")))

  # drop the 2nd column
  col_to_drop = ss_column_ids(ss_id)[2]
  ss_delete_columns(ss_id, col_to_drop)
  expect_equal( ss_read_sheet(ss_id), tibble::tibble("PK" = c(1)))
})
