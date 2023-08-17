small_df = data.frame(primary_key = c(1,2), other_field = c(3,4))
big_df = mtcars

test_that("Replacing a small sheet with a big one works", {
  small_df = data.frame(primary_key = c(1,2), other_field = c(3,4))
  big_df = mtcars

  resp = ss_write_sheet("small_to_big", small_df)
  ss_id = ss_sheetid(resp)
  ss_replace_sheet(ss_id, big_df)
  new_sheet_data = ss_read_sheet(ss_id)
  expect_equal(colnames(new_sheet_data), colnames(big_df))
  expect_equal(nrow(new_sheet_data), nrow(big_df))
  ss_delete_sheet(ss_id)
})

test_that("Replacing a big sheet with a small one works", {
  resp = ss_write_sheet("big_to_small", big_df)
  ss_id = ss_sheetid(resp)
  ss_replace_sheet(ss_id, small_df)
  new_sheet_data = ss_read_sheet(ss_id)
  expect_equal(colnames(new_sheet_data), colnames(small_df))
  expect_equal(nrow(new_sheet_data), nrow(small_df))
  ss_delete_sheet(ss_id)
})
