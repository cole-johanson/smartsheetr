test_that("Getting the sheetId from various write objects works", {
  skip_if_offline()

  # ss_write_sheet returns a ss_writesheet_resp, and ss_sheetid(ss_writesheet_resp) returns a number
  ss_writesheet_resp = ss_write_sheet(paste0('smartsheetr-test-',random_sheet_name()))
  expect_s3_class(ss_writesheet_resp, "ss_writesheet_resp")
  writesheet_ss_id = ss_sheetid(ss_writesheet_resp)
  expect_true(inherits(writesheet_ss_id, "numeric"))

  # ss_add_columns returns a ss_addcolumns_resp, and ss_sheetid(ss_addcolumns_resp) returns a number
  ss_addcolumns_resp = ss_add_columns(writesheet_ss_id, data.frame("FK"=character()))
  expect_s3_class(ss_addcolumns_resp, "ss_addcolumns_resp")
  addcolumns_ss_id = ss_sheetid(ss_addcolumns_resp)
  expect_true(inherits(addcolumns_ss_id, "numeric"))

  # ss_add_rows returns a ss_addrows_resp, and ss_sheetid(ss_addrows_resp) returns a number
  ss_addrows_resp = ss_add_rows(writesheet_ss_id, data.frame("PK"="A", "FK"="B"))
  expect_s3_class(ss_addrows_resp, "ss_addrows_resp")
  addrows_ss_id = ss_sheetid(ss_addrows_resp)
  expect_true(inherits(addrows_ss_id, "numeric"))

  ss_delete_sheet(writesheet_ss_id)

  # ss_write_sheet_columns returns a ss_createsheet_resp, and ss_sheetid(ss_createsheet_resp) returns a number
  ss_createsheet_resp = ss_write_sheet_columns(paste0('smartsheetr-test-',random_sheet_name()))
  expect_s3_class(ss_createsheet_resp, "ss_createsheet_resp")
  createsheet_ss_id = ss_sheetid(ss_createsheet_resp)
  expect_true(inherits(createsheet_ss_id, "numeric"))
  ss_delete_sheet(createsheet_ss_id)
})
