test_that("Listing users works", {
  skip_if_offline()
  expect_s3_class(ss_list_users(),"data.frame")
})

test_that("Sharing sheets works", {
  skip_if_offline()
  # Write a sheet
  ss_resp = ss_write_sheet(paste0("smartsheetr-test-",random_sheet_name()))
  ss_id = ss_sheetid(ss_resp)
  # Get a user
  users = ss_list_users()
  user = users[users$id == '6396039696541572','email']
  # Share with user
  ss_sheet_share(ss_id, user)
  # Confirm the user is on the list of users
  expect_true(user %in% ss_list_sheet_shares(ss_id)$email)
  # Delete the sheet
  ss_delete_sheet(ss_id)
})
