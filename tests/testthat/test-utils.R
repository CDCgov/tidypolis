test_that("Testing check_missing_static_files()", {
  skip()
  test_folder_path <-
    "Software_Development/Test_Data/check_static_file_test_folder"

  # All static files are present
  expect_length(check_missing_static_files(
    file.path(
      test_folder_path,
      "core_files_present"
    ),
    TRUE
  ), 0)
  # All static files are absent
  expect_length(check_missing_static_files(
    file.path(
      test_folder_path,
      "core_files_absent"
    ),
    TRUE
  ), 6)

  # One file is missing
  expect_length(check_missing_static_files(
    file.path(
      test_folder_path,
      "one_file_absent"
    ),
    TRUE
  ), 1)
})
