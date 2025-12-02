test_that("Testing the call_single_url() function", {

  local_mocked_bindings(
    RETRY = function(verb, url, config, times, quiet, terminate_on_success) {
      mock_json <- '{"value": [
        {"epid": "PAK-2024-001", "name": "Philip Santos", "date_onset": "NULL", "age_months": "36", "vaccinated": "TRUE"},
        {"epid": "PAK-2024-002", "name": "Phillip Khan", "date_onset": "", "age_months": "24", "vaccinated": "FALSE"},
        {"epid": "PAK-2024-003", "name": "Felipe Torres", "date_onset": "2024-01-15", "age_months": "NULL", "vaccinated": "TRUE"}
      ]}'
      list(content = charToRaw(mock_json))
    },
    .package = "httr"
  )

  result <- call_single_url("https://test.example.com/data")

  # Test 1: Successfully returning response
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)

  # Test 3: Successfully assigning correct data types
  expect_type(result$epid, "character")
  expect_type(result$name, "character")
  expect_s3_class(result$date_onset, "Date")
  expect_type(result$age_months, "double")
  expect_type(result$vaccinated, "logical")
  expect_true(is.na(result$date_onset[1]))
  expect_true(is.na(result$date_onset[2]))
  expect_true(is.na(result$age_months[3]))

  # Test 2: Successfully handling responses with a non-200 status
  local_mocked_bindings(
    RETRY = function(verb, url, config, times, quiet, terminate_on_success) {
      stop("Connection failed")
    },
    .package = "httr"
  )

  expect_error(call_single_url("https://test.example.com/error"))


  # Test 5: Sparse numeric column should not be inferred as logical
  local_mocked_bindings(
    RETRY = function(verb, url, config, times, quiet, terminate_on_success) {
      # Simulate 3000 rows with mostly NULLs in age_months
      mock_data <- lapply(1:3000, function(i) {
        list(
          epid = paste0("PAK-2024-", sprintf("%03d", i)),
          name = paste("Person", i),
          date_onset = if (i %% 100 == 0) "2024-01-15" else "NULL",
          age_months = if (i %% 100 == 0) as.character(i %% 60) else "NULL",
          vaccinated = if (i %% 2 == 0) "TRUE" else "FALSE"
        )
      })
      mock_json <- jsonlite::toJSON(list(value = mock_data), auto_unbox = TRUE)
      list(content = charToRaw(mock_json))
    },
    .package = "httr"
  )

  result_sparse <- call_single_url("https://test.example.com/sparse")

  # Ensure correct types
  expect_type(result_sparse$epid, "character")
  expect_type(result_sparse$name, "character")
  expect_s3_class(result_sparse$date_onset, "Date")
  expect_type(result_sparse$age_months, "double")  # This is the key check
  expect_type(result_sparse$vaccinated, "logical")

  # Ensure no unintended logical columns
  non_logical_cols <- sapply(result_sparse, function(col) !is.logical(col))
  expect_true(all(non_logical_cols[names(non_logical_cols) != "vaccinated"]))


  # Test 6: All NULLs in first 3000 rows should not infer numeric column as logical
  local_mocked_bindings(
    RETRY = function(verb, url, config, times, quiet, terminate_on_success) {
      # First 3000 rows: age_months = "NULL"
      # Row 3001: age_months = "48"
      mock_data <- lapply(1:3001, function(i) {
        list(
          epid = paste0("PAK-2024-", sprintf("%04d", i)),
          name = paste("Person", i),
          date_onset = "2024-01-01",
          age_months = if (i <= 3000) "NULL" else "48",
          vaccinated = if (i %% 2 == 0) "TRUE" else "FALSE"
        )
      })
      mock_json <- jsonlite::toJSON(list(value = mock_data), auto_unbox = TRUE)
      list(content = charToRaw(mock_json))
    },
    .package = "httr"
  )

  result_nulls <- call_single_url("https://test.example.com/nulls")

  # Check that age_months is still treated as numeric (double), not logical
  expect_type(result_nulls$age_months, "double")
  expect_true(is.na(result_nulls$age_months[1]))
  expect_equal(result_nulls$age_months[3001], 48)

})
