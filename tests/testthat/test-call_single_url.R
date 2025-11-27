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
})
