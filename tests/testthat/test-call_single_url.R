# tests/testthat/test-call_single_url.R

testthat::test_that("call_single_url() is deprecated", {
  testthat::local_mocked_bindings(
    RETRY = function(verb, url, config, times, quiet, terminate_on_success) {
      # Use proper JSON types:
      # - null (not "NULL")
      # - numbers (not "36")
      # - booleans (not "TRUE")
      # Note: date_onset stays character unless call_single_url() explicitly parses Date.
      mock_json <- '{
        "value": [
          {"epid":"PAK-2024-001","name":"Philip Santos","date_onset":null,"age_months":36,"vaccinated":true},
          {"epid":"PAK-2024-002","name":"Phillip Khan","date_onset":"","age_months":24,"vaccinated":false},
          {"epid":"PAK-2024-003","name":"Felipe Torres","date_onset":"2024-01-15","age_months":null,"vaccinated":true}
        ]
      }'

      structure(
        list(
          status_code = 200L,
          content = charToRaw(mock_json)
        ),
        class = "response"
      )
    },
    .package = "httr"
  )
  
  expect_snapshot({
    result <- call_single_url("https://test.example.com/data")
    expect_equal(nrow(result), 3)
  })
})

testthat::test_that("call_single_url() returns a tibble for a 200 response", {
  withr::local_options(lifecycle_verbosity = "quiet")

  testthat::local_mocked_bindings(
    RETRY = function(verb, url, config, times, quiet, terminate_on_success) {
      # Use proper JSON types:
      # - null (not "NULL")
      # - numbers (not "36")
      # - booleans (not "TRUE")
      # Note: date_onset stays character unless call_single_url() explicitly parses Date.
      mock_json <- '{
        "value": [
          {"epid":"PAK-2024-001","name":"Philip Santos","date_onset":null,"age_months":36,"vaccinated":true},
          {"epid":"PAK-2024-002","name":"Phillip Khan","date_onset":"","age_months":24,"vaccinated":false},
          {"epid":"PAK-2024-003","name":"Felipe Torres","date_onset":"2024-01-15","age_months":null,"vaccinated":true}
        ]
      }'

      structure(
        list(
          status_code = 200L,
          content = charToRaw(mock_json)
        ),
        class = "response"
      )
    },
    .package = "httr"
  )

  result <- call_single_url("https://test.example.com/data")

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 3)

  # Types based on current call_single_url() behavior:
  testthat::expect_type(result$epid, "character")
  testthat::expect_type(result$name, "character")

  # date_onset will be character because call_single_url() does not parse Date
  testthat::expect_type(result$date_onset, "character")

  # These will be properly typed if JSON uses numbers/booleans
  testthat::expect_type(result$age_months, "integer")
  testthat::expect_type(result$vaccinated, "logical")

  # Missingness checks
  testthat::expect_true(is.na(result$date_onset[1])) # null -> NA
  testthat::expect_identical(result$date_onset[2], "") # empty string stays ""
  testthat::expect_true(is.na(result$age_months[3])) # null -> NA
})

testthat::test_that("call_single_url() returns NA for non-200 status codes", {
  withr::local_options(lifecycle_verbosity = "quiet")

  testthat::local_mocked_bindings(
    RETRY = function(verb, url, config, times, quiet, terminate_on_success) {
      structure(
        list(
          status_code = 500L,
          content = charToRaw('{"value":[]}')
        ),
        class = "response"
      )
    },
    .package = "httr"
  )

  result <- call_single_url("https://test.example.com/non200")
  testthat::expect_true(is.na(result))
})

testthat::test_that("call_single_url() propagates connection errors thrown by httr::RETRY()", {
  withr::local_options(lifecycle_verbosity = "quiet")

  testthat::local_mocked_bindings(
    RETRY = function(verb, url, config, times, quiet, terminate_on_success) {
      stop("Connection failed")
    },
    .package = "httr"
  )

  testthat::expect_error(
    call_single_url("https://test.example.com/error"),
    "Connection failed"
  )
})

testthat::test_that("Sparse numeric fields remain numeric when JSON uses real nulls and numbers", {
  withr::local_options(lifecycle_verbosity = "quiet")

  testthat::local_mocked_bindings(
    RETRY = function(verb, url, config, times, quiet, terminate_on_success) {
      # Mostly nulls in age_months; some numeric values.
      mock_data <- lapply(1:3000, function(i) {
        list(
          epid = paste0("PAK-2024-", sprintf("%03d", i)),
          name = paste("Person", i),
          date_onset = if (i %% 100 == 0) "2024-01-15" else NULL,
          age_months = if (i %% 100 == 0) (i %% 60) else NULL,
          vaccinated = (i %% 2 == 0)
        )
      })

      mock_json <- jsonlite::toJSON(
        list(value = mock_data),
        auto_unbox = TRUE,
        null = "null"
      )

      structure(
        list(
          status_code = 200L,
          content = charToRaw(mock_json)
        ),
        class = "response"
      )
    },
    .package = "httr"
  )

  result_sparse <- call_single_url("https://test.example.com/sparse")

  testthat::expect_s3_class(result_sparse, "tbl_df")
  testthat::expect_equal(nrow(result_sparse), 3000)

  testthat::expect_type(result_sparse$epid, "character")
  testthat::expect_type(result_sparse$name, "character")
  testthat::expect_type(result_sparse$date_onset, "character") # not parsed to Date by function
  testthat::expect_type(result_sparse$age_months, "integer")
  testthat::expect_type(result_sparse$vaccinated, "logical")

  # Ensure only vaccinated is logical
  logical_cols <- vapply(result_sparse, is.logical, logical(1))
  testthat::expect_identical(names(result_sparse)[logical_cols], "vaccinated")
})

testthat::test_that("A numeric column with first 3000 nulls is still numeric if later values are numbers", {
  withr::local_options(lifecycle_verbosity = "quiet")

  testthat::local_mocked_bindings(
    RETRY = function(verb, url, config, times, quiet, terminate_on_success) {
      mock_data <- lapply(1:3001, function(i) {
        list(
          epid = paste0("PAK-2024-", sprintf("%04d", i)),
          name = paste("Person", i),
          date_onset = "2024-01-01",
          age_months = if (i <= 3000) NULL else 48,
          vaccinated = (i %% 2 == 0)
        )
      })

      mock_json <- jsonlite::toJSON(
        list(value = mock_data),
        auto_unbox = TRUE,
        null = "null"
      )

      structure(
        list(
          status_code = 200L,
          content = charToRaw(mock_json)
        ),
        class = "response"
      )
    },
    .package = "httr"
  )

  result_nulls <- call_single_url("https://test.example.com/nulls")

  testthat::expect_equal(nrow(result_nulls), 3001)
  testthat::expect_type(result_nulls$age_months, "integer")
  testthat::expect_true(is.na(result_nulls$age_months[1]))
  testthat::expect_identical(result_nulls$age_months[3001], as.integer(48))
})