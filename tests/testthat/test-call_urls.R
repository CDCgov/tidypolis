# tests/testthat/test-call_urls.R

testthat::test_that("call_urls() returns combined data as a tibble", {
  withr::local_options(lifecycle_verbosity = "quiet")
  withr::local_envvar(API_DEBUG = "FALSE")

  # Prevent call_urls() from registering the doFuture backend (parallel)
  testthat::local_mocked_bindings(
    registerDoFuture = function(...) invisible(NULL),
    .package = "doFuture"
  )

  # Ensure foreach runs sequentially in the current process
  foreach::registerDoSEQ()

  # Optional: avoid progress handlers complexity in tests
  testthat::local_mocked_bindings(
    with_progress = function(expr) force(expr),
    handlers = function(...) invisible(NULL),
    progressor = function(...) function(...) invisible(NULL),
    .package = "progressr"
  )

  # Mock call_single_url where call_urls() looks it up (its function environment)
  testthat::local_mocked_bindings(
    call_single_url = function(url, ...) {
      if (grepl("skip=0", url)) {
        dplyr::tibble(
          campaign_id = c("SIA-2024-001", "SIA-2024-002"),
          supervisor = c("Philip Okafor", "Phillipa Chen"),
          campaign_date = c("2024-01-15", "NULL"),
          household_number = c("45", "67"),
          hard_to_reach = c("TRUE", "FALSE")
        )
      } else {
        dplyr::tibble(
          campaign_id = c("SIA-2024-003", "SIA-2024-004"),
          supervisor = c("Felipe Akbar", "Phil Desai"),
          campaign_date = c("", "2024-02-20"),
          household_number = c("NA", "102"),
          hard_to_reach = c("TRUE", "NULL")
        )
      }
    },
    .env = environment(call_urls)
  )

  urls <- c(
    "https://api.test.com/data?skip=0",
    "https://api.test.com/data?skip=100"
  )

  result <- call_urls(urls)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 4)
  testthat::expect_named(
    result,
    c(
      "campaign_id",
      "supervisor",
      "campaign_date",
      "household_number",
      "hard_to_reach"
    )
  )
})

testthat::test_that("call_urls() emits a deprecation warning", {
  withr::local_envvar(API_DEBUG = "FALSE")

  testthat::local_mocked_bindings(
    registerDoFuture = function(...) invisible(NULL),
    .package = "doFuture"
  )
  foreach::registerDoSEQ()

  testthat::local_mocked_bindings(
    with_progress = function(expr) force(expr),
    handlers = function(...) invisible(NULL),
    progressor = function(...) function(...) invisible(NULL),
    .package = "progressr"
  )

  testthat::local_mocked_bindings(
    call_single_url = function(url, ...) {
      dplyr::tibble(
        campaign_id = "SIA-2024-001",
        supervisor = "Someone",
        campaign_date = "2024-01-15",
        household_number = "45",
        hard_to_reach = "TRUE"
      )
    },
    .env = environment(call_urls)
  )

  urls <- c("https://api.test.com/data?skip=0")

  testthat::expect_snapshot(
    {
      testthat::expect_warning(
        call_urls(urls),
        class = "lifecycle_warning_deprecated"
      )
    }
  )

})

testthat::test_that("call_urls() cleans NULL/NA/empty strings and converts column types", {
  withr::local_options(lifecycle_verbosity = "quiet")
  withr::local_envvar(API_DEBUG = "FALSE")

  testthat::local_mocked_bindings(
    registerDoFuture = function(...) invisible(NULL),
    .package = "doFuture"
  )
  foreach::registerDoSEQ()

  testthat::local_mocked_bindings(
    with_progress = function(expr) force(expr),
    handlers = function(...) invisible(NULL),
    progressor = function(...) function(...) invisible(NULL),
    .package = "progressr"
  )

  testthat::local_mocked_bindings(
    call_single_url = function(url, ...) {
      if (grepl("skip=0", url)) {
        dplyr::tibble(
          campaign_id = c("SIA-2024-001", "SIA-2024-002"),
          supervisor = c("Philip Okafor", "Phillipa Chen"),
          campaign_date = c("2024-01-15", "NULL"),
          household_number = c("45", "67"),
          hard_to_reach = c("TRUE", "FALSE")
        )
      } else {
        dplyr::tibble(
          campaign_id = c("SIA-2024-003", "SIA-2024-004"),
          supervisor = c("Felipe Akbar", "Phil Desai"),
          campaign_date = c("", "2024-02-20"),
          household_number = c("NA", "102"),
          hard_to_reach = c("TRUE", "NULL")
        )
      }
    },
    .env = environment(call_urls)
  )

  urls <- c(
    "https://api.test.com/data?skip=0",
    "https://api.test.com/data?skip=100"
  )

  result <- call_urls(urls)

  # These should hold because call_urls() does:
  # - converts "NULL"/"NA"/"" -> NA_character_
  # - then readr::type_convert()
  testthat::expect_type(result$campaign_id, "character")
  testthat::expect_type(result$supervisor, "character")
  testthat::expect_s3_class(result$campaign_date, "Date")
  testthat::expect_type(result$household_number, "double")
  testthat::expect_type(result$hard_to_reach, "logical")

  testthat::expect_true(is.na(result$campaign_date[2])) # "NULL" -> NA -> Date NA
  testthat::expect_true(is.na(result$campaign_date[3])) # "" -> NA -> Date NA
  testthat::expect_true(is.na(result$household_number[3])) # "NA" -> NA -> double NA
  testthat::expect_true(is.na(result$hard_to_reach[4])) # "NULL" -> NA -> logical NA
})