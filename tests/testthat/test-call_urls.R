test_that("Testing the call_urls() function", {
  
  mockery::stub(
    call_urls,
    "call_single_url",
    function(url, ...) {
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
    }
  )
  
  withr::local_envvar(API_DEBUG = "FALSE")
  
  urls <- c("https://api.test.com/data?skip=0", "https://api.test.com/data?skip=100")
  result <- call_urls(urls)
  
  # Test 1: Successfully returning response
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  
  # Test 3: Successfully assigning correct data types
  expect_type(result$campaign_id, "character")
  expect_s3_class(result$campaign_date, "Date")
  expect_type(result$household_number, "double")
  expect_type(result$hard_to_reach, "logical")
  expect_true(is.na(result$campaign_date[2]))
  expect_true(is.na(result$campaign_date[3]))
  expect_true(is.na(result$household_number[3]))
  expect_true(is.na(result$hard_to_reach[4]))
})
