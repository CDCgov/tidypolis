test_that("Testing the call_single_url() function", {
  # We need to ensure that the date columns are not inadvertently inferred
  # as a logical type. Note that by default jsonlite::fromJSON will infer data
  # types automatically when it gets parsed to a dataframe. So even when we
  # turn the dataframe into a tibble and then force all columns to be character,
  # it's still going to be incorrect if the parsing in fromJSON is also incorrect.

  # Test 1: Successfully returning response
  # Test 2: Successfully handling responses with a non-200 status
  # Test 3: Successfully assigning correct data types

  })
