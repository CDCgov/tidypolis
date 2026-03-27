# call_single_url() is deprecated

    Code
      result <- call_single_url("https://test.example.com/data")
    Condition
      Warning:
      `call_single_url()` was deprecated in tidypolis 2.1.2.
      i Please use `call_urls_in_parallel()` instead.
    Code
      expect_equal(nrow(result), 3)

