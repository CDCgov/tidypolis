# call_urls() emits a deprecation warning

    Code
      testthat::expect_warning(call_urls(urls), class = "lifecycle_warning_deprecated")

