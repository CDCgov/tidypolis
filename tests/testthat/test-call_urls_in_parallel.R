# tests/testthat/test-call_urls_in_parallel.R

testthat::test_that("call_urls_in_parallel() returns a tibble for a single page", {
  urls <- c("https://api.test.com/data?page=1")

  # Mock the helper so no real httr2 requests happen
  testthat::local_mocked_bindings(
    call_urls_in_parallel_helper = function(
      urls,
      polis_key,
      requests_per_minute,
      concurrent_requests
    ) {
      testthat::expect_identical(urls, c("https://api.test.com/data?page=1"))
      testthat::expect_identical(polis_key, "TESTKEY")
      testthat::expect_identical(requests_per_minute, 30)
      testthat::expect_identical(concurrent_requests, 10)

      list(
        data = dplyr::tibble(id = 1:2, name = c("a", "b")),
        next_links = character()
      )
    },
    .env = environment(call_urls_in_parallel)
  )

  res <- call_urls_in_parallel(
    urls = urls,
    polis_key = "TESTKEY",
    requests_per_minute = 30,
    concurrent_requests = 10
  )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_equal(nrow(res), 2)
  testthat::expect_named(res, c("id", "name"))
})

testthat::test_that("call_urls_in_parallel() follows @odata.nextLink pagination until exhausted", {
  urls <- c("https://api.test.com/data?page=1")

  calls <- character()

  testthat::local_mocked_bindings(
    call_urls_in_parallel_helper = function(
      urls,
      polis_key,
      requests_per_minute,
      concurrent_requests
    ) {
      calls <<- c(calls, urls)

      # 1st call returns 2 rows + one next link
      if (identical(urls, c("https://api.test.com/data?page=1"))) {
        return(list(
          data = dplyr::tibble(id = 1:2),
          next_links = c("https://api.test.com/data?page=2")
        ))
      }

      # 2nd call returns 1 row + no next link
      if (identical(urls, c("https://api.test.com/data?page=2"))) {
        return(list(
          data = dplyr::tibble(id = 3),
          next_links = character()
        ))
      }

      testthat::fail(paste(
        "Unexpected urls passed to helper:",
        paste(urls, collapse = ",")
      ))
    },
    .env = environment(call_urls_in_parallel)
  )

  res <- call_urls_in_parallel(
    urls = urls,
    polis_key = "TESTKEY",
    requests_per_minute = 30,
    concurrent_requests = 10
  )

  testthat::expect_equal(res$id, 1:3)
  testthat::expect_equal(
    calls,
    c("https://api.test.com/data?page=1", "https://api.test.com/data?page=2")
  )
})

testthat::test_that("call_urls_in_parallel() returns empty tibble when helper returns no data", {
  urls <- c("https://api.test.com/data?page=1")

  testthat::local_mocked_bindings(
    call_urls_in_parallel_helper = function(
      urls,
      polis_key,
      requests_per_minute,
      concurrent_requests
    ) {
      list(
        data = dplyr::tibble(),
        next_links = character()
      )
    },
    .env = environment(call_urls_in_parallel)
  )

  res <- call_urls_in_parallel(
    urls = urls,
    polis_key = "TESTKEY"
  )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_equal(nrow(res), 0)
})

testthat::test_that("call_urls_in_parallel() passes arguments through to helper on every iteration", {
  urls <- c("https://api.test.com/data?page=1")

  seen <- list()

  testthat::local_mocked_bindings(
    call_urls_in_parallel_helper = function(
      urls,
      polis_key,
      requests_per_minute,
      concurrent_requests
    ) {
      seen <<- append(
        seen,
        list(list(
          urls = urls,
          polis_key = polis_key,
          requests_per_minute = requests_per_minute,
          concurrent_requests = concurrent_requests
        ))
      )

      if (identical(urls, c("https://api.test.com/data?page=1"))) {
        return(list(
          data = dplyr::tibble(x = 1),
          next_links = c("https://api.test.com/data?page=2")
        ))
      }

      list(
        data = dplyr::tibble(x = 2),
        next_links = character()
      )
    },
    .env = environment(call_urls_in_parallel)
  )

  res <- call_urls_in_parallel(
    urls = urls,
    polis_key = "TESTKEY",
    requests_per_minute = 99,
    concurrent_requests = 7
  )

  testthat::expect_equal(res$x, c(1, 2))
  testthat::expect_equal(length(seen), 2)

  testthat::expect_identical(
    seen[[1]]$urls,
    c("https://api.test.com/data?page=1")
  )
  testthat::expect_identical(
    seen[[2]]$urls,
    c("https://api.test.com/data?page=2")
  )

  testthat::expect_identical(seen[[1]]$polis_key, "TESTKEY")
  testthat::expect_identical(seen[[2]]$polis_key, "TESTKEY")

  testthat::expect_identical(seen[[1]]$requests_per_minute, 99)
  testthat::expect_identical(seen[[2]]$requests_per_minute, 99)

  testthat::expect_identical(seen[[1]]$concurrent_requests, 7)
  testthat::expect_identical(seen[[2]]$concurrent_requests, 7)
})
