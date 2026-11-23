# These fixtures contain dummy keys and never call POLIS or EDAV.
mock_polis_key_get <- function(status = 200L, body = '{"value":[]}',
                               content_type = "application/json") {
  force(status)
  force(body)
  force(content_type)
  function(url, config, handle) {
    structure(list(
      status_code = status,
      headers = if (is.null(content_type)) list() else
        list("content-type" = content_type),
      content = charToRaw(body),
      request = list(headers = config$headers)
    ), class = "response")
  }
}

test_that("empty and malformed keys fail without making an HTTP request", {
  local_mocked_bindings(GET = function(...) stop("Unexpected HTTP request"))
  for (key in list("", " \t ", NA_character_, NULL, character(),
                   c("first", "second"), 123)) {
    expect_false(test_polis_key(key))
    expect_error(validate_polis_key(key), "single nonempty")
  }
})

test_that("validation sends the key in a combined authenticated request", {
  fake_get <- mock_polis_key_get(content_type = "application/json; charset=utf-8")
  handles <- list()
  local_mocked_bindings(GET = function(url, config, handle) {
    expect_identical(url, "https://extranet.who.int/polis/api/v2/Case?$top=0")
    expect_s3_class(config, "request")
    expect_identical(unname(config$headers["authorization-token"]), "new-key")
    expect_identical(unname(config$headers["Accept"]), "application/json")
    expect_identical(config$options$followlocation, 0L)
    expect_equal(config$options$timeout_ms, 30000)
    expect_true(as.logical(config$options$ssl_verifypeer))
    expect_equal(config$options$ssl_verifyhost, 2L)
    handles[[length(handles) + 1L]] <<- handle$handle
    fake_get(url, config, handle)
  })
  expect_true(test_polis_key("  new-key  "))
  expect_true(test_polis_key("new-key"))
  expect_false(identical(handles[[1]], handles[[2]]))
})

test_that("authentication failures, redirects, and server errors fail validation", {
  for (status in c(401L, 403L, 302L, 500L, 503L)) {
    with_mocked_bindings({
      expect_false(test_polis_key("new-key"))
      expect_error(validate_polis_key("new-key"), as.character(status))
    }, GET = mock_polis_key_get(status = status))
  }
})

test_that("HTTP 200 alone does not validate a key", {
  invalid_responses <- list(
    mock_polis_key_get(body = "<html>Login</html>", content_type = "text/html"),
    mock_polis_key_get(content_type = "application/jsonp"),
    mock_polis_key_get(content_type = NULL),
    mock_polis_key_get(body = "not JSON"),
    mock_polis_key_get(body = '{"error":"Access denied"}'),
    mock_polis_key_get(body = '{"value":null}'),
    mock_polis_key_get(body = '{"value":"unexpected"}')
  )
  for (fake_get in invalid_responses) {
    with_mocked_bindings({
      expect_false(test_polis_key("new-key"))
      expect_error(validate_polis_key("new-key"))
    }, GET = fake_get)
  }
})

test_that("missing request headers and connection failures cannot pass validation", {
  local_mocked_bindings(GET = function(...) {
    structure(list(request = list(headers = character())), class = "response")
  })
  expect_false(test_polis_key("new-key"))
  expect_error(validate_polis_key("new-key"), "not attached")

  with_mocked_bindings({
    expect_false(test_polis_key("new-key"))
    expect_error(validate_polis_key("new-key"), "Could not contact POLIS")
  }, GET = function(...) stop("Connection failed"))
})

test_that("a local update preserves all other fields and verifies the saved file", {
  source_file <- tempfile(fileext = ".rds")
  output_folder <- tempfile()
  on.exit(unlink(c(source_file, output_folder), recursive = TRUE), add = TRUE)
  creds <- list(polis_api_key = "old-key", polis_data_folder = "POLIS",
                extra = list(owner = "team", version = 3L))
  saveRDS(creds, source_file)
  output_file <- file.path(output_folder, "nested", "creds.rds")
  local_mocked_bindings(GET = mock_polis_key_get())

  old_key <- Sys.getenv("POLIS_API_KEY", unset = NA_character_)
  on.exit(if (is.na(old_key)) Sys.unsetenv("POLIS_API_KEY") else
    Sys.setenv(POLIS_API_KEY = old_key), add = TRUE)
  Sys.setenv(POLIS_API_KEY = "session-before")

  result <- withVisible(suppressMessages(update_polis_key(
    output_file, key = " new-key ", creds_file = source_file, edav = FALSE
  )))
  expected <- creds
  expected$polis_api_key <- "new-key"
  expect_identical(readRDS(output_file), expected)
  expect_identical(readRDS(source_file), creds)
  expect_identical(result$value,
                   normalizePath(output_file, winslash = "/", mustWork = TRUE))
  expect_false(result$visible)
  expect_identical(Sys.getenv("POLIS_API_KEY"), "new-key")
  expect_true(test_polis_key())

  # This is the same check a user can run after reopening the local file.
  expect_true(test_polis_key(readRDS(output_file)$polis_api_key))
})

test_that("EDAV is read only and messages do not expose either key", {
  original_key <- Sys.getenv("POLIS_API_KEY", unset = NA_character_)
  output_file <- tempfile(fileext = ".rds")
  on.exit(unlink(output_file), add = TRUE)
  creds <- list(polis_api_key = "old-dummy-secret", polis_data_folder = "POLIS",
                owner = "team")
  calls <- 0L
  local_mocked_bindings(
    GET = mock_polis_key_get(),
    tidypolis_io = function(io, file_path, edav, edav_default_dir) {
      calls <<- calls + 1L
      expect_identical(io, "read")
      expect_identical(file_path, "POLIS/creds.rds")
      expect_true(edav)
      expect_identical(edav_default_dir, "GID/PEB/SIR")
      creds
    }
  )
  messages <- character()
  withCallingHandlers(
    update_polis_key(output_file, key = "new-dummy-secret", set_session_key = FALSE),
    message = function(e) {
      messages <<- c(messages, conditionMessage(e))
      invokeRestart("muffleMessage")
    }
  )
  expect_identical(calls, 1L)
  expect_identical(readRDS(output_file)$owner, "team")
  expect_false(any(grepl("old-dummy-secret|new-dummy-secret", messages)))
  expect_identical(Sys.getenv("POLIS_API_KEY", unset = NA_character_), original_key)
})

test_that("failed validation leaves existing credentials and session key unchanged", {
  source_file <- tempfile(fileext = ".rds")
  output_file <- tempfile(fileext = ".rds")
  on.exit(unlink(c(source_file, output_file)), add = TRUE)
  creds <- list(polis_api_key = "old-key", polis_data_folder = "POLIS")
  saveRDS(creds, source_file)
  saveRDS(creds, output_file)
  local_mocked_bindings(GET = mock_polis_key_get(status = 401L))
  old_key <- Sys.getenv("POLIS_API_KEY", unset = NA_character_)
  on.exit(if (is.na(old_key)) Sys.unsetenv("POLIS_API_KEY") else
    Sys.setenv(POLIS_API_KEY = old_key), add = TRUE)
  Sys.setenv(POLIS_API_KEY = "session-before")

  expect_error(update_polis_key(
    output_file, key = "rejected-key", creds_file = source_file,
    edav = FALSE, overwrite = TRUE
  ), "401")
  expect_identical(readRDS(output_file), creds)
  expect_identical(readRDS(source_file), creds)
  expect_identical(Sys.getenv("POLIS_API_KEY"), "session-before")
})

test_that("local overwrites require an explicit opt in", {
  source_file <- tempfile(fileext = ".rds")
  output_file <- tempfile(fileext = ".rds")
  on.exit(unlink(c(source_file, output_file)), add = TRUE)
  creds <- list(polis_api_key = "old-key", polis_data_folder = "POLIS")
  saveRDS(creds, source_file)
  saveRDS(creds, output_file)
  local_mocked_bindings(GET = function(...) stop("Unexpected HTTP request"))
  expect_error(update_polis_key(
    output_file, key = "new-key", creds_file = source_file, edav = FALSE
  ), "already exists")
  expect_identical(readRDS(output_file), creds)

  with_mocked_bindings({
    suppressMessages(update_polis_key(
      output_file, key = "new-key", creds_file = source_file, edav = FALSE,
      overwrite = TRUE, set_session_key = FALSE
    ))
  }, GET = mock_polis_key_get())
  expect_identical(readRDS(output_file)$polis_api_key, "new-key")
})

test_that("save failures and readback mismatches do not change the session key", {
  source_file <- tempfile(fileext = ".rds")
  output_file <- tempfile(fileext = ".rds")
  on.exit(unlink(c(source_file, output_file)), add = TRUE)
  saveRDS(list(polis_api_key = "old-key", polis_data_folder = "POLIS"), source_file)
  local_mocked_bindings(GET = mock_polis_key_get())
  old_key <- Sys.getenv("POLIS_API_KEY", unset = NA_character_)
  on.exit(if (is.na(old_key)) Sys.unsetenv("POLIS_API_KEY") else
    Sys.setenv(POLIS_API_KEY = old_key), add = TRUE)
  Sys.setenv(POLIS_API_KEY = "session-before")

  with_mocked_bindings({
    expect_error(update_polis_key(
      output_file, key = "new-key", creds_file = source_file, edav = FALSE
    ), "permissions")
  }, write_rds = function(...) stop("Permission denied"))
  expect_false(file.exists(output_file))
  expect_identical(Sys.getenv("POLIS_API_KEY"), "session-before")

  with_mocked_bindings({
    expect_error(update_polis_key(
      output_file, key = "new-key", creds_file = source_file, edav = FALSE
    ), "did not match")
  }, read_rds = function(...) list(polis_api_key = "wrong-key"))
  expect_identical(Sys.getenv("POLIS_API_KEY"), "session-before")
})

test_that("malformed source credentials cannot produce an output file", {
  source_file <- tempfile(fileext = ".rds")
  output_file <- tempfile(fileext = ".rds")
  on.exit(unlink(c(source_file, output_file)), add = TRUE)
  local_mocked_bindings(GET = function(...) stop("Unexpected HTTP request"))
  for (creds in list("not a list", list(polis_api_key = "old-key"))) {
    saveRDS(creds, source_file)
    expect_error(update_polis_key(
      output_file, key = "new-key", creds_file = source_file, edav = FALSE
    ), "existing credentials")
    expect_false(file.exists(output_file))
  }
})

test_that("noninteractive updates require an explicit key", {
  skip_if(interactive())
  local_mocked_bindings(tidypolis_io = function(...) stop("Unexpected EDAV call"))
  expect_error(update_polis_key(tempfile(fileext = ".rds")), "Supply key explicitly")
})

test_that("blank replacement keys fail before reading shared credentials", {
  local_mocked_bindings(tidypolis_io = function(...) stop("Unexpected EDAV call"))
  expect_error(update_polis_key(tempfile(fileext = ".rds"), key = " \t "),
               "single nonempty")
})
