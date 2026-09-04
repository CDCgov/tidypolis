#' Call multiple URLs
#'
#' @description 
#' `r lifecycle::badge("deprecated")`
#' 
#' Call multiple URLs
#' 
#' @param urls array of url strings
#' @returns tibble with all data
#' @keywords internal
call_urls <- function(urls) {

  lifecycle::deprecate_warn("2.1.2", "call_urls()", "call_urls_in_parallel()")

  doFuture::registerDoFuture() ## tell foreach to use future

  if (stringr::str_starts(Sys.getenv("SF_PARTNER"), "posit_workbench")) {
    future::plan(future::multicore)
  } else {
    future::plan(future::multisession) ## parallelize over a local PSOCK cluster
  }

  options(doFuture.rng.onMisuse = "ignore")
  xs <- seq_along(urls)

  progressr::handlers("cli")

  progressr::with_progress({
    p <- progressr::progressor(along = xs)
    y <-
      foreach::`%dopar%`(foreach::foreach(
        x = xs,
        .packages = c("tidypolis", "dplyr", "jsonlite", "httr")
      ), {
        # signal a progression update
        p()
        # jitter the parallel calls to not overwhelm the server
        # Sys.sleep(1 + stats::rpois(1, 10)/100)
        log <- dplyr::tibble(
          time = Sys.time(),
          call = urls[x],
          event = "MADE CALL"
        )

        tryCatch(
          {
            response <- call_single_url(urls[x])
            log <- log |>
              dplyr::add_row(
                time = Sys.time(),
                call = urls[x],
                event = "FINISHED CALL"
              )
          },
          error = \(e) {
            response <- NA
            log <- log |>
              dplyr::add_row(
                time = Sys.time(),
                call = urls[x],
                event = "CALL FAILED"
              )
          }
        )

        dplyr::tibble(
          response = list(response),
          log = list(log)
        )
      })
  })

  resp <- dplyr::bind_rows(y) |>
    dplyr::filter(!is.na(response)) |>
    dplyr::pull(response) |>
    dplyr::bind_rows()

  # Convert string representations of NULL/NA to actual NA values
  resp <- resp |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ dplyr::case_when(
          as.character(.x) %in% c("NULL", "NA", "") ~ NA_character_,
          TRUE ~ as.character(.x)
        )
      )
    )

  # Infer and apply column types (works with sparse data)
  resp <- readr::type_convert(resp, col_types = readr::cols())

  log <- dplyr::bind_rows(y) |>
    dplyr::pull(log) |>
    dplyr::bind_rows()

  if (as.logical(Sys.getenv("API_DEBUG"))) {
    api_log <- tidypolis_io(io = "read", file_path = Sys.getenv("POLIS_API_LOG_FILE"))
    api_log <- dplyr::bind_rows(api_log, log)
    tidypolis_io(api_log,
      io = "write", file_path = Sys.getenv("POLIS_API_LOG_FILE")
    )
  }

  gc()
  return(resp)
}