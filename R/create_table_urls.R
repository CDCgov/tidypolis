# Helper functions ----


#' Cuts the update dates into time intervals
#'
#' @description
#' Cuts the time intervals based on an anchor date, with the last record
#' being open ended.
#'
#' @param anchor_date `str` The latest update date from a table.
#' @param update_col `str` Name of the column with the update date, if applicable.
#' @param days_intervals `int` Number of days for each interval.
#' @details
#' Majority of the code in this function was generated using Microsoft 365 Copilot
#' Version number: 2.20260115.41.0. The code was reviewed and
#' edited by Mervin Cuadera (xrg9).
#'
#' @returns `str` A vector of strings representing date intervals.
#' @keywords internal
#'
week_cuts_api <- function(anchor_date, update_col, days_intervals = 7) {
  # Normalize anchor_date to POSIXct in UTC
  a <- if (inherits(anchor_date, "POSIXct")) {
    lubridate::with_tz(anchor_date, "UTC")
  } else {
    lubridate::ymd_hms(anchor_date, tz = "UTC")
  }

  by <- lubridate::ddays(days_intervals)
  now_utc <- lubridate::with_tz(Sys.time(), "UTC")

  # If now is before anchor_date, produce a single open-ended cut
  if (now_utc <= a) {
    start <- format(a, "%Y-%m-%dT%H:%M:%OS3Z")
    return(dplyr::tibble(
      start = start,
      end = NA_character_,
      open_ended = TRUE,
      interval = start
    ))
  }

  # Compute number of day bins up to "now"
  n_bins <- floor(as.numeric(difftime(now_utc, a, units = "secs")) /
                    as.numeric(by, units = "secs")) + 1

  starts <- a + by * (0:(n_bins - 1))
  ends   <- starts + by

  # Format as RFC3339/ISO-8601 with milliseconds and Z (UTC)
  fmt <- "%Y-%m-%dT%H:%M:%OS3Z"
  starts_str <- format(starts, fmt)
  ends_str   <- format(ends,   fmt)

  # Build output rows: all fixed bins + one final open-ended bin
  fixed_rows <- dplyr::tibble(
    start = starts_str[-length(starts_str)],
    end   = ends_str[-length(ends_str)],
    open_ended = FALSE,
    interval   = paste0(
      update_col,
      " gt ", starts_str[-length(starts_str)],
      " and ",
      update_col, " le ", ends_str[-length(ends_str)]
    )
  )

  last_row <- dplyr::tibble(
    start = starts_str[length(starts_str)],
    end   = NA_character_,
    open_ended = TRUE,
    interval   = paste0(update_col, " gt ", starts_str[length(starts_str)])
  )

  full_cut <- dplyr::bind_rows(fixed_rows, last_row) |>
    dplyr::pull(interval)

  full_cut[1] <- gsub("gt", "ge", full_cut[1])

  return(full_cut)
}


#' Create table URLs
#'
#' @description
#' Create URLs for a table to assist in parallelization. Each table will need to be
#' segmented based on their structure.
#'
#' @param url `str` Base url to be queried.
#' @param table_data `tibble` One row tibble with the data for a specific table.
#' @details
#' Valid values for `.table` can be found by looking at the cache via [get_polis_cache()].
#'
#' @returns `str` Array of URLs to be used in [call_urls()].
#' @keywords internal
create_table_urls <- function(url,
                              table_data) {

  with_update_col <- c("virus", "case", "human_specimen", "environmental_sample",
                       "activity", "sub_activity", "population")
  with_update_date <- ifelse(table_data$table %in% with_update_col, TRUE, FALSE)

  if (with_update_date) {

    update_date <- table_data$polis_update_value

    if (!is.na(update_date)) {
      # Download from last update date
      date_intervals <- week_cuts_api(update_date, table_data$polis_update_id , 7)
    } else {
      # Download all data in the table beginning from 2000-01-01
      # since no last sync data
      date_intervals <- week_cuts_api("2000-01-01T00:00:00Z", table_data$polis_update_id, 365)
    }

    urls <- paste0(url, "?$filter=", date_intervals)
    urls <- gsub(" ", "+", urls)
    return(urls)
  } else {
    # IM and LQAS are the tables that need to be downloaded in full because they
    # lack an updated date. However, they are relatively small so we will not be using any
    # date slicing
    return(url)
  }

}
