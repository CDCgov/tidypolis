#' Filters the cases based on the current epi-week as defined by WHO
#'
#' @param tibble `tibble` A tibble containing the virus table.
#' @param date_col `str` The date column to filter on.
#' @param week_floor_date `Date` The date to anchor on, typically the Monday of the
#' current week.
#'
#' @returns `tibble` Filtered virus table.
#' @keywords internal
#'
filter_current_week <- function(tibble, date_col, week_floor_date) {
  # Dates
  current_week_start <- week_floor_date - days(5)
  current_week_start_endemics <- week_floor_date - days(6)
  current_week_end <- week_floor_date + days(1)

  tibble |>
    dplyr::filter(measurement %in% c("cVDPV 1", "cVDPV 2", "cVDPV 3", "VDPV 1", "VDPV 2", "VDPV 3", "WILD 1"),
                  !!dplyr::sym(date_col) <= current_week_end &
                    (!!dplyr::sym(date_col) >= current_week_start |
                       (!!dplyr::sym(date_col) >= current_week_start_endemics &
                          place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN"))))
}
