#' Utility function to fix years
#'
#' @param df `tibble` Dataframe to be used for fixing years.
#' @param yrs `array` Numeric array of years to fix data.
#' @details
#' This is an internal function used in the sirfunctions (CDCGov/sirfunctions)
#' package.
#'
#' @returns `tibble` Long format table to fix years.
#' @keywords internal
f.yrs.01 <- function(df, yrs) {
  quo.yrs <- dplyr::enquo(yrs)

  shape01 <- df %>%
    dplyr::filter((yr.st <= !!quo.yrs & yr.end >= !!quo.yrs) |
                    yr.st == !!quo.yrs) %>%
    dplyr::mutate(active.year.01 = !!quo.yrs)

  return(shape01)
}
