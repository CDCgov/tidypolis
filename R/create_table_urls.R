#' Create table URLs
#'
#' @description create urls from table size and base url
#' @param url `str` base url to be queried
#' @param table_size `int` integer of download
#' @param type `str` "full" or "partial"
#' @returns array of urls
create_table_urls <- function(url,
                              table_size,
                              type) {
  prior_scipen <- getOption("scipen")
  options(scipen = 999)

  if (sum(type %in% c("full", "partial", "lab", "lab-partial")) > 0) {
    if (type == "full") {
      urls <-
        paste0(url, "?$top=2000&$skip=", as.character(seq(0, as.numeric(table_size), by = 2000)))
    }

    if (type == "partial") {
      urls <-
        paste0(url, "&$top=2000&$skip=", seq(0, as.numeric(table_size), by = 2000))
    }

    if (type == "lab") {
      urls <-
        paste0(url, "?$top=1000&$skip=", as.character(seq(0, as.numeric(table_size), by = 1000)))
    }

    if (type == "lab-partial") {
      urls <-
        paste0(url, "&$top=1000&$skip=", seq(0, as.numeric(table_size), by = 1000))
    }
  }
  return(urls)
}
