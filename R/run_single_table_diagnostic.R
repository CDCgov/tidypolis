#' Run single table diagnostic
#'
#' @description
#' Run diagnostics on a single API table. These are
#'
#' @param .table `str` Table name. Valid values are "virus", "case", "human_specimen",
#' "environmental_sample", "activity", "sub_activity", "lqas", "im",  "population".
#' @param key `str` POLIS API Key.
#' @returns `tibble` Diagnostic data on whether data was successfully pulled from POLIS.
run_single_table_diagnostic <- function(.table, key = Sys.getenv("POLIS_API_Key")) {
    base_url <- "https://extranet.who.int/polis/api/v2/"
    table_data <- get_polis_cache(.table = .table)
    table_url <- paste0(base_url, table_data$endpoint)

    tick <- Sys.time()
    data_return <- tryCatch(
      call_urls_in_parallel(paste0(table_url, "?$top=1000"), key),
      error = function(cond) {
        return("Error")
      }
    )
    tock <- Sys.time()

    data_time <- round(tock - tick, 2)

    tick <- Sys.time()
    id_url <- paste0(table_url, "?$select=", table_data$polis_id, "&$top=1000")
    id_return <- tryCatch(
      call_urls_in_parallel(id_url, key),
      error = function(cond) {
        return("Error")
      }
    )
    tock <- Sys.time()
    id_time <- round(tock - tick, 2)

    return(
      dplyr::tibble(
        "table" = .table,
        "data" = ifelse(is.data.frame(data_return), "Success", "Error"),
        "data_time" = data_time,
        "id" = ifelse(is.data.frame(id_return), "Success", "Error"),
        "id_time" = id_time
      )
    )
}
