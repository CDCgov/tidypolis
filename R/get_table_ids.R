#' Get the unique IDs for a particular POLIS table
#'
#' @description
#' Return the full IDs available in API table.
#'
#' @param table_data `tibble` One row tibble with the associated table data.
#' @param api_key `str` POLIS API Key.
#' @param parallel_calls `str` Whether to get table IDs using parallel calls.
#' @returns `str` A vector of IDs.
#' @export
get_table_ids <- function(table_data, api_key = Sys.getenv("POLIS_API_KEY"), parallel_calls = TRUE) {
    cli::cli_process_start(paste0("Downloading ", table_data$table, " table IDs"))

    # Variables: URL, Token, Filters, ...
    polis_api_root_url <- "https://extranet.who.int/polis/api/v2/"

    if (parallel_calls == FALSE) {
      api_url <-
        paste0(
          polis_api_root_url,
          table_data$endpoint,
          "?$select=Id,",
          table_data$polis_id
        )
      response <- call_urls_in_parallel(api_url)
    } else {
      api_url <-
        paste0(
          polis_api_root_url,
          table_data$endpoint,
          "?$select=Id,",
          table_data$polis_update_id, ",",
          table_data$polis_id
        )
      
      # Find the minimum update date from POLIS
      min_date_url <- paste0(
          polis_api_root_url,
          table_data$endpoint,
          "?$select=Id,",
          table_data$polis_update_id,
          "&$orderby=",
          table_data$polis_update_id,
          "%20asc&$top=1"
        )
      
      min_date <- call_urls_in_parallel(min_date_url, api_key) |> 
        pull(table_data$polis_update_id)
      
      table_data$polis_update_value[1] <- min_date # force to download from earliest updated date in the table
      urls <- create_table_urls(api_url, table_data, 365)

      # create_table_urls() appends "?$filter=" but our api_url
      # already has a param so we must convert "?$filter" to "&$filter" to
      # form a valid URL
      urls <- stringr::str_replace_all(urls, stringr::fixed("?$filter"), stringr::fixed("&$filter"))
      response <- call_urls_in_parallel(urls)
    }

    if (nrow(response) != 0) {
      ids <- response |> dplyr::pull(table_data$polis_id)
    } else {
      ids <- NA
    }

    cli::cli_process_done()

    return(ids)
  }
