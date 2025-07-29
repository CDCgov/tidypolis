#' Check POLIS API Endpoints
#' @description Tests WHO POLIS API endpoints for connectivity and estimate download time
#' @param .table `str` Table to check endpoint or run NULL to check for each Table
#' @returns tibble with status and associated information for each tested endpoint
#' @importFrom tibble tibble
#' @export
check_polis_api_endpoints <- function(.table = NULL) {
  api_key <- Sys.getenv("POLIS_API_KEY")
  cache_file <- Sys.getenv("POLIS_CACHE_FILE")

  # Filter Cache and Extract Tables
  cache_data <- read_rds(cache_file) |>
    dplyr::filter(!is.na(.data$polis_id), !is.na(.data$endpoint), !is.na(.data$table)) |>
    dplyr::mutate(max_download_size = ifelse(.data$table %in% c("human_specimen", "environmental_sample", "activity", "sub_activity", "lqas"), 1000L, 2000L))

  # User Input Validation
  if ((!is.null(.table) && length(.table) > 1) || (!is.null(.table) && !.table %in% cache_data$table)) {
    cli::cli_alert_danger("Provide a valid table name or run () for all tables.")
    return(invisible())
  }

  selected_tables <- if (is.null(.table)) cache_data$table else .table
  table_info <- cache_data |> dplyr::filter(.data$table %in% selected_tables)

  # Process table
  1:nrow(table_info) |>
    lapply(function(i) {
      row_data <- table_info[i, ]
      start_time <- Sys.time()
      api_url <- paste0("https://extranet.who.int/polis/api/v2/", row_data$endpoint, "?$inlinecount=allpages&$top=100")
      response <- httr::RETRY("GET", api_url, httr::add_headers("authorization-token" = api_key), times = 10, pause_min = 2, quiet = TRUE, terminate_on_success = TRUE)
      time_taken <- Sys.time() |> difftime(start_time, units = "secs") |> as.numeric()
      status_info <- httr::http_status(response)
      json_payload <- tryCatch(response$content |> rawToChar() |> jsonlite::fromJSON(), error = function(e) NULL)

      # API endpoint summary
      cli::cli_rule(paste("Table:", row_data$table))
      cli::cli_text("API URL: {api_url}")
      cli::cli_text("{status_info$message}")
      cli::cli_text("Response Time: {round(time_taken, 2)} sec")
      cli::cli_text("Total Records: {if (!is.null(json_payload$`odata.count`)) format(as.integer(json_payload$`odata.count`), big.mark = ',') else 'NA'}")
      cli::cli_text("Max Download Size: {format(row_data$max_download_size, big.mark = ',')} per call")
      cli::cli_text("Calls Needed: {if (!is.null(json_payload$`odata.count`)) ceiling(as.integer(json_payload$`odata.count`) / row_data$max_download_size) else 'NA'}")
      cli::cli_text("Estimated Total Download Time: {if (!is.null(json_payload$`odata.count`)) paste(round(time_taken * ceiling(as.integer(json_payload$`odata.count`) / row_data$max_download_size), 2), 'sec') else 'NA'}")
      cli::cli_text("Date and Time Checked: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}")
      if (!is.null(json_payload)) {
        cli::cli_text("Payload Type: {if (!is.null(json_payload$`odata.count`)) 'OData format' else 'JSON format'}")
        cli::cli_text("Columns: {if (!is.null(json_payload$value) && is.data.frame(json_payload$value)) ncol(json_payload$value) else 0}")
      }
      cli::cli_text("")

      # Return tibble
      tibble(table_name = row_data$table, status_code = response$status_code, category = status_info$category, time_taken_sec = round(time_taken, 2), api_url = api_url, checked_at = Sys.time())
    }) |> dplyr::bind_rows()
}
