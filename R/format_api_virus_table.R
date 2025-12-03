#' Helper function to standardize names of the API virus table
#'
#' @description
#' Allows comparison to the preprocessed positives file.
#'
#'
#' @param api_virus_table `tibble` The virus table downloaded from the POLIS API.
#'
#' @returns `tibble` A virus table with the same names as those found in the preprocessed positives file.
#' @keywords internal
#'

format_api_virus_table <- function(api_virus_table) {
  relevant_cols <- c("EPID", "VirusDate", "VirusReportingWeekAndYear", "VirusTypeName",
                     "CreatedDate", "UpdatedDate", "PublishDate",
                     "VdpvReportedToHQDate", "VdpvClassificationChangeDate",
                     "Admin0Name", "Admin1Name",

                     "Virus Date", "Place Admin 0", "Place Admin 1" # these are from CORE READY virus files
                     )

  df <- api_virus_table |>
    dplyr::select(dplyr::any_of(relevant_cols)) |>
    dplyr::rename_with(stringr::str_to_lower) |>
    dplyr::rename_with(recode,
                       `virus date` = "dateonset",
                       `place admin 0` = "place.admin.0",
                       `place admin 1` = "place.admin.1",
                       virusdate = "dateonset",
                       virustypename = "measurement",
                       admin0name = "place.admin.0",
                       admin1name = "place.admin.1",
                       vdpvreportedtohqdate = "datenotificationtohq",
    ) |>
    dplyr::mutate(dplyr::across(dplyr::contains("date"), \(x) lubridate::as_date(x))) |>
    dplyr::mutate(year = lubridate::year(dateonset))

  # Add report_date only if the required columns exist
  if (all(c("measurement", "vdpvclassificationchangedate", "datenotificationtohq") %in% names(df))) {
    df <- df |>
      dplyr::mutate(report_date = dplyr::case_when(
        stringr::str_detect(measurement, "VDPV") ~ vdpvclassificationchangedate,
        stringr::str_detect(measurement, "WILD") ~ datenotificationtohq
      ))
  } else {
    df$report_date <- NA_Date_
  }

  df <- df |>
    dplyr::mutate(measurement = stringr::str_replace(measurement, "([A-Za-z])([0-9]+)", "\\1 \\2"))

  return(df)
}

