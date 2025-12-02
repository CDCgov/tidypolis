#' Analyze errors in the Positives dataset pulled directly from the POLIS API
#'
#' @description
#' Checks for errors in the virus table downloaded from the POLIS API. It is meant
#' to catch errors related to inconsistencies in detections reported in the current
#' week.
#'
#'
#' @param virus_table_path `str` Absolute path to the positives file.
#' @param week_floor_date `date` Start date of the week to analyze. Defaults to the Monday of the previous week.
#' @param edav `logical` Whether to load the positives file from EDAV or locally. Defaults to TRUE.
#'
#' @returns `list` A list containing details of the data errors, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' data_errors <- check_new_viruses()
#' }
check_new_viruses <- function(week_floor_date = lubridate::floor_date(Sys.Date(), unit = "week", week_start = 1),
                              virus_table_path =  "GID/PEB/SIR/POLIS/data/virus.rds",
                              edav = TRUE) {
  week_floor_date <- lubridate::as_date(week_floor_date)
  cli::cli_process_start("Loading positives dataset from POLIS API")
  positives <- sirfunctions::sirfunctions_io("read", NULL, file_loc = virus_table_path, edav = edav)
  cli::cli_process_done()

  # Only include relevant columns
  relevant_cols <- c("EPID", "VirusDate", "VirusReportingWeekAndYear", "VirusTypeName",
                     "CreatedDate", "UpdatedDate", "PublishDate",
                     "VdpvReportedToHQDate", "VdpvClassificationChangeDate",
                     "Admin0Name", "Admin1Name")
  positives_formatted <- positives |>
    dplyr::select(dplyr::any_of(relevant_cols)) |>
    dplyr::rename_with(stringr::str_to_lower) |>
    dplyr::rename(
      dateonset = "virusdate",
      measurement = "virustypename",
      place.admin.0 = "admin0name",
      place.admin.1 = "admin1name",
      datenotificationtohq = "vdpvreportedtohqdate",
    ) |>
    dplyr::mutate(dplyr::across(dplyr::contains("date"), \(x) lubridate::as_date(x))) |>
    dplyr::mutate(year = lubridate::year(dateonset),
                  report_date = dplyr::case_when(
                    measurement %in% c("cVDPV1", "cVDPV2", "cVDPV3", "VDPV1", "VDPV2", "VDPV3") ~ vdpvclassificationchangedate,
                    measurement == "WILD1" ~ datenotificationtohq
                  )) |>
    dplyr::mutate(measurement = stringr::str_replace(measurement, "([A-Za-z])([0-9]+)", "\\1 \\2"))

  if(max(positives_formatted$createddate, na.rm = TRUE) < week_floor_date) {
    cli::cli_alert_warning(paste0("The max date for the positives dataset is ", max(positives_formatted$createddate, na.rm = TRUE),
                               " but the week start date passed is more recent."))
  }

  # Helper function for filtering
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

  # Viruses
  newly_added <- filter_current_week(positives_formatted, "createddate", week_floor_date)
  newly_updated <- filter_current_week(positives_formatted, "updateddate", week_floor_date)
  newly_changed_classification <- filter_current_week(positives_formatted, "vdpvclassificationchangedate", week_floor_date)
  newly_reported <- filter_current_week(positives_formatted, "report_date", week_floor_date)

  cli::cli_alert(paste0("Since ", week_floor_date, ":"))
  cli::cli_li(paste0("Newly added virus records: ", nrow(newly_added)))
  cli::cli_li(paste0("Newly updated virus records: ",  nrow(newly_updated)))
  cli::cli_li(paste0("Newly reported virus records: ", nrow(newly_reported)))

  # Records check
  new_w_hq_date <- filter_current_week(newly_added, "datenotificationtohq", week_floor_date) |> dplyr::pull(epid)
  new_w_vdpv_change_date <- filter_current_week(newly_added, "vdpvclassificationchangedate",week_floor_date) |> dplyr::pull(epid)
  new_w_good_report_date <- filter_current_week(newly_reported, "report_date", week_floor_date) |> dplyr::pull(epid)

  # Positives records with errors
  cli::cli_process_start("Analyzing potential errors for newly added viruses")
  new_no_hq_date <- newly_added |>
    dplyr::filter(!epid %in% new_w_hq_date, measurement == "WILD 1")
  new_no_vdpv_change_date <- newly_added |>
    dplyr::filter(!epid %in% c(new_w_vdpv_change_date),
                  measurement != "WILD 1")
  new_no_report_date <- newly_added |>
    dplyr::filter(!epid %in% c(new_w_vdpv_change_date, new_w_hq_date))
  cli::cli_process_done()

  # Weekly reporting error
  current_week <- names(which.max(table(newly_reported$virusreportingweekandyear)))
  cli::cli_alert_info(paste0("The current week is: ", current_week))
  new_but_different_week <- newly_added |>
    dplyr::filter(virusreportingweekandyear != current_week)

  cli::cli_li(paste0("Newly added viruses reported for a different week: ", nrow(new_but_different_week)))
  cli::cli_li(paste0("Newly added WILD 1 viruses with no date notification to HQ: ", nrow(new_no_hq_date)))
  cli::cli_li(paste0("Newly added VDPV viruses with no VDPV Classification Change Date: ", nrow(new_no_vdpv_change_date)))
  cli::cli_li(paste0("Newly added viruses with no report date: ", nrow(new_no_report_date)))
  cli::cli_alert_info("NOTE: Report date is based on the following: VDPVs (VDPV Classification Change Date); WILD 1 (Notification to HQ Date)")

  cli::cli_alert_info("Please check the outputted list for more details on the issues highlighted")

  error_list <- list()
  error_list$newly_added <- newly_added
  error_list$newly_changed_classification <- newly_changed_classification
  cli::cli_alert_info("NOTE: Viruses in newly_reported are those with report dates only.")
  error_list$newly_reported <- newly_reported
  error_list$new_no_hq_date <- new_no_hq_date
  error_list$new_no_vdpv_change_date <- new_no_vdpv_change_date
  error_list$new_no_report_date <- new_no_report_date

  invisible(error_list)

  }
