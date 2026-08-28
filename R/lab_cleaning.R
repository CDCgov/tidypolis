# Private functions ----

#' Impute missing geographic information from the AFP linelist
#'
#' @param lab_data `tibble` Lab data to clean.
#' @param afp_data `tibble` AFP data.
#'
#' @returns `tibble` Lab data set with imputed geographic columns based on the
#' AFP table.
#' @keywords internal
impute_missing_lab_geo <- function(lab_data, afp_data = NULL) {
  lab_data <- dplyr::rename(lab_data,
                            EpidNumber = EPID
  )
  lab_data <- lab_data |>
    tidyr::separate_wider_regex(
      cols = "EpidNumber",
      c(
        epid_ctry = ".*", "[-/]",
        epid_prov = ".*", "[-/]",
        epid_dist = ".*", "[-/]",
        epid_04 = ".*", "[-/]",
        epid_05 = ".*"
      ),
      names_repair = "check_unique",
      too_few = "align_start",
      cols_remove = F
    )

  if (!is.null(afp_data)) {
    afp_data <- dplyr::rename(
      afp_data,
      ctry = place.admin.0,
      prov = place.admin.1,
      dist = place.admin.2,
      sex = person.sex,
      date = dateonset,
      year = yronset,
      date.notify = datenotify,
      date.invest = dateinvest,
      cdc.class = cdc.classification.all
    )

    cli::cli_process_start("Obtaining geographic information based on matching EPIDs from the AFP linelist")
    lab_data$ctry <- afp_data$ctry[match(lab_data$EpidNumber, afp_data$epid)]
    lab_data$prov <- afp_data$prov[match(lab_data$EpidNumber, afp_data$epid)]
    lab_data$dist <- afp_data$dist[match(lab_data$EpidNumber, afp_data$epid)]

    lab_data$adm0guid <- afp_data$adm0guid[match(lab_data$EpidNumber, afp_data$epid)]
    lab_data$adm1guid <- afp_data$adm1guid[match(lab_data$EpidNumber, afp_data$epid)]
    lab_data$adm2guid <- afp_data$adm2guid[match(lab_data$EpidNumber, afp_data$epid)]
    cli::cli_process_done()

    # If these columns are available (in WHO lab)
    if ("Province" %in% names(lab_data)) {
      lab_data <- lab_data |>
        dplyr::mutate(
          prov = dplyr::if_else(is.na(prov),
                                afp_data$prov[match(lab_data$Province, afp_data$prov)],
                                prov
          )
        )
    }

    if ("District" %in% names(lab_data)) {
      lab_data <- lab_data |>
        dplyr::mutate(
          dist = dplyr::if_else(is.na(dist),
                                afp_data$dist[match(lab_data$District, afp_data$dist)],
                                dist
          )
        )
    }

    # Additional data cleaning steps
    geo_lookup_table <- afp_data |>
      dplyr::select(
        "epid", dplyr::matches("guid"),
        dplyr::contains("$adm"), "ctry", "prov", "dist", "year"
      ) |>
      tidyr::separate_wider_regex(
        cols = "epid",
        c(
          epid_ctry = ".*", "[-/]",
          epid_prov = ".*", "[-/]",
          epid_dist = ".*", "[-/]",
          epid_04 = ".*", "[-/]",
          epid_05 = ".*"
        ),
        too_few = "align_start"
      ) |>
      dplyr::select(
        "epid_ctry", "epid_prov", "epid_dist",
        "ctry", "prov", "dist",
        dplyr::matches("adm[0-3]guid"), "year"
      ) |>
      dplyr::distinct()

    # Geomatching algorithm
    cli::cli_process_start("Beginning geomatching based on AFP lookup table")

    # Imputing missing countries
    cli::cli_process_start("Imputing ctry and adm0guid")
    cli::cli_alert_info(paste0("Initial records missing ctry: ", sum(is.na(lab_data$ctry))))
    cli::cli_alert_info(paste0("Initial records missing adm0guid: ", sum(is.na(lab_data$adm0guid))))
    ctry_lookup_table <- geo_lookup_table |>
      dplyr::select("epid_ctry", "ctry", "adm0guid", "year") |>
      dplyr::distinct() |>
      tidyr::drop_na("ctry")

    # Check look up table for potential duplicated rows
    ctry_lookup_row_dups <- ctry_lookup_table |>
      dplyr::mutate(epid_comb = stringr::str_c(epid_ctry, year, sep = "-")) |>
      dplyr::group_by(epid_comb, epid_ctry, year) |>
      dplyr::summarise(n = dplyr::n()) |>
      dplyr::filter(n > 1) |>
      dplyr::ungroup()

    # Remove duplicates from the look up table
    ctry_lookup_row_dups <- ctry_lookup_row_dups |>
      dplyr::select(!dplyr::any_of(c("epid_comb", "n")))
    ctry_lookup_table <- dplyr::anti_join(
      ctry_lookup_table,
      ctry_lookup_row_dups
    )
    lab_data <- lab_data |>
      dplyr::left_join(ctry_lookup_table, by = dplyr::join_by(epid_ctry, year)) |>
      dplyr::mutate(
        ctry = dplyr::coalesce(ctry.x, ctry.y),
        adm0guid = dplyr::coalesce(adm0guid.x, adm0guid.y)
      )
    cli::cli_alert_info(paste0("Final records missing ctry: ", sum(is.na(lab_data$ctry))))
    cli::cli_alert_info(paste0("Final records missing adm0guid: ", sum(is.na(lab_data$adm0guid))))
    cli::cli_process_done()

    # Imputing missing provinces
    cli::cli_process_start("Imputing prov and adm1guid")
    cli::cli_alert_info(paste0("Initial records missing prov: ", sum(is.na(lab_data$prov))))
    cli::cli_alert_info(paste0("Initial records missing adm1guid: ", sum(is.na(lab_data$adm1guid))))
    prov_lookup_table <- geo_lookup_table |>
      dplyr::select("epid_ctry", "epid_prov", "ctry", "prov", "adm1guid", "year") |>
      dplyr::distinct() |>
      tidyr::drop_na("prov")

    # Check look up table for potential duplicated rows
    prov_lookup_row_dups <- prov_lookup_table |>
      dplyr::mutate(epid_comb = stringr::str_c(epid_ctry, epid_prov, ctry, year, sep = "-")) |>
      dplyr::group_by(epid_comb, epid_ctry, ctry, epid_prov, year) |>
      dplyr::summarise(n = dplyr::n()) |>
      dplyr::filter(n > 1) |>
      dplyr::ungroup()

    # Remove duplicates from the look up table
    prov_lookup_row_dups <- prov_lookup_row_dups |>
      dplyr::select(!dplyr::any_of(c("epid_comb", "n")))
    prov_lookup_table <- dplyr::anti_join(prov_lookup_table, prov_lookup_row_dups)

    lab_data <- lab_data |>
      dplyr::left_join(prov_lookup_table, by = dplyr::join_by(epid_ctry, ctry, epid_prov, year)) |>
      dplyr::mutate(
        prov = dplyr::coalesce(prov.x, prov.y),
        adm1guid = dplyr::coalesce(adm1guid.x, adm1guid.y)
      )
    cli::cli_alert_info(paste0("Final records missing prov: ", sum(is.na(lab_data$prov))))
    cli::cli_alert_info(paste0("Final records missing adm1guid: ", sum(is.na(lab_data$adm1guid))))
    cli::cli_process_done()

    # Imputing district
    cli::cli_process_start("Imputing dist and adm2guid")
    cli::cli_alert_info(paste0("Initial records missing dist: ", sum(is.na(lab_data$dist))))
    cli::cli_alert_info(paste0("Initial records missing adm2guid: ", sum(is.na(lab_data$adm2guid))))
    dist_lookup_table <- geo_lookup_table |>
      dplyr::select("epid_ctry", "epid_prov", "epid_dist", "ctry", "prov", "dist", "adm2guid", "year") |>
      dplyr::distinct() |>
      tidyr::drop_na("dist")

    # Check look up table for potential duplicated rows
    dist_lookup_row_dups <- dist_lookup_table |>
      dplyr::mutate(epid_comb = stringr::str_c(epid_ctry, epid_prov, epid_dist, ctry, prov, year, sep = "-")) |>
      dplyr::group_by(
        epid_comb, epid_ctry, ctry,
        epid_prov, prov,
        epid_dist,
        year
      ) |>
      dplyr::summarise(n = dplyr::n()) |>
      dplyr::filter(n > 1) |>
      dplyr::ungroup()

    # Remove duplicates from the look up table
    dist_lookup_row_dups <- dist_lookup_row_dups |>
      dplyr::select(!dplyr::any_of(c("epid_comb", "n")))
    dist_lookup_table <- dplyr::anti_join(dist_lookup_table, dist_lookup_row_dups)

    lab_data <- lab_data |>
      dplyr::left_join(dist_lookup_table,
                       by = dplyr::join_by(epid_ctry, epid_prov, ctry, prov, epid_dist, year)
      ) |>
      dplyr::mutate(
        dist = dplyr::coalesce(dist.x, dist.y),
        adm2guid = dplyr::coalesce(adm2guid.x, adm2guid.y)
      ) |>
      dplyr::select(-dplyr::ends_with(".y"), -dplyr::ends_with(".x"))
    cli::cli_alert_info(paste0("Final records missing dist: ", sum(is.na(lab_data$dist))))
    cli::cli_alert_info(paste0("Final records missing adm2guid: ", sum(is.na(lab_data$adm2guid))))
    cli::cli_process_done()

    # check for correctness
    check <- lab_data |>
      dplyr::select(
        dplyr::starts_with("epid_"), dplyr::matches("adm[0-2]"),
        "ctry", "prov", "dist", "EpidNumber", "year"
      )
    mismatch_ctry <- dplyr::anti_join(check, ctry_lookup_table)
    mismatch_dist <- dplyr::anti_join(check, dist_lookup_table)
    mismatch_prov <- dplyr::anti_join(check, prov_lookup_table)

    cli::cli_process_done()

    # Message for values without any province or district information
  } else {
    cli::cli_alert_warning("AFP linelist not attached. Geographic columns will be empty.")
    lab_data$ctry <- NA
    lab_data$prov <- NA
    lab_data$dist <- NA
    lab_data$adm0guid <- NA
    lab_data$adm1guid <- NA
    lab_data$adm2guid <- NA
  }

  return(lab_data)
}

#' Robust data parser for lab dates from Excel file
#'
#' @keywords internal
parse_lab_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }

  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }

  x_chr <- trimws(as.character(x))

  x_chr[x_chr %in% c(
    "",
    "NA",
    "N/A",
    "n/a",
    "NULL",
    "null",
    "-",
    "--",
    "."
  )] <- NA_character_

  parsed <- suppressWarnings(lubridate::parse_date_time(
    x_chr,
    orders = c(
      "ymd",
      "ymd HMS",
      "ymd HM",
      "ymd IMS p",
      "ymd I:M:S p",
      "mdy",
      "mdy HMS",
      "mdy HM",
      "mdy IMS p",
      "mdy I:M:S p",
      "dmy",
      "dmy HMS",
      "dmy HM",
      "dmy IMS p",
      "dmy I:M:S p"
    ),
    tz = "UTC"
  ))

  out <- as.Date(parsed)

  # Handle Excel serial dates if date-like values came through as numbers.
  serial <- suppressWarnings(as.numeric(x_chr))
  serial_idx <- is.na(out) & !is.na(serial) & serial > 20000 & serial < 60000

  out[serial_idx] <- as.Date(serial[serial_idx], origin = "1899-12-30")

  out
}

# Public functions ----
#' Prep lab data
#'
#' Main lab data preparation function.
#' Requires Excel files from WHO saved to a local file path.
#' Expects the names of Excel files to contain the pattern `[Region] Lab Extract`
#' and the Sheets within the Excel files to contain the pattern `[Region] Lab data`
#'
#' uploads to EDAV if use_edav = TRUE, otherwise, returns the lab data frame as an output that can be written as a CSV file to a local directory
#'
#' Will also output a CSV file with cases with implausible dates to the working directory if output_lab_checks=TRUE
#'
#' @param lab_data_path `string` filepath to lab datasets.
#' @param afp_data `tibble` AFP linelist. If using tidypolis, use `raw_data$afp`.
#' @param lab_locs_path `str` Location of testing lab locations. Default is `NULL`. Will download from EDAV, if use_edav=TRUE.
#' @param use_edav `logical` Whether to obtain lab locs data from EDAV Defaults to `TRUE`.
#' @param save_to_edav `logical` Whether to save Rda file to EDAV, Defaults to `FALSE`.
#' @param output_lab_checks `logical` Whether to save CSV file with cases with implausible dates to the working directory, Defaults to `FALSE`.
#' @returns `tibble` Cleaned lab data.
#' @examples
#' \dontrun{
#' With lab data and lab locs stored locally:
#' raw_data<-sirfunctions::get_all_polio_data(attach.spatial.data = FALSE)
#' lab_data <- prep_lab_data(
#' lab_data_path = "C:/Users/abc1/labdata/2026-06-30 Lab Extract (AFP only since 2023-30Jun2026)/",
#' afp_data = raw_data$afp,
#' lab_locs_path = "C:/Users/abc1/labdata/Routine lab testing locations.csv"
#' use_edav=FALSE,
#' )
#'
#' # Using EDAV for lab locs:
#' raw_data<-sirfunctions::get_all_polio_data(attach.spatial.data = FALSE)
#' lab_data <- prep_lab_data(
#' lab_data_path = "C:/Users/abc1/labdata/2026-06-30 Lab Extract (AFP only since 2023-30Jun2026)/",
#' afp_data = raw_data$afp,
#' lab_locs_path = NULL,
#' use_edav=TRUE
#' )
#' }
#' @export
prep_lab_data <- function(lab_data_path,
                          afp_data = NULL,
                          lab_locs_path = NULL,
                          use_edav = TRUE,
                          save_to_edav = FALSE,
                          output_lab_checks = FALSE) {

  # 1. Extract ----

  # list the Excel files in the current directory with the string "Lab Extract.xlsx" as part of the filepath, then read them in based on the WHO region
  # e.g., if the filepath contains EMRO, read in the sheet "EMRO Lab data (AFP)"
  # loop over all the files
  # flexible in case we ever get WPRO data

  who_regions = c("AFRO", "EMRO", "WPRO", "EURO", "SEARO", "PAHO")
  region_pattern <- paste0("(", paste(who_regions, collapse = "|"), ").*Lab Extract")

  # list the files following the pattern "[region] Lab Extract"
  files <- list.files(
    path = lab_data_path,
    pattern = paste0(region_pattern, ".*\\.(xlsx|csv)$"),
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(files) == 0) {
    warning("No matching files found in ", lab_data_path)
    return(character(0))
  }

  # create empty list for collecting the lab data
  lab_data_list <- list()

  cli::cli_process_start("Extracting data from Excel files")
  # loop over the regions files, pull out the data into an element of a list
  for (region in who_regions) {

    # find files matching this specific region + "Lab Extract"
    pattern <- paste0(region, ".*Lab Extract")
    matched_files <- files[stringr::str_detect(basename(files), stringr::regex(pattern, ignore_case = TRUE))]

    if (length(matched_files) == 0) {
      warning("No file found for region: ", region)
      next
    }

    if (length(matched_files) > 1) {
      # extract YYYY-MM-DD from each filename and pick the most recent
      file_dates <- lubridate::as_date(stringr::str_extract(basename(matched_files), "\\d{4}-\\d{2}-\\d{2}"))

      if (any(is.na(file_dates))) {
        warning("Multiple files found for region ", region,
                " but not all filenames contain a valid YYYY-MM-DD date - using the first: ",
                basename(matched_files[1]))
        f <- matched_files[1]
      } else {
        # get the date of the latest file to check for cases with future dates entered in the system
        file_date <- file_dates[which.max(file_dates)]
        f <- matched_files[which.max(file_dates)]
        warning("Multiple files found for region ", region, " - using the most recent: ", basename(f))
      }
    } else {
      # even if only one file, still need the date of the file to compare against the date values
      file_date <- lubridate::as_date(stringr::str_extract(basename(matched_files), "\\d{4}-\\d{2}-\\d{2}"))
      f <- matched_files[1]
    }

    # construct the expected sheet name ("[Region] Lab data") if the file is an Excel
    sheet_name <- NULL
    if (stringr::str_ends(f, "//.xlsx")) {
      sheet_names <- readxl::excel_sheets(f)
      target_sheet <- sheet_names[stringr::str_detect(sheet_names, stringr::regex(paste0(region, ".*Lab data"), ignore_case = TRUE))]

      if (length(target_sheet) == 0) {
        warning("No matching Lab data sheet found in ", basename(f))
        next
      }
      sheet_name <- target_sheet[1]
    }

    # if there's no file date, default to the current date
    if (is.na(file_date)){
      file_date=Sys.Date()
    }
    # load in the data from the sheet for this region
    df <- sirfunctions::load_lab_data(f, sheet_name = sheet_name) |>
      dplyr::mutate(source_file = basename(f), whoregion = region, download_date=file_date)

    # convert any column with "date" in its name to Date type using the robust data parser function
    date_cols <- names(df)[stringr::str_detect(names(df), stringr::regex("date", ignore_case = TRUE))]
    df <- df |>
      dplyr::mutate(dplyr::across(dplyr::any_of(date_cols), parse_lab_date))

    # add to the list of data tables
    lab_data_list[[region]] <- df
  }
  cli::cli_process_done()

  # drop any regions that had no file (keep only successfully read ones)
  lab_data_list <- lab_data_list[!sapply(lab_data_list, is.null)]

  # confirm all the files have the same column names
  reference_region <- names(lab_data_list)[1]
  reference_cols <- names(lab_data_list[[1]])

  for (region in names(lab_data_list)) {
    current_cols <- names(lab_data_list[[region]])

    if (!identical(current_cols, reference_cols)) {
      cli::cli_alert_danger("Mismatch in region: {region}")
      cli::cli_alert_info("  In {region} but not {reference_region}: {setdiff(current_cols, reference_cols)}")
      cli::cli_alert_info("  In {reference_region} but not {region}: {setdiff(reference_cols, current_cols)}")
    } else {
      cli::cli_alert_success("Match: {region}")
    }
  }

  # smush
  lab_data <- dplyr::bind_rows(lab_data_list)


  # 2. Quality Check ----

  cli::cli_process_start("Starting lab data checks")

  # 2.1 Check dates -----
  lab_date_cols <- c(
    "CaseDate",
    "ParalysisOnsetDate",
    "DateStoolCollected",
    "StoolDateSentToLab",
    "DateStoolReceivedinLab",
    "DateFinalCellCultureResult",
    "DateFinalrRTPCRResults",
    "ReportDateSequenceResultSent",
    "DateIsolateRcvdForSeq",
    "DateLArmIsolate",
    "DateRArmIsolate",
    "DateofSequencing",
    "DateNotificationtoHQ"
  )

  if (length(dplyr::setdiff(lab_date_cols, names(lab_data))>0)) {
    cli::cli_alert_info(paste0("Some expected date columns missing from lab data: ", dplyr::setdiff(lab_date_cols, names(lab_data))))
  }

  # look for dates after the date of the extract - using the date of the extract from the filename
  lab_checks <- lab_data |>
    dplyr::mutate(dplyr::across(
      dplyr::contains("date", ignore.case = TRUE) & !dplyr::any_of("download_date"),
      ~ .x > download_date,
      .names = "flag_{.col}"
    ),
    # look for dates that come before dates that shouldn't
    bad_stool_before_onset =
      !is.na(DateStoolCollected) &
      !is.na(ParalysisOnsetDate) &
      DateStoolCollected < ParalysisOnsetDate,

    bad_stool_far_after_onset =
      !is.na(DateStoolCollected) &
      !is.na(ParalysisOnsetDate) &
      DateStoolCollected > ParalysisOnsetDate + lubridate::days(60),

    bad_sent_before_collected =
      !is.na(StoolDateSentToLab) &
      !is.na(DateStoolCollected) &
      StoolDateSentToLab < DateStoolCollected,

    bad_sent_after_received =
      !is.na(StoolDateSentToLab) &
      !is.na(DateStoolReceivedinLab) &
      StoolDateSentToLab > DateStoolReceivedinLab,

    bad_received_before_collected =
      !is.na(DateStoolReceivedinLab) &
      !is.na(DateStoolCollected) &
      DateStoolReceivedinLab < DateStoolCollected,

    bad_culture_before_received =
      !is.na(DateFinalCellCultureResult) &
      !is.na(DateStoolReceivedinLab) &
      DateFinalCellCultureResult < DateStoolReceivedinLab,

    bad_culture_far_after_received =
      !is.na(DateFinalCellCultureResult) &
      !is.na(DateStoolReceivedinLab) &
      DateFinalCellCultureResult > DateStoolReceivedinLab + lubridate::days(180),

    bad_seq_received_before_culture =
      !is.na(DateIsolateRcvdForSeq) &
      !is.na(DateFinalCellCultureResult) &
      DateIsolateRcvdForSeq < DateFinalCellCultureResult,

    bad_sequencing_before_seq_received =
      !is.na(DateofSequencing) &
      !is.na(DateIsolateRcvdForSeq) &
      DateofSequencing < DateIsolateRcvdForSeq
    ) |>
    dplyr::filter(dplyr::if_any(dplyr::starts_with("flag_") | dplyr::starts_with("bad_")))

  # counts of how many cases had bad dates
  # dplyr::summarise(lab_checks, dplyr::across(dplyr::starts_with("bad_") | starts_with("flag_"), ~ sum(.x, na.rm = TRUE)))  |>
  #   tidyr::pivot_longer(cols = dplyr::everything(), names_to = "flag_column", values_to = "n_true")

  # output any data quality issues

  # look for any EPIDs that are not in the AFP data

  # check if EPID starts with ' and then remove the ' if it is there!
  lab_data <- lab_data |>
    dplyr::mutate(EPID = stringr::str_remove(EPID, "^'"))

  missing_epids <- lab_data |> dplyr::filter(!(EPID %in% afp_data$epid))

  cli::cli_alert_info(paste0("There are ", nrow(missing_epids), " EPIDs in the lab data that are not in the AFP data"))

  # Write QA file for the bad dates for manual review.
  if(output_lab_checks) {
    readr::write_csv(
      lab_checks |>
        dplyr::select(
          dplyr::any_of(c("source_file", "row_id")),
          EPID,
          SpecimenNumber,
          Name,
          contains("date", ignore.case=TRUE),
          starts_with("flag_"),
          starts_with("bad_")),
      paste0("qa_bad_lab_dates_", Sys.Date(), ".csv")
    )
    readr::write_csv(
      missing_epids |>
        dplyr::select(
          dplyr::any_of(c("source_file", "row_id")),
          EPID,
          SpecimenNumber,
          Name),
      paste0("qa_missing_epids_", Sys.Date(), ".csv")
    )

  }
  cli::cli_process_done()

  # 3. Clean ----
  # IMPORTANT: wait to put WHO region and lab_locs file on until the very end

  # 3.1 steps from create_cleaned_lab_data.R ----

  # Repair clearly impossible dates by setting them to NA
  cli::cli_process_start("Cleaning date variables")

  lab_data <- lab_data |>
    #replace dates with NA if they occurred after the download date
    dplyr::mutate(dplyr::across(
      dplyr::contains("date", ignore.case = TRUE) & !dplyr::any_of("download_date"),
      ~ dplyr::if_else(.x > download_date, as.Date(NA), .x)
    )) |>

    dplyr::mutate(
      # Stool collected should not be before onset or implausibly far after onset.
      DateStoolCollected = dplyr::if_else(
        !is.na(DateStoolCollected) &
          !is.na(ParalysisOnsetDate) &
          (
            DateStoolCollected < ParalysisOnsetDate |
              DateStoolCollected > ParalysisOnsetDate + lubridate::days(60)
          ),
        as.Date(NA),
        DateStoolCollected
      ),

      # Stool sent should not be before collection or after lab receipt when those dates exist.
      StoolDateSentToLab = dplyr::if_else(
        !is.na(StoolDateSentToLab) &
          (
            (!is.na(DateStoolCollected) & StoolDateSentToLab < DateStoolCollected) |
              (!is.na(DateStoolReceivedinLab) & StoolDateSentToLab > DateStoolReceivedinLab)
          ),
        as.Date(NA),
        StoolDateSentToLab
      ),

      # Lab receipt should not be before stool collection.
      DateStoolReceivedinLab = dplyr::if_else(
        !is.na(DateStoolReceivedinLab) &
          !is.na(DateStoolCollected) &
          DateStoolReceivedinLab < DateStoolCollected,
        as.Date(NA),
        DateStoolReceivedinLab
      ),

      # Final culture should not be before lab receipt or implausibly far after lab receipt.
      DateFinalCellCultureResult = dplyr::if_else(
        !is.na(DateFinalCellCultureResult) &
          !is.na(DateStoolReceivedinLab) &
          (
            DateFinalCellCultureResult < DateStoolReceivedinLab |
              DateFinalCellCultureResult > DateStoolReceivedinLab + lubridate::days(180)
          ),
        as.Date(NA),
        DateFinalCellCultureResult
      ),

      # Sequencing receipt should not be before final culture when both are present.
      DateIsolateRcvdForSeq = dplyr::if_else(
        !is.na(DateIsolateRcvdForSeq) &
          !is.na(DateFinalCellCultureResult) &
          DateIsolateRcvdForSeq < DateFinalCellCultureResult,
        as.Date(NA),
        DateIsolateRcvdForSeq
      ),

      # Sequencing date should not be before isolate receipt when both are present.
      DateofSequencing = dplyr::if_else(
        !is.na(DateofSequencing) &
          !is.na(DateIsolateRcvdForSeq) &
          DateofSequencing < DateIsolateRcvdForSeq,
        as.Date(NA),
        DateofSequencing
      )
    ) |>
    dplyr::mutate(
      # clean case date
      CaseDate = dplyr::coalesce(CaseDate, ParalysisOnsetDate),
      ParalysisOnsetDate = dplyr::coalesce(ParalysisOnsetDate, CaseDate),
    )
  cli::cli_process_done()

  # Standardize NULL values
  # Desired convention:
  # - FinalITDResult: "NULL" -> blank string ""

  if ("FinalITDResult" %in% names(lab_data)) {
    lab_data <- lab_data |>
      dplyr::mutate(
        FinalITDResult = {
          x <- as.character(FinalITDResult)
          x <- trimws(x)
          x[!is.na(x) & x %in% c("NULL", "null")] <- ""
          x
        }
      )
  }

  if ("FinalCellCultureResult" %in% names(lab_data)) {
    lab_data <- lab_data |>
      dplyr::mutate(
        FinalCellCultureResult = {
          x <- as.character(FinalITDResult)
          x <- trimws(x)
          x[!is.na(x) & x %in% c("NULL", "null")] <- ""
          x
        }
      )
  }
  # 3.2 steps from clean_lab_data_regional() ----

  # drop EPID/Specimen number duplicates - look for the row with the most complete information
  cli::cli_process_start("Starting deduplication")

  lab_data_distinct <- lab_data |>
    # drop any rows that are entirely duplicates
    dplyr::distinct() |>
    # drop rows with Specimen number not in 1 or 2
    dplyr::filter(SpecimenNumber %in% c(1, 2)) |>
    # fill in missing dates within the EPID/Specimen combination
    dplyr::group_by(EPID, SpecimenNumber) |>
    # flag EPID/Specimen combos that have multiple rows, for later review
    dplyr::mutate(n_rows_combo = dplyr::n(),
                  has_duplicate_combo = n_rows_combo > 1) |>
    # assume missing dates are the same as other specimen rows in the data
    tidyr::fill(ParalysisOnsetDate, DateStoolCollected, .direction = "updown") |>
    dplyr::ungroup()

  # get number of duplicate EPID/Specimen IDs
  cli::cli_alert_info(paste0("There are ", nrow(missing_epids), " duplicate lab entries."))

  lab_data_distinct <- lab_data_distinct |>
    # drop any rows that are newly entirely duplicates
    dplyr::distinct() |>
    dplyr::arrange(EPID, SpecimenNumber) |>
    # # NEW: keep the row with the most complete dates
    # dplyr::mutate(missing_dates = rowSums(is.na(dplyr::across(dplyr::all_of(lab_date_cols))))) |>
    # # NEW: keep the row with the most recent dates
    # dplyr::rowwise() |>
    # dplyr::mutate(max_lab_date = max(dplyr::c_across(dplyr::all_of(lab_date_cols)), na.rm = TRUE)) |>
    # dplyr::ungroup()
    # the code above was very slow, the syntax below is faster
    dplyr::mutate(max_lab_date = do.call(pmax, c(dplyr::across(dplyr::all_of(lab_date_cols)), na.rm = TRUE)))


  lab_data_distinct <- lab_data_distinct |>
    # sort within EPID and Specimen number to get the latest date
    dplyr::arrange(EPID, SpecimenNumber, desc(max_lab_date))

  # now this is sorted so it chooses the top case with the least number of missing dates (change from how previous clean_lab_data() function worked)
  lab_data2 <- lab_data_distinct[!duplicated(lab_data_distinct[c("EPID", "SpecimenNumber")]), ]

  cli::cli_process_done()

  # track MAD-AND-ABV-22-609 to figure out how to choose the right row to keep
  # dplyr::filter(lab_data2, EPID=="MAD-AND-ABV-22-609") |>
  #   dplyr::select(missing_dates,
  #          SpecimenNumber,
  #          DateNotificationtoHQ,
  #          DateofSequencing,
  #          DateRArmIsolate,
  #          DateLArmIsolate,
  #          DateIsolateRcvdForSeq,
  #          ReportDateSequenceResultSent,
  #          DateFinalrRTPCRResults,
  #          DateFinalCellCultureResult,
  #          DateStoolReceivedinLab,
  #          StoolDateSentToLab,
  #          DateStoolCollected,
  #          ParalysisOnsetDate) |>
  #   View()

  cli::cli_process_start("Creating calculated variables and filtering out negative time intervals")

  lab_data2 <- lab_data2 |>
    # drop columns
    dplyr::select(-max_lab_date) |>
    # create intervals
    dplyr::mutate(
      # Intervals from stool arrival to sequencing
      ## timeliness of stool collection to arrival in lab
      days.collect.lab = DateStoolReceivedinLab - DateStoolCollected,

      ## timeliness of stool arriving in lab to final culture results
      days.lab.culture = DateFinalCellCultureResult - DateStoolReceivedinLab,

      ## timeliness of final culture results to arrival at the sequencing lab
      days.seq.ship = DateIsolateRcvdForSeq - DateFinalCellCultureResult,

      ## timeliness of arrival at sequencing lab to sequencing results
      days.seq.rec.res = DateofSequencing - DateIsolateRcvdForSeq,,

      # Interval measuring sequencing results from date of arrival (NOT part of KPI
      ## timeliness of ITD results to arrival at sequencing lab
      days.itd.arriveseq = DateIsolateRcvdForSeq - DateFinalrRTPCRResults,

      ## timeliness of ITD results to sequencing results
      days.itd.seqres = DateofSequencing - DateFinalrRTPCRResults,

      # Measures overall lab timeliness
      ## timeliness of arriving in lab to sequencing
      days.lab.seq = DateofSequencing - DateStoolReceivedinLab,

      # Met target yes/no
      met.targ.collect.lab = ifelse(days.collect.lab < 3, 1, 0),
      negative.spec = ifelse(!stringr::str_detect(FinalCellCultureResult, "ITD") &
                               FinalITDResult == "", 1, 0),
      met.lab.culture = ifelse(days.lab.culture < 14, 1, 0),
    ) |>

    # filtering out negative time intervals and nonsensical dates
    # filtering out negative time intervals
    dplyr::filter((days.collect.lab >= 0 | is.na(days.collect.lab)) &
                    (days.lab.culture >= 0 | is.na(days.lab.culture)) &
                    (days.seq.ship >= 0 | is.na(days.seq.ship)) &
                    (days.lab.seq >= 0 | is.na(days.lab.seq)) &
                    (days.itd.seqres >= 0 | is.na(days.itd.seqres)) &
                    (days.itd.arriveseq >= 0 | is.na(days.itd.arriveseq)) &
                    (days.seq.rec.res >= 0 | is.na(days.seq.rec.res))) |>
    # Filtering nonsensical dates
    dplyr::filter(
      (DateStoolCollected >= ParalysisOnsetDate | is.na(ParalysisOnsetDate)),
      # remove a blank specimen row
      !is.na(EPID)
    ) |>

    # adding additional subintervals (these aren't present in regional lab data, so are created as dummy variables)
    dplyr::mutate(
      days.coll.sent.field = NA,
      days.sent.field.rec.nat = NA,
      days.rec.nat.sent.lab = NA,
      days.sent.lab.rec.lab = NA,
      days.rec.lab.culture = NA,
      # need to change cote d'ivoire so later merges work
      Name = ifelse(stringr::str_detect(Name, "IVOIRE"),
                    "COTE D IVOIRE", Name),
      #it also needs year for the merge with geos
      year = lubridate::year(ParalysisOnsetDate)
    ) |>
    # these columns are also removed at some point in the original cleaning code
    dplyr::select(-dplyr::contains("cIntratypeIs"))
  cli::cli_process_done()

  # 4. Merge additional columns ----
  cli::cli_process_start("Adding GEOs and Lab locations")

  # impute missing lab geo

  lab_data2 <- impute_missing_lab_geo(lab_data2, afp_data)


  # add lab locs
  lab_locs <- sirfunctions::get_lab_locs(lab_locs_path, use_edav)

  lab_data3 <- lab_data2 |>
    # rename columns for merging
    dplyr::rename(
      country = Name,
      EPID = EpidNumber
    ) |>
    dplyr::left_join(
      lab_locs |> dplyr::select("country":"num.ship.seq.samples")
    ) |>
    # change seq.capacity column wording (this has implications for when we use this file downstream in get_seq_capacity())
    dplyr::mutate(seq.capacity = dplyr::if_else(seq.capacity == "yes",
                                                "Sequencing capacity",
                                                "No sequencing capacity"),
                  #list of culture labs that sent samples to CDC for sequencing prior to February 2025, Nigeria and Uganda started doing their own sequencing.
                  #Adding in redundancy for Nigeria in case lab locs file gets changed
                  seq.lab = dplyr::case_when(
                    # labs that previously shipped to Atlanta for sequencing
                    DateStoolCollected < lubridate::as_date("2025-02-01") & culture.itd.lab %in% c("Cameroon","KEMRI-Kenya","IBADAN-Nigeria","Ibadan-Nigeria","Nigeria","Senegal","Ethiopia","Oman/Jordan") ~ "CDC-Atlanta",
                    # lab that used to sequence Uganda before UVRI was accredited
                    DateStoolCollected < lubridate::as_date("2025-02-01") & country == "UGANDA" ~ "Noguchi-Ghana",
                    .default = seq.lab),
                  seq.cat = dplyr::if_else(country %in% c("NIGERIA", "UGANDA") & DateStoolCollected < lubridate::as_date("2025-02-01"), "Shipped for sequencing", seq.cat),
                  seq.capacity = dplyr::if_else(country %in% c("NIGERIA", "UGANDA") & DateStoolCollected < lubridate::as_date("2025-02-01"), "No sequencing capacity", seq.capacity)
    ) |>
    #new step: all countries without values for seq.capacity in lab locs are No sequencing capacity/shipped for seque by default
    dplyr::mutate(
      seq.capacity = dplyr::if_else(is.na(seq.capacity), "No sequencing capacity", seq.capacity),
      seq.cat = dplyr::if_else(is.na(seq.cat) & seq.capacity=="No sequencing capacity", "Shipped for sequencing", seq.cat)
    )
  cli::cli_process_done()

  # 5. Output or Save to EDAV -----

  # If save_to_edav is TRUE save to EDAV, otherwise return the data frame as an output of the function

  if (save_to_edav) {
    # COMMENTED OUT FOR NOW TO AVOID DISASTER
    # tidypolis_io(obj = lab_data3, io = "write", file_path = "Data/lab/cleaned_lab_data.rda")
  }
  else {
    cli::cli_alert_info("save_to_edav=FALSE, returning lab data as output object")
    return(lab_data3)
  }

}
