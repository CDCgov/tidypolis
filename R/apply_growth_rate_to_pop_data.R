# Private ----

#' Calculate pop counts for new year
#'
#' @description
#' Applies the growth rate from the previous year to calculate new populations
#' for the new year.
#'
#' @param pop_w_gr `tibble` Population file with growth rates.
#'
#' @returns `tibble` New year population data.
#' @keywords internal
#'
get_new_year_pop <- function(pop_w_gr) {
  # Forward fill growth rates
  pop_w_gr <- pop_w_gr |>
    dplyr::group_by(ADM0_NAME) |>
    dplyr::arrange(year, .by_group = TRUE) |>
    tidyr::fill(growth_rate, .direction = "downup") |>
    dplyr::ungroup()

  # Obtain growth rates for the new year
  pop_new_year <- pop_w_gr |>
    dplyr::filter(year == lubridate::year(Sys.Date()) - 1) |>
    dplyr::mutate(year = lubridate::year(Sys.Date())) |>
    dplyr::mutate(across(dplyr::any_of(c("u15pop", "u5pop", "totpop")),
                         \(x) x * (1+growth_rate)))

  # Join new year data to ctry pop
  pop_final <- dplyr::bind_rows(pop_w_gr, pop_new_year) |>
    dplyr::arrange(year) |>
    dplyr::select(-growth_rate)

  return(pop_final)
}

#' Load population growth rates
#'
#' @details
#' Loads and formats the population growth rate file for use in R.
#'
#'
#' @param growth_path `str` Path to WPP Excel file.
#' @details
#' The Excel file uses the "Estimates" tab. The first few rows (16 rows or so) are deleted for ease of loading into
#' R. The source of this dataset is in the [World Population Prospects](https://population.un.org/wpp/downloads?folder=Standard%20Projections&group=Most%20used)
#' website, using the "Compact" file.
#'
#' @return `tibble` Growth rates for each country by year.
#' @keywords internal
load_growth_rates <- function(
    growth_path,
    edav
) {

  growth_rates <- tidypolis_io(io = "read", file_path = growth_path,
                              edav = edav, edav_default_dir = NULL)
  growth_rates <- growth_rates$Estimates

  # Select and standardize output
  growth_rates <- growth_rates |>
    dplyr::select(
      Admin0Name = `Region, subregion, country or area *`,
      year = Year,
      growth_rate = `Population Growth Rate (percentage)`
    ) |>
    dplyr::filter(
      !is.na(Admin0Name),
      Admin0Name != "REGION, SUBREGION, COUNTRY OR AREA *"
    ) |>
    dplyr::mutate(
      Admin0Name = toupper(Admin0Name),
      Admin0Name = dplyr::case_when(
        stringr::str_detect(Admin0Name, "IVOIRE") ~
          "COTE D IVOIRE",
        Admin0Name == "UNITED KINGDOM" ~
          "THE UNITED KINGDOM",
        Admin0Name == "DEM. PEOPLE'S REPUBLIC OF KOREA" ~
          "DEMOCRATIC PEOPLE'S REPUBLIC OF KOREA",
        Admin0Name == "STATE OF PALESTINE" ~
          "OCCUPIED PALESTINIAN TERRITORY, INCLUDING EAST JERUSALEM",
        TRUE ~ Admin0Name
      ),
      year = as.numeric(year),
      growth_rate = as.numeric(growth_rate) / 100) |> # convert to decimal
    dplyr::arrange(Admin0Name, year)

  cli::cli_alert_info("Note: Growth rates are in decimal form, not percentages!")

  return(growth_rates)

}
# Public ----

apply_growth_rate_to_pop_data <- function(pop_dir = "GID/PEB/SIR/Data/pop",
                                          ctry_pop_path = file.path(pop_dir, "ctry.pop.long"),
                                          prov_pop_path = file.path(pop_dir, "prov.pop.long"),
                                          dist_pop_path = file.path(pop_dir, "dist.pop.long"),
                                          growth_path = file.path(pop_dir,
                                                                  "pop raw/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx"),
                                          edav = TRUE) {

  # Get names of the pop files
  pop_files <- dplyr::tibble(file_name = tidypolis_io(io = "list", file_path = pop_dir,
                                                       edav = edav, edav_default_dir = NULL))
  ctry_file <- pop_files |>
    dplyr::filter(stringr::str_detect(file_name, ctry_pop_path)) |>
    dplyr::pull()
  prov_file <- pop_files |>
    dplyr::filter(stringr::str_detect(file_name, prov_pop_path)) |>
    dplyr::pull()
  dist_file <- pop_files |>
    dplyr::filter(stringr::str_detect(file_name, dist_pop_path)) |>
    dplyr::pull()

  # Load growth rate file
  gr <- load_growth_rates(growth_path, edav)

  # Load country pop
  ctry_pop <- tidypolis_io(io = "read", file_path = ctry_file,
                           edav = edav, edav_default_dir = NULL)
  prov_pop <- tidypolis_io(io = "read", file_path = prov_file,
                           edav = edav, edav_default_dir = NULL)
  dist_pop <- tidypolis_io(io = "read", file_path = dist_file,
                           edav = edav, edav_default_dir = NULL)

  # Join growth rates based on ADM0_NAME
  ctry_w_gr <- dplyr::left_join(ctry_pop, gr, by = c("ADM0_NAME" = "Admin0Name",
                                                     "year"))
  prov_w_gr <- dplyr::left_join(prov_pop, gr, by = c("ADM0_NAME" = "Admin0Name",
                                                     "year"))
  dist_w_gr <- dplyr::left_join(dist_pop, gr, by = c("ADM0_NAME" = "Admin0Name",
                                                     "year"))

  # Create population with new growth rates
  final_ctry <- get_new_year_pop(ctry_w_gr)
  final_prov <- get_new_year_pop(prov_w_gr)
  final_dist <- get_new_year_pop(dist_w_gr)

  # Output results with unofficial suffix to indicate that the new year's pop
  # are calculated using growth rates rather than being obtained directly from
  # WHO

  final_ctry |> tidypolis_io(io = "write",
                             file_path = file_path(pop_dir, "ctry.pop_long_unofficial.rds"),
                             edav = edav,
                             edav_default_dir = NULL)
  final_prov |> tidypolis_io(io = "write",
                             file_path = file_path(pop_dir, "prov.pop_long_unofficial.rds"),
                             edav = edav,
                             edav_default_dir = NULL)
  final_dist |> tidypolis_io(io = "write",
                             file_path = file_path(pop_dir, "dist.pop_long_unofficial.rds"),
                             edav = edav,
                             edav_default_dir = NULL)

}
