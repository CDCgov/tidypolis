#' Sample points for missing lat/lon
#' @description Validate AFP geography and sample points for missing GPS data.
#' @param df01 `tibble` table of afp data
#' @param global.dist.01 `sf` spatial file of all locations
#' @details District matches must agree with the source country name/GUID and
#' onset year. An existing district GUID resolves overlaps within that country;
#' unresolved matches retain their input geography and receive a `geo.review`
#' reason. Names and corrected GUIDs come from the same accepted reference row.
#' Original `admin0guid`, `admin1guid`, and `admin2guid` values are retained.
#' @returns A tibble with corrected `Admin*GUID` fields, `lat`, `lon`,
#'   `geo.corrected`, and a `geo.review` reason (`NA` for accepted matches).
#' @md
#' @keywords internal
f.pre.stsample.01 <- function(df01, global.dist.01) {
  normalize_guid <- function(x) {
    x <- toupper(gsub("[{}[:space:]]", "", as.character(x)))
    x[x %in% c("", "NA")] <- NA_character_
    result <- rep(NA_character_, length(x))
    present <- !is.na(x)
    if (any(present)) result[present] <- paste0("{", x[present], "}")
    result
  }
  normalize_name <- function(x) toupper(trimws(as.character(x)))
  different <- function(x, y) {
    (is.na(x) != is.na(y)) | (!is.na(x) & !is.na(y) & x != y)
  }

  data <- dplyr::as_tibble(sf::st_drop_geometry(df01))
  original_guids <- original_names <- vector("list", 3L)
  for (level in 0:2) {
    raw <- paste0("admin", level, "guid")
    corrected <- paste0("Admin", level, "GUID")
    original_guids[[level + 1L]] <- normalize_guid(data[[raw]])
    original_names[[level + 1L]] <- normalize_name(data[[paste0("place.admin.", level)]])
    # Keep repairs made upstream; never rebuild corrected GUIDs from raw values.
    data[[corrected]] <- if (corrected %in% names(data)) {
      dplyr::coalesce(normalize_guid(data[[corrected]]), original_guids[[level + 1L]])
    } else {
      original_guids[[level + 1L]]
    }
  }
  data$.geo_case_id <- seq_len(nrow(data))
  data$.geo_source_country <- original_guids[[1L]]
  data$.geo_source_name <- original_names[[1L]]
  data$geo.corrected <- rep(0L, nrow(data))
  data$geo.review <- rep(NA_character_, nrow(data))
  latitude <- suppressWarnings(as.numeric(data$polis.latitude))
  longitude <- suppressWarnings(as.numeric(data$polis.longitude))
  has_point <- is.finite(latitude) & is.finite(longitude) &
    abs(latitude) <= 90 & abs(longitude) <= 180 &
    !(latitude == 0 & longitude == 0)
  data$lat <- data$lon <- rep(NA_real_, nrow(data))
  data$lat[has_point] <- latitude[has_point]
  data$lon[has_point] <- longitude[has_point]
  if (!nrow(data)) {
    return(dplyr::select(data, -dplyr::starts_with(".geo_")))
  }
  if (is.na(sf::st_crs(global.dist.01))) stop("District shapes must have a CRS.")

  shapes <- global.dist.01 |>
    dplyr::mutate(
      GUID = normalize_guid(GUID), ADM0_GUID = normalize_guid(ADM0_GUID),
      ADM1_GUID = normalize_guid(ADM1_GUID), .geo_shape_id = dplyr::row_number()
    ) |>
    dplyr::select(.geo_shape_id, GUID, ADM0_GUID, ADM1_GUID,
                  ADM0_NAME, ADM1_NAME, ADM2_NAME, yr.st, yr.end)
  attributes <- dplyr::as_tibble(sf::st_drop_geometry(shapes))
  countries <- attributes |>
    dplyr::distinct(ADM0_GUID, ADM0_NAME, yr.st, yr.end) |>
    dplyr::mutate(.geo_name = normalize_name(ADM0_NAME))

  # Resolve each source country/year once. A supplied name and GUID must agree;
  # a missing GUID can be resolved only from an unambiguous country name.
  requests <- data |>
    dplyr::distinct(.geo_source_country, .geo_source_name, yronset)
  resolved <- lapply(seq_len(nrow(requests)), function(i) {
    request <- requests[i, ]
    candidates <- countries[
      which(request$yronset >= countries$yr.st & request$yronset <= countries$yr.end), ]
    if (!is.na(request$.geo_source_country)) {
      candidates <- candidates[which(candidates$ADM0_GUID == request$.geo_source_country), ]
    } else if (is.na(request$.geo_source_name) || request$.geo_source_name == "") {
      candidates <- candidates[0, ]
    }
    if (!is.na(request$.geo_source_name) && request$.geo_source_name != "") {
      candidates <- candidates[which(candidates$.geo_name == request$.geo_source_name), ]
    }
    candidates <- dplyr::distinct(candidates, ADM0_GUID, .geo_name)
    valid <- nrow(candidates) == 1L && !is.na(candidates$ADM0_GUID[1L])
    request$.geo_expected_country <- if (valid) candidates$ADM0_GUID[1L] else NA_character_
    request$.geo_expected_name <- if (valid) candidates$.geo_name[1L] else NA_character_
    request$geo.review <- if (valid) NA_character_ else "unresolved_source_country"
    request
  })
  data <- data |>
    dplyr::select(-geo.review) |>
    dplyr::left_join(dplyr::bind_rows(resolved),
      by = c(".geo_source_country", ".geo_source_name", "yronset"))

  # Keep the existing planar fallback for invalid spherical geometry. Omitting
  # the CRS only during GEOS topology repair preserves the original coordinates.
  repair_geos <- function(x) {
    crs <- sf::st_crs(x)
    geometry <- sf::st_make_valid(sf::st_set_crs(sf::st_geometry(x), NA))
    sf::st_geometry(x) <- sf::st_set_crs(geometry, crs)
    x
  }
  spatial_join <- function(points, polygons) {
    old_s2 <- sf::sf_use_s2()
    on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
    tryCatch(
      sf::st_join(points, sf::st_make_valid(polygons), left = FALSE),
      error = function(e) {
        suppressMessages(sf::sf_use_s2(FALSE))
        sf::st_join(points, repair_geos(polygons), left = FALSE)
      }
    )
  }
  country_candidates <- function(x) {
    x |>
      dplyr::filter(
        !is.na(GUID), ADM0_GUID == .geo_expected_country,
        normalize_name(ADM0_NAME) == .geo_expected_name,
        yronset >= yr.st, yronset <= yr.end
      ) |>
      # Duplicate versions with identical metadata are not distinct choices.
      dplyr::distinct(.geo_case_id, GUID, ADM0_GUID, ADM1_GUID,
                      ADM0_NAME, ADM1_NAME, ADM2_NAME, .keep_all = TRUE)
  }
  apply_matches <- function(data, matches) {
    rows <- match(matches$.geo_case_id, data$.geo_case_id)
    fields <- c(Admin0GUID = "ADM0_GUID", Admin1GUID = "ADM1_GUID", Admin2GUID = "GUID",
                place.admin.0 = "ADM0_NAME", place.admin.1 = "ADM1_NAME", place.admin.2 = "ADM2_NAME")
    for (field in names(fields)) data[[field]][rows] <- matches[[fields[[field]]]]
    data$geo.review[rows] <- NA_character_
    data
  }

  point_rows <- which(has_point & !is.na(data$.geo_expected_country))
  if (length(point_rows)) {
    cli::cli_process_start("Matching AFP coordinates within their source countries")
    data$geo.review[point_rows] <- "no_country_spatial_match"
    points <- data[point_rows, ] |>
      dplyr::select(.geo_case_id, yronset, Admin2GUID,
                    .geo_expected_country, .geo_expected_name, lat, lon) |>
      sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
      sf::st_transform(sf::st_crs(shapes))
    point_shapes <- shapes[shapes$ADM0_GUID %in% data$.geo_expected_country[point_rows], ]
    candidates <- spatial_join(points, point_shapes) |>
      sf::st_drop_geometry() |>
      dplyr::as_tibble() |>
      country_candidates() |>
      dplyr::mutate(.geo_district_match = !is.na(Admin2GUID) & Admin2GUID == GUID) |>
      dplyr::group_by(.geo_case_id) |>
      dplyr::filter(!any(.geo_district_match) | .geo_district_match) |>
      dplyr::ungroup() |>
      dplyr::add_count(.geo_case_id, name = ".geo_n")
    ambiguous <- candidates$.geo_case_id[candidates$.geo_n > 1L]
    data$geo.review[data$.geo_case_id %in% ambiguous] <- "ambiguous_district_match"
    data <- apply_matches(data, candidates[candidates$.geo_n == 1L, ])
    cli::cli_process_done()
  }

  # Missing-coordinate sampling uses the same country/year rule and a known
  # district GUID. Group by reference row so each input gets exactly one point.
  missing_rows <- which(!has_point & !is.na(data$.geo_expected_country))
  if (length(missing_rows)) {
    data$geo.review[missing_rows] <- "missing_district_guid"
    eligible <- missing_rows[!is.na(data$Admin2GUID[missing_rows])]
    data$geo.review[eligible] <- "no_country_district_match"
    candidates <- data[eligible, ] |>
      dplyr::select(.geo_case_id, yronset, Admin2GUID,
                    .geo_expected_country, .geo_expected_name) |>
      dplyr::inner_join(attributes, by = c("Admin2GUID" = "GUID"),
                        keep = TRUE, na_matches = "never", relationship = "many-to-many") |>
      country_candidates() |>
      dplyr::add_count(.geo_case_id, name = ".geo_n")
    ambiguous <- candidates$.geo_case_id[candidates$.geo_n > 1L]
    data$geo.review[data$.geo_case_id %in% ambiguous] <- "ambiguous_district_match"
    accepted <- candidates[candidates$.geo_n == 1L, ]
    for (shape_id in unique(accepted$.geo_shape_id)) {
      matches <- accepted[accepted$.geo_shape_id == shape_id, ]
      polygon <- shapes[shapes$.geo_shape_id == shape_id, ]
      sampled <- tryCatch({
        old_s2 <- sf::sf_use_s2()
        sample_geometry <- tryCatch(
          sf::st_sample(sf::st_make_valid(polygon), size = nrow(matches), exact = TRUE),
          error = function(e) {
            suppressMessages(sf::sf_use_s2(FALSE))
            tryCatch(
              sf::st_sample(repair_geos(polygon), size = nrow(matches), exact = TRUE),
              finally = suppressMessages(sf::sf_use_s2(old_s2))
            )
          }
        )
        sf::st_coordinates(sf::st_transform(sample_geometry, 4326))
      }, error = function(e) NULL)
      rows <- match(matches$.geo_case_id, data$.geo_case_id)
      if (is.null(sampled) || nrow(sampled) != nrow(matches) || any(!is.finite(sampled))) {
        data$geo.review[rows] <- "coordinate_sampling_failed"
      } else {
        data <- apply_matches(data, matches)
        data$lon[rows] <- sampled[, "X"]
        data$lat[rows] <- sampled[, "Y"]
      }
    }
  }

  # Raw GUIDs remain available for auditing; corrected fields and their names
  # are already paired, so no second, potentially incomplete name join is needed.
  changed <- rep(FALSE, nrow(data))
  for (level in 0:2) {
    changed <- changed |
      different(original_guids[[level + 1L]], data[[paste0("Admin", level, "GUID")]]) |
      different(original_names[[level + 1L]], normalize_name(data[[paste0("place.admin.", level)]]))
  }
  data$geo.corrected <- as.integer(changed)
  data |>
    dplyr::select(-dplyr::starts_with(".geo_"),
                  -dplyr::any_of(c("wrongAdmin0GUID", "wrongAdmin1GUID", "wrongAdmin2GUID")))
}
