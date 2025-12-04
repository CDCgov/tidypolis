#' Sample points for missing lat/lon
#' @description Create random samples of points for missing GPS data
#' @param df01 `tibble` table of afp data
#' @param global.dist.01 `sf` spatial file of all locations
#' @returns tibble with lat/lon for all unsampled locations
#' @keywords internal
f.pre.stsample.01 <- function(df01, global.dist.01) {
  # need to identify cases with no lat/lon
  empty.coord <- df01 |>
    dplyr::filter(is.na(polis.latitude) | is.na(polis.longitude) |
                    (polis.latitude == 0 & polis.longitude == 0))

  cli::cli_process_start("Spatially joining AFP cases to global districts")
  # create sf object from lat lon and make global.dist valid
  df01.sf <- df01 |>
    dplyr::filter(!epid %in% empty.coord$epid) |>
    dplyr::mutate(
      lon = polis.longitude,
      lat = polis.latitude
    ) |>
    sf::st_as_sf(
      coords = c(x = "lon", y = "lat"),
      crs = sf::st_crs(global.dist.01)
    )

  global.dist.02 <- sf::st_make_valid(global.dist.01)

  # identify bad shape rows after make_valid
  check.dist.2 <- dplyr::as_tibble(sf::st_is_valid(global.dist.02))

  # removing all bad shapes post make valid
  valid.shapes <- global.dist.02[check.dist.2$value, ] |>
    dplyr::select(GUID, ADM1_GUID, ADM0_GUID, yr.st, yr.end, Shape)

  cli::cli_process_start("Evaluating invalid district shapes")
  # invalid shapes for which we'll turn off s2
  invalid.shapes <- global.dist.02[!check.dist.2$value, ] |>
    dplyr::select(GUID, ADM1_GUID, ADM0_GUID, yr.st, yr.end, Shape)

  # do 2 seperate st_joins the first, df02, is for valid shapes and those attached cases
  df02 <- sf::st_join(df01.sf |>
                        dplyr::filter(!Admin2GUID %in% invalid.shapes$GUID), valid.shapes, left = T) |>
    dplyr::filter(yronset >= yr.st & yronset <= yr.end)

  # second st_join is for invalid shapes and those attached cases, turning off s2
  sf::sf_use_s2(F)
  df03 <- sf::st_join(df01.sf |>
                        dplyr::filter(!Admin2GUID %in% valid.shapes$GUID), invalid.shapes, left = T) |>
    dplyr::filter(yronset >= yr.st & yronset <= yr.end)
  sf::sf_use_s2(T)

  cli::cli_process_done()

  # bind back together df02 and df03
  df04 <- dplyr::bind_rows(df02, df03)

  cli::cli_process_done()

  # df04 has a lot of dupes due to overlapping shapes, need to appropriately de dupe
  # identify duplicate obs
  dupes <- df04 |>
    dplyr::group_by(epid) |>
    dplyr::mutate(n = n()) |>
    dplyr::ungroup() |>
    dplyr::filter(n > 1)

  # duplicate obs where adm2guid matches GUID in shapefile
  dupes.01 <- dupes |>
    dplyr::filter(Admin2GUID == GUID) |>
    dplyr::select(-n)

  # duplicate obs where adm2guid is NA or doesn't match to shapefile
  dupes.02 <- dupes |>
    dplyr::filter(!epid %in% dupes.01$epid) |>
    dplyr::group_by(epid) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(-n)

  # fixed duplicates
  dupes.fixed <- dplyr::bind_rows(dupes.01, dupes.02)

  rm(dupes, dupes.01, dupes.02)

  # remove the duplicate cases from df04 and bind back the fixed dupes
  df05 <- df04 |>
    dplyr::filter(!epid %in% dupes.fixed$epid) |>
    dplyr::bind_rows(dupes.fixed) |>
    dplyr::mutate(
      Admin2GUID = paste0("{", stringr::str_to_upper(admin2guid), "}", sep = ""),
      Admin1GUID = paste0("{", stringr::str_to_upper(admin1guid), "}", sep = ""),
      Admin0GUID = paste0("{", stringr::str_to_upper(admin0guid), "}", sep = "")
    )

  # fix guids after de-duping
  fix.bad.guids <- df05 |>
    dplyr::filter(Admin2GUID != GUID | Admin1GUID != ADM1_GUID | Admin0GUID != ADM0_GUID) |>
    dplyr::mutate(
      Admin2GUID = ifelse(Admin2GUID != GUID, GUID, Admin2GUID),
      Admin1GUID = ifelse(Admin1GUID != ADM1_GUID, ADM1_GUID, Admin1GUID),
      Admin0GUID = ifelse(Admin0GUID != ADM0_GUID, ADM0_GUID, Admin0GUID),
      geo.corrected = 1
    ) |>
    # if fix.bad.guids is empty, then the GUID cols become logical but the
    # join in df06 requires them to be of char type.
    dplyr::mutate(
      Admin2GUID = as.character(Admin2GUID),
      Admin1GUID = as.character(Admin1GUID),
      Admin0GUID = as.character(Admin0GUID)
    )

  # bind back cases with fixed guids
  df06 <- df05 |>
    dplyr::filter(!epid %in% fix.bad.guids$epid) |>
    dplyr::mutate(geo.corrected = 0) |>
    dplyr::bind_rows(fix.bad.guids)

  rm(fix.bad.guids)
  # identify dropped obs. obs are dropped primarily because they match to a shape that doesn't
  # exist for the case's year onset (there are holes in the global map for certain years)
  df04$geometry <- NULL
  # antijoin from df01 to keep polis.lat/lon
  dropped.obs <- dplyr::anti_join(df01, df04, by = "epid") |>
    dplyr::filter(!epid %in% df04$epid & epid %in% df01.sf$epid)

  # bring df05 and dropped observations back together, create lat/lon var from sf object previously created
  df07 <- dplyr::bind_cols(
    dplyr::as_tibble(df06),
    sf::st_coordinates(df06) %>%
      {
        if (nrow(df06) == 0) {
          # if df06 is empty, as_tibble won't work and we need to create it manually
          dplyr::tibble(
            X = as.double(NA),
            Y = as.double(NA)
          ) |>
            dplyr::filter(!is.na(X))
        } else {
          dplyr::as_tibble(.)
        }
      } |>
      dplyr::rename("lon" = "X", "lat" = "Y")
  ) %>%
    {
      if (nrow(dropped.obs) != 0) {
        dplyr::bind_rows(., dropped.obs)
      } else {
        .
      }
    } |>
    dplyr::select(-dplyr::all_of(c("GUID", "yr.st", "yr.end")))

  df07$geometry <- NULL

  sf::st_geometry(global.dist.02) <- NULL

  # feed only cases with empty coordinates into st_sample (vars = GUID, nperarm, id, Shape)
  if (nrow(empty.coord |> dplyr::filter(Admin2GUID != "{NA}")) > 0) {
    # remove NAs because can't be sampled
    empty.coord.01 <- empty.coord |>
      dplyr::as_tibble() |>
      dplyr::group_by(Admin2GUID) |>
      dplyr::summarise(nperarm = dplyr::n()) |>
      dplyr::arrange(Admin2GUID) |>
      dplyr::mutate(id = dplyr::row_number()) |>
      dplyr::ungroup() |>
      dplyr::filter(Admin2GUID != "{NA}")

    empty.coord.02 <- global.dist.01 |>
      dplyr::select(GUID) |>
      dplyr::filter(GUID %in% empty.coord.01$Admin2GUID) |>
      dplyr::left_join(empty.coord.01, by = c("GUID" = "Admin2GUID"))

    if (nrow(empty.coord.02) == 0) {
      cli::cli_alert_info("All cases with missing coordinates have GUIDs that do not exist in the spatial dataset.")
      df08 <- df07
    } else {
      cli::cli_process_start("Placing random points for cases with bad coordinates")

      pt01 <- lapply(1:nrow(empty.coord.02), function(x) {
        tryCatch(
          expr = {
            suppressMessages(sf::st_sample(empty.coord.02[x, ],
                                           dplyr::pull(empty.coord.02[x, ], "nperarm"),
                                           exact = T
            )) |> sf::st_as_sf()
          },
          error = function(e) {
            guid <- empty.coord.02[x, ]$GUID[1]
            ctry_prov_dist_name <- global.dist.01 |>
              dplyr::filter(GUID == guid) |>
              dplyr::select(ADM0_NAME, ADM1_NAME, ADM2_NAME)

            cli::cli_alert_warning(paste0(
              "Fixing errors for:\n",
              "Country: ", ctry_prov_dist_name$ADM0_NAME, "\n",
              "Province: ", ctry_prov_dist_name$ADM1_NAME, "\n",
              "District: ", ctry_prov_dist_name$ADM2_NAME
            ))

            suppressWarnings({
              sf::sf_use_s2(F)
              int <- empty.coord.02[x, ] |> sf::st_centroid(of_largest_polygon = T)
              sf::sf_use_s2(T)

              sf::st_buffer(int, dist = 3000) |>
                sf::st_sample(dplyr::slice(empty.coord.02, x) |>
                                dplyr::pull(nperarm)) |>
                sf::st_as_sf()
            })
          }
        )
      }) |>
        dplyr::bind_rows()

      cli::cli_process_done()

      pt01_joined <- dplyr::bind_cols(
        pt01,
        empty.coord.02 |>
          dplyr::as_tibble() |>
          dplyr::select(GUID, nperarm) |>
          tidyr::uncount(nperarm)
      ) |>
        dplyr::left_join(
          dplyr::as_tibble(empty.coord.02) |>
            dplyr::select(-Shape),
          by = "GUID"
        )

      pt02 <- pt01_joined |>
        dplyr::as_tibble() |>
        dplyr::select(-nperarm, -id) |>
        dplyr::group_by(GUID) |>
        dplyr::arrange(GUID, .by_group = TRUE) |>
        dplyr::mutate(id = dplyr::row_number()) |>
        as.data.frame()

      pt03 <- empty.coord |>
        dplyr::group_by(Admin2GUID) |>
        dplyr::arrange(Admin2GUID, .by_group = TRUE) |>
        dplyr::mutate(id = dplyr::row_number()) |>
        dplyr::ungroup()

      pt04 <- dplyr::full_join(pt03, pt02, by = c("Admin2GUID" = "GUID", "id"))

      pt05 <- pt04 |>
        dplyr::bind_cols(
          dplyr::as_tibble(pt04$x),
          sf::st_coordinates(pt04$x) |>
            dplyr::as_tibble() |>
            dplyr::rename("lon" = "X", "lat" = "Y")
        ) |>
        dplyr::select(-id)

      pt05$x <- NULL
      pt05$geometry <- NULL

      df08 <- dplyr::bind_rows(df07, pt05)
    }
  } else {
    df08 <- df07
  }

  # bind back placed point cases with df06 and finished
  df09 <- df08 |>
    dplyr::left_join(global.dist.01 |> dplyr::select(ADM0_NAME, ADM1_NAME, ADM2_NAME, ADM0_GUID, ADM1_GUID, GUID),
                     by = c("Admin0GUID" = "ADM0_GUID", "Admin1GUID" = "ADM1_GUID", "Admin2GUID" = "GUID")
    ) |>
    dplyr::mutate(
      geo.corrected = ifelse(paste0("{", stringr::str_to_upper(admin2guid), "}", sep = "") != Admin2GUID, 1, 0),
      geo.corrected = ifelse(paste0("{", stringr::str_to_upper(admin1guid), "}", sep = "") != Admin1GUID, 1, geo.corrected),
      geo.corrected = ifelse(paste0("{", stringr::str_to_upper(admin0guid), "}", sep = "") != Admin0GUID, 1, geo.corrected),
      place.admin.0 = ifelse((place.admin.0 != ADM0_NAME | is.na(place.admin.0)) & !is.na(ADM0_NAME), ADM0_NAME, place.admin.0),
      place.admin.1 = ifelse((place.admin.1 != ADM1_NAME | is.na(place.admin.1)) & !is.na(ADM1_NAME), ADM1_NAME, place.admin.1),
      place.admin.2 = ifelse((place.admin.2 != ADM2_NAME | is.na(place.admin.2)) & !is.na(ADM2_NAME), ADM2_NAME, place.admin.2)
    ) |>
    dplyr::select(-c(
      "wrongAdmin0GUID", "wrongAdmin1GUID", "wrongAdmin2GUID", "ADM1_GUID", "ADM0_GUID", "ADM0_NAME",
      "ADM1_NAME", "ADM2_NAME"
    )) |>
    dplyr::mutate(geo.corrected = ifelse(is.na(geo.corrected), 0, geo.corrected))

  df09$Shape <- NULL

  final.guid.check <- df09 |>
    dplyr::filter((paste0("{", stringr::str_to_upper(admin2guid), "}", sep = "") != Admin2GUID |
                     paste0("{", stringr::str_to_upper(admin1guid), "}", sep = "") != Admin1GUID |
                     paste0("{", stringr::str_to_upper(admin0guid), "}", sep = "") != Admin0GUID) &
                    geo.corrected == 0) |>
    dplyr::select(epid, yronset, place.admin.0, place.admin.1, place.admin.2, admin0guid, admin1guid, admin2guid, Admin0GUID, Admin1GUID, Admin2GUID, geo.corrected)


  final.names.check <- df09 |>
    dplyr::select(epid, yronset, place.admin.0, place.admin.1, place.admin.2, admin0guid, admin1guid, admin2guid, Admin0GUID, Admin1GUID, Admin2GUID, geo.corrected) |>
    dplyr::filter((is.na(place.admin.0) & !is.na(admin0guid)) |
                    (is.na(place.admin.1) & !is.na(admin1guid)) |
                    (is.na(place.admin.2) & !is.na(admin2guid)))

  if (nrow(final.guid.check) > 0 | nrow(final.names.check) > 0) {
    cli::cli_alert_warning("A GUID or name has been misclassified, please run pre.stsample manually to identify")
    stop()
  } else {
    rm(final.names.check, final.guid.check)
  }

  return(df09)
}
