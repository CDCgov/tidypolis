library(testthat)
library(dplyr)
library(cli)

# Load required data

file_path <- file.path(
  "Sandbox/Software_Development/Test_Data",
  "afp.linelist.fixed.04.simplified.sample.xlsx"
)

afp.linelist.fixes.04 <- sirfunctions::edav_io("read", file_loc = file_path)

global.dist.01 <- sirfunctions::get_all_polio_data()
global.dist.01 <- global.dist.01$global.dist
global.dist.01 <- global.dist.01 |>
  dplyr::filter(ADM0_GUID %in% unique(afp.linelist.fixes.04$orig$Admin0GUID))

# ---- Tests ----

test_that("All records have lat/lon and Admin2GUIDs", {
  case_1 <- f.pre.stsample.01(
    afp.linelist.fixes.04$all_with_pts |>
      select(epid, yronset,
             place.admin.0, place.admin.1, place.admin.2,
             admin0guid, admin1guid, admin2guid,
             Admin0GUID, Admin1GUID, Admin2GUID,
             polis.latitude, polis.longitude),
    global.dist.01
  )

  with_coords <- case_1 |>
    dplyr::filter(!is.na(lat), !is.na(lon)) |>
    nrow()

  expect_equal(with_coords, 10)
  expect_equal(nrow(case_1), nrow(afp.linelist.fixes.04$all_with_pts))
})


test_that("Some records missing lat/lon but have Admin2GUIDs", {
  case_2 <- f.pre.stsample.01(
    afp.linelist.fixes.04$some_empty_with_adm2guid,
    global.dist.01
  )

  with_coords <- case_2 |>
    dplyr::filter(!is.na(lat), !is.na(lon)) |>
    nrow()

  expect_equal(with_coords, 10)
  expect_equal(nrow(case_2), nrow(afp.linelist.fixes.04$some_empty_with_adm2guid))
})


test_that("All records missing lat/lon but have Admin2GUIDs", {
  case_3 <- f.pre.stsample.01(
    afp.linelist.fixes.04$empty_with_adm2guid,
    global.dist.01
  )

  with_coords <- case_3 |>
    dplyr::filter(!is.na(lat), !is.na(lon)) |>
    nrow()

  expect_equal(with_coords, 10)
  expect_equal(nrow(case_3), nrow(afp.linelist.fixes.04$empty_with_adm2guid))
})


test_that("All records missing both lat/lon and Admin2GUIDs", {
  case_4 <- f.pre.stsample.01(
    afp.linelist.fixes.04$empty_no_adm2guid,
    global.dist.01
  )

  with_coords <- case_4 |>
    dplyr::filter(!is.na(lat), !is.na(lon)) |>
    nrow()

  expect_equal(with_coords, 0)
  expect_equal(nrow(case_4), nrow(afp.linelist.fixes.04$empty_no_adm2guid))
})


test_that("No geocorrection needed", {
  no_correction <- afp.linelist.fixes.04$orig |>
    dplyr::filter(epid != "INDMPRWA20034")

  result <- f.pre.stsample.01(no_correction, global.dist.01)

  geo_corrected <- result |>
    dplyr::filter(geo.corrected == 1) |>
    nrow()

  expect_equal(geo_corrected, 0)
  expect_equal(nrow(result), nrow(no_correction))
})


test_that("One record needs geocorrection", {
  needs_correction <- afp.linelist.fixes.04$orig |>
    dplyr::filter(epid == "INDMPRWA20034")

  result <- f.pre.stsample.01(needs_correction, global.dist.01)

  geo_corrected <- result |>
    dplyr::filter(geo.corrected == 1) |>
    nrow()

  expect_equal(geo_corrected, 1)
  expect_equal(nrow(result), nrow(needs_correction))
})


test_that("All records with unknown Admin2GUIDs are not geocorrected", {
  unknown_guid_data <- afp.linelist.fixes.04$orig |>
    dplyr::mutate(Admin2GUID = "UNKNOWN_GUID",
                  admin2guid = "UNKNOWN_GUID",
                  polis.latitude = NA,
                  polis.longitude = NA) |>
    dplyr::filter(Admin2GUID == "UNKNOWN_GUID")

  result <- f.pre.stsample.01(unknown_guid_data, global.dist.01)

  geo_corrected <- result |>
    dplyr::filter(geo.corrected == 1) |>
    nrow()

  expect_equal(nrow(result), nrow(unknown_guid_data))
  expect_equal(geo_corrected, 0)
})

test_that("Some records have unknown ADM2GUIDs and should not be geocorrected", {
  afp.copy <- afp.linelist.fixes.04$orig
  afp.copy[c(1, 2, 3),]$Admin2GUID <- "{UNKNOWN_GUID}"
  afp.copy[c(1, 2, 3),]$admin2guid  <- "UNKNOWN_GUID"
  afp.copy[c(1, 2, 3),]$polis.latitude <- NA
  afp.copy[c(1, 2, 3),]$polis.longitude <- NA

  afp.copy <- afp.copy |>
    dplyr::filter(epid != "INDMPRWA20034")

  result <- f.pre.stsample.01(afp.copy, global.dist.01)

  geo_corrected <- result |>
    dplyr::filter(geo.corrected == 1) |>
    nrow()

  expect_equal(nrow(result), nrow(afp.copy))
  expect_equal(geo_corrected, 0)

})

test_that("Output contains expected columns", {
  result <- f.pre.stsample.01(afp.linelist.fixes.04$all_with_pts, global.dist.01)

  expected_cols <- c("epid", "lat", "lon", "geo.corrected")
  expect_true(all(expected_cols %in% names(result)))
})
