# Run from the repository root in R:
# testthat::test_file("Sandbox/test_ethiopia_geography_audit.R")
# Synthetic rectangles test inference independently of EDAV credentials/data.

testthat::skip_if_not_installed("sf")
testthat::skip_if_not_installed("dplyr")
testthat::skip_if_not_installed("tibble")
audit_script <- if (file.exists("Sandbox/ethiopia_geography_audit.R")) {
  "Sandbox/ethiopia_geography_audit.R"
} else "ethiopia_geography_audit.R"
source(audit_script, local = TRUE)

eth_test_shapes <- function(boxes, names, starts, ends = rep(NA_character_, length(names)),
                            guids = paste0("g", seq_along(names)), level = "province",
                            parents = rep("p0", length(names)),
                            parent_names = rep("Parent", length(names))) {
  polygons <- lapply(boxes, function(b) {
    sf::st_polygon(list(matrix(c(b[1], b[3], b[2], b[3], b[2], b[4],
                                b[1], b[4], b[1], b[3]), ncol = 2, byrow = TRUE)))
  })
  sf::st_sf(
    ADM0_NAME = rep("ETHIOPIA", length(names)),
    ADM1_NAME = if (level == "province") names else parent_names,
    ADM2_NAME = names, ADM1_GUID = parents, GUID = guids,
    STARTDATE = as.Date(starts), ENDDATE = as.Date(ends),
    geometry = sf::st_sfc(polygons, crs = 6933)
  )
}

eth_test_audit <- function(x, level = "province", ...) {
  baseline <- eth_test_shapes(list(c(0, 2, 0, 2)), "Baseline", "2000-01-01")
  if (level == "province") {
    audit_ethiopia_geography(x, baseline, as_of = as.Date("2022-01-01"), ...)
  } else {
    audit_ethiopia_geography(baseline, x, as_of = as.Date("2022-01-01"), ...)
  }
}

testthat::test_that("multiple records retain the scalar level and correct name fields", {
  provinces <- eth_test_shapes(list(c(0, 1, 0, 1), c(1, 2, 0, 1)),
                               c("Province A", "Province B"), rep("2000-01-01", 2))
  provinces$ADM2_NAME <- NULL
  provinces$ADM1_GUID <- NULL
  prepared <- .eth_prepare(provinces, "province", TRUE)$shapes
  testthat::expect_equal(prepared$level, rep("province", 2))
  testthat::expect_equal(prepared$name, c("Province A", "Province B"))
  testthat::expect_equal(prepared$record_id, c("province_1", "province_2"))
  testthat::expect_true(all(is.na(prepared$parent_guid)))

  districts <- eth_test_shapes(list(c(0, 1, 0, 1), c(1, 2, 0, 1)),
                              c("District A", "District B"), rep("2000-01-01", 2),
                              level = "district", parents = c("{p1}", "p2"),
                              parent_names = c("Province A", "Province B"))
  prepared <- .eth_prepare(districts, "district", TRUE)$shapes
  testthat::expect_equal(prepared$level, rep("district", 2))
  testthat::expect_equal(prepared$name, c("District A", "District B"))
  testthat::expect_equal(prepared$parent_guid, c("P1", "P2"))
  testthat::expect_equal(prepared$parent_name, c("Province A", "Province B"))
})

testthat::test_that("a rename is found even when the GUID persists", {
  x <- eth_test_shapes(rep(list(c(0, 2, 0, 2)), 2), c("Old", "New"),
                       c("2000-01-01", "2011-01-01"), c("2010-12-31", NA),
                       guids = c("{abc}", "ABC"))
  result <- eth_test_audit(x)
  testthat::expect_equal(result$changes$change_type, "rename_candidate")
  testthat::expect_equal(result$changes$recorded_date, as.Date("2011-01-01"))
  testthat::expect_equal(result$changes$old_guid, result$changes$new_guid)
  testthat::expect_equal(result$changes$old_overlap_pct, 100)
})

testthat::test_that("splits retain each child, including a small child", {
  x <- eth_test_shapes(list(c(0, 2, 0, 2), c(0, 0.001, 0, 2), c(0.001, 2, 0, 2)),
                       c("Old", "Small", "Large"),
                       c("2000-01-01", "2011-01-01", "2011-01-01"),
                       c("2010-12-31", NA, NA))
  result <- eth_test_audit(x)
  testthat::expect_equal(result$changes$change_type, rep("split_candidate", 2))
  testthat::expect_equal(result$changes$new_overlap_pct, c(100, 100))
  testthat::expect_equal(result$changes$old_coverage_pct, c(100, 100))
})

testthat::test_that("mergers and many-to-many reorganizations are distinguished", {
  merger <- eth_test_shapes(list(c(0, 1, 0, 2), c(1, 2, 0, 2), c(0, 2, 0, 2)),
                            c("Left", "Right", "Merged"),
                            c("2000-01-01", "2000-01-01", "2011-01-01"),
                            c("2010-12-31", "2010-12-31", NA))
  testthat::expect_equal(eth_test_audit(merger)$changes$change_type, rep("merger_candidate", 2))
  reorganization <- eth_test_shapes(
    list(c(0, 1, 0, 2), c(1, 2, 0, 2), c(0, 2, 0, 1), c(0, 2, 1, 2)),
    c("Left", "Right", "Bottom", "Top"),
    c("2000-01-01", "2000-01-01", "2011-01-01", "2011-01-01"),
    c("2010-12-31", "2010-12-31", NA, NA)
  )
  changes <- eth_test_audit(reorganization)$changes
  testthat::expect_equal(changes$change_type, rep("reorganization_candidate", 4))
  testthat::expect_length(unique(changes$event_group), 1)
})

testthat::test_that("districts can change parent without changing their own name", {
  x <- eth_test_shapes(rep(list(c(0, 2, 0, 2)), 2), rep("District", 2),
                       c("2000-01-01", "2011-01-01"), c("2010-12-31", NA),
                       level = "district", parents = c("p1", "p2"),
                       parent_names = c("Province A", "Province B"))
  changes <- eth_test_audit(x, level = "district")$changes
  testthat::expect_equal(changes$change_type, "parent_change_candidate")
  testthat::expect_true(changes$parent_guid_changed)
  testthat::expect_false(changes$name_changed)
})

testthat::test_that("same-day overlap is flagged, and end-date convention is configurable", {
  x <- eth_test_shapes(rep(list(c(0, 2, 0, 2)), 2), c("Old", "New"),
                       c("2000-01-01", "2011-01-01"), c("2011-01-01", NA))
  inclusive <- eth_test_audit(x)$changes
  testthat::expect_true(all(inclusive$change_type == "overlapping_records_review"))
  exclusive <- eth_test_audit(x, end_date_inclusive = FALSE)$changes
  testthat::expect_equal(exclusive$change_type, "rename_candidate")
})

testthat::test_that("date gaps produce unresolved records rather than invented links", {
  x <- eth_test_shapes(rep(list(c(0, 2, 0, 2)), 2), c("Old", "New"),
                       c("2000-01-01", "2011-02-01"), c("2010-12-31", NA))
  changes <- eth_test_audit(x)$changes
  testthat::expect_setequal(changes$change_type, c("unmatched_old_record", "unmatched_new_record"))
})

testthat::test_that("baseline, open-ended and future records are handled without invented events", {
  x <- eth_test_shapes(rep(list(c(0, 2, 0, 2)), 2), c("Old", "Future"),
                       c("2000-01-01", "2030-01-01"), c("2029-12-31", NA))
  result <- eth_test_audit(x)
  testthat::expect_equal(nrow(result$changes), 0)
  testthat::expect_equal(nrow(result$inventory), 3)
  x$STARTDATE[1] <- as.Date(NA_character_)
  result <- eth_test_audit(x)
  testthat::expect_true(any(grepl("Missing start date", result$issues$issue)))
  testthat::expect_false(result$inventory$usable[1])
})

testthat::test_that("boundary-only contact is not a predecessor link", {
  x <- eth_test_shapes(list(c(0, 1, 0, 1), c(1, 2, 0, 1)), c("Old", "New"),
                       c("2000-01-01", "2011-01-01"), c("2010-12-31", NA))
  changes <- eth_test_audit(x)$changes
  testthat::expect_setequal(changes$change_type, c("unmatched_old_record", "unmatched_new_record"))
})
