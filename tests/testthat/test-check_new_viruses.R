
library(testthat)
library(dplyr)
library(lubridate)
library(mockery)

# Mock data for raw virus table
mock_raw_table <- tibble(
  EPID = c("E1", "E2", "E3"),
  VirusDate = as.Date(c("2025-11-27", "2025-11-28", "2025-11-29")),
  VirusTypeName = c("VDPV2", "WILD1", "VDPV1"),
  CreatedDate = as.Date(c("2025-11-27", "2025-11-28", "2025-11-29")),
  UpdatedDate = as.Date(c("2025-11-28", "2025-11-29", "2025-11-30")),
  PublishDate = as.Date(c("2025-11-28", "2025-11-29", "2025-11-30")),
  VdpvReportedToHQDate = as.Date(c("2025-11-29", NA, "2025-11-30")),
  VdpvClassificationChangeDate = as.Date(c("2025-11-30", NA, "2025-12-01")),
  Admin0Name = c("NIGERIA", "AFGHANISTAN", "PAKISTAN"),
  Admin1Name = c("Region1", "Region2", "Region3"),
  VirusReportingWeekAndYear = c("2025-W48", "2025-W48", "2025-W48")
)

test_that("check_new_viruses handles raw virus table input", {
  result <- check_new_viruses(virus_table = mock_raw_table, week_floor_date = as.Date("2025-12-01"))

  expect_type(result, "list")
  expect_true(all(c("newly_added", "newly_updated", "newly_changed_classification",
                    "newly_reported", "new_no_hq_date", "new_no_vdpv_change_date",
                    "new_no_report_date", "new_but_different_week") %in% names(result)))
})

test_that("check_new_viruses handles preprocessed virus table input", {
  preprocessed <- mock_raw_table |>
    format_api_virus_table() |>
    select(epid, dateonset, place.admin.0, place.admin.1, measurement,
           datenotificationtohq, vdpvclassificationchangedate, report_date)

  result <- check_new_viruses(virus_table = preprocessed, week_floor_date = as.Date("2025-12-01"))

  expect_type(result, "list")
  expect_true("newly_reported" %in% names(result))
})

test_that("check_new_viruses returns empty lists when no new viruses", {
  empty_tbl <- mock_raw_table |>
    mutate(PublishDate = as.Date("2025-10-01"),
           UpdatedDate = as.Date("2025-10-01"),
           VdpvClassificationChangeDate = as.Date("2025-10-01"),
           report_date = as.Date("2025-10-01"))

  result <- check_new_viruses(virus_table = empty_tbl, week_floor_date = as.Date("2025-12-01"))

  expect_equal(nrow(result$newly_added), 0)
  expect_equal(nrow(result$newly_updated), 0)
  expect_equal(nrow(result$newly_reported), 0)
})

test_that("check_new_viruses handles missing optional columns gracefully", {
  partial_tbl <- mock_raw_table |>
    select(-UpdatedDate, -VdpvReportedToHQDate)

  expect_no_error({
    result <- check_new_viruses(virus_table = partial_tbl, week_floor_date = as.Date("2025-12-01"))
  })
})


test_that("check_new_viruses handles missing virusreportingweekandyear column", {
  partial_tbl <- mock_raw_table |>
    select(-VirusReportingWeekAndYear)

  expect_no_error({
    result <- check_new_viruses(virus_table = partial_tbl, week_floor_date = as.Date("2025-12-01"))
  })
})


test_that("check_new_viruses handles file path input", {
  mock_data <- mock_raw_table
  mock_read <- function(mode, ..., file_loc, edav) mock_data

  stub(check_new_viruses, "sirfunctions::sirfunctions_io", mock_read)

  expect_no_error({
    result <- check_new_viruses(virus_table = "mock/path.rds", week_floor_date = as.Date("2025-12-01"))
  })
})


test_that("check_new_viruses handles preprocessed table without created/updated dates", {
  preprocessed <- mock_raw_table |>
    format_api_virus_table() |>
    select(epid, dateonset, place.admin.0, place.admin.1, measurement,
           datenotificationtohq, vdpvclassificationchangedate, report_date)

  result <- check_new_viruses(virus_table = preprocessed, week_floor_date = as.Date("2025-12-01"))


  expect_s3_class(result$newly_added, "tbl_df")
  expect_equal(nrow(result$newly_added), 0)

})


test_that("check_new_viruses handles missing virusreportingweekandyear in newly_reported", {
  tbl <- mock_raw_table |>
    mutate(VirusReportingWeekAndYear = NULL)

  result <- check_new_viruses(virus_table = tbl, week_floor_date = as.Date("2025-12-01"))

  expect_true("newly_reported" %in% names(result))
})


test_that("check_new_viruses handles new_no_report_date when no HQ or VDPV dates", {
  tbl <- mock_raw_table |>
    mutate(VdpvReportedToHQDate = NA, VdpvClassificationChangeDate = NA)

  result <- check_new_viruses(virus_table = tbl, week_floor_date = as.Date("2025-12-01"))

  expect_true(nrow(result$new_no_report_date) > 0)
})

