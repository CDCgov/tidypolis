
library(testthat)
library(dplyr)
library(lubridate)
library(tibble)

test_that("filters standard virus types within current week", {
  week_floor <- as.Date("2025-12-01")  # Monday
  input <- tibble(
    measurement = c("cVDPV 2", "VDPV 1", "WILD 1"),
    dateonset = as.Date(c("2025-11-27", "2025-11-30", "2025-12-02")),
    place.admin.0 = c("NIGERIA", "NIGERIA", "NIGERIA")
  )

  result <- filter_current_week(input, "dateonset", week_floor)

  expect_equal(nrow(result), 3)
})

test_that("filters endemic countries with extended window", {
  week_floor <- as.Date("2025-12-01")
  input <- tibble(
    measurement = c("cVDPV 1", "VDPV 2"),
    dateonset = as.Date(c("2025-11-25", "2025-11-26")),  # 6 and 5 days before
    place.admin.0 = c("AFGHANISTAN", "PAKISTAN")
  )

  result <- filter_current_week(input, "dateonset", week_floor)

  expect_equal(nrow(result), 2)
})

test_that("excludes non-target measurements", {
  week_floor <- as.Date("2025-12-01")
  input <- tibble(
    measurement = c("OTHER", "UNKNOWN"),
    dateonset = as.Date(c("2025-11-28", "2025-11-29")),
    place.admin.0 = c("NIGERIA", "AFGHANISTAN")
  )

  result <- filter_current_week(input, "dateonset", week_floor)

  expect_equal(nrow(result), 0)
})

test_that("excludes dates outside the window", {
  week_floor <- as.Date("2025-12-01")
  input <- tibble(
    measurement = c("VDPV 1", "WILD 1"),
    dateonset = as.Date(c("2025-11-20", "2025-12-05")),
    place.admin.0 = c("NIGERIA", "PAKISTAN")
  )

  result <- filter_current_week(input, "dateonset", week_floor)

  expect_equal(nrow(result), 0)
})

test_that("handles edge dates correctly", {
  week_floor <- as.Date("2025-12-01")
  input <- tibble(
    measurement = c("VDPV 2", "WILD 1"),
    dateonset = as.Date(c("2025-11-26", "2025-12-02")),  # start and end of window
    place.admin.0 = c("NIGERIA", "NIGERIA")
  )

  result <- filter_current_week(input, "dateonset", week_floor)

  expect_equal(nrow(result), 2)
})
