testthat::test_that("retention calculation works", {
  file_path = system.file("app/fake_data/tabs_3_and_4.csv",
                          package = "retentionsApp",
                          mustWork = TRUE)
  raw_retention = readr::read_csv(file_path, show_col_types = FALSE)
  filter_by = c("gender", "ipeds_race_ethnicity")
  filter_values = list(
    "gender" = "Female",
    "ipeds_race_ethnicity" = "Hispanic",
    "department" = unique(raw_retention$department),
    "program" = unique(raw_retention$program),
    "gpa_band" = unique(raw_retention$gpa_band),
    "college" = unique(raw_retention$college)
  )
  metric = "fall_returned"
  group = "college"
  agency = "third week"
  tab = 3
  retention_rates = retentionsApp:::calculate_retention(raw_retention, # nolint
                                                       filter_by,
                                                       filter_values,
                                                       metric,
                                                       group,
                                                       agency,
                                                       tab)
  testthat::expect_equal(round(retention_rates$return_rate[1], 3), 43.939)
  testthat::expect_equal(nrow(retention_rates), 48)
  testthat::expect_equal(ncol(retention_rates), 5)
  testthat::expect_false(any(is.na(retention_rates$return_rate)))
  testthat::expect_snapshot(retention_rates)
})
