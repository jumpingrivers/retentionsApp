testthat::test_that("retention calculation works", {
  file_path = system.file("app/fake_data/tabs_3_and_4.csv",
                          package = "retentionsApp",
                          mustWork = TRUE)
  tabs_3_and_4 = readr::read_csv(file_path, show_col_types = FALSE)
  filter_by = c("gender", "ipeds_race_ethnicity")
  filter_values = list(
    "gender" = "Female",
    "ipeds_race_ethnicity" = "Hispanic",
    "department" = unique(tabs_3_and_4$department),
    "program" = unique(tabs_3_and_4$program),
    "gpa_band" = unique(tabs_3_and_4$gpa_band),
    "college" = unique(tabs_3_and_4$college)
  )
  metric = "fall_returned"
  group = "college"
  agency = "third week"
  tab = 3
  retention_rates = retentionsApp:::calculate_retention(tabs_3_and_4, # nolint
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
