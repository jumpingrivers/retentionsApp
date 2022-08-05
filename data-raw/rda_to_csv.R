load("data-raw/fake_data.rda")

tabs_3_and_4_df = dplyr::mutate(
  tabs_3_and_4_df,
  gender = dplyr::if_else(.data$gender == TRUE, "Female", "Male"))

readr::write_csv(tab_1_df, "inst/app/fake_data/tab_1_df.csv")
readr::write_csv(tab_1_goals, "inst/app/fake_data/tab_1_goals.csv")
readr::write_csv(tabs_3_and_4_df, "inst/app/fake_data/tabs_3_and_4.csv")
