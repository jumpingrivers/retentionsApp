#' Get goal data
#'
#' Reads goal data from temporary CSV file.
#' To be replaced by call to database.
get_goal_data = function() {
  goal_data = readr::read_csv("inst/app/fake_data/tab_1_goals.csv",
                              show_col_types = FALSE,
                              progress = FALSE)
  return(goal_data)
}

#' Get summarised retention data
#'
#' Reads summarised retention data from temporary CSV file.
#' To be replaced by call to database.
get_summarised_retention_data = function() {
  summarised_data = readr::read_csv("inst/app/fake_data/tab_1_df.csv",
                                    show_col_types = FALSE,
                                    progress = FALSE)
  return(summarised_data)
}

#' Get raw retention data
#'
#' Reads raw retention data from temporary CSV file.
#' To be replaced by call to database.
get_raw_retention_data = function() {
  raw_data = readr::read_csv("inst/app/fake_data/tabs_3_and_4.csv",
                             show_col_types = FALSE,
                             progress = FALSE)

  return(raw_data)
}
