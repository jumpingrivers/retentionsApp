#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server = function(input, output, session) {

  goals = get_goal_data()
  summarised_retention = get_summarised_retention_data()
  raw_retention = get_raw_retention_data()

  mod_tab_1_server("tab_1_1", goals, summarised_retention)
  mod_tab_2_server("tab_2_1")
  mod_tab_3_server("tab_3_1", raw_retention)
  mod_tab_4_server("tab_4_1", raw_retention)
  mod_help_server("help_1")
}
