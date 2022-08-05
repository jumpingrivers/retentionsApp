#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server = function(input, output, session) {
  mod_tab_1_server("tab_1_1")
  mod_tab_2_server("tab_2_1")
  mod_tab_3_server("tab_3_1")
  mod_tab_4_server("tab_4_1")
  mod_help_server("help_1")
}
