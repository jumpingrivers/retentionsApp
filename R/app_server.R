#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_overview_server("overview_1")
  mod_sankey_server("sankey_1")
  mod_line_button_server("line_button_1")
  mod_line_agency_server("line_agency_1")
  mod_help_server("help_1")
}
