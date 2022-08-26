#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui = function(request) {
  shiny::tagList(
    shiny::tags$head(
      golem_add_external_resources()
    ),
    shiny::navbarPage(
      title = title_logo(),
      windowTitle = "Retention Dashboard",
      theme = litera_theme(),
      mod_tab_1_ui("tab_1_1"),
      mod_tab_2_ui("tab_2_1"),
      mod_tab_3_ui("tab_3_1"),
      mod_tab_4_ui("tab_4_1"),
      mod_help_ui("help_1")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
golem_add_external_resources = function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  shiny::tags$head(
    shiny::tags$link(rel = "icon", type = "image/png", href = "www/favicon.png")
  )

}
