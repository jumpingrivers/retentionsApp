#' tab_2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_2_ui = function(id) {
  ns = shiny::NS(id)
  shiny::tabPanel(
    "Tab 2",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(
          ns("cohort"),
          "Select cohort:",
          choices = c("Cohort 1", "Cohort 2")
        ),
        shiny::selectInput(
          ns("metric"),
          "Select metric:",
          choices = c("Metric 1", "Metric 2")
        ),
        shiny::selectInput(
          ns("group"),
          "Select first-level group:",
          choices = c("Group 1", "Group 2")
        ),
        shiny::selectInput(
          ns("group"),
          "Select second-level group:",
          choices = c("Group 1", "Group 2")
        ),
        shiny::selectInput(
          ns("group"),
          "Select third-level group:",
          choices = c("Group 1", "Group 2")
        ),
      ),
      shiny::mainPanel("Sankey widget")
    )
  )
}

#' tab_2 Server Functions
#'
#' @noRd
mod_tab_2_server = function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns = session$ns # nolint

  })
}
