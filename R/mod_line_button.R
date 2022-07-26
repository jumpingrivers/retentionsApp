#' line_button UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_line_button_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tabPanel(
    "Line 1",
    shiny::sidebarLayout(
      sidebarPanel(
        shiny::selectInput(
          ns("metric"),
          "Select metric:",
          choices = c("Metric 1", "Metric 2")
        ),
        shiny::selectInput(
          ns("group"),
          "Select group:",
          choices = c("Group 1", "Group 2")
        ),
        shiny::selectInput(
          ns("group-types"),
          "Select group types:",
          choices = c("Group 1", "Group 2")
        ),
        shiny::actionButton(
          ns("create"),
          "Create UI element"
        ),
        shiny::actionButton(
          ns("remove"),
          "Remove UI element"
        )
      ),
      mainPanel("Line chart created in plotly")
    )
  )
}

#' line_button Server Functions
#'
#' @noRd
mod_line_button_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
