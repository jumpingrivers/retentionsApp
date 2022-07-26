#' line_agency UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_line_agency_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tabPanel(
    "Line 2",
    shiny::sidebarLayout(
      sidebarPanel(
        shiny::selectInput(
          ns("agency"),
          "Select agency:",
          choices = c("Agency 1", "Agency 2")
        ),
        shiny::selectInput(
          ns("metric"),
          "Select metric:",
          choices = c("Metric 1", "Metric 2")
        ),
        shiny::selectInput(
          ns("group"),
          "Select group:",
          choices = c("Group 1", "Group 2")
        )
      ),
      mainPanel("Line chart created in plotly")
    )
  )
}

#' line_agency Server Functions
#'
#' @noRd
mod_line_agency_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_line_agency_ui("line_agency_1")

## To be copied in the server
# mod_line_agency_server("line_agency_1")
