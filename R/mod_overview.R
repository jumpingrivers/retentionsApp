#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_overview_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tabPanel(
    "Overview",
    shiny::fluidRow(
      shiny::column(width = 8, shiny::wellPanel("Plot")),
      shiny::column(width = 4, shiny::wellPanel("Info about plot"))
    ),
    shinydashboard::valueBox("100%", "test", width = 3),
    shinydashboard::valueBox("100%", "test", width = 3),
    shinydashboard::valueBox("100%", "test", width = 3),
    shinydashboard::valueBox("100%", "test", width = 3)
  )
}

#' overview Server Functions
#'
#' @noRd
mod_overview_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
