#' tab_4 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_4_ui = function(id) {
  ns = shiny::NS(id)
  shiny::tabPanel(
    "Tab 4",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(
          ns("agency"),
          "Select agency:",
          choices = unique(tabs_3_and_4$metric)
        ),
        shiny::selectInput(
          ns("metric"),
          "Select Metric:",
          choices = c(
            "spring_returned",
            "fall_returned",
            "second_fall_returned",
            "third_fall_returned",
            "fourth_fall_returned",
            "fifth_fall_returned",
            "sixth_fall_returned"
          )
        ),
        shiny::selectInput(
          ns("group"),
          "Select Group:",
          choices = c("college",
                      "department",
                      "program",
                      "gender",
                      "ipeds_race_ethnicity",
                      "gpa_band")
        )
      ),
      shiny::mainPanel(
        plotly::plotlyOutput(ns("retention_lines"))
      )
    )
  )
}

#' tab_4 Server Functions
#'
#' @noRd
mod_tab_4_server = function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns = session$ns # nolint

    retention_data = shiny::reactive({
      calculate_retention(tabs_3_and_4,
                          input_filter_by = "",
                          input_filter_values = "",
                          input_agency = input$agency,
                          input_metric = input$metric,
                          input_group = input$group,
                          tab = 4)
    })

    output$retention_lines = plotly::renderPlotly({
      retention_line_chart(retention_data(), input$group)
    })
  })
}

tabs_3_and_4 = readr::read_csv("inst/app/fake_data/tabs_3_and_4.csv", show_col_types = FALSE)
