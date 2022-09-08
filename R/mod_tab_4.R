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
          choices = c(
            c("Third Week" = "third week"),
            "USHE",
            c("End of Term" = "end of term"),
            c("IPDES" = "ipdes")
          ),
          selected = "end of term"
        ),
        shiny::selectInput(
          ns("metric"),
          "Select Metric:",
          choices = c(
            c("Fall to Spring" = "spring_returned"),
            c("Fall to Fall" = "second_fall_returned"),
            c("Third Year" = "third_fall_returned"),
            c("Fourth Year" = "fourth_fall_returned"),
            c("Fifth Year" = "fifth_fall_returned"),
            c("Sixth Year" = "sixth_fall_returned")
          )
        ),
        shiny::selectInput(
          ns("group"),
          "Select Group:",
          choices = c(
            c("College" = "college"),
            c("Department" = "department"),
            c("Program" = "program"),
            c("Gender" = "gender"),
            c("Race/Ethnicity" = "ipeds_race_ethnicity"),
            c("GPA Band" = "gpa_band"))
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
mod_tab_4_server = function(id, raw_retention) {
  shiny::moduleServer(id, function(input, output, session) {
    ns = session$ns # nolint

    retention_data = shiny::reactive({
      calculate_retention(raw_retention,
                          input_filter_by = "",
                          input_filter_values = "",
                          input_agency = input$agency,
                          input_metric = input$metric,
                          input_group = input$group,
                          tab = 4)
    })

    output$retention_lines = plotly::renderPlotly({
      retention_line_chart(retention_data(), input$group, colors_10())
    })
  })
}
