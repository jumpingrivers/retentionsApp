#' tab_3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_3_ui = function(id) {
  ns = shiny::NS(id)
  shiny::tabPanel(
    "Tab 3",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
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
                      "gpa_band"),
          selected = "college"
        ),
        shiny::uiOutput(ns("add_filter")),
        conditional_filter_input(tabs_3_and_4, "gender", "Gender", id),
        conditional_filter_input(tabs_3_and_4, "ipeds_race_ethnicity", "Race/Ethnicity", id),
        conditional_filter_input(tabs_3_and_4, "college", "College", id),
        conditional_filter_input(tabs_3_and_4, "department", "Department", id),
        conditional_filter_input(tabs_3_and_4, "program", "Program", id),
        conditional_filter_input(tabs_3_and_4, "gpa_band", "GPA Band", id)
      ),
      shiny::mainPanel(
        plotly::plotlyOutput(ns("retention_lines"))
      )
    )
  )
}

#' tab_3 Server Functions
#'
#' @noRd
mod_tab_3_server = function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns = session$ns # nolint

    output$add_filter = shiny::renderUI({
      shinyWidgets::pickerInput(
        ns("add_filter"),
        "Add Filter:",
        choices = remaining_group_choices(input$group),
        multiple = TRUE
      )
    })

    filter_inputs = shiny::reactive({
      list(
        "gender" = input$gender,
        "ipeds_race_ethnicity" = input$ipeds_race_ethnicity,
        "program" = input$program,
        "department" = input$department,
        "college" = input$college,
        "gpa_band" = input$gpa_band
      )
    })


    retention_data = shiny::reactive({
      df = calculate_retention(
        tabs_3_and_4,
        input_filter_by = input$add_filter,
        input_filter_values = filter_inputs(),
        input_metric = input$metric,
        input_group = input$group,
        input_agency = "third week",
        tab = 3)
      shiny::validate(
        shiny::need(nrow(df) != 0,
                    "There is no data which matches these filters.
                    \n Please update your filters.")
      )
      return(df)
    })


    output$retention_lines = plotly::renderPlotly({
      retention_line_chart(retention_data(), input$group)
    })
  })
}

tabs_3_and_4 = readr::read_csv("inst/app/fake_data/tabs_3_and_4.csv", show_col_types = FALSE)

#' Create remaining group choices
#'
#' Return all group choices except the selected group.
#'
#' @param selected_group Group selected in input widget
remaining_group_choices = function(selected_group = "") {
  all_choices = c("college",
                  "department",
                  "program",
                  "gender",
                  "ipeds_race_ethnicity",
                  "gpa_band")
  without_selected = all_choices[all_choices != selected_group]
  return(without_selected)
}

#' Create conditional filter input
#'
#' Create conditional panel containing pickerInput
#' for any groups selected in add_filter.
#' @param df Tibble containing retentions data
#' @param col_name Which column to create filter for
#' @param input_label Label for pickerInput
#' @param id ID for namespace
conditional_filter_input = function(df, col_name, input_label, id) {
  ns = shiny::NS(id)
  shiny::conditionalPanel(
    condition = glue::glue("input.add_filter.includes('{col_name}')"),
    shinyWidgets::pickerInput(
      ns(glue::glue("{col_name}")),
      label = input_label,
      choices = unique(df[[col_name]]),
      selected = unique(df[[col_name]]),
      multiple = TRUE,
      options = list(`actions-box` = TRUE)),
    ns = shiny::NS("tab_3_1")
  )
}
