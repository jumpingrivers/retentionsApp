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
            c("Fall to Spring" = "spring_returned"),
            c("Fall to Fall" = "fall_returned"),
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
            c("GPA Band" = "gpa_band")),
          selected = "college"
        ),
        shiny::uiOutput(ns("add_filter")),
        conditional_filter_panel("gender", "Gender", id),
        conditional_filter_panel("ipeds_race_ethnicity", "Race/Ethnicity", id),
        conditional_filter_panel("college", "College", id),
        conditional_filter_panel("department", "Department", id),
        conditional_filter_panel("program", "Program", id),
        conditional_filter_panel("gpa_band", "GPA Band", id)
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
mod_tab_3_server = function(id, raw_retention) {
  shiny::moduleServer(id, function(input, output, session) {
    ns = session$ns # nolint

    output$gender_panel = conditional_filter_input(
      raw_retention,
      "gender",
      "Gender",
      id,
      session)

    output$ipeds_race_ethnicity_panel = conditional_filter_input(
      raw_retention,
      "ipeds_race_ethnicity",
      "Race/Ethnicity",
      id,
      session)

    output$college_panel = conditional_filter_input(
      raw_retention,
      "college",
      "College",
      id,
      session)

    output$department_panel = conditional_filter_input(
      raw_retention,
      "department",
      "Department",
      id,
      session)

    output$program_panel = conditional_filter_input(
      raw_retention,
      "program",
      "Program",
      id,
      session)

    output$gpa_band_panel = conditional_filter_input(
      raw_retention,
      "gpa_band",
      "GPA Band",
      id,
      session)

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
        raw_retention,
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
      retention_line_chart(retention_data(), input$group, colors_10())
    })
  })
}

#' Create remaining group choices
#'
#' Return all group choices except the selected group.
#'
#' @param selected_group Group selected in input widget
remaining_group_choices = function(selected_group = "") {
  all_choices = c(
    c("College" = "college"),
    c("Department" = "department"),
    c("Program" = "program"),
    c("Gender" = "gender"),
    c("Race/Ethnicity" = "ipeds_race_ethnicity"),
    c("GPA Band" = "gpa_band"))
  without_selected = all_choices[all_choices != selected_group]
  return(without_selected)
}

#' Create conditional filter input
#'
#' Create conditional panel containing pickerInput
#' created by `conditional_filter_input()`.
#' This function is used in the UI (contains `shiny::uiOutput`)
#' @param col_name Which column to create filter for
#' @param input_label Label for pickerInput
#' @param id ID for namespace
conditional_filter_panel = function(col_name, input_label, id) {
  ns = shiny::NS(id)
  shiny::conditionalPanel(
    condition = glue::glue("input.add_filter.includes('{col_name}')"),
    shiny::uiOutput(ns(glue::glue("{col_name}_panel"))),
    ns = shiny::NS(id)
  )
}

#' Create conditional filter input
#'
#' Create pickerInput to be used in
#' conditional panel created by `conditional_filter_panel()`
#' This function is used in the server (contains `shiny::renderUI`)
#' @param df Tibble containing retentions data
#' @inheritParams conditional_filter_panel
#' @param session Shiny session
conditional_filter_input = function(df, col_name, input_label, id, session) {
  ns = session$ns
  shiny::renderUI({
    shinyWidgets::pickerInput(
      ns(glue::glue("{col_name}")),
      label = input_label,
      choices = unique(df[[col_name]]),
      selected = unique(df[[col_name]]),
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    )
  })
}
