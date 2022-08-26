#' tab_1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_1_ui = function(id) {
  ns = shiny::NS(id)
  shiny::tabPanel(
    "Tab 1",
    shiny::tags$style(".small-box {
                        background-color: rgba(0,0,0,0.03) !important;
                        border-width: 1px;
                        border-color: rgba(0,0,0,0.1);
                        border-style: solid;
                        border-radius: .25rem;
                        padding-left: 1rem;
	                      box-shadow: none;
                      }

                      .fa-percent {
                        font-size: 70px;
                        position: absolute;
                        top: 20%;
                        right: 15%;
                        opacity: 0.2
                      }"),
    shiny::fluidRow(
      shinydashboard::valueBoxOutput(ns("fall_to_fall"), width = 3),
      shinydashboard::valueBoxOutput(ns("fall_to_spring"), width = 3),
      shinydashboard::valueBoxOutput(ns("second_year"), width = 3),
      shinydashboard::valueBoxOutput(ns("third_year"), width = 3)
    ),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(width = 4, shiny::wellPanel("Info about plot")),
      shiny::column(width = 8, shiny::wellPanel(
        plotly::plotlyOutput(ns("retention_lines"))
      ))
    )
  )
}

#' tab_1 Server Functions
#'
#' @noRd
mod_tab_1_server = function(id, goals, summarised_retention) {
  shiny::moduleServer(id, function(input, output, session) {
    ns = session$ns # nolint

    output$fall_to_fall = shinydashboard::renderValueBox({
      create_valuebox(goals, "fall to fall", colors_value_boxes())
    })
    output$fall_to_spring = shinydashboard::renderValueBox({
      create_valuebox(goals, "fall to spring", colors_value_boxes())
    })
    output$second_year = shinydashboard::renderValueBox({
      create_valuebox(goals, "second year", colors_value_boxes())
    })
    output$third_year = shinydashboard::renderValueBox({
      create_valuebox(goals, "third year", colors_value_boxes())
    })

    output$retention_lines = plotly::renderPlotly({
      goal_line_chart(summarised_retention, colors_value_boxes())
    })

  })
}

#' Create value box
#'
#' Create shinydashboard valuebox with custom text colours
#' @param df Tibble containing goal data
#' @param metric_filter Which metric to filter data by
#' @param text_color Named character vector containing custom colors
create_valuebox = function(df, metric_filter, text_color) {
df = process_valuebox_data(df, metric_filter, text_color)
  shinydashboard::valueBox(
    shiny::tags$h3(df$goal,
                   style = glue::glue("color: {df$color}; font-size: 80px;")),
    shiny::tags$p(df$metric,
                  style = "color: #000; margin-left: 5px;"),
                           icon = shiny::icon("percent"))
}

#' Process value box data
#'
#' Format goal data for value boxes
#'   - filter by metric
#'   - add % suffix to goal value
#'   - join data with custom colours
#' @param df Tibble containing goal data
#' @param metric_filter Which metric to filter data by
#' @param text_color Named character vector containing custom colors
process_valuebox_data = function(df, metric_filter, text_color) {
  df %>%
    dplyr::filter(.data$metric == metric_filter) %>%
    dplyr::mutate(metric = stringr::str_to_title(.data$metric),
                  goal = as.character(.data$goal)) %>%
    dplyr::inner_join(dplyr::tibble(metric = names(text_color),
                                    color = text_color),
                      by = "metric")
}
