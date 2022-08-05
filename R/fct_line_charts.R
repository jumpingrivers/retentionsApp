#' Create tab 1 line chart
#'
#' Line chart containing return rates for tabs 1
#' @param df Tibble containing goal rates
#' @param custom_colors Named character vector containing custom colors
goal_line_chart = function(df, custom_colors) {
  df %>%
    dplyr::mutate(metric = stringr::str_to_title(.data$metric)) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$year,
                                 y = .data$return_rate,
                                 color = .data$metric)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point() +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::scale_color_manual(
      values = custom_colors
    ) +
    ggplot2::labs(
      y = "Return rate",
      x = "Year",
      color = "Metric\n"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1),
                                limits = c(0, 100))
}


#' Create retention line chart
#'
#' Line chart containing calculated return rates for tabs 3 and 4
#' @param df Tibble containing calculated return rate
#' @param group What to colour/group line chart by
retention_line_chart = function(df, group) {
  df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$cohort,
                                 y = .data$return_rate,
                                 color = !!rlang::sym(group))) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point() +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::labs(
      y = "Return rate",
      x = "Cohort",
      color = "College\n"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1),
                                limits = c(0, 100))
}
