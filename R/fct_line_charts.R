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
    custom_theme() +
    ggplot2::scale_color_manual(
      values = custom_colors
    ) +
    ggplot2::labs(
      y = "Return rate",
      x = "Year",
      color = "Metric\n"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1),
                                limits = c(0, 100)) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks())
}


#' Create retention line chart
#'
#' Line chart containing calculated return rates for tabs 3 and 4
#' @param df Tibble containing calculated return rate
#' @param group What to colour/group line chart by
#' @param custom_colors Named character vector containing custom colors
retention_line_chart = function(df, group, custom_colors) {
  g = df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$cohort,
                                 y = .data$return_rate,
                                 color = !!rlang::sym(group))) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point() +
    custom_theme() +
    ggplot2::labs(
      y = "Return rate",
      x = "Cohort",
      color = stringr::str_to_title(gsub("_", " ", group))
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks())

  if (length(unique(df[[group]])) <= 10) {
    g = g +
      ggplot2::scale_color_manual(
        values = custom_colors
      )
  }

  return(g)
}

#' Custom plot theme
#'
#' Returns custom plot theme, which is a modification of
#' `ggplot2::theme_minimal()`.
custom_theme = function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_text(color = "#a6a6a6", size = 10),
      plot.caption = ggplot2::element_text(color = "#a6a6a6", size = 8, face = "italic")
    )
}
