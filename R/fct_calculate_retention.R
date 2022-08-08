#' Calculate retention
#'
#' Using user-entered filters and columns for grouping,
#' calculate rentention rates across cohorts.
#' @param df Tibble containing retention data
#' @param input_filter_by Which filters are selected e.g. gender
#' @param input_filter_values List of selected values from filters
#' @param input_metric Metric e.g. Fall to Fall
#' @param input_group Group e.g. College
#' @param input_agency Agency e.g. "third week"
#' @param tab Which tab? 3 or 4
calculate_retention = function(df,
                               input_filter_by,
                               input_filter_values,
                               input_metric,
                               input_group,
                               input_agency,
                               tab) {
  filtered_data = filter_data(df, input_agency, input_filter_by, input_filter_values, tab)
  if (nrow(filtered_data) == 0) {
    return(filtered_data)
  }
  return_rates = filtered_data %>%
    calculate_headcount(input_metric, input_group) %>%
    calculate_return_rate(input_metric)
  return(return_rates)
}

#' Filter data
#'
#' Filter agency and any added group filters.
#'
#' @param df Tibble containing retention data
#' @param input_agency Agency e.g. "third week"
#' @param input_filter_by Which filters are selected e.g. gender
#' @param input_filter_values List of selected values from filters
#' @param tab Which tab? 3 or 4
filter_data = function(df, input_agency, input_filter_by, input_filter_values, tab) {
  df %>%
    dplyr::filter(.data$metric == input_agency) %>%
    filter_groups(input_filter_by, input_filter_values, tab)
}

#' Filter groups
#'
#' Filter values based on user inputs.
#' @param df Tibble containing retention data
#' @param filter_by Which filters are selected e.g. gender
#' @param filter_values List of selected values from filters
#' @param tab Which tab? 3 or 4
filter_groups = function(df, filter_by, filter_values, tab) {
  if (tab == 3) {
    for (i in seq_along(filter_values)) {
      df = filter_group(
        df,
        filter_by = filter_by,
        group = names(filter_values)[i],
        values = filter_values[[i]]
      )
    }
  }
  return(df)
}

#' Filter group
#'
#' Filter values based on user inputs, only if
#' group is actually selected.
#' @param df Tibble containing retention data
#' @param filter_by Which filters are selected e.g. gender
#' @param group Current filter e.g. gender
#' @param values Current values e.g. Male
filter_group = function(df, filter_by, group, values) {
  if (group %in% filter_by) {
    filtered_df = dplyr::filter(df, !!rlang::sym(group) %in% values)
    return(filtered_df)
  }
  return(df)
}

#' Calculate headcount
#'
#' Calculate number of students by cohort, metric, and group.
#'
#' @param df Tibble containing retention data
#' @param input_metric Metric e.g. Fall to Fall
#' @param input_group Group e.g. College
calculate_headcount = function(df, input_metric, input_group) {
  df %>%
    dplyr::select(.data$cohort, !!rlang::sym(input_metric), !!rlang::sym(input_group)) %>%
    dplyr::group_by(.data$cohort, !!rlang::sym(input_metric), !!rlang::sym(input_group)) %>%
    dplyr::summarize(headcount = dplyr::n()) %>%
    dplyr::ungroup()
}

#' Calculate return rate
#'
#' Reshape data and calculate return rate.
#'
#' @param df Tibble containing retention data
#' @param input_metric Metric e.g. Fall to Fall
calculate_return_rate = function(df, input_metric) {
  df %>%
    tidyr::pivot_wider(names_from = !!rlang::sym(input_metric),
                       values_from = .data$headcount) %>%
    dplyr::mutate(total = .data$`TRUE` + .data$`FALSE`) %>%
    dplyr::rename(returned = .data$`TRUE`) %>%
    dplyr::select(-.data$`FALSE`) %>%
    dplyr::mutate(return_rate = (.data$returned / .data$total) * 100)
}
