# ----------------------------------------------------------------
# Reporting

#' Group helper for report result summaries.
#'
#' @param group Single benchmark group to report on.
#' @param key Group key information.
report_summaries_benchmark_group_helper <- function (group, key) {

    # Group walk calls us once even with empty tibble.
    if (nrow (group) == 0) return ()

    # Summarize every numeric column except index and total with time first.
    metrics <- c ('time', sort (setdiff (names (group |> select_if (is.numeric)), c ('index', 'time', 'total'))))

    WIDTH_VALUES <- 24
    WIDTH_METRIC <- min (max (c (8, str_length (metrics))), console_width () - 2*WIDTH_VALUES - 8)

    cli_h2 (key $ benchmark)

    cat_line (sprintf (glue ('%-{WIDTH_METRIC}s   %-{WIDTH_VALUES}s   %-{WIDTH_VALUES}s'), 'metric', 'mean', 'median'))

    for (metric in metrics) {

        ci_mean <- compute_hierarchical_percentile_ci (group, .data [[metric]], base::mean, base::mean)
        ci_median <- compute_hierarchical_percentile_ci (group, .data [[metric]], stats::median, stats::median)

        cat_line (sprintf (glue ('%-{WIDTH_METRIC}s   %s (%s - %s)   %s (%s - %s)'),
            str_trunc (metric, WIDTH_METRIC, 'center'),
            pretty_num (ci_mean $ mid, style = '6'),
            pretty_num (ci_mean $ lo, style = '6'),
            pretty_num (ci_mean $ hi, style = '6'),
            pretty_num (ci_median $ mid, style = '6'),
            pretty_num (ci_median $ lo, style = '6'),
            pretty_num (ci_median $ hi, style = '6')))
    }

    cat_line ()
}


#' Group helper for report result summaries.
#'
#' @param group Single virtual machine group to report on.
#' @param key Group key information.
report_summaries_vm_group_helper <- function (group, key) {

    # Group walk calls us once even with empty tibble.
    if (nrow (group) == 0) return ()

    cli_h1 (glue ('{key $ vm_name} {key $ vm_version} ({key $ vm})'))

    if (str_length (key $ vm_configuration)) {
        cli_text ('{key $ vm_configuration}')
        cli_rule ()
    }

    group |> group_by (.data $ vm, across (starts_with ('vm_')), .data $ benchmark) |>
        group_walk (report_summaries_benchmark_group_helper, .keep = TRUE)
}


#' Report result summaries.
#'
#' Generates result summaries for human consumption.
#'
#' @param input Data path or data tibble to summarize.
#' @param warmup Duration to discard as warm up time.
#' @export
report_summaries <- function (input = '.', warmup = 5*60) {

    # End users may not appreciate too detailed reporting.
    log_threshold (WARN)

    # To offer maximum simplicity in use, load data if input is data path.
    if (is.character (input)) {
        cli_alert_info ('Loading data from {.path {input}}')
        input <- load_path_json (input)
    }

    assert_renaissance (input)

    input |>
        filter (.data $ total >= warmup) |>
        group_by (.data $ vm, across (starts_with ('vm_'))) |>
        group_walk (report_summaries_vm_group_helper, .keep = TRUE)

    cli_rule ()
}
