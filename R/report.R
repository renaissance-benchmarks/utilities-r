# ----------------------------------------------------------------
# Labels

#' Prepare default plot labels.
#'
#' @param data Data to generate labels for.
#' @return Tibble with default plot labels.
#' @export
plot_default_labels <- function (data) {
    data |>
        distinct (.data $ vm, .data $ vm_name, .data $ vm_version, .data $ vm_configuration) |>
        mutate (vm_label = glue ('{vm_name} {vm_version} ({vm})')) |>
        arrange (.data $ vm_label) |>
        mutate (vm_order = seq.int (1, n ())) |>
        mutate (vm_jdk = NA) |>
        relocate (
            .data $ vm_label, .data $ vm_order, .data $ vm_jdk,
            .data $ vm, .data $ vm_name, .data $ vm_version, .data $ vm_configuration)
}


# ----------------------------------------------------------------
# Reporting summaries

#' Group helper for report result summaries.
#'
#' @param group Single benchmark group to report on.
#' @param key Group key information.
report_summaries_benchmark_group_helper <- function (group, key) {

    # Group walk calls us once even with empty tibble.
    if (nrow (group) == 0) return ()

    # Summarize every numeric column except index and total with time first.
    metrics <- c ('time', sort (setdiff (names (group |> select_if (is.numeric)), c ('index', 'time', 'total'))))

    SEPARATOR <- '   '
    WIDTH_VALUES <- 24
    WIDTH_METRIC <- min (max (c (8, str_length (metrics))), console_width () - 2*WIDTH_VALUES - 8)

    cli_h2 (key $ benchmark)

    cli_verbatim (glue (
        '{style_bold (ansi_align ("metric", WIDTH_METRIC))}',
        SEPARATOR,
        '{style_bold (ansi_align ("mean", WIDTH_VALUES))}',
        SEPARATOR,
        '{style_bold (ansi_align ("median", WIDTH_VALUES))}',
    ))

    format_ci <- function (ci) {
        mid_textual <- style_bold (pretty_num (ci $ mid, style = '6'))
        lo_textual <- str_trim (pretty_num (ci $ lo, style = '6'))
        hi_textual <- str_trim (pretty_num (ci $ hi, style = '6'))
        ci_textual <- glue ('{mid_textual} {col_silver ("(")}{lo_textual} {col_silver ("-")} {hi_textual}{col_silver (")")}')
        ci_aligned <- ansi_align (ci_textual, WIDTH_VALUES)

        return (ci_aligned)
    }

    for (metric in metrics) {

        ci_mean <- compute_hierarchical_percentile_ci (group, .data [[metric]], base::mean, base::mean)
        ci_median <- compute_hierarchical_percentile_ci (group, .data [[metric]], stats::median, stats::median)

        cli_verbatim (glue (
            '{ansi_align (ansi_strtrim (metric, WIDTH_METRIC), WIDTH_METRIC)}',
            SEPARATOR,
            '{format_ci (ci_mean)}',
            SEPARATOR,
            '{format_ci (ci_median)}',
        ))
    }
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


# ----------------------------------------------------------------
# Plotting summaries

plot_website_stripes_group_helper <- function (group, key) {

    STRIPE_ROWS <- 2
    STRIPE_WIDTH <- 400
    STRIPE_HEIGHT <- 250

    # Group walk calls us once even with empty tibble.
    if (nrow (group) == 0) return ()

    # Compute confidence intervals relative to baseline.
    baseline <- group |> distinct (.data $ vm, .data $ vm_order) |> filter (.data $ vm_order == min (.data $ vm_order)) |> pull (.data $ vm)
    relative <- compute_hierarchical_relative_percentile_ci (group, baseline, .data $ time, .combination = .Primitive ('/'))

    nice <- ggplot (relative,
        aes (
            x = fct_reorder (.data $ vm_label, .data $ vm_order),
            y = 100 / .data $ mid,
            ymin = 100 / .data $ hi,
            ymax = 100 / .data $ lo,
            fill = fct_reorder (.data $ vm_label, .data $ vm_order))) +
        geom_col () +
        geom_errorbar (width = 0.5, color = '#555555') +
        facet_wrap (vars (.data $ benchmark), nrow = STRIPE_ROWS, scales = 'free_y', strip.position = 'bottom') +
        labs (x = NULL, y = 'Average throughput relative to baseline [%]', fill = 'JVM implementation') +
        theme (
            text = element_text (family = 'Serif', color = '#555555'),
            legend.position = 'bottom',
            axis.text.x = element_blank (),
            axis.ticks.x = element_blank (),
            axis.title.y = element_text (size = 14, margin = margin (r = 10)),
            strip.text.x = element_text (angle = 90, vjust = 0.5, hjust = 1, size = 14, color = '#555555'),
            strip.background = element_blank (),
            legend.text = element_text (size = 14),
            legend.title = element_text (size = 14),
            legend.background = element_rect (fill = 'transparent', color = NA),
            legend.box.background = element_rect (fill = 'transparent', color = NA),
            plot.background = element_rect (fill = 'transparent', color = NA))

    ggsave (glue ('stripe-jdk-{key $ vm_jdk}.png'), nice, width = STRIPE_WIDTH, height = STRIPE_HEIGHT, units = 'mm', bg = 'transparent')

    readr::write_csv (group, glue ('summary-jdk-{key $ vm_jdk}.csv'))
}


#' Plot stripe image used on benchmark website.
#'
#' @param input Data path or data tibble to summarize.
#' @param warmup Duration to discard as warm up time.
#' @param labels Labels file or labels tibble.
#'
#' @examples
#' data <- load_path_json (rren_example ())
#' # Edit labels manually to provide JDK version.
#' labels <- plot_default_labels (data)
#' plot_website_stripes (data, labels = labels)
#'
#' @export
plot_website_stripes <- function (input = '.', warmup = 5*60, labels = 'labels.csv') {

    # To offer maximum simplicity in use, load data if input is data path.
    if (is.character (input)) input <- load_path_json (input)
    if (is.character (labels)) labels <- readr::read_csv (labels, col_types = readr::cols (vm_jdk = 'i', vm_order = 'i', .default = 'f'))

    input |>
        # Drop warm up data.
        filter (.data $ total >= warmup) |>
        # Reduce data volume.
        select (.data $ vm, starts_with ('vm_'), .data $ benchmark, .data $ run, .data $ index, .data $ time) |>
        # Inject labels.
        left_join (labels, by = c ('vm', 'vm_name', 'vm_version', 'vm_configuration')) |>
        # Plot.
        group_by (.data $ vm_jdk) |>
        group_walk (plot_website_stripes_group_helper)
}


plot_website_violins_group_helper <- function (group, key) {

    PLOT_ROWS <- 4
    PLOT_WIDTH <- 300
    PLOT_HEIGHT <- 300

    # Group walk calls us once even with empty tibble.
    if (nrow (group) == 0) return ()

    nice <- ggplot (group,
        aes (
            x = fct_reorder (.data $ vm_label, .data $ vm_order),
            y = .data $ time,
            fill = fct_reorder (.data $ vm_label, .data $ vm_order))) +
        geom_violin (scale = 'width', width = 1) +
        geom_boxplot (width = 0.2) +
        facet_wrap (vars (.data $ benchmark), nrow = PLOT_ROWS, scales = 'free_y') +
        theme (legend.position = 'none', axis.text.x = element_text (angle = 90, vjust = 0.5, hjust = 1)) +
        labs (
            x = NULL,
            y = 'Single repetition time [s]',
            title = glue ('Warm repetition time distribution except outliers on JDK {key $ vm_jdk}')) +
        scale_fill_brewer (palette = 'Blues', type = 'qual')

    ggsave (glue ('violin-jdk-{key $ vm_jdk}.png'), nice, width = PLOT_WIDTH, height = PLOT_HEIGHT, units = 'mm')
}


#' Plot violin image used on benchmark website.
#'
#' @param input Data path or data tibble to summarize.
#' @param warmup Duration to discard as warm up time.
#' @param labels Labels file or labels tibble.
#'
#' @examples
#' data <- load_path_json (rren_example ())
#' # Edit labels manually to provide JDK version.
#' labels <- plot_default_labels (data)
#' plot_website_violins (data, labels = labels)
#'
#' @export
plot_website_violins <- function (input = '.', warmup = 5*60, labels = 'labels.csv') {

    # To offer maximum simplicity in use, load data if input is data path.
    if (is.character (input)) input <- load_path_json (input)
    if (is.character (labels)) labels <- readr::read_csv (labels, col_types = readr::cols (vm_jdk = 'i', vm_order = 'i', .default = 'f'))

    input |>
        # Drop warm up data.
        filter (.data $ total >= warmup) |>
        # Reduce data volume.
        select (.data $ vm, starts_with ('vm_'), .data $ benchmark, .data $ run, .data $ index, .data $ time) |>
        # Filter outliers.
        remove_outliers_window (.data $ time) |>
        # Inject labels.
        left_join (labels, by = c ('vm', 'vm_name', 'vm_version', 'vm_configuration')) |>
        # Plot.
        group_by (.data $ vm_jdk) |>
        group_walk (plot_website_violins_group_helper)
}
