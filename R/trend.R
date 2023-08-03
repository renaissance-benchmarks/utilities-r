# ----------------------------------------------------------------
# Trend computation

#' Compute trends.
#'
#' Uses linear least squares regression to compute trend of each run.
#'
#' @param .input Data.
#' @param .column Column to compute trend for.
#' @return Summarized tibble with trends column.
#' @export
compute_trends <- function (.input, .column) {
    assert_renaissance (.input, .check_metadata = FALSE)

    .input |>
        group_by (.data $ vm, .data $ run, .data $ benchmark) |>
        summarize ('{{.column}}_trend' := stats::lm (y ~ x, list (x = .data $ index, y = {{.column}})) $ coefficients [['x']], .groups = 'drop')
}
