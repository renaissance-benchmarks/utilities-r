# ----------------------------------------------------------------
# Confidence interval computation

#' Group helper for flat percentile bootstrap confidence interval.
#'
#' @param .input Data to compute confidence interval for.
#' @param .statistic Statistic to compute confidence interval for.
#' @param .confidence Confidence level.
#' @param .R Number of replicates.
#' @return Tibble with confidence interval.
compute_flat_percentile_cis_group_helper <- function (.input, .statistic, .confidence, .R) {

    sampler <- function (n, R) {
        assert_true (n == length (.input))

        # Applies recommended correction for low variance estimate.
        resample::samp.bootstrap (n, R, n - 1)
    }

    # Extra indirection on statistic needed due to the way the `resample` package examines arguments.
    data_boot <- resample::bootstrap (.input, function (x) .statistic (x), .R, sampler = sampler)
    data_ci <- resample::CI.percentile (data_boot, confidence = .confidence)
    tibble (lo = data_ci [1], hi = data_ci [2], mid = .statistic (.input))
}


#' Compute flat percentile bootstrap confidence interval.
#'
#' Uses bootstrap to compute percentile confidence interval on data summarized across runs.
#'
#' @param .input Data summarized across runs.
#' @param .column Column to compute confidence interval for.
#' @param .statistic Statistic to compute confidence interval for.
#' @param .confidence Confidence level.
#' @param .R Replicates.
#' @return Summarized tibble with confidence interval columns.
#' @export
compute_flat_percentile_cis <- function (.input, .column, .statistic = mean, .confidence = 0.99, .R = 10000) {
    assert_renaissance (.input, .check_index = FALSE, .check_total = FALSE, .check_metadata = FALSE)

    .input |>
        group_by (.data $ vm, .data $ benchmark) |>
        reframe (compute_flat_percentile_cis_group_helper ({{ .column }}, .statistic, .confidence, .R)) |>
        rename ('{{.column}}_lo' := 'lo', '{{.column}}_hi' := 'hi', '{{.column}}_mid' := 'mid')
}
