# ----------------------------------------------------------------
# Filtering by position

#' Preserve last samples in each run by count.
#'
#' Useful for extracting a set of representative samples for each run,
#' assuming the last part of each run is the most representative.
#' Can specify a maximum share of samples that the function
#' preserves. Runs with fewer samples are removed entirely.
#'
#' @param .data Measurement results to filter.
#' @param .n How many samples taken from end to preserve.
#' @param .max_share Maximum share of samples to preserve.
#' @return Filtered measurement results.
#' @export
preserve_last_n <- function (.data, .n, .max_share = 0.5) {
    assert_renaissance (.data)

    .data |>
        group_by (.data $ vm, .data $ run, .data $ benchmark) |>
        filter (.data $ index > max (.data $ index) - .env $ .n) |>
        filter (min (.data $ index) > max (.data $ index) * .env $ .max_share) |>
        ungroup ()
}


#' Preserve last samples in each run by time.
#'
#' Useful for extracting a set of representative samples for each run,
#' assuming the last part of each run is the most representative.
#' Can specify a maximum share of samples that the function
#' preserves. Runs with fewer samples are removed entirely.
#'
#' When equal sample count per run is requested,
#' it is chosen to be the median sample count.
#'
#' @param .data Measurement results to filter.
#' @param .sec How old samples taken from end to preserve.
#' @param .max_share Maximum share of samples to preserve.
#' @param .equal_count_per_run Make sample count per run equal.
#' @return Filtered measurement results.
#' @export
preserve_last_sec <- function (.data, .sec, .max_share = 0.5, .equal_count_per_run = FALSE) {
    assert_renaissance (.data)

    if (.equal_count_per_run) {

        # A tibble giving sample counts per run if equal count per run were not done.
        counts_per_run <- .data |>
            group_by (.data $ vm, .data $ run, .data $ benchmark) |>
            filter (.data $ total > max (.data $ total) - .env $ .sec) |>
            filter (min (.data $ index) > max (.data $ index) * .env $ .max_share) |>
            summarize (.count = n (), .groups = 'drop')

        # Median sample count is chosen for each compatible combination.
        counts_per_c <- counts_per_run |>
            group_by (.data $ vm, .data $ benchmark) |>
            summarize (.count = stats::median (.data $ .count), .groups = 'drop')

        # Inject desired count into data and filter accordingly.
        result <- .data |>
            inner_join (counts_per_c, by = c ('vm', 'benchmark')) |>
            group_by (.data $ vm, .data $ run, .data $ benchmark) |>
            filter (.data $ index > max (.data $ index) - .data $ .count) |>
            filter (min (.data $ index) > max (.data $ index) * .env $ .max_share) |>
            select (-.data $ .count) |>
            ungroup ()

    } else {

        result <- .data |>
            group_by (.data $ vm, .data $ run, .data $ benchmark) |>
            filter (.data $ total > max (.data $ total) - .env $ .sec) |>
            filter (min (.data $ index) > max (.data $ index) * .env $ .max_share) |>
            ungroup ()
    }

    return (result)
}


# ----------------------------------------------------------------
# Filtering by magnitude

#' Compute outlier flags.
#'
#' The outlier detection method is based on quantiles but permits retaining short tails.
#' A sample is considered an outlier if it is outside an interval computed as
#' `(lo - range * slack, hi + range * slack)` with `lo` and `hi` denoting
#' the `limit` and `1 - limit` quantiles and `range = hi - lo`.
#'
#' @param .data Sample vector.
#' @param .limit Quantile that separates inliers from outliers.
#' @param .slack Tolerated distance from limit quantile.
#' @return Boolean vector of outlier flags.
#'
#' @examples
#' # An outlier gets flagged.
#' identify_outliers_global (c (1:10, 100))
#'
#' # No outlier gets flagged with short tails.
#' identify_outliers_global (1:10)
#'
#' @seealso [identify_vector_outliers_window()]
#' @export
identify_vector_outliers_global <- function (.data, .limit = 0.05, .slack = 0.1) {
    assert_vector (.data, strict = TRUE)

    limits <- stats::quantile (.data, c (.limit, 1 - .limit))
    range <- limits [2] - limits [1]
    limit_lo <- limits [1] - range * .slack
    limit_hi <- limits [2] + range * .slack
    return (.data < limit_lo | .data > limit_hi)
}


#' Compute outlier flags using sliding window.
#'
#' Same as [identify_outliers_global()] except the quantile computation is performed
#' in a sliding window centered on each sample. Currently the computation is not
#' really efficient and runs in `O (n*w*log (w))` for window size `w`.
#'
#' @param .data Sample vector.
#' @param .window Window size in samples.
#' @param .limit Quantile that separates inliers from outliers.
#' @param .slack Tolerated distance from limit quantile.
#' @return Boolean vector of outlier flags.
#'
#' @examples
#' # A sample of 100 is considered an outlier when near
#' # different values but not when near similar values.
#' identify_outliers_window (c (100, 15:25, 95:105, 100), .window = 10)
#'
#' @seealso [identify_vector_outliers_global()]
#' @export
identify_vector_outliers_window <- function (.data, .window = 333, .limit = 0.05, .slack = 0.1) {
    assert_vector (.data, strict = TRUE)

    # For less data than window size use global filter.
    if (length (.data) < .window) return (identify_outliers_global (.data, .limit, .slack))

    # Compute window limits.
    # This is terribly inefficient.
    # Consider incremental computation.
    positions <- seq.int (1, length (.data) - .window + 1)
    limits <- sapply (positions, function (position) stats::quantile (.data [position : (position + .window - 1)], c (.limit, 1 - .limit)))
    ranges <- limits [2, ] - limits [1, ]
    limits_lo <- limits [1, ] - ranges * .slack
    limits_hi <- limits [2, ] + ranges * .slack

    # Stretch limits across border samples.
    limits_lo <- c (rep.int (first (limits_lo), floor ((.window - 1) / 2)), limits_lo, rep (last (limits_lo), ceiling ((.window - 1) / 2)))
    limits_hi <- c (rep.int (first (limits_hi), floor ((.window - 1) / 2)), limits_hi, rep (last (limits_hi), ceiling ((.window - 1) / 2)))

    return (.data < limits_lo | .data > limits_hi)
}


#' Remove outliers.
#'
#' Uses [identify_vector_outliers_global()] to identify outliers in given column. Then, removes the outlier rows.
#'
#' @param .data Data.
#' @param .column Column to identify outliers in.
#' @param ... Parameters to [identify_vector_outliers_global()].
#' @return Tibble with rows filtered.
#' @export
remove_outliers_global <- function (.data, .column, ...) {
    assert_renaissance (.data)

    .data |>
        group_by (.data $ vm, .data $ run, .data $ benchmark) |>
        filter (!identify_vector_outliers_global ({{ .column }}, ...)) |>
        ungroup ()
}


#' Remove outliers using sliding window.
#'
#' Uses [identify_vector_outliers_window()] to identify outliers in given column. Then, removes the outlier rows.
#'
#' @param .data Data.
#' @param .column Column to identify outliers in.
#' @param ... Parameters to [identify_vector_outliers_window()].
#' @return Tibble with rows filtered.
#' @export
remove_outliers_window <- function (.data, .column, ...) {
    assert_renaissance (.data)

    .data |>
        group_by (.data $ vm, .data $ run, .data $ benchmark) |>
        filter (!identify_vector_outliers_window ({{ .column }}, ...)) |>
        ungroup ()
}


#' Group helper for list outliers.
#'
#' Given the outlier flags, the index and the total time,
#' returns a list of outlier positions and intervals.
#'
#' @details
#'
#' The computation assumes that an outlier impacts a single repetition,
#' the outlier interval is therefore the interval of that repetition.
list_outliers_group_helper <- function (.flags, .index, .total) {
    tibble (
        total_before = c (0, head (.total, -1)) [.flags],
        total_after = .total [.flags],
        index = .index [.flags],
    )
}


#' List outliers.
#'
#' Uses [identify_vector_outliers_global()] to identify outliers in given column. Then, returns the list of outlier positions and intervals.
#'
#' @param .data Data.
#' @param .column Column to identify outliers in.
#' @param ... Parameters to [identify_vector_outliers_global()].
#' @return Tibble with outlier list.
#' @export
list_outliers_global <- function (.data, .column, ...) {
    assert_renaissance (.data)

    .data |>
        group_by (.data $ vm, .data $ run, .data $ benchmark) |>
        reframe (list_outliers_group_helper (identify_vector_outliers_global ({{ .column }}, ...), .data $ index, .data $ total))
}


#' List outliers using sliding window.
#'
#' Uses [identify_vector_outliers_window()] to identify outliers in given column. Then, returns the list of outlier positions and intervals.
#'
#' @param .data Data.
#' @param .column Column to identify outliers in.
#' @param ... Parameters to [identify_vector_outliers_window()].
#' @return Tibble with outlier list.
#' @export
list_outliers_window <- function (.data, .column, ...) {
    assert_renaissance (.data)

    .data |>
        group_by (.data $ vm, .data $ run, .data $ benchmark) |>
        reframe (list_outliers_group_helper (identify_vector_outliers_window ({{ .column }}, ...), .data $ index, .data $ total))
}
