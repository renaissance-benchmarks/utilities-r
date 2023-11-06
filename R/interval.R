# ----------------------------------------------------------------
# Bootstrap

bootstrap_replicates_vector <- function (.input, .statistic, .R) {
    # Sample one less than length to compensate variability bias.
    # This may not make much sense for some statistics ?
    length_reduced <- length (.input) - 1
    result <- rep.int (NA, .R)
    # TODO This can be made (much) faster by blocking.
    for (counter in seq.int (1, .R)) {
        indices <- sample (length_reduced, replace = TRUE)
        samples <- .input [indices]
        result [counter] <- .statistic (samples)
    }
    return (result)
}


bootstrap_replicates_list_vectors <- function (.input, .statistic_outer, .statistic_inner, .R) {
    # Sample one less than length to compensate variability bias.
    # This may not make much sense for some statistics ?
    length_reduced <- length (.input) - 1
    if (length_reduced < 1) {
        # Just one outer level, perform one level bootstrap.
        result <- bootstrap_replicates_vector (.input [[1]], function (x) .statistic_outer (.statistic_inner (x)), .R)
    } else {
        # More outer levels, perform two level bootstrap.
        result <- rep.int (NA, .R)
        for (counter_result in seq.int (1, .R)) {
            # TODO This can be made (much) faster by blocking.
            indices_outer <- sample (length_reduced, replace = TRUE)
            samples_outer <- rep.int (NA, length_reduced)
            for (counter_outer in seq.int (1, length_reduced)) {
                index_outer <- indices_outer [counter_outer]
                indices_inner <- sample (length (.input [[index_outer]]), replace = TRUE)
                samples_inner <- .input [[index_outer]] [indices_inner]
                samples_outer [counter_outer] <- .statistic_inner (samples_inner)
            }
            result [counter_result] <- .statistic_outer (samples_outer)
        }
    }
    return (result)
}


# ----------------------------------------------------------------
# Confidence interval computation

#' Compute flat percentile bootstrap confidence interval.
#'
#' @param .input Data to compute confidence interval for.
#' @param .statistic Statistic to compute confidence interval for.
#' @param .confidence Confidence level.
#' @param .R Number of replicates.
#' @return Tibble with confidence interval.
#' @export
compute_vector_flat_percentile_ci <- function (.input, .statistic = mean, .confidence = 0.99, .R = 10000) {

    replicates <- bootstrap_replicates_vector (.input, .statistic, .R)
    interval <- stats::quantile (replicates, probs = c ((1 - .confidence)/2, (1 + .confidence)/2))
    middle <- .statistic (.input)

    tibble (lo = interval [1], hi = interval [2], mid = middle)
}


#' Compute hierarchical percentile bootstrap confidence interval.
#'
#' @param .input Data to compute confidence interval for.
#' @param .statistic_outer Statistic to apply at the outer level.
#' @param .statistic_inner Statistic to apply at the inner leve.
#' @param .confidence Confidence level.
#' @param .R Number of replicates.
#' @return Tibble with confidence interval.
#' @export
compute_list_vectors_hierarchical_percentile_ci <- function (.input, .statistic_outer = mean, .statistic_inner = mean, .confidence = 0.99, .R = 10000) {

    replicates <- bootstrap_replicates_list_vectors (.input, .statistic_outer, .statistic_inner, .R)
    interval <- stats::quantile (replicates, probs = c ((1 - .confidence)/2, (1 + .confidence)/2))
    middle <- .statistic_outer (vapply (.input, .statistic_inner, numeric (1)))

    tibble (lo = interval [1], hi = interval [2], mid = middle)
}


#' Compute flat percentile bootstrap confidence interval.
#'
#' Uses [compute_vector_flat_percentile_ci()] to compute percentile confidence interval on data that is not structured into runs.
#'
#' @param .input Data not structured into runs.
#' @param .column Column to compute confidence interval for.
#' @param ... Parameters to [compute_vector_flat_percentile_ci()].
#' @return Summarized tibble with confidence interval columns.
#' @export
compute_flat_percentile_ci <- function (.input, .column, ...) {
    assert_renaissance (.input, .check_index = FALSE, .check_total = FALSE, .check_metadata = FALSE)

    .input |>
        group_by (.data $ vm, .data $ benchmark) |>
        reframe (compute_vector_flat_percentile_ci ({{ .column }}, ...))
}


#' Compute hierarchical percentile bootstrap confidence interval.
#'
#' Uses [compute_list_vectors_hierarchical_percentile_ci()] to compute percentile confidence interval on data that is structured into runs.
#'
#' @param .input Data structured into runs.
#' @param .column Column to compute confidence interval for.
#' @param ... Parameters to [compute_vector_flat_percentile_ci()].
#' @return Summarized tibble with confidence interval columns.
#' @export
compute_hierarchical_percentile_ci <- function (.input, .column, ...) {
    assert_renaissance (.input, .check_index = FALSE, .check_total = FALSE, .check_metadata = FALSE)

    .input |>
        group_by (.data $ vm, .data $ benchmark) |>
        reframe (compute_list_vectors_hierarchical_percentile_ci (split ({{ .column }}, .data $ run, drop = TRUE), ...))
}
