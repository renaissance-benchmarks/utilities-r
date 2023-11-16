# ----------------------------------------------------------------
# Bootstrap

bootstrap_replicates_vector <- function (.input, .statistic, .R, .reduction) {

    # Reduction in sample size can help compensate bootstrap standard error bias.
    length_original <- length (.input)
    length_reduced <- length (.input) - .reduction

    result <- rep.int (NA, .R)
    # TODO This can be made (much) faster by blocking.
    for (counter in seq.int (1, .R)) {
        indices <- sample (length_original, length_reduced, replace = TRUE)
        samples <- .input [indices]
        result [counter] <- .statistic (samples)
    }

    return (result)
}


bootstrap_replicates_list_vectors <- function (.input, .statistic_outer, .statistic_inner, .R, .reduction) {

    # Reduction in sample size can help compensate bootstrap standard error bias.
    length_original <- length (.input)
    length_reduced <- length (.input) - .reduction

    if (length_original == 1) {

        # Just one outer level, perform one level bootstrap.
        # Note that this does not reduce sample size in inner level.
        result <- bootstrap_replicates_vector (.input [[1]], function (x) .statistic_outer (.statistic_inner (x)), .R, 0)

    } else {
        # More outer levels, perform two level bootstrap.
        result <- rep.int (NA, .R)
        for (counter_result in seq.int (1, .R)) {
            # TODO This can be made (much) faster by blocking.
            indices_outer <- sample (length_original, length_reduced, replace = TRUE)
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
#' @param .reduction Replicate size reduction.
#' @return Tibble with confidence interval.
#' @export
compute_vector_flat_percentile_ci <- function (.input, .statistic = mean, .confidence = 0.99, .R = 10000, .reduction = 0) {

    replicates <- bootstrap_replicates_vector (.input, .statistic, .R, .reduction)
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
#' @param .reduction Replicate size reduction.
#' @return Tibble with confidence interval.
#' @export
compute_list_vectors_hierarchical_percentile_ci <- function (.input, .statistic_outer = mean, .statistic_inner = mean, .confidence = 0.99, .R = 10000, .reduction = 0) {

    replicates <- bootstrap_replicates_list_vectors (.input, .statistic_outer, .statistic_inner, .R, .reduction)
    interval <- stats::quantile (replicates, probs = c ((1 - .confidence)/2, (1 + .confidence)/2))
    middle <- .statistic_outer (vapply (.input, .statistic_inner, numeric (1)))

    tibble (lo = interval [1], hi = interval [2], mid = middle)
}


#' Compute flat percentile bootstrap confidence interval on data set pair.
#'
#' @param .input_one Data set one to compute with.
#' @param .input_two Data set two to compute with.
#' @param .combination Combination to compute for both data sets.
#' @param .statistic Statistic to compute for each data set.
#' @param .confidence Confidence level.
#' @param .R Number of replicates.
#' @param .reduction Replicate size reduction.
#' @return Tibble with confidence interval.
#' @export
compute_vector_pair_flat_percentile_ci <- function (.input_one, .input_two, .combination = .Primitive ('-'), .statistic = mean, .confidence = 0.99, .R = 10000, .reduction = 0) {

    replicates_one <- bootstrap_replicates_vector (.input_one, .statistic, .R, .reduction)
    replicates_two <- bootstrap_replicates_vector (.input_two, .statistic, .R, .reduction)
    replicates <- .combination (replicates_one, replicates_two)
    interval <- stats::quantile (replicates, probs = c ((1 - .confidence)/2, (1 + .confidence)/2))
    middle <- .combination (.statistic (.input_one), .statistic (.input_two))

    tibble (lo = interval [1], hi = interval [2], mid = middle)
}


#' Compute hierarchical percentile bootstrap confidence interval on data set pair.
#'
#' @param .input_one Data set one to compute with.
#' @param .input_two Data set two to compute with.
#' @param .combination Combination to compute for both data sets.
#' @param .statistic_outer Statistic to compute at the outer level.
#' @param .statistic_inner Statistic to compute at the inner leve.
#' @param .confidence Confidence level.
#' @param .R Number of replicates.
#' @param .reduction Replicate size reduction.
#' @return Tibble with confidence interval.
#' @export
compute_list_vectors_pair_hierarchical_percentile_ci <- function (.input_one, .input_two, .combination = .Primitive ('-'), .statistic_outer = mean, .statistic_inner = mean, .confidence = 0.99, .R = 10000, .reduction = 0) {

    replicates_one <- bootstrap_replicates_list_vectors (.input_one, .statistic_outer, .statistic_inner, .R, .reduction)
    replicates_two <- bootstrap_replicates_list_vectors (.input_two, .statistic_outer, .statistic_inner, .R, .reduction)
    replicates <- .combination (replicates_one, replicates_two)
    interval <- stats::quantile (replicates, probs = c ((1 - .confidence)/2, (1 + .confidence)/2))
    middle <- .combination (
        .statistic_outer (vapply (.input_one, .statistic_inner, numeric (1))),
        .statistic_outer (vapply (.input_two, .statistic_inner, numeric (1))))

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
        group_by (.data $ vm, across (starts_with ('vm_')), .data $ benchmark) |>
        reframe (compute_vector_flat_percentile_ci ({{ .column }}, ...))
}


#' Compute hierarchical percentile bootstrap confidence interval.
#'
#' Uses [compute_list_vectors_hierarchical_percentile_ci()] to compute percentile confidence interval on data that is structured into runs.
#'
#' @param .input Data structured into runs.
#' @param .column Column to compute confidence interval for.
#' @param ... Parameters to [compute_list_vectors_hierarchical_percentile_ci()].
#' @return Summarized tibble with confidence interval columns.
#' @export
compute_hierarchical_percentile_ci <- function (.input, .column, ...) {
    assert_renaissance (.input, .check_index = FALSE, .check_total = FALSE, .check_metadata = FALSE)

    .input |>
        group_by (.data $ vm, across (starts_with ('vm_')), .data $ benchmark) |>
        reframe (compute_list_vectors_hierarchical_percentile_ci (split ({{ .column }}, .data $ run, drop = TRUE), ...))
}


#' Compute flat relative percentile bootstrap confidence interval.
#'
#' Uses [compute_vector_pair_flat_percentile_ci()] to compute percentile
#' confidence interval on data that is not structured into runs.
#'
#' The statistic is computed relative to baseline virtual machine.
#'
#' @param .input Data not structured into runs.
#' @param .baseline Baseline virtual machine.
#' @param .column Column to compute confidence interval for.
#' @param ... Parameters to [compute_vector_pair_flat_percentile_ci()].
#' @return Summarized tibble with confidence interval columns.
#' @export
compute_flat_relative_percentile_ci <- function (.input, .baseline, .column, ...) {
    assert_renaissance (.input, .check_index = FALSE, .check_total = FALSE, .check_metadata = FALSE)

    # Subset baseline to reduce amount of filtering per benchmark.
    baseline_for_vm <- .input |> filter (.data $ vm == .baseline)

    baseline_for_benchmark <- function (benchmark) {
        force (benchmark)
        baseline <- baseline_for_vm |> filter (.data $ benchmark == .env $ benchmark)
        return (baseline |> pull ({{ .column }}))
    }

    .input |>
        group_by (.data $ vm, across (starts_with ('vm_')), .data $ benchmark) |>
        reframe (compute_vector_pair_flat_percentile_ci (
            {{ .column }},
            baseline_for_benchmark (cur_group () $ benchmark),
            ...))
}


#' Compute hierarchical relative percentile bootstrap confidence interval.
#'
#' Uses [compute_list_vectors_pair_hierarchical_percentile_ci()] to compute percentile
#' confidence interval on data that is structured into runs.
#'
#' The statistic is computed relative to baseline virtual machine.
#'
#' @param .input Data structured into runs.
#' @param .baseline Baseline virtual machine.
#' @param .column Column to compute confidence interval for.
#' @param ... Parameters to [compute_list_vectors_pair_hierarchical_percentile_ci()].
#' @return Summarized tibble with confidence interval columns.
#' @export
compute_hierarchical_relative_percentile_ci <- function (.input, .baseline, .column, ...) {
    assert_renaissance (.input, .check_index = FALSE, .check_total = FALSE, .check_metadata = FALSE)

    # Subset baseline to reduce amount of filtering per benchmark.
    baseline_for_vm <- .input |> filter (.data $ vm == .baseline)

    baseline_for_benchmark <- function (benchmark) {
        force (benchmark)
        baseline <- baseline_for_vm |> filter (.data $ benchmark == .env $ benchmark)
        return (split (baseline_for_vm |> pull ({{ .column }}), baseline_for_vm $ run, drop = TRUE))
    }

    .input |>
        group_by (.data $ vm, across (starts_with ('vm_')), .data $ benchmark) |>
        reframe (compute_list_vectors_pair_hierarchical_percentile_ci (
            split ({{ .column }}, .data $ run, drop = TRUE),
            baseline_for_benchmark (cur_group () $ benchmark),
            ...))
}
