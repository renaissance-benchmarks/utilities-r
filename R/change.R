# ----------------------------------------------------------------
# Change points detection

#' Locate change point segments in samples.
#'
#' Treats samples as segments separated by change points.
#' Returns indices of the first sample of each segment.
#'
#' Relies on the PELT algorithm implementation in the [changepoint::cpt.meanvar()] function.
#'
#' @param .data Measurement results to analyze.
#' @param .penalty A numerical penalty or `barrett` for penalty computation from [Barrett et al.](https://doi.org/10.1145/3133876).
#' @return Vector of segment starting points.
#' @export
locate_vector_segments <- function (.data, .penalty) {
    assert_vector (.data, strict = TRUE)

    # Make sure samples are not integer because the library fails silently on integer inputs.
    .data <- as.numeric (.data)

    # Convert penalty settings to what the library accepts.
    if (is.character (.penalty)) {
        .penalty <- match.arg (.penalty, c ('barrett'))
        if (.penalty == 'barrett') {
            pen_type <- 'Manual'
            pen_value <- 15 * log (length (.data))
        }
    }
    if (is.numeric (.penalty)) {
        pen_type <- 'Manual'
        pen_value <- .penalty
    }

    positions <- tryCatch (
        changepoint::cpts (changepoint::cpt.meanvar (.data, method = 'PELT', penalty = pen_type, pen.value = pen_value)),
        error = function (e) return (NULL))

    # The library returns indices of the last sample before change.
    # Convert this to indices of the first changed sample.
    # Include the initial segment position too.
    return (c (1, positions + 1))
}


#' Group helper for list segment boundaries.
#'
#' Given the segment positions, the index and the total time,
#' returns a list of segment boundary positions and intervals.
#'
#' @details
#'
#' The computation assumes that a segment boundary exists in either boundary repetition,
#' the boundary interval is therefore formed by the mid points of the two repetitions,
#' arguing that less than half of each boundary repetition is impacted
#' (otherwise segments would be assigned differently).
#'
#' Head and tail boundaries are not included.
#'
#' @param .positions Segment positions.
#' @param .index Data index.
#' @param .total Data total.
#' @return Tibble with segment boundary list.
list_segment_boundaries_group_helper <- function (.positions, .index, .total) {
    padded_middle <- (c (NA, 0, .total) + c (NA, .total, NA)) / 2
    tibble (
        total_before = padded_middle [.positions],
        total_after = padded_middle [.positions + 1],
        index = .index [.positions],
    ) |> tidyr::drop_na ()
}


#' List segment boundaries.
#'
#' Uses [locate_vector_segments()] to identify segments in given column. Then, returns the list of segment boundary positions and intervals.
#'
#' @param .data Data.
#' @param .column Column to identify segments in.
#' @param ... Parameters to [locate_vector_segments()].
#' @return Tibble with segment boundary list.
#' @export
list_segment_boundaries <- function (.data, .column, ...) {
    assert_renaissance (.data, .check_metadata = FALSE)

    .data |>
        group_by (.data $ vm, .data $ run, .data $ benchmark) |>
        reframe (list_segment_boundaries_group_helper (locate_vector_segments ({{ .column }}, ...), .data $ index, .data $ total))
}
