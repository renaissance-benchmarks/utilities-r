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
        if (penalty == 'barrett') {
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
