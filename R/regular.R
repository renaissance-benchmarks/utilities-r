# ----------------------------------------------------------------
# Regular position identification

#' Get dimensions.
#'
#' Returns dimensions of the data, useful to compute relative frequencies in regular position identification.
#'
#' @param .data Data.
#' @return Tibble with data dimensions.
#' @export
list_dimensions <- function (.data) {
    assert_renaissance (.data)

    .data |>
        group_by (.data $ vm, .data $ run, .data $ benchmark) |>
        summarize (index_last = max (index), total_last = max (total), .groups = 'drop')
}


#' Identify regular occurrences based on position.
#'
#' Identifies regular occurrences in a list of artifact positions and intervals.
#' An artifact is considered regular if it appears in certain minimum share of runs.
#'
#' @param .artifact Artifact positions and intervals.
#' @param .dimensions Data dimensions to compute share from.
#' @param .min_share Minimum share of runs to identify regular artifacts.
#' @param .min_runs Minimum count of runs to compute share from.
#' @return Tibble with regular artifacts list.
#' @export
list_regular_artifacts <- function (.artifact, .dimensions, .min_share = 2/3, .min_runs = 11) {

    counts_artifacts <- .artifacts |>
        group_by (vm, benchmark, index) |>
        summarize (artifacts = n (), .groups = 'drop') |>
        mutate (runs_delta = 0L)

    deltas_runs_first <- .dimensions |>
        group_by (vm, run, benchmark) |>
        summarize (index = 1L, runs_delta = +1L, .groups = 'drop') |>
        select (-run)
    deltas_runs_last <- .dimensions |>
        group_by (vm, run, benchmark) |>
        summarize (index = index_last + 1L, runs_delta = -1L, .groups = 'drop') |>
        select (-run)

    timeline <- bind_rows (counts_artifacts, deltas_runs_first, deltas_runs_last) |>
        group_by (vm, benchmark) |>
        arrange (index, .by_group = TRUE) |>
        mutate (runs = cumsum (runs_delta)) |>
        select (-runs_delta) |>
        ungroup ()

    result <- timeline |>
        mutate (share = artifacts / runs) |>
        filter (share >= .min_share, runs >= .min_runs)

    return (result)
}
