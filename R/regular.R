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
#' An artifact is considered regular if it appears in the same position
#' in certain minimum share of runs.
#'
#' @param .artifacts Artifact positions and intervals.
#' @param .dimensions Data dimensions to compute share from.
#' @param .min_share Minimum share of runs to identify regular artifacts.
#' @param .min_runs Minimum count of runs to compute share from.
#' @return Tibble with regular artifacts list.
#' @export
list_regular_artifacts_by_position <- function (.artifacts, .dimensions, .min_share = 2/3, .min_runs = 11) {

    counts_artifacts <- .artifacts |>
        group_by (vm, benchmark, index) |>
        summarize (artifacts = n (), .groups = 'drop') |>
        mutate (runs_delta = 0L)

    deltas_runs_first <- .dimensions |>
        mutate (index = 1L, runs_delta = +1L) |>
        select (-run, -index_last, -total_last)
    deltas_runs_last <- .dimensions |>
        mutate (index = index_last + 1L, runs_delta = -1L) |>
        select (-run, -index_last, -total_last)

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


#' Identify regular occurrences based on intervals.
#'
#' Identifies regular occurrences in a list of artifact positions and intervals.
#' An artifact is considered regular if it appears in the same interval
#' in certain minimum share of runs.
#'
#' @param .artifacts Artifact positions and intervals.
#' @param .dimensions Data dimensions to compute share from.
#' @param .min_share Minimum share of runs to identify regular artifacts.
#' @param .min_runs Minimum count of runs to compute share from.
#' @return Tibble with regular artifacts list.
#' @export
list_regular_artifacts_by_interval <- function (.artifacts, .dimensions, .min_share = 2/3, .min_runs = 11) {

    deltas_artifacts_before <- .artifacts |>
        mutate (total = total_before, runs_delta = 0L, artifacts_delta = +1L) |>
        select (-run, -total_before, -total_after, -index)
    deltas_artifacts_after <- .artifacts |>
        mutate (total = total_after, runs_delta = 0L, artifacts_delta = -1L) |>
        select (-run, -total_before, -total_after, -index)

    deltas_runs_first <- .dimensions |>
        mutate (total = 0, runs_delta = +1L, artifacts_delta = 0L) |>
        select (-run, -index_last, -total_last)
    deltas_runs_last <- .dimensions |>
        mutate (total = total_last, runs_delta = -1L, artifacts_delta = 0L) |>
        select (-run, -index_last, -total_last)

    timeline <- bind_rows (deltas_artifacts_before, deltas_artifacts_after, deltas_runs_first, deltas_runs_last) |>
        group_by (vm, benchmark) |>
        arrange (total, .by_group = TRUE) |>
        mutate (runs = cumsum (runs_delta), artifacts = cumsum (artifacts_delta)) |>
        select (-runs_delta, -artifacts_delta) |>
        ungroup ()

    result_interim <- timeline |>
        mutate (share = artifacts / runs) |>
        group_by (vm, benchmark) |>
        mutate (current = (share >= .min_share) & (runs >= .min_runs), previous = c (FALSE, head (current, -1)))

    result_before <- result_interim |>
        filter (current & !previous) |>
        group_by (total, .add = TRUE) |>
        summarize (.groups = 'drop') |>
        rename (total_before = total)
    result_after <- result_interim |>
        filter (previous & !current) |>
        group_by (total, .add = TRUE) |>
        summarize (.groups = 'drop') |>
        rename (total_after = total)

    result <- inner_join (result_before, result_after, by = c ('vm', 'benchmark'))

    return (result)
}
