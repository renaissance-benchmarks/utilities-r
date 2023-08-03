# ----------------------------------------------------------------
# Regular position identification

#' Get dimensions.
#'
#' Returns dimensions of the data, useful to compute relative frequencies in regular position identification.
#'
#' @param .input Data.
#' @return Tibble with data dimensions.
#' @export
list_dimensions <- function (.input) {
    assert_renaissance (.input, .check_metadata = FALSE)

    .input |>
        group_by (.data $ vm, .data $ run, .data $ benchmark) |>
        summarize (index_last = max (.data $ index), total_last = max (.data $ total), .groups = 'drop')
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
        group_by (.data $ vm, .data $ benchmark, .data $ index) |>
        summarize (artifacts = n (), .groups = 'drop') |>
        mutate (runs_delta = 0L)

    deltas_runs_first <- .dimensions |>
        mutate (index = 1L, runs_delta = +1L) |>
        select (!c ('run', 'index_last', 'total_last'))
    deltas_runs_last <- .dimensions |>
        mutate (index = .data $ index_last + 1L, runs_delta = -1L) |>
        select (!c ('run', 'index_last', 'total_last'))

    timeline <- bind_rows (counts_artifacts, deltas_runs_first, deltas_runs_last) |>
        group_by (.data $ vm, .data $ benchmark) |>
        arrange (.data $ index, .by_group = TRUE) |>
        mutate (runs = cumsum (.data $ runs_delta)) |>
        select (!c ('runs_delta')) |>
        ungroup ()

    result <- timeline |>
        mutate (share = .data $ artifacts / .data $ runs) |>
        filter (.data $ share >= .min_share, .data $ runs >= .min_runs)

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
        mutate (total = .data $ total_before, runs_delta = 0L, artifacts_delta = +1L) |>
        select (!c ('run', 'total_before', 'total_after', 'index'))
    deltas_artifacts_after <- .artifacts |>
        mutate (total = .data $ total_after, runs_delta = 0L, artifacts_delta = -1L) |>
        select (!c ('run', 'total_before', 'total_after', 'index'))

    deltas_runs_first <- .dimensions |>
        mutate (total = 0, runs_delta = +1L, artifacts_delta = 0L) |>
        select (!c ('run', 'index_last', 'total_last'))
    deltas_runs_last <- .dimensions |>
        mutate (total = .data $ total_last, runs_delta = -1L, artifacts_delta = 0L) |>
        select (!c ('run', 'index_last', 'total_last'))

    timeline <- bind_rows (deltas_artifacts_before, deltas_artifacts_after, deltas_runs_first, deltas_runs_last) |>
        group_by (.data $ vm, .data $ benchmark) |>
        arrange (.data $ total, .by_group = TRUE) |>
        mutate (runs = cumsum (.data $ runs_delta), artifacts = cumsum (.data $ artifacts_delta)) |>
        select (!c ('runs_delta', 'artifacts_delta')) |>
        ungroup ()

    result_interim <- timeline |>
        mutate (share = .data $ artifacts / .data $ runs) |>
        group_by (.data $ vm, .data $ benchmark) |>
        mutate (current = (.data $ share >= .min_share) & (.data $ runs >= .min_runs), previous = c (FALSE, utils::head (.data $ current, -1)))

    result_before <- result_interim |>
        filter (.data $ current & !.data $ previous) |>
        group_by (.data $ total, .add = TRUE) |>
        summarize (.groups = 'drop_last') |>
        mutate (interval = row_number ()) |>
        rename (total_before = 'total')
    result_after <- result_interim |>
        filter (.data $ previous & !.data $ current) |>
        group_by (.data $ total, .add = TRUE) |>
        summarize (.groups = 'drop_last') |>
        mutate (interval = row_number ()) |>
        rename (total_after = 'total')

    result <- full_join (result_before, result_after, by = c ('vm', 'benchmark', 'interval'), relationship = 'one-to-one') |> select (!c ('interval'))

    return (result)
}
