# ----------------------------------------------------------------
# Helpers

add_index <- function (data_read) {
    data_read |> mutate (index = row_number ())
}

add_time_from_nanos <- function (data_read) {
    data_read |> mutate (time = .data $ nanos / 1e9)
}

add_time_from_duration <- function (data_read) {
    data_read |> mutate (time = .data $ duration_ns / 1e9)
}

add_cumulative_time_from_uptime <- function (data_read) {
    data_read |> mutate (total = (.data $ uptime_ns + .data $ duration_ns) / 1e9)
}

add_cumulative_time_from_time_unix <- function (data_read) {
    # Using accumulated repetition time misses gaps between repetitions.
    # Using UNIX time reduces resolution to miliseconds.
    # As a compromise we use UNIX time to strech
    # accumulated repetition time.
    total <- cumsum (data_read |> pull (.data $ time))
    unix_beg <- data_read |> slice_head () |> pull (.data $ unixts.before)
    unix_end <- data_read |> slice_tail () |> pull (.data $ unixts.after)
    unix_range <- (unix_end - unix_beg) / 1e3
    time_range <- last (total)
    scale <- unix_range / time_range
    total <- total * scale
    data_read |> add_column (total)
}

normalize_column_names <- function (data_read) {
    data_read |> rename_with (str_replace_all, everything (), '[-:]', '_')
}


# ----------------------------------------------------------------
# Version specific meta extraction

extract_meta_versions_1_2 <- function (data_json) {
    tibble (
        vm_name = factor (data_json [['environment']] [['vm']] [['name']]),
        vm_version = factor (data_json [['environment']] [['vm']] [['vm_version']]),
        vm_configuration = factor (paste (data_json [['environment']] [['vm']] [['args']], collapse = ' ')))
}

extract_meta_versions_5_6 <- function (data_json) {
    tibble (
        vm_name = factor (data_json [['environment']] [['vm']] [['name']]),
        vm_version = factor (data_json [['environment']] [['vm']] [['version']]),
        vm_configuration = factor (paste (data_json [['environment']] [['vm']] [['args']], collapse = ' ')))
}

extract_meta <- function (data_json, version) {
    if (version %in% c (5, 6)) return (extract_meta_versions_5_6 (data_json))
    if (version %in% c (1, 2)) return (extract_meta_versions_1_2 (data_json))
    log_error ('Data format version {version} not supported.')
    stop ()
}


# ----------------------------------------------------------------
# Version specific data extraction

extract_data_versions_1_2 <- function (data_json, benchmark) {

    data_read <- as_tibble (data_json [['results']] [[benchmark]])
    assert_tibble (data_read, min.rows = 1)

    data_read |>
        # Transform basic columns into canonical form.
        add_index () |>
        add_time_from_nanos () |>
        add_cumulative_time_from_time_unix () |>
        # Preserve optional columns not transformed into canonical form.
        select (!c ('nanos', 'unixts.before', 'unixts.after')) |>
        normalize_column_names ()
}


extract_data_versions_5_6 <- function (data_json, benchmark) {

    data_read <- as_tibble (data_json [['data']] [[benchmark]] [['results']])
    assert_tibble (data_read, min.rows = 1)

    data_read |>
        # Transform basic columns into canonical form.
        add_index () |>
        add_time_from_duration () |>
        add_cumulative_time_from_uptime () |>
        # Preserve optional columns not transformed into canonical form.
        select (!c ('duration_ns', 'uptime_ns')) |>
        normalize_column_names ()
}


extract_data <- function (data_json, benchmark, version, meta_read) {
    data_read <- NULL
    if (version %in% c (5, 6)) data_read <- (extract_data_versions_5_6 (data_json, benchmark))
    if (version %in% c (1, 2)) data_read <- (extract_data_versions_1_2 (data_json, benchmark))
    assert_tibble (data_read, min.rows = 1)
    bind_cols (data_read, meta_read, tibble (benchmark = factor (benchmark)))
}


# ----------------------------------------------------------------
# Load data

#' Load data from given file.
#'
#' @details
#'
#' The basic timing columns are translated into canonical form:
#' - `time` will contain individual repetition time in seconds
#' - `total` will contain aggregate execution time at the end of repetition in seconds
#' - `index` will contain the index of the measurement in the run starting from 1
#'
#' Additional metadata is included as factor columns:
#' - `vm` is a hash of all configuration metadata and can be used to distinguish incompatible measurement configurations
#' - `vm_name` is the name of the virtual machine
#' - `vm_version` is the version of the virtual machine
#' - `vm_configuration` gives concatenated command line arguments
#' - `run` is a unique identifier of the run
#' - `benchmark` is the name of the benchmark
#'
#' @param data_file Data file to load from.
#' @return Tibble with loaded data.
#'
#' @examples
#' load_file_json (rren_example ('results-small-version-5.json'))
#'
#' @seealso [load_path_json()]
#' @export
load_file_json <- function (data_file) {

    data_json <- jsonlite::fromJSON (data_file)

    version <- data_json [['format_version']]
    benchmarks <- data_json [['benchmarks']]

    # Extract meta.
    # Use file name as run id.
    # Use short config digest as vm id.
    meta_read <- extract_meta (data_json, version) |>
        mutate (
            run = factor (.env $ data_file),
            vm = factor (digest::digest (c (.data $ vm_name, .data $ vm_version, .data $ vm_configuration), algo = 'murmur32')))

    # Extract benchmark data individually.
    data_list <- lapply (benchmarks, function (benchmark) extract_data (data_json, benchmark, version, meta_read))
    data_read <- bind_rows (data_list)

    log_info ('Loaded {nrow (data_read)} rows from file {data_file}.')

    return (data_read)
}


#' Load data from given path recursively.
#'
#' @param data_path Data path to load from.
#' @param pattern Regular expression pattern to match.
#' @param cache File name to use as data cache.
#' @return Tibble with loaded data.
#'
#' @examples
#' load_path_json (rren_example (), pattern = '^results-small-version-[0-9]+\\.json$')
#'
#' @seealso [load_file_json()]
#' @export
load_path_json <- function (data_path, pattern = '\\.json(|\\.gz|\\.xz|\\.bz2)$', cache = NULL) {

    if (!is.null (cache) && fs::file_exists (cache)) {

        data_read <- readRDS (cache)
        log_info ('Loaded {nrow (data_read)} rows from cache {cache}.')

    } else {

        data_file_list <- list.files (data_path, pattern, recursive = TRUE, full.names = TRUE)
        data_list <- lapply (data_file_list, load_file_json)
        data_read <- bind_rows (data_list)
        log_info ('Loaded {nrow (data_read)} rows from path {data_path}.')

        if (!is.null (cache)) {
            saveRDS (data_read, cache)
        }
    }

    return (data_read)
}


# ----------------------------------------------------------------
# Sanity

check_columns <- function (.input, .names, .test, .type) {
    for (name in .names) {
        if (!name %in% colnames (.input)) return (glue::glue ('Column {name} must be present'))
        if (!.test (.input [[name]])) return (glue::glue ('Column {name} must be {.type}'))
    }
    return (TRUE)
}


#' Check whether data resembles typical measurement results.
#'
#' @param .input Measurement results to test.
#' @param .check_index Check presence of index.
#' @param .check_total Check presence of total.
#' @param .check_metadata Check presence of descriptive metadata columns.
#'
#' @return Error message or TRUE.
#'
#' @export
check_renaissance <- function (.input, .check_index = TRUE, .check_total = TRUE, .check_metadata = TRUE) {

    # The basic structure is a tibble.
    res <- check_tibble (.input)
    if (!isTRUE (res)) return (res)

    # Combination columns are factorial.
    res <- check_columns (.input, c ('vm', 'run', 'benchmark'), test_factor, 'a factor')
    if (!isTRUE (res)) return (res)

    # Metadata columns are factorial.
    if (.check_metadata) {
        res <- check_columns (.input, c ('vm_name', 'vm_version', 'vm_configuration'), test_factor, 'a factor')
        if (!isTRUE (res)) return (res)
    }

    if (.check_index) {
        # Index column is non negative integer.
        res <- check_columns (.input, c ('index'), test_integer, 'an integer')
        if (!isTRUE (res)) return (res)
        res <- check_columns (.input, c ('index'), function (x) all (x >= 0), 'non negative')
        if (!isTRUE (res)) return (res)
    }

    if (.check_total) {
        # Total column is non negative number.
        res <- check_columns (.input, c ('total'), test_numeric, 'a number')
        if (!isTRUE (res)) return (res)
        res <- check_columns (.input, c ('total'), function (x) all (x >= 0), 'non negative')
        if (!isTRUE (res)) return (res)
    }

    return (TRUE)
}


#' Check whether data resembles typical measurement results.
#'
#' @param .input Measurement results to test.
#' @param .var.name Internal.
#' @param add Internal.
#' @param ... Parameters to [check_renaissance()].
#' @return Invisible .data or an exception.
#'
#' @seealso [check_renaissance]
#' @export
assert_renaissance <- function (.input, .var.name = vname (.data), add = NULL, ...) {
    # Does not use `makeAssertionFunction` because `check` complains about arguments.
    makeAssertion (.input, check_renaissance (.input, ...), .var.name, add)
}


#' Check whether data resembles typical measurement results.
#'
#' @param .input Measurement results to test.
#' @param info Internal.
#' @param label Internal.
#' @param ... Parameters to [check_renaissance()].
#' @return An expectation.
#'
#' @seealso [check_renaissance()]
#' @export
expect_renaissance <- function (.input, info = NULL, label = vname (.data), ...) {
    # Does not use `makeExpectationFunction` because `check` complains about arguments.
    makeExpectation (.input, check_renaissance (.input, ...), info = info, label = label)
}
