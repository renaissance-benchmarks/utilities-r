# ----------------------------------------------------------------
# Helpers

add_index <- function (data_read) {
    data_read %>% mutate (index = row_number ())
}

add_time_from_nanos <- function (data_read) {
    data_read %>% mutate (time = .data $ nanos / 1e9)
}

add_time_from_duration <- function (data_read) {
    data_read %>% mutate (time = .data $ duration_ns / 1e9)
}

add_cumulative_time_from_uptime <- function (data_read) {
    data_read %>% mutate (total = (.data $ uptime_ns + .data $ duration_ns) / 1e9)
}

add_cumulative_time_from_time_unix <- function (data_read) {
    # Using accumulated repetition time misses gaps between repetitions.
    # Using UNIX time reduces resolution to miliseconds.
    # As a compromise we use UNIX time to strech
    # accumulated repetition time.
    total <- cumsum (data_read %>% pull (.data $ time))
    unix_beg <- data_read %>% slice_head () %>% pull (.data $ unixts.before)
    unix_end <- data_read %>% slice_tail () %>% pull (.data $ unixts.after)
    unix_range <- (unix_end - unix_beg) / 1e3
    time_range <- last (total)
    scale <- unix_range / time_range
    total <- total * scale
    data_read %>% add_column (total)
}

normalize_column_names <- function (data_read) {
    data_read %>% rename_with (str_replace_all, everything (), '[-:]', '_')
}


# ----------------------------------------------------------------
# Version specific meta extraction

extract_meta_versions_1_2 <- function (data_json) {
    tibble (
        vm_name = factor (data_json [['environment']] [['vm']] [['name']]),
        vm_version = factor (data_json [['environment']] [['vm']] [['vm_version']]),
        vm_configuration = factor (paste (data_json [['environment']] [['vm']] [['args']], collapse = ' ')))
}

extract_meta_version_5 <- function (data_json) {
    tibble (
        vm_name = factor (data_json [['environment']] [['vm']] [['name']]),
        vm_version = factor (data_json [['environment']] [['vm']] [['version']]),
        vm_configuration = factor (paste (data_json [['environment']] [['vm']] [['args']], collapse = ' ')))
}

extract_meta <- function (data_json, version) {
    if (version %in% c (5)) return (extract_meta_version_5 (data_json))
    if (version %in% c (1, 2)) return (extract_meta_versions_1_2 (data_json))
    log_error ('Data format version {version} not supported.')
    stop ()
}


# ----------------------------------------------------------------
# Version specific data extraction

extract_data_versions_1_2 <- function (data_json, benchmark) {

    data_read <- as_tibble (data_json [['results']] [[benchmark]])
    assert_tibble (data_read, min.rows = 1)

    data_read %>%
        # Transform basic columns into canonical form.
        add_index () %>%
        add_time_from_nanos () %>%
        add_cumulative_time_from_time_unix () %>%
        # Preserve optional columns not transformed into canonical form.
        select (-.data $ nanos, -.data $ unixts.before, -.data $ unixts.after) %>%
        normalize_column_names ()
}


extract_data_version_5 <- function (data_json, benchmark) {

    data_read <- as_tibble (data_json [['data']] [[benchmark]] [['results']])
    assert_tibble (data_read, min.rows = 1)

    data_read %>%
        # Transform basic columns into canonical form.
        add_index () %>%
        add_time_from_duration () %>%
        add_cumulative_time_from_uptime () %>%
        # Preserve optional columns not transformed into canonical form.
        select (-.data $ duration_ns, -.data $ uptime_ns) %>%
        normalize_column_names ()
}


extract_data <- function (data_json, benchmark, version, meta_read) {
    data_read <- NULL
    if (version %in% c (5)) data_read <- (extract_data_version_5 (data_json, benchmark))
    if (version %in% c (1, 2)) data_read <- (extract_data_versions_1_2 (data_json, benchmark))
    assert_tibble (data_read, min.rows = 1)
    bind_cols (data_read, meta_read, tibble (benchmark = factor (benchmark)))
}


# ----------------------------------------------------------------
# Public

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
#' @param data_file data file to load from
#' @return tibble with loaded data
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
    meta_read <- extract_meta (data_json, version) %>%
        mutate (
            run = data_file,
            vm = digest::digest (c (.data $ vm_name, .data $ vm_version, .data $ vm_configuration), algo = 'murmur32'))

    # Extract benchmark data individually.
    data_list <- lapply (benchmarks, function (benchmark) extract_data (data_json, benchmark, version, meta_read))
    data_read <- bind_rows (data_list)

    log_info ('Loaded {nrow (data_read)} rows from file {data_file}.')

    return (data_read)
}


#' Load data from given path recursively.
#'
#' @param data_path data path to load from
#' @param pattern regular expression pattern to match
#' @param cache file name to use as data cache
#' @return tibble with loaded data
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
