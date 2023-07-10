# ----------------------------------------------------------------
# Helpers

#' Return full path to an example file.
#'
#' @param file_name Example file name.
#' @return Example file full path.
#' @export
rren_example <- function (file_name = '') {
    system.file ('extdata', file_name, package = 'rren')
}


#' Return data frame with artificial data.
#'
#' @param data Data to use.
#' @param vm_count Count of VM combinations to generate.
#' @param run_count Count of run combinations to generate.
#' @param benchmark_count Count of benchmark combinations to generate.
#' @return data Frame with artificial data.
#' @export
rren_artificial <- function (data, vm_count = 1L, run_count = 1L, benchmark_count = 1L) {

    result <- tibble (time = data) |>
        mutate (
            index = row_number (),
            total = cumsum (data))

    list_times_vm <- lapply (seq (vm_count),
        function (x) result |> mutate (
            vm_name = factor (glue::glue ('Name {x}')),
            vm_version = factor (glue::glue ('Version {x}')),
            vm_configuration = factor (glue::glue ('Configuration {x}')),
            vm = factor (digest::digest (c (vm_name, vm_version, vm_configuration), algo = 'murmur32'))))

    result_times_vm <- bind_rows (list_times_vm)

    list_times_run <- lapply (seq (run_count), function (x) result_times_vm |> mutate (run = factor (glue::glue ('Run {x}'))))
    result_times_run <- bind_rows (list_times_run)

    list_times_benchmark <- lapply (seq (benchmark_count), function (x) result_times_run |> mutate (benchmark = factor (glue::glue ('Benchmark {x}'))))
    result_times_benchmark <- bind_rows (list_times_benchmark)

    return (result_times_benchmark)
}
