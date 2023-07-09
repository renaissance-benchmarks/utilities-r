# ----------------------------------------------------------------
# Helpers

#' Return full path to an example file.
#'
#' @param file_name example file name
#' @return example file full path
#' @export
rren_example <- function (file_name = '') {
    system.file ('extdata', file_name, package = 'rren')
}


#' Return data frame with artificial data.
#'
#' @param data data
#' @return data frame with artificial data
#' @export
rren_artificial <- function (data, benchmark) {
    result <- tibble (time = data) |>
        mutate (
            index = row_number (),
            total = cumsum (data),
            run = factor ('Artificial Run'),
            benchmark = factor ('Artificial Benchmark'),
            vm_name = factor ('Artificial Name'),
            vm_version = factor ('Artificial Version'),
            vm_configuration = factor ('Artificial Configuration'),
            vm = factor (digest::digest (c (vm_name, vm_version, vm_configuration), algo = 'murmur32')))

    return (result)
}
