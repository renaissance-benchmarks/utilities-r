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
