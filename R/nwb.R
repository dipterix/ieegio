#' @export
read_nwb <- function(file, mode = c("r", "w", "r+", "a", "w-", "x"), ...) {
  mode <- match.arg(mode)
  NWBHDF5IO$new(path = file, mode = mode, ...)
}
