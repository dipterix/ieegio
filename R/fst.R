#' @rdname low-level-read-write
#'
#' @examples
#'
#' # ---- fst ----------------------------------------------------------------
#'
#'
#' f <- tempfile(fileext = ".fst")
#' x <- data.frame(
#'   a = 1:10,
#'   b = rnorm(10),
#'   c = letters[1:10]
#' )
#'
#' io_write_fst(x, con = f)
#'
#' # default reads in proxy
#' io_read_fst(f)
#'
#' # load as data.table
#' io_read_fst(f, "data_table")
#'
#' # load as data.frame
#' io_read_fst(f, "data_frame")
#'
#' # get header
#' io_read_fst(f, "header_only")
#'
#' # clean up
#' unlink(f)
#'
#' @export
io_read_fst <- function(con, method = c("proxy", "data_table", "data_frame", "header_only"), ..., old_format = FALSE) {
  method <- match.arg(method)
  switch(
    method,
    "proxy" = { fst::fst(path = con, old_format = old_format) },
    "data_table" = { fst::read_fst(path = con, as.data.table = TRUE, old_format = old_format, ...) },
    "data_frame" = { fst::read_fst(path = con, as.data.table = FALSE, old_format = old_format, ...) },
    "header_only" = { fst::metadata_fst(path = con, old_format = old_format) }
  )
}

#' @rdname low-level-read-write
#' @export
io_write_fst <- function(x, con, compress = 50, ...) {
  fst::write_fst(x = x, path = con, compress = compress, ...)
}
