#' @rdname low-level-read-write
#'
#' @examples
#'
#' # ---- yaml ---------------------------------------------------------------
#'
#' f <- tempfile(fileext = ".yaml")
#'
#' x <- list(a = 1L, b = 2.3, c = "a")
#' io_write_yaml(x, f)
#'
#' io_read_yaml(f)
#'
#' # clean up
#' unlink(f)
#'
#' @export
io_read_yaml <- function(con, ...){
  yaml::read_yaml(file = con, ...)
}

#' @rdname low-level-read-write
#' @export
io_write_yaml <- function(x, con, ..., sorted = FALSE){
  if(inherits(x, 'fastmap')) {
    # fastmap
    x <- x$as_list(sort = sorted)
  } else if(inherits(x, 'fastmap2')) {
    # dipsaus::fastmap2
    x <- x[["@as_list"]](sort = sorted)
  } else if(inherits(x, c('fastqueue', 'fastqueue2'))) {
    # fastqueue or dipsaus::fastqueue2
    x <- x$as_list()
  } else if(sorted){
    x <- as.list(x, sorted = sorted, ...)
  } else {
    # as.list generics only requires `x`, therefore `sorted` may cause errors
    x <- as.list(x, ...)
  }
  yaml::write_yaml(x, file = con, ...)
  if(!inherits(con, "connection")) {
    con <- normalizePath(con)
  }
  invisible(con)
}
