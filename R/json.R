#' @rdname low-level-read-write
#'
#' @examples
#'
#'
#' # ---- json ---------------------------------------------------------------
#' f <- tempfile(fileext = ".json")
#'
#' x <- list(a = 1L, b = 2.3, c = "a", d = 1+1i)
#'
#' # default is serialize
#' io_write_json(x, f)
#'
#' io_read_json(f)
#'
#' cat(readLines(f), sep = "\n")
#'
#' # just values
#' io_write_json(x, f, serialize = FALSE, pretty = FALSE)
#'
#' io_read_json(f)
#'
#' cat(readLines(f), sep = "\n")
#'
#' # clean up
#' unlink(f)
#'
#'
#' @export
io_read_json <- function(con, ...){

  s <- readLines(con)
  args <- list(...)

  s <- trimws(paste(s, collapse = ""))

  re <- NULL
  if(nzchar(s)) {
    ok <- FALSE
    tryCatch({
      withRestarts({
        re <- jsonlite::unserializeJSON(s)
        ok <- TRUE
      }, abort = function() {})
    }, error = function(e) {})
    if(!ok) {
      tryCatch({
        withRestarts({
          args$txt <- s
          re <- do.call(jsonlite::fromJSON, args)
          ok <- TRUE
        }, abort = function() {})
      }, error = function(e) {})
    }
  }

  re
}

#' @rdname low-level-read-write
#' @export
io_write_json <- function(x, con = stdout(), ...,
                           digits = ceiling(-log10(.Machine$double.eps)),
                           pretty = TRUE, serialize = TRUE) {

  if(serialize){
    s <- jsonlite::serializeJSON(x, digits = digits, pretty = pretty)
  } else {
    s <- jsonlite::toJSON(x, digits = digits, pretty = pretty, ...)
  }

  writeLines(s, con)
  invisible()
}
