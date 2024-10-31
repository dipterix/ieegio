#' Creates a \code{NWBHDF5IO} file container
#' @description
#' Class definition for 'PyNWB' container; use \code{\link{read_nwb}}
#' for construction function.
#'
#' @examples
#' \dontrun{
#'
#' # Running this example requires a .nwb file
#'
#' library(rnwb)
#' container <- NWBHDF5IO$new(path = file)
#' container$with({
#'
#'   data <- container$read()
#'   electrode_table <- data$electrodes[convert = TRUE]
#'
#' })
#'
#' print(electrode_table)
#'
#' }
NWBHDF5IO <- R6::R6Class(
  classname = "NWBHDF5IO",
  private = list(
    construct_params = NULL,
    .file_handler = NULL,
    ensure_file_handler = function() {
      # check if .file_handler is valid
      ptr <- private$.file_handler
      if(is.null(ptr) || is_invalid_py_pointer(ptr)) {
        nwb <- pynwb_module()
        ptr <- do.call(nwb$NWBHDF5IO, private$construct_params)
        private$.file_handler <- ptr
      }
      return(ptr)
    },
    finalize = function(...) {
      self$close()
    }
  ),
  public = list(
    #' @description
    #' Initialize the class
    #' @param path Path to a \code{'.nwb'} file
    #' @param mode Mode for opening the file
    #' @param ... Other parameters passed to \code{nwb$NWBHDF5IO}
    initialize = function(
    path = NULL, mode = c("r", "w", "r+", "a", "w-", "x"), ...) {
      if(length(path)) {
        path <- normalizePath(path)
      }
      mode <- match.arg(mode)
      private$construct_params <- list(
        path = path, mode = mode, ...
      )
      ensure_py_package("pynwb")
    },

    #' @description
    #' Get internal file handler. Please make sure you close the handler
    #' correctly.
    #' @return File handler, i.e. 'PyNWB' \code{NWBHDF5IO} instance.
    get_handler = function() {
      private$ensure_file_handler()
    },

    #' @description
    #' Open the connections, must be used together with \code{$close} method.
    #' For high-level method, see \code{$with}
    #' @return container itself
    #' @examples
    #' \dontrun{
    #'
    #' # low-level method to open NWB file, for safer methods, see
    #' # `container$with()` below
    #'
    #' container$open()
    #'
    #' data <- container$read()
    #'
    #' # process data...
    #'
    #' # Make sure the container is closed!
    #' container$close()
    #'
    #' }
    open = function() {
      private$ensure_file_handler()
      passed <- FALSE
      # Make sure closing the file links when exiting the function
      on.exit({
        if(!passed) {
          self$close()
        }
      })
      if(!self$opened) {
        stop("Cannot open file...")
      }
      passed <- TRUE
      invisible(self)
    },

    #' @description
    #' Close the connections (low-level method, see 'with' method below)
    #' @param close_links Whether to close all files linked to from this
    #' file; default is true
    #' @return Nothing
    close = function(close_links = TRUE) {
      if( close_links ) {
        close_links <- TRUE
      } else {
        close_links <- FALSE
      }
      ptr <- private$.file_handler
      if(is.null(ptr) || is_invalid_py_pointer(ptr)) { return(invisible()) }
      # clean up and close the file handler
      ieegio_debug("Closing the [NWBHDF5IO] file handler")
      tryCatch({
        ptr$close(close_links = close_links)
      }, error = warning)
      # important: remove the pointer or segfault
      private$.file_handler <- NULL
      invisible()
    },

    #' @description
    #' Close all opened, linked-to files. \code{'MacOS'} and \code{'Linux'}
    #' automatically release the linked-to file after the linking file is
    #' closed, but \code{'Windows'} does not, which prevents the linked-to
    #' file from being deleted or truncated. Use this method to close all
    #' opened, linked-to files.
    #' @return Nothing
    close_linked_files = function() {
      ptr <- private$.file_handler
      if(is.null(ptr) || is_invalid_py_pointer(ptr)) { return(invisible()) }
      # clean up and close the file handler
      ieegio_debug("Closing the [NWBHDF5IO] all linked files")
      tryCatch({
        ptr$close_linked_files()
      }, error = warning)
      # important: remove the pointer or segfault
      private$.file_handler <- NULL
      invisible()
    },

    #' @description
    #' Read the \code{'NWB'} file from the 'IO' source. Please use along with
    #' \code{'$with'} method
    #' @return \code{'NWBFile'} container
    read = function() {
      if(!self$opened) {
        stop("The file is not opened. Must run under `x$with` context")
      }
      handler <- private$ensure_file_handler()
      return(handler$read())
    },

    #' @description
    #' Safe wrapper for reading and handling \code{'NWB'} file. See class examples.
    #' @param expr R expression to evaluate
    #' @param quoted Whether \code{expr} is quoted; default is false
    #' @param envir environment for \code{expr} to evaluate; default is the
    #' parent frame (see \code{parent.frame})
    #' @return Whatever results generated by \code{expr}
    #' @examples
    #' \dontrun{
    #'
    #' container$with({
    #'   data <- container$read()
    #'   # process data
    #' })
    #'
    #' }
    with = function(expr, quoted = FALSE, envir = parent.frame()) {
      if(!quoted) {
        expr <- substitute(expr)
      }
      private$ensure_file_handler()
      # Make sure closing the file links when exiting the function
      on.exit({ self$close() })
      if(!self$opened) {
        stop("Cannot open file...")
      }
      eval(expr, envir = envir)
    }
  ),
  active = list(

    #' @field opened Whether the container is opened.
    opened = function() {
      ptr <- private$.file_handler
      if(is.null(ptr) || is_invalid_py_pointer(ptr)) {
        return(FALSE)
      }
      return(TRUE)
    }
  )
)

#' @export
close.pynwb.NWBHDF5IO <- function(con, close_links = TRUE, ...) {
  if(close_links) {
    close_links <- TRUE
  } else {
    close_links <- FALSE
  }
  try({
    con$close(close_links = close_links)
    return(invisible(TRUE))
  })
  invisible(FALSE)
}

#' @export
close.NWBHDF5IO <- function(con, close_links = TRUE, ...) {
  con$close(close_links = close_links)
}
