#' Lazy Load 'HDF5' File via \code{\link[hdf5r]{hdf5r-package}}
#'
#' @description Wrapper for class \code{\link{LazyH5}}, which load data with
#' "lazy" mode - only read part of dataset when needed.
#'
#' @param file 'HDF5' file
#' @param name \code{group/data_name} path to dataset (\code{H5D} data)
#' @param read_only only used if \code{ram=FALSE}, whether the returned
#' \code{\link{LazyH5}} instance should be read only
#' @param ram load data to memory immediately, default is false
#' @param quiet whether to suppress messages
#'
#' @returns If \code{ram} is true, then return data as arrays, otherwise return
#' a \code{\link{LazyH5}} instance.
#'
#' @seealso \code{\link{io_write_h5}}
#'
#' @examples
#' file <- tempfile()
#' x <- array(1:120, dim = c(4,5,6))
#'
#' # save x to file with name /group/dataset/1
#' io_write_h5(x, file, '/group/dataset/1', quiet = TRUE)
#'
#' # read data
#' y <- io_read_h5(file, '/group/dataset/1', ram = TRUE)
#' class(y)   # array
#'
#' z <- io_read_h5(file, '/group/dataset/1', ram = FALSE)
#' class(z)   # LazyH5
#'
#' dim(z)
#'
#' # clean up
#' unlink(file)
#'
#' @export
io_read_h5 <- function(file, name, read_only = TRUE, ram = FALSE, quiet = FALSE){

  re <- tryCatch({
    re <- LazyH5$new(file_path = file, data_name = name, read_only = read_only, quiet = quiet)
    re$open()
    re
  }, error = function(e){

    if(!read_only){
      stop('Another process is locking the file. Cannot open file with write permission; use ', sQuote('io_write_h5'), ' instead...\n  file: ', file, '\n  name: ', name)
    }
    if(!quiet){
      cat('Open failed. Attempt to open with a temporary copy...\n')
    }

    # Fails when other process holds a connection to it!
    # If read_only, then copy the file to local directory
    tmpf <- tempfile(fileext = 'conflict.h5')
    file.copy(file, tmpf)
    LazyH5$new(file_path = tmpf, data_name = name, read_only = read_only)
  })

  if(ram){
    f <- re
    re <- re[]
    f$close()
  }
  re
}




#' Save objects to 'HDF5' file without trivial checks
#' @param x an array, a matrix, or a vector
#' @param file path to 'HDF5' file
#' @param name path/name of the data; for example, \code{"group/data_name"}
#' @param chunk chunk size
#' @param level compress level from 0 - no compression to 10 - max compression
#' @param replace should data be replaced if exists
#' @param new_file should removing the file if old one exists
#' @param ctype data type such as "character", "integer", or "numeric". If
#' set to \code{NULL} then automatically detect types. Note for complex data
#' please store separately the real and imaginary parts.
#' @param quiet whether to suppress messages, default is false
#' @param ... passed to other \code{LazyH5$save}
#' @returns Absolute path of the file saved
#'
#' @seealso \code{\link{io_read_h5}}
#' @examples
#'
#' file <- tempfile()
#' x <- array(1:120, dim = 2:5)
#'
#' # save x to file with name /group/dataset/1
#' io_write_h5(x, file, '/group/dataset/1', chunk = dim(x))
#'
#' # load data
#' y <- io_read_h5(file, '/group/dataset/1')
#'
#' # read data to memory
#' y[]
#'
#' # clean up
#' unlink(file)
#'
#' @export
io_write_h5 <- function(x, file, name, chunk = 'auto', level = 4,replace = TRUE,
                    new_file = FALSE, ctype = NULL, quiet = FALSE, ...){
  f <- tryCatch({
    f <- LazyH5$new(file, name, read_only = FALSE, quiet = quiet)
    f$open()
    f$close()
    f
  }, error = function(e){
    if( !quiet ){
      cat('Saving failed. Attempt to unlink the file and retry...\n')
    }
    if(file.exists(file)){
      # File is locked,
      tmpf <- tempfile(fileext = 'conflict.w.h5')
      file.copy(file, tmpf)
      unlink(file, recursive = FALSE, force = TRUE)
      file.copy(tmpf, file)
      unlink(tmpf)
    }
    # Otherwise it's some weird error, or dirname not exists, expose the error
    LazyH5$new(file, name, read_only = FALSE)
  })
  on.exit({
    f$close(all = TRUE)
  }, add = TRUE)
  f$save(x, chunk = chunk, level = level, replace = replace, new_file = new_file, ctype = ctype, force = TRUE, ...)

  return(invisible(normalizePath(file, winslash = "/")))
}


#' Check whether a 'HDF5' file can be opened for read/write
#' @param file path to file
#' @param mode \code{'r'} for read access and \code{'w'} for write access
#' @param close_all whether to close all connections or just close current
#' connection; default is false. Set this to \code{TRUE} if you want to
#' close all other connections to the file
#' @returns \code{io_h5_valid} returns a logical value indicating whether the
#' file can be opened. \code{io_h5_names} returns a character vector of
#' dataset names.
#'
#' @examples
#'
#' x <- array(1:27, c(3,3,3))
#' f <- tempfile()
#'
#' # No data written to the file, hence invalid
#' io_h5_valid(f, 'r')
#'
#' io_write_h5(x, f, 'dset')
#' io_h5_valid(f, 'w')
#'
#' # Open the file and hold a connection
#' ptr <- hdf5r::H5File$new(filename = f, mode = 'w')
#'
#' # Can read, but cannot write
#' io_h5_valid(f, 'r')  # TRUE
#' io_h5_valid(f, 'w')  # FALSE
#'
#' # However, this can be reset via `close_all=TRUE`
#' io_h5_valid(f, 'r', close_all = TRUE)
#' io_h5_valid(f, 'w')  # TRUE
#'
#' # Now the connection is no longer valid
#' ptr
#'
#' # clean up
#' unlink(f)
#'
#' @export
io_h5_valid <- function(file, mode = c('r', 'w'), close_all = FALSE){
  mode <- match.arg(mode)
  tryCatch({
    file <- normalizePath(file, mustWork = TRUE)
    f <- hdf5r::H5File$new(filename = file, mode = mode)
    if(close_all){
      f$close_all()
    } else {
      f$close()
    }
    TRUE
  }, error = function(e){
    FALSE
  })

}


#' @rdname io_h5_valid
#' @export
io_h5_names <- function(file){
  # make sure the file is valid
  if(!io_h5_valid(file, 'r')){ return(FALSE) }
  file <- normalizePath(file, mustWork = TRUE)
  f <- hdf5r::H5File$new(filename = file, mode = 'r')
  names <- hdf5r::list.datasets(f)
  f$close()
  names
}
