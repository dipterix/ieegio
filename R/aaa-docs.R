#' @name low-level-read-write
#' @title Low-level file read and write
#' @description
#' Interfaces to read from or write to files with common formats.
#' @param con connection or file
#' @param x data to write to disk
#' @param method method to read table. For \code{'fst'}, the choices are
#' \describe{
#' \item{\code{'proxy'}}{do not read data to memory, query the table when
#' needed;}
#' \item{\code{'data_table'}}{read as \code{\link[data.table]{data.table}};}
#' \item{\code{'data_frame'}}{read as \code{\link{data.frame}};}
#' \item{\code{'header_only'}}{read \code{'fst'} table header.}
#' }
#' For \code{'mat'}, the choices are
#' \describe{
#' \item{\code{'auto'}}{automatically try the native option, and then
#' \code{'pymatreader'} if fails;}
#' \item{\code{'R.matlab'}}{use the native method (provided
#' by \code{\link[R.matlab]{readMat}}); only support 'MAT 5.0' format;}
#' \item{\code{'pymatreader'}}{use 'Python' library \code{'pymatreader'};}
#' \item{\code{'mat73'}}{use 'Python' library \code{'mat73'}.}
#' }
#' @param old_format see \code{\link[fst]{fst}}
#' @param compress compress level from 0 to 100; default is 50
#' @param digits,pretty for writing numeric values to 'json' format
#' @param serialize set to \code{TRUE} to serialize the data to 'json' format
#' (with the data types, default); or \code{FALSE} to save the
#' values without types
#' @param sorted whether to sort the list; default is \code{FALSE}
#' @param verbose whether to print out the process
#' @param on_convert_error for reading \code{'mat'} files with 'Python' modules,
#' the results will be converted to R objects in the end. Not all objects
#' can be converted. This input defines the behavior when the conversion fails;
#' choices are \code{"error"}, \code{"warning"}, or \code{"ignore"}
#' @param ... passed to internal function calls
#'
#' @returns The reader functions returns the data extracted from files, mostly
#' as R objects, with few exceptions on some 'Matlab' files. When reading a
#' 'Matlab' file requires using 'Python' modules, \code{io_read_mat} will
#' try its best effort to convert 'Python' objects to R. However, such
#' conversion might fail. In this case, the result might partially contain
#' 'Python' objects with warnings.
#'
NULL


#' @name read_brainvis
#' @title Read 'BrainVision' data
#' @param file file path to the data file
#' @param extract_path location to where the extracted information is to be
#' stored
#' @param header_only whether to only load header data
#' @param cache_ok whether existing cache should be reused; default is
#' \code{TRUE}. This input can speed up reading large data files; set to
#' \code{FALSE} to delete cache before importing.
#' @param verbose whether to print processing messages; default is \code{TRUE}
#' @returns A cached object that is readily to be loaded to memory; see
#' \code{\link{SignalDataCache}} for class definition.
#'
NULL
