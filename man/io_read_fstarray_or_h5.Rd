% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LazyFST-class.R
\name{io_read_fstarray_or_h5}
\alias{io_read_fstarray_or_h5}
\title{Function try to load 'FST' arrays, if not found, read 'HDF5' arrays}
\usage{
io_read_fstarray_or_h5(
  fst_path,
  h5_path,
  h5_name,
  fst_need_transpose = FALSE,
  fst_need_drop = FALSE,
  ram = FALSE
)
}
\arguments{
\item{fst_path}{'FST' file cache path}

\item{h5_path}{alternative 'HDF5' file path}

\item{h5_name}{'HDF5' data name}

\item{fst_need_transpose}{does 'FST' data need transpose?}

\item{fst_need_drop}{drop dimensions}

\item{ram}{whether to load to memory directly or perform lazy loading}
}
\value{
If 'FST' cache file exists, returns \code{\link{LazyFST}} object,
otherwise returns \code{\link[ieegio]{LazyH5}} instance
}
\description{
Experimental function; use with caution.
}
\details{
RAVE stores data with redundancy. One electrode data
is usually saved with two copies in different formats: 'HDF5' and
'FST', where 'HDF5' is cross-platform and supported by multiple
languages such as \code{Matlab}, \code{Python}, etc, while 'FST'
format is supported by R only, with super high read/write speed.
\code{load_fst_or_h5} checks whether the presence of 'FST' file,
if failed, then it reads data from persistent 'HDF5' file.
}
