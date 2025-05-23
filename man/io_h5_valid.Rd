% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hdf5.R
\name{io_h5_valid}
\alias{io_h5_valid}
\alias{io_h5_names}
\title{Check whether a 'HDF5' file can be opened for read/write}
\usage{
io_h5_valid(file, mode = c("r", "w"), close_all = FALSE)

io_h5_names(file)
}
\arguments{
\item{file}{path to file}

\item{mode}{\code{'r'} for read access and \code{'w'} for write access}

\item{close_all}{whether to close all connections or just close current
connection; default is false. Set this to \code{TRUE} if you want to
close all other connections to the file}
}
\value{
\code{io_h5_valid} returns a logical value indicating whether the
file can be opened. \code{io_h5_names} returns a character vector of
dataset names.
}
\description{
Check whether a 'HDF5' file can be opened for read/write
}
\examples{

x <- array(1:27, c(3,3,3))
f <- tempfile()

# No data written to the file, hence invalid
io_h5_valid(f, 'r')

io_write_h5(x, f, 'dset')
io_h5_valid(f, 'w')

# Open the file and hold a connection
ptr <- hdf5r::H5File$new(filename = f, mode = 'w')

# Can read, but cannot write
io_h5_valid(f, 'r')  # TRUE
io_h5_valid(f, 'w')  # FALSE

# However, this can be reset via `close_all=TRUE`
io_h5_valid(f, 'r', close_all = TRUE)
io_h5_valid(f, 'w')  # TRUE

# Now the connection is no longer valid
ptr

# clean up
unlink(f)

}
