# Check whether a 'HDF5' file can be opened for read/write

Check whether a 'HDF5' file can be opened for read/write

## Usage

``` r
io_h5_valid(file, mode = c("r", "w"), close_all = FALSE)

io_h5_names(file)
```

## Arguments

- file:

  path to file

- mode:

  `'r'` for read access and `'w'` for write access

- close_all:

  whether to close all connections or just close current connection;
  default is false. Set this to `TRUE` if you want to close all other
  connections to the file

## Value

`io_h5_valid` returns a logical value indicating whether the file can be
opened. `io_h5_names` returns a character vector of dataset names.

## Examples

``` r
x <- array(1:27, c(3,3,3))
f <- tempfile()

# No data written to the file, hence invalid
io_h5_valid(f, 'r')
#> [1] FALSE

io_write_h5(x, f, 'dset')
#> /tmp/Rtmp8QI123/file23343f2d1d8f => dset (Dataset Created)
#> /tmp/Rtmp8QI123/file23343f2d1d8f => dset (Dataset Removed)
#> /tmp/Rtmp8QI123/file23343f2d1d8f => dset (Dataset Created)
io_h5_valid(f, 'w')
#> [1] TRUE

# Open the file and hold a connection
ptr <- hdf5r::H5File$new(filename = f, mode = 'w')

# Can read, but cannot write
io_h5_valid(f, 'r')  # TRUE
#> [1] TRUE
io_h5_valid(f, 'w')  # FALSE
#> [1] FALSE

# However, this can be reset via `close_all=TRUE`
io_h5_valid(f, 'r', close_all = TRUE)
#> [1] TRUE
io_h5_valid(f, 'w')  # TRUE
#> [1] TRUE

# Now the connection is no longer valid
ptr
#> Class: H5File
#> ID: Object invalid

# clean up
unlink(f)
```
