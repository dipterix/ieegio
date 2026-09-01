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
  connections to the file. This only applies to the `'hdf5r'` backend;
  `'h5lite'` never holds the file open

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
#> /tmp/RtmpniHXPY/file229c5a1a5790 => dset (Dataset Created)
#> /tmp/RtmpniHXPY/file229c5a1a5790 => dset (Dataset Removed)
#> /tmp/RtmpniHXPY/file229c5a1a5790 => dset (Dataset Created)
io_h5_valid(f, 'w')
#> [1] TRUE

# clean up
unlink(f)

```
