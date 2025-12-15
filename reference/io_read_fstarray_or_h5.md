# Function try to load 'FST' arrays, if not found, read 'HDF5' arrays

Experimental function; use with caution.

## Usage

``` r
io_read_fstarray_or_h5(
  fst_path,
  h5_path,
  h5_name,
  fst_need_transpose = FALSE,
  fst_need_drop = FALSE,
  ram = FALSE
)
```

## Arguments

- fst_path:

  'FST' file cache path

- h5_path:

  alternative 'HDF5' file path

- h5_name:

  'HDF5' data name

- fst_need_transpose:

  does 'FST' data need transpose?

- fst_need_drop:

  drop dimensions

- ram:

  whether to load to memory directly or perform lazy loading

## Value

If 'FST' cache file exists, returns
[`LazyFST`](http://dipterix.org/ieegio/reference/LazyFST.md) object,
otherwise returns
[`LazyH5`](http://dipterix.org/ieegio/reference/LazyH5.md) instance

## Details

RAVE stores data with redundancy. One electrode data is usually saved
with two copies in different formats: 'HDF5' and 'FST', where 'HDF5' is
cross-platform and supported by multiple languages such as `Matlab`,
`Python`, etc, while 'FST' format is supported by R only, with super
high read/write speed. `load_fst_or_h5` checks whether the presence of
'FST' file, if failed, then it reads data from persistent 'HDF5' file.
