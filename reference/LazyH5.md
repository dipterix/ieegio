# Lazy 'HDF5' file loader

Provides hybrid data structure for 'HDF5' file. The class is not
intended for direct-use. Please see
[`io_read_h5`](http://dipterix.org/ieegio/reference/io_read_h5.md) and
[`io_write_h5`](http://dipterix.org/ieegio/reference/io_write_h5.md).

## Public fields

- `quiet`:

  whether to suppress messages

## Methods

### Public methods

- [`LazyH5$do_finalize()`](#method-LazyH5-do_finalize)

- [`LazyH5$print()`](#method-LazyH5-print)

- [`LazyH5$new()`](#method-LazyH5-new)

- [`LazyH5$save()`](#method-LazyH5-save)

- [`LazyH5$open()`](#method-LazyH5-open)

- [`LazyH5$close()`](#method-LazyH5-close)

- [`LazyH5$subset()`](#method-LazyH5-subset)

- [`LazyH5$get_dims()`](#method-LazyH5-get_dims)

- [`LazyH5$get_type()`](#method-LazyH5-get_type)

------------------------------------------------------------------------

### Method `do_finalize()`

garbage collection method

#### Usage

    LazyH5$do_finalize()

#### Returns

none

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

overrides print method

#### Usage

    LazyH5$print()

#### Returns

self instance

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    LazyH5$new(file_path, data_name, read_only = FALSE, quiet = FALSE)

#### Arguments

- `file_path`:

  where data is stored in 'HDF5' format

- `data_name`:

  the data stored in the file

- `read_only`:

  whether to open the file in read-only mode. It's highly recommended to
  set this to be true, otherwise the file connection is exclusive.

- `quiet`:

  whether to suppress messages, default is false

#### Returns

self instance

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

save data to a 'HDF5' file

#### Usage

    LazyH5$save(
      x,
      chunk = "auto",
      level = 7,
      replace = TRUE,
      new_file = FALSE,
      force = TRUE,
      ctype = NULL,
      size = NULL,
      ...
    )

#### Arguments

- `x`:

  vector, matrix, or array

- `chunk`:

  chunk size, length should matches with data dimension

- `level`:

  compress level, from 1 to 9

- `replace`:

  if the data exists in the file, replace the file or not

- `new_file`:

  remove the whole file if exists before writing?

- `force`:

  if you open the file in read-only mode, then saving objects to the
  file will raise error. Use `force=TRUE` to force write data

- `ctype`:

  data type, see [`mode`](https://rdrr.io/r/base/mode.html), usually the
  data type of `x`. Try `mode(x)` or `storage.mode(x)` as hints.

- `size`:

  deprecated, for compatibility issues

- `...`:

  passed to self [`open()`](https://rdrr.io/r/base/connections.html)
  method

------------------------------------------------------------------------

### Method [`open()`](https://rdrr.io/r/base/connections.html)

open connection

#### Usage

    LazyH5$open(new_dataset = FALSE, robj, ...)

#### Arguments

- `new_dataset`:

  only used when the internal pointer is closed, or to write the data

- `robj`:

  data array to save

- `...`:

  passed to `createDataSet` in `hdf5r` package

------------------------------------------------------------------------

### Method [`close()`](https://rdrr.io/r/base/connections.html)

close connection

#### Usage

    LazyH5$close(all = TRUE)

#### Arguments

- `all`:

  whether to close all connections associated to the data file. If true,
  then all connections, including access from other programs, will be
  closed

------------------------------------------------------------------------

### Method [`subset()`](https://rdrr.io/r/base/subset.html)

subset data

#### Usage

    LazyH5$subset(..., drop = FALSE, stream = FALSE, envir = parent.frame())

#### Arguments

- `drop`:

  whether to apply [`drop`](https://rdrr.io/r/base/drop.html) the subset

- `stream`:

  whether to read partial data at a time

- `envir`:

  if `i,j,...` are expressions, where should the expression be evaluated

- `i, j, ...`:

  index along each dimension

#### Returns

subset of data

------------------------------------------------------------------------

### Method `get_dims()`

get data dimension

#### Usage

    LazyH5$get_dims(stay_open = TRUE)

#### Arguments

- `stay_open`:

  whether to leave the connection opened

#### Returns

dimension of the array

------------------------------------------------------------------------

### Method `get_type()`

get data type

#### Usage

    LazyH5$get_type(stay_open = TRUE)

#### Arguments

- `stay_open`:

  whether to leave the connection opened

#### Returns

data type, currently only character, integer, raw, double, and complex
are available, all other types will yield "unknown"
