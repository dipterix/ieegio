# R6 Class to Load 'FST' Files

provides low-level hybrid array loading for 'FST' file; used internally

## Value

none

none

none

vector, dimensions

subset of data

## Author

Zhengjia Wang

## Methods

### Public methods

- [`LazyFST$open()`](#method-LazyFST-open)

- [`LazyFST$close()`](#method-LazyFST-close)

- [`LazyFST$save()`](#method-LazyFST-save)

- [`LazyFST$new()`](#method-LazyFST-new)

- [`LazyFST$get_dims()`](#method-LazyFST-get_dims)

- [`LazyFST$subset()`](#method-LazyFST-subset)

------------------------------------------------------------------------

### Method [`open()`](https://rdrr.io/r/base/connections.html)

to be compatible with
[`LazyH5`](http://dipterix.org/ieegio/reference/LazyH5.md)

#### Usage

    LazyFST$open(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method [`close()`](https://rdrr.io/r/base/connections.html)

close the connection

#### Usage

    LazyFST$close(..., .remove_file = FALSE)

#### Arguments

- `...`:

  ignored

- `.remove_file`:

  whether to remove the file when garbage collected

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

to be compatible with
[`LazyH5`](http://dipterix.org/ieegio/reference/LazyH5.md)

#### Usage

    LazyFST$save(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    LazyFST$new(file_path, transpose = FALSE, dims = NULL, ...)

#### Arguments

- `file_path`:

  where the data is stored

- `transpose`:

  whether to load data transposed

- `dims`:

  data dimension, only support 1 or 2 dimensions

- `...`:

  ignored

------------------------------------------------------------------------

### Method `get_dims()`

get data dimension

#### Usage

    LazyFST$get_dims(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method [`subset()`](https://rdrr.io/r/base/subset.html)

subset data

#### Usage

    LazyFST$subset(i = NULL, j = NULL, ..., drop = TRUE)

#### Arguments

- `i, j, ...`:

  index along each dimension

- `drop`:

  whether to apply [`drop`](https://rdrr.io/r/base/drop.html) the subset

## Examples

``` r

library(ieegio)

# Data to save, 8 MB
x <- matrix(rnorm(1000000), ncol = 100)

# Save to local disk
f <- tempfile()
io_write_fst(as.data.frame(x), con = f)

# Load via LazyFST
dat <- LazyFST$new(file_path = f, dims = c(10000, 100))

# dat < 1 MB

# Check whether the data is identical
range(dat[] - x)
#> [1] 0 0

system.time(dat[,1])
#>    user  system elapsed 
#>   0.016   0.000   0.015 

system.time(dat[1:100,])
#>    user  system elapsed 
#>   0.012   0.002   0.005 

```
