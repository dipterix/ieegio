# Low-level file read and write

Interfaces to read from or write to files with common formats.

## Usage

``` r
io_read_fst(
  con,
  method = c("proxy", "data_table", "data_frame", "header_only"),
  ...,
  old_format = FALSE
)

io_write_fst(x, con, compress = 50, ...)

io_read_ini(con, ...)

io_read_json(con, ...)

io_write_json(
  x,
  con = stdout(),
  ...,
  digits = ceiling(-log10(.Machine$double.eps)),
  pretty = TRUE,
  serialize = TRUE
)

io_read_mat(
  con,
  method = c("auto", "R.matlab", "pymatreader", "mat73"),
  verbose = TRUE,
  on_convert_error = c("warning", "error", "ignore"),
  ...
)

io_write_mat(x, con, method = c("R.matlab", "scipy"), ...)

io_read_yaml(con, ...)

io_write_yaml(x, con, ..., sorted = FALSE)
```

## Arguments

- con:

  connection or file

- method:

  method to read table. For `'fst'`, the choices are

  `'proxy'`

  :   do not read data to memory, query the table when needed;

  `'data_table'`

  :   read as
      [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html);

  `'data_frame'`

  :   read as [`data.frame`](https://rdrr.io/r/base/data.frame.html);

  `'header_only'`

  :   read `'fst'` table header.

  For `'mat'`, the choices are

  `'auto'`

  :   automatically try the native option, and then `'pymatreader'` if
      fails;

  `'R.matlab'`

  :   use the native method (provided by
      [`readMat`](https://rdrr.io/pkg/R.matlab/man/readMat.html)); only
      support 'MAT 5.0' format;

  `'pymatreader'`

  :   use 'Python' library `'pymatreader'`;

  `'mat73'`

  :   use 'Python' library `'mat73'`.

- ...:

  passed to internal function calls

- old_format:

  see [`fst`](http://www.fstpackage.org/reference/fst.md)

- x:

  data to write to disk

- compress:

  compress level from 0 to 100; default is 50

- digits, pretty:

  for writing numeric values to 'json' format

- serialize:

  set to `TRUE` to serialize the data to 'json' format (with the data
  types, default); or `FALSE` to save the values without types

- verbose:

  whether to print out the process

- on_convert_error:

  for reading `'mat'` files with 'Python' modules, the results will be
  converted to R objects in the end. Not all objects can be converted.
  This input defines the behavior when the conversion fails; choices are
  `"error"`, `"warning"`, or `"ignore"`

- sorted:

  whether to sort the list; default is `FALSE`

## Value

The reader functions returns the data extracted from files, mostly as R
objects, with few exceptions on some 'Matlab' files. When reading a
'Matlab' file requires using 'Python' modules, `io_read_mat` will try
its best effort to convert 'Python' objects to R. However, such
conversion might fail. In this case, the result might partially contain
'Python' objects with warnings.

## Examples

``` r
# ---- fst ----------------------------------------------------------------


f <- tempfile(fileext = ".fst")
x <- data.frame(
  a = 1:10,
  b = rnorm(10),
  c = letters[1:10]
)

io_write_fst(x, con = f)

# default reads in proxy
io_read_fst(f)
#> <fst file>
#> 10 rows, 3 columns (file214b3bd0e8bb.fst)
#> 
#>            a           b           c
#>    <integer>    <double> <character>
#> 1          1 -0.36820945           a
#> 2          2 -0.87907826           b
#> 3          3  0.11353542           c
#> 4          4  1.20821308           d
#> 5          5 -1.47573976           e
#> 6          6 -0.06128622           f
#> 7          7  0.82444316           g
#> 8          8  0.46535571           h
#> 9          9  1.32589447           i
#> 10        10 -0.19138072           j

# load as data.table
io_read_fst(f, "data_table")
#>         a           b      c
#>     <int>       <num> <char>
#>  1:     1 -0.36820945      a
#>  2:     2 -0.87907826      b
#>  3:     3  0.11353542      c
#>  4:     4  1.20821308      d
#>  5:     5 -1.47573976      e
#>  6:     6 -0.06128622      f
#>  7:     7  0.82444316      g
#>  8:     8  0.46535571      h
#>  9:     9  1.32589447      i
#> 10:    10 -0.19138072      j

# load as data.frame
io_read_fst(f, "data_frame")
#>     a           b c
#> 1   1 -0.36820945 a
#> 2   2 -0.87907826 b
#> 3   3  0.11353542 c
#> 4   4  1.20821308 d
#> 5   5 -1.47573976 e
#> 6   6 -0.06128622 f
#> 7   7  0.82444316 g
#> 8   8  0.46535571 h
#> 9   9  1.32589447 i
#> 10 10 -0.19138072 j

# get header
io_read_fst(f, "header_only")
#> <fst file>
#> 10 rows, 3 columns (file214b3bd0e8bb.fst)
#> 
#> * 'a': integer
#> * 'b': double
#> * 'c': character

# clean up
unlink(f)



# ---- json ---------------------------------------------------------------
f <- tempfile(fileext = ".json")

x <- list(a = 1L, b = 2.3, c = "a", d = 1+1i)

# default is serialize
io_write_json(x, f)

io_read_json(f)
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2.3
#> 
#> $c
#> [1] "a"
#> 
#> $d
#> [1] 1+1i
#> 

cat(readLines(f), sep = "\n")
#> {
#>   "type": "list",
#>   "attributes": {
#>     "names": {
#>       "type": "character",
#>       "attributes": {},
#>       "value": ["a", "b", "c", "d"]
#>     }
#>   },
#>   "value": [
#>     {
#>       "type": "integer",
#>       "attributes": {},
#>       "value": [1]
#>     },
#>     {
#>       "type": "double",
#>       "attributes": {},
#>       "value": [2.2999999999999998]
#>     },
#>     {
#>       "type": "character",
#>       "attributes": {},
#>       "value": ["a"]
#>     },
#>     {
#>       "type": "complex",
#>       "attributes": {},
#>       "value": ["1+1i"]
#>     }
#>   ]
#> }

# just values
io_write_json(x, f, serialize = FALSE, pretty = FALSE)

io_read_json(f)
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2.3
#> 
#> $c
#> [1] "a"
#> 
#> $d
#> [1] "1+1i"
#> 

cat(readLines(f), sep = "\n")
#> {"a":[1],"b":[2.2999999999999998],"c":["a"],"d":["1+1i"]}

# clean up
unlink(f)



# ---- Matlab .mat --------------------------------------------------------

if (FALSE) { # \dontrun{

f <- tempfile(fileext = ".mat")

x <- list(a = 1L, b = 2.3, c = "a", d = 1+1i)

# save as MAT 5.0
io_write_mat(x, f)

io_read_mat(f)

# require setting up Python environment

io_read_mat(f, method = "pymatreader")

# MAT 7.3 example
sample_data <- ieegio_sample_data("mat_v73.mat")
io_read_mat(sample_data)

# clean up
unlink(f)

} # }



# ---- yaml ---------------------------------------------------------------

f <- tempfile(fileext = ".yaml")

x <- list(a = 1L, b = 2.3, c = "a")
io_write_yaml(x, f)

io_read_yaml(f)
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2.3
#> 
#> $c
#> [1] "a"
#> 

# clean up
unlink(f)
```
