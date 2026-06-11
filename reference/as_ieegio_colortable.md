# Convert to ieegio color table

Convert a data frame or existing label table to an `ieegio_colortable`
object.

## Usage

``` r
as_ieegio_colortable(x, ...)
```

## Arguments

- x:

  object to convert

- ...:

  additional arguments passed to methods

## Value

An `ieegio_colortable` object.

## Examples

``` r
ct <- as_ieegio_colortable(data.frame(
  Key = 1:3,
  R   = c(255L, 0L, 0L),
  G   = c(0L, 255L, 0L),
  B   = c(0L, 0L, 255L),
  A   = c(255L, 255L, 255L)
))
print(ct)
#> <ieegio Color Table>
#>   3 color stops  |  Key range: [1, 3]
#>     [1]  #FF0000
#>     [2]  #00FF00
#>     [3]  #0000FF
```
