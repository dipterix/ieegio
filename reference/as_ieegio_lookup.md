# Convert to ieegio lookup table

Convert a data frame to an `ieegio_lookup` object.

## Usage

``` r
as_ieegio_lookup(x, type = "auto", ...)
```

## Arguments

- x:

  object to convert

- type:

  `"discrete"` or `"continuous"` (or `"auto"` to detect)

- ...:

  additional arguments

## Value

An `ieegio_lookup` object.

## Examples

``` r
# Discrete lookup: integer keys mapped to region labels
lut_disc <- as_ieegio_lookup(data.frame(
  Key   = 1:3,
  Label = c("Cortex", "White matter", "CSF")
))
print(lut_disc)
#> <ieegio Lookup [discrete]>
#>   4 labels
#>     [0]  Unknown
#>     [1]  Cortex
#>     [2]  White matter
#>     [3]  CSF

# Continuous lookup: piecewise value-to-scaled mapping
lut_cont <- as_ieegio_lookup(data.frame(
  Value  = c(-5, 0, 5),
  Scaled = c(0, 0.5, 1)
))
print(lut_cont)
#> <ieegio Lookup [continuous]>
#>   3 breakpoints  |  Value: [-5, 5]  ->  Scaled: [0, 1]
```
