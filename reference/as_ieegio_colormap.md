# Convert to `ieegio_colormap`

Combine or convert objects into an `ieegio_colormap`.

## Usage

``` r
as_ieegio_colormap(
  x,
  lookup = NULL,
  colorspace = "RGB",
  data_range = NULL,
  type = c("auto", "discrete", "continuous"),
  ...
)

# S3 method for class 'threeBrain_colormap'
as_ieegio_colormap(
  x,
  lookup = NULL,
  colorspace = "RGB",
  data_range = NULL,
  type = c("auto", "discrete", "continuous"),
  ...
)
```

## Arguments

- x:

  an `ieegio_colortable`, existing `ieegio_colormap`, or label-table
  `data.frame`

- lookup:

  optional `ieegio_lookup` to attach; replaces existing lookup when `x`
  is already an `ieegio_colormap`

- colorspace:

  one of `"RGB"` (default), `"sRGB"`, `"HSV"`, `"HCL"`, `"Lab"`

- data_range:

  optional numeric(1 or 2) fixed data range for continuous color maps;
  `NA`/`Inf` positions are filled from data at call time; `NULL` defers
  to the lookup table

- type:

  one of `"auto"` (default), `"discrete"`, or `"continuous"`. When
  `lookup = NULL`, this controls whether a discrete or continuous color
  map is returned. `"auto"` produces discrete unless `data_range` is
  also supplied.

- ...:

  additional arguments

## Value

An `ieegio_colormap` object.

## Examples

``` r
# From a color table alone (discrete colormap, no labels)
ct <- as_ieegio_colortable(data.frame(
  Key = 1:3,
  R = c(255L, 0L, 0L), G = c(0L, 200L, 0L),
  B = c(0L, 0L, 180L), A = c(255L, 255L, 255L)
))
cm <- as_ieegio_colormap(ct)
print(cm)
#> <ieegio Colormap [discrete]>
#>   Colorspace   : RGB
#>   Color stops  : 3  (keys 1 to 3)

# Continuous colormap from the same color table
cm_cont <- as_ieegio_colormap(ct, type = "continuous", data_range = c(-10, 10))
print(cm_cont)
#> <ieegio Colormap [continuous]>
#>   Colorspace   : RGB
#>   Color stops  : 3  (keys 1 to 3)
#>   Data range   : [-10, 10]
#>   Lookup       : 2 breakpoints

# From a label table data.frame (adds both colors and a discrete lookup)
lt <- data.frame(
  Key   = 1:2,
  Label = c("Region A", "Region B"),
  R = c(200L, 50L), G = c(50L, 200L),
  B = c(50L,  50L), A = c(255L, 255L)
)
cm2 <- as_ieegio_colormap(lt)
print(cm2)
#> <ieegio Colormap [discrete]>
#>   Colorspace   : RGB
#>   Color stops  : 2  (keys 1 to 2)
#>   Lookup       : 3 labels
```
