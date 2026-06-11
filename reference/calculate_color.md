# Map data values or atlas keys to hex colors

Apply an `ieegio_colormap` to a vector of values or atlas keys,
returning a character vector of hex color strings.

## Usage

``` r
calculate_color(
  x,
  colormap,
  type = c("auto", "discrete", "continuous"),
  colorspace = NULL,
  keep_alpha = TRUE,
  na = "#00000000"
)
```

## Arguments

- x:

  numeric or integer vector of data values / atlas keys

- colormap:

  an `ieegio_colormap` object

- type:

  one of `"auto"` (default), `"discrete"`, `"continuous"`.
  Auto-detection: integer class or any value outside the 0-1 range maps
  to discrete; numeric values all within 0-1 map to continuous.

- colorspace:

  override the color space stored in `colormap`; one of `"RGB"`,
  `"sRGB"`, `"HSV"`, `"HCL"`, `"Lab"`. `NULL` (default) inherits from
  `colormap$colorspace`.

- keep_alpha:

  logical; if `TRUE` (default), colors with alpha below 255 are returned
  as `#RRGGBBAA`. Set to `FALSE` to always return `#RRGGBB`, dropping
  the alpha channel (needed for formats that do not support alpha in
  hex).

- na:

  color string used to replace `NA` inputs in the output; defaults to
  `"#00000000"` (transparent black). Set to `NA` to leave `NA` inputs as
  `NA` in the output.

## Value

Character vector of `#RRGGBB` (or `#RRGGBBAA` when `keep_alpha=TRUE` and
alpha is below 255) color strings, same length as `x`.

## Examples

``` r
# Build a red-to-green color table with 5 stops
ct <- as_ieegio_colortable(data.frame(
  Key = c(1L, 5L),
  R = c(255L, 0L), G = c(0L, 255L), B = c(0L, 0L), A = c(255L, 255L)
))
cm <- as_ieegio_colormap(ct)

# Discrete: Key 0 is always black; intermediate keys are interpolated
calculate_color(c(0L, 1L, 3L, 5L), cm)
#> [1] "#00000000" "#FF0000"   "#807F00"   "#00FF00"  

# Continuous: attach a value-to-scaled lookup and map numeric data
lut <- as_ieegio_lookup(data.frame(Value = c(0, 1), Scaled = c(0, 1)))
cm_cont <- as_ieegio_colormap(ct, lookup = lut)
calculate_color(c(0, 0.25, 0.5, 0.75, 1), cm_cont)
#> [1] "#FF0000" "#BF4000" "#808000" "#40BF00" "#00FF00"

# NA handling: default fills NA with transparent black
calculate_color(c(1L, NA_integer_, 5L), cm)
#> [1] "#FF0000"   "#00000000" "#00FF00"  
# Pass na = NA to keep NA in the output
calculate_color(c(1L, NA_integer_, 5L), cm, na = NA)
#> [1] "#FF0000" NA        "#00FF00"
# Drop alpha channel for formats that do not support it
calculate_color(c(1L, 5L), cm, keep_alpha = FALSE)
#> [1] "#FF0000" "#00FF00"
```
