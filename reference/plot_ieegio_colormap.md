# Plot a color map legend

Plot a visual legend for an `ieegio_colormap` object.

The continuous method draws a vertical gradient bar with tick marks on
the requested axis side (default left); the bar is rendered flush to the
opposite edge. Ticks are placed at the minimum, maximum, and zero (when
zero falls strictly inside the range).

The discrete method draws vertically stacked labeled color squares.
Labels appear to the right of each square. When there are more labels
than `max_labels`, the excess are shown as `"... (N omitted)"`.

## Usage

``` r
# S3 method for class 'ieegio_colormap_continuous'
plot(x, data_range = NULL, side = 2L, las = 1L, n_colors = 256L, ...)

# S3 method for class 'ieegio_colormap_discrete'
plot(x, max_labels = NA, cex = 1, border = NA, ...)
```

## Arguments

- x:

  an `ieegio_colormap_continuous` or `ieegio_colormap_discrete` object

- data_range:

  numeric(2); overrides the color bar range. Falls back to
  `x$data_range`, then to `c(0, 1)` if still missing.

- side:

  integer; axis side for tick marks (see
  [`axis`](https://rdrr.io/r/graphics/axis.html)). `2` (left, default)
  places ticks on the left; `4` on the right.

- las:

  integer; label orientation passed to
  [`axis`](https://rdrr.io/r/graphics/axis.html). Default `1` (always
  horizontal).

- n_colors:

  integer; number of color samples used to render the gradient bar.
  Default `256`.

- ...:

  currently unused; reserved for future arguments.

- max_labels:

  integer or `NA`; maximum number of labels to display in the discrete
  legend. `NA` (default) auto-sizes from the current plot-region height.

- cex:

  numeric; character expansion for label text. Default `1`.

- border:

  color for the box border in the discrete legend; `NA` (default) draws
  no border.

## Value

Invisibly returns `x`.

## Examples

``` r
# Continuous legend --------------------------------------------------
ct <- as_ieegio_colortable(data.frame(
  Key = c(0L, 50L, 100L),
  R = c(0L, 255L, 255L), G = c(0L, 255L, 0L),
  B = c(255L, 0L, 0L),   A = c(255L, 255L, 255L)
))
lut <- as_ieegio_lookup(data.frame(
  Value = c(-3, 0, 3), Scaled = c(0, 0.5, 1)
))
cm_cont <- as_ieegio_colormap(ct, lookup = lut, data_range = c(-3, 3))
grDevices::pdf(NULL)
plot(cm_cont)
grDevices::dev.off()
#> agg_record_20567477318c 
#>                       2 

# Discrete legend ----------------------------------------------------
ct2 <- as_ieegio_colortable(data.frame(
  Key = 1:4,
  R = c(255L, 0L, 0L, 128L), G = c(0L, 200L, 0L, 0L),
  B = c(0L, 0L, 180L, 180L), A = c(255L, 255L, 255L, 255L)
))
lut2 <- as_ieegio_lookup(data.frame(
  Key   = 1:4,
  Label = c("Cortex", "White matter", "CSF", "Hippocampus")
))
cm_disc <- as_ieegio_colormap(ct2, lookup = lut2)
grDevices::pdf(NULL)
plot(cm_disc)
grDevices::dev.off()
#> agg_record_20567477318c 
#>                       2 
```
