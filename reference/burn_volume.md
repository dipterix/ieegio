# Burn image at given positions

Burn image at given positions with given color and radius.

## Usage

``` r
burn_volume(
  image,
  ras_position,
  col = "red",
  radius = 1,
  reshape = FALSE,
  alpha = FALSE,
  blank_underlay = FALSE,
  ...,
  preview = NULL
)
```

## Arguments

- image:

  volume

- ras_position:

  image-defined right-anterior-posterior positions, an `nx3` matrix,
  each row is an 'RAS' coordinate

- col:

  vector of integer or characters, color of each contact

- radius:

  vector of positive number indicating the burning radius

- reshape:

  whether to reshape the image at a different resolution; default is
  false; can be `TRUE` (image resolution will be doubled), a single
  number (size of isotropic volume along one side), or a length of three
  defining the new shape.

- alpha:

  whether to include alpha (transparent) channel. Default is false for
  compatibility concerns (legacy software might not support reading
  alpha channel). In this case, the background will be black. If
  `alpha=TRUE` is set, then the background will be fully transparent.

- blank_underlay:

  whether to use blank image or the input `image` as underlay; default
  is `FALSE` (using `image` as underlay); alternative is `TRUE`, and use
  black or transparent background

- ...:

  passed to
  [`as_ieegio_volume`](http://dipterix.org/ieegio/reference/as_ieegio_volume.md),
  useful if `image` is an array

- preview:

  indices (integer) of the position to visualize; default is `NULL` (no
  preview)

## Value

Color image that is burnt; see
[`imaging-volume`](http://dipterix.org/ieegio/reference/imaging-volume.md).

## Examples

``` r
if(interactive()) {

dim <- c(6, 6, 6)
image <- as_ieegio_volume(
  array(rnorm(prod(dim)), dim),
  vox2ras = rbind(cbind(diag(1, 3), -dim / 2),
                  c(0, 0, 0, 1))
)

ras_positions <- rbind(c(1, -1, 1.5), c(-2.25, -1, -0.75))


burned <- burn_volume(
  image,
  ras_positions,
  col = c("red", "green"),
  radius = 0.5,
  reshape = c(24, 24, 24)
)

plot(
  burned,
  position = ras_positions[1, ],
  zoom = 15,
  pixel_width = 0.25
)

}
```
