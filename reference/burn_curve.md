# Burn a curve trajectory into a volume

Burn density values along a `Catmull-Rom` spline trajectory into a
numeric volume. The curve is sampled at sub-voxel resolution and voxels
within `thickness` of the trajectory are assigned density values.

## Usage

``` r
burn_curve(
  image,
  curve,
  thickness = 1,
  density = 1,
  reshape = FALSE,
  antialias_type = c("reduced", "ignore", "fill", "threshold"),
  merge = c("mean", "max", "min"),
  n_samples = NULL,
  ...
)
```

## Arguments

- image:

  volume providing the coordinate space; see
  [`imaging-volume`](http://dipterix.org/ieegio/reference/imaging-volume.md)

- curve:

  a `ravetools_curve` object (see
  [`catmull_rom_3d`](https://dipterix.org/ravetools/reference/catmull_rom_3d.html)),
  or an \\n \times 3\\ numeric matrix of key points in 'RAS' world
  coordinates. If a matrix is supplied,
  [`catmull_rom_3d`](https://dipterix.org/ravetools/reference/catmull_rom_3d.html)
  is called automatically (requires the ravetools package).

- thickness:

  tube radius (half-width) around the curve in world ('RAS') coordinates
  (millimeters). Can be a positive scalar or a function of `t` in
  `[0, 1]` that returns a positive scalar, with the same call convention
  as `curve$get_point(t)`.

- density:

  value to burn into voxels within the tube. Can be a numeric scalar or
  a function of `t` in `[0, 1]` that returns a numeric scalar.

- reshape:

  whether to reshape the image before burning; identical semantics to
  [`burn_volume`](http://dipterix.org/ieegio/reference/burn_volume.md):
  `FALSE` (default, keep original resolution), `TRUE` (double each
  dimension), a single positive number (isotropic new size), or a
  length-3 integer vector.

- antialias_type:

  how to handle voxels at the edge of the tube. One of `"reduced"`
  (default), `"ignore"`, `"fill"`, or `"threshold"`:

  `"ignore"`

  :   Only voxels whose center is strictly within `thickness` are burned
      at full density.

  `"fill"`

  :   Any voxel that overlaps the tube (center within
      `thickness + half_voxel_diagonal`) is fully burned.

  `"threshold"`

  :   Voxels are burned at full density if more than 50\\ within
      `thickness`; otherwise skipped.

  `"reduced"`

  :   Density is scaled proportionally by the fraction of the voxel
      (8-corner approximation) that lies within `thickness`.

- merge:

  how to combine density values when multiple curve samples illuminate
  the same voxel. One of `"mean"` (default, average of all contributing
  samples), `"max"` (maximum value), or `"min"` (minimum value).

- n_samples:

  number of points to sample along the curve. Default `NULL` computes
  `ceiling(arc_length / min_voxel_size * 3)` (3x oversampling), with a
  minimum of 10.

- ...:

  passed to
  [`as_ieegio_volume`](http://dipterix.org/ieegio/reference/as_ieegio_volume.md),
  useful when `image` is an array.

## Value

A numeric `ieegio_volume` with the same spatial extent as `image` (or
the reshaped extent). Background voxels contain `0`. When multiple curve
samples illuminate the same voxel, values are combined according to
`merge`.

## Examples

``` r

if (interactive()) {

dm <- c(50, 50, 50)
image <- as_ieegio_volume(
  array(0, dm),
  vox2ras = rbind(cbind(diag(1, 3), -dm / 2),
                  c(0, 0, 0, 1))
)

# Three-point trajectory (Catmull-Rom from matrix)
key_pts <- rbind(c(-50, 0, 0), c(0, 20, 2), c(-10, 0, -1))

curve <- ravetools::catmull_rom_3d(key_pts)

plot(curve, use_rgl = FALSE)

# Constant thickness and density
burned <- burn_curve(image, key_pts, thickness = 3, density = 100)

plot(burned, zoom = 3, position = c(0, 0, 0), which = "axial")

# Varying thickness and density along the curve
burned2 <- burn_curve(
  image, curve,
  thickness      = function(t) 2 + t * 4,
  density        = function(t) 100 * (1 - t) + 100,
  antialias_type = "reduced"
)
plot(burned2, zoom = 3, position = c(0, 0, 0), which = "axial")

}

```
