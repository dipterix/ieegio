# Create smooth surface from volume mask or data

Create smooth surface from volume mask or data

## Usage

``` r
volume_to_surface(
  volume,
  lambda = 0.2,
  degree = 2,
  threshold_lb = 0.5,
  threshold_ub = NA,
  smooth_method = c("implicit", "explicit", "none"),
  smooth_iterations = 10L,
  max_vertices = 5e+05,
  ...
)
```

## Arguments

- volume:

  volume object or path to the NIfTI volume files, see
  `as_ieegio_volume` for details

- lambda, degree:

  smooth parameters; see
  [`vcg_smooth_implicit`](https://dipterix.org/ravetools/reference/vcg_smooth.html)
  for details. To disable smoothing, set `lambda` to negative or `NA`

- threshold_lb, threshold_ub:

  threshold of volume, see
  [`vcg_isosurface`](https://dipterix.org/ravetools/reference/vcg_isosurface.html);
  default is any voxel value above 0.5

- smooth_method:

  `"implicit"` (default) smooths with
  [`vcg_smooth_implicit`](https://dipterix.org/ravetools/reference/vcg_smooth.html)
  using `lambda` and `degree`; `"explicit"` smooths with
  [`mris_smooth`](https://dipterix.org/ravetools/reference/mris_smooth.html)
  instead, repeated neighbor averaging whose memory grows only linearly
  with the surface (with a ravetools version that does not have
  `mris_smooth`, the `"laplace"` type of
  [`vcg_smooth_explicit`](https://dipterix.org/ravetools/reference/vcg_smooth.html)
  is used); `"none"` returns the surface without smoothing

- smooth_iterations:

  number of averaging rounds when `smooth_method` is `"explicit"`;
  default is `10`

- max_vertices:

  used only when `smooth_method` is `"implicit"`, whose memory grows
  quickly with the surface size: surfaces with more vertices than this
  are reduced to about this many with `ravetools::vcg_decimate()` before
  smoothing, which removes vertices from flat regions first and keeps
  the shape; default is `500000`. Because the smoothing works in mesh
  steps, the same `lambda` and `degree` smooth a reduced surface more;
  use a larger value or `Inf` to smooth at full resolution. With a
  ravetools version that does not have `vcg_decimate`, surfaces with
  more than `20000` vertices are not smoothed, since the implicit
  smoothing of those versions can crash on large surfaces

- ...:

  passed to `as_ieegio_volume`

## Value

A `as_ieegio_surface` object; the surface is transformed into anatomical
space defined by the volume.

## Examples

``` r


# toy example; in practice, use tha path to the volume
volume <- array(0, dim = rep(30, 3))
volume[11:20, 11:20, 3:28] <- 1
volume[3:28, 11:20, 11:20] <- 1
volume[11:20, 3:28, 11:20] <- 1
vox2ras <- diag(1, 4)

surf <- volume_to_surface(volume, vox2ras = vox2ras)

if(interactive()) {
  plot(surf)
}

```
