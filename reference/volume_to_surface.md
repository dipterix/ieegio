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
