# Transform surface between coordinate spaces

Transforms surface vertex positions from one coordinate space or
orientation to another, optionally applying an additional custom
transform.

## Usage

``` r
surface_to_surface(surface, space_from = "", space_to = "", transform = NULL)
```

## Arguments

- surface:

  an `ieegio_surface` object or file path; see
  [`as_ieegio_surface`](http://dipterix.org/ieegio/reference/as_ieegio_surface.md)
  for valid inputs

- space_from:

  source coordinate space; either an `ieegio_space` object (from
  [`new_space`](http://dipterix.org/ieegio/reference/new_space.md)) or a
  character string; default is empty string

- space_to:

  target coordinate space; either an `ieegio_space` object or a
  character string; default is empty string

- transform:

  optional 4x4 affine transformation matrix or `ieegio_transforms`
  object to apply; see
  [`as_ieegio_transform`](http://dipterix.org/ieegio/reference/as_ieegio_transform.md)

## Value

A transformed `ieegio_surface` object with updated vertex positions and
transform metadata

## Details

The function handles orientation changes (e.g., `"RAS"` to `"LPS"`) and
optional custom transforms. It creates a transform chain consisting of:
an affine (orientation alignment from source), the custom transform, and
a post-affine (final orientation alignment to target).

If the provided transform has a `"passive"` interpretation, it is
automatically converted to an `"active"` interpretation before being
applied to the vertex coordinates.

## See also

[`as_ieegio_surface`](http://dipterix.org/ieegio/reference/as_ieegio_surface.md)
for creating surface objects,
[`new_space`](http://dipterix.org/ieegio/reference/new_space.md) for
defining coordinate spaces,
[`transform_orientation`](http://dipterix.org/ieegio/reference/transform_orientation.md)
for orientation transforms,
[`volume_to_surface`](http://dipterix.org/ieegio/reference/volume_to_surface.md)
for creating surfaces from volumes

## Examples

``` r
library(ieegio)

# geometry
geom_file <- "gifti/GzipBase64/sujet01_Lwhite.surf.gii"

if(ieegio_sample_data(geom_file, test = TRUE)) {

surf_ras <- read_surface(ieegio_sample_data(geom_file))
plot(surf_ras)

# ---- Change axis orientation ------------------
# convert from RAS orientation to LPS
surf_lps <- surface_to_surface(
  surf_ras,
  space_from = new_space("", orientation = "RAS"),
  space_to = new_space("", orientation = "LPS")
)
plot(surf_lps)

# validate
lps_verts <- diag(c(-1, -1, 1, 1)) %*% surf_ras$geometry$vertices
range(surf_lps$geometry$vertices - lps_verts)

# ---- Apply transforms ------------------
transform <- matrix(
  byrow = TRUE, nrow = 4,
  c(
    0.5, 0, 0.3, 1,
    0, -1, 0.2, 2,
    0, 0.7, -0.5, 4,
    0, 0, 0, 1
  )
)
surf_stretch <- surface_to_surface(surf_ras, transform = transform)
plot(surf_stretch)

# validate
stretch_verts <- transform %*% surf_ras$geometry$vertices
range(surf_stretch$geometry$vertices - stretch_verts)


}
#> [1] 0 0
```
