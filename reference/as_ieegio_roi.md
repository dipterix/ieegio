# Convert objects to `'ieegio'` region of interest

Marks an image volume, surface, point cloud, or streamlines object as a
region of interest (`ROI`). The returned object is the underlying
`'ieegio'` instance with an extra `"ieegio_roi"` class and an
`"roi_info"` attribute; see 'Details'.

## Usage

``` r
as_ieegio_roi(x, ...)

# Default S3 method
as_ieegio_roi(x, ...)

# S3 method for class 'ieegio_roi'
as_ieegio_roi(x, ...)

# S3 method for class 'character'
as_ieegio_roi(x, type = c("auto", "volume", "surface", "streamlines"), ...)

# S3 method for class 'ieegio_streamlines'
as_ieegio_roi(x, threshold_lb = NA, threshold_ub = NA, ...)

# S3 method for class 'list'
as_ieegio_roi(x, threshold_lb = NA, threshold_ub = NA, ...)

# S3 method for class 'matrix'
as_ieegio_roi(x, type = c("pointcloud", "streamlines"), ...)

# S3 method for class 'data.frame'
as_ieegio_roi(x, type = c("pointcloud", "streamlines"), ...)

# S3 method for class 'numeric'
as_ieegio_roi(x, ...)

# S3 method for class 'integer'
as_ieegio_roi(x, ...)

# S3 method for class 'array'
as_ieegio_roi(x, ...)

# S3 method for class 'ieegio_volume'
as_ieegio_roi(x, threshold_lb = NA, threshold_ub = NA, ...)

# S3 method for class 'niftiImage'
as_ieegio_roi(x, threshold_lb = NA, threshold_ub = NA, ...)

# S3 method for class 'nifti'
as_ieegio_roi(x, threshold_lb = NA, threshold_ub = NA, ...)

# S3 method for class 'ants.core.ants_image.ANTsImage'
as_ieegio_roi(x, threshold_lb = NA, threshold_ub = NA, ...)

# S3 method for class 'ieegio_surface'
as_ieegio_roi(
  x,
  threshold_expr = NULL,
  quoted = FALSE,
  env = parent.frame(),
  ...
)

# S3 method for class 'mesh3d'
as_ieegio_roi(
  x,
  threshold_expr = NULL,
  quoted = FALSE,
  env = parent.frame(),
  ...
)

# S3 method for class 'fs.surface'
as_ieegio_roi(
  x,
  threshold_expr = NULL,
  quoted = FALSE,
  env = parent.frame(),
  ...
)

# S3 method for class 'ieegio_roi'
print(x, ...)
```

## Arguments

- x:

  R object or file path to convert; when `x` is a character vector of
  file paths, only the first element is used and the remaining elements
  are ignored

- ...:

  passed to the underlying
  [`as_ieegio_volume`](http://dipterix.org/ieegio/reference/as_ieegio_volume.md),
  [`as_ieegio_surface`](http://dipterix.org/ieegio/reference/as_ieegio_surface.md),
  or
  [`as_ieegio_streamlines`](http://dipterix.org/ieegio/reference/imaging-streamlines.md)
  methods

- type:

  type of the region of interest. For file paths, the choices are
  `"auto"` (default, inferred from the file extension), `"volume"`,
  `"surface"`, and `"streamlines"`; for matrices and data frames, the
  choices are `"pointcloud"` (default) and `"streamlines"`. There is no
  `"pointcloud"` choice for file paths: a point cloud on disk is simply
  a surface file that carries no face index, so the file is read as a
  surface and the label is corrected afterwards from the face count.

- threshold_lb, threshold_ub:

  lower and upper bounds selecting the parts that belong to the region
  of interest; default is `NA` (no bound). Non-finite bounds are
  dropped. For image volumes the bounds select voxels by intensity; for
  streamlines they select tracts by arc length, in the unit of the world
  coordinate system

- threshold_expr:

  expression selecting the vertices that belong to the region of
  interest, evaluated with `.x` bound to the surface, `.a` and `.m`
  bound to the annotation and measurement tables, and `.a1` and `.m1`
  bound to the first column of those tables; default is `NULL` (no
  selection)

- quoted:

  whether `threshold_expr` has been quoted already; default is `FALSE`

- env:

  environment in which `threshold_expr` is evaluated; default is the
  calling frame

## Value

`as_ieegio_roi` returns the converted `'ieegio'` object with
`"ieegio_roi"` prepended to its class and an `"roi_info"` attribute.
`print` returns the object invisibly.

## Details

A region of interest is a decorator describing *how* to derive the
region from the object it wraps; it does not alter the underlying data.
The criteria are stored in the `"roi_info"` attribute, a list with the
resolved `type` plus, where applicable, `threshold_lb`, `threshold_ub`,
or `threshold_fun`. Applying them is left to the functions that consume
a region of interest, and those functions document the behavior
themselves;
[`resolve_roi_as`](http://dipterix.org/ieegio/reference/resolve_roi_as.md)
is the general one, applying the criteria and returning the region
itself.

The two bounds are interpreted against whatever quantity is natural for
the enclosed object: voxel intensity for an image volume, and tract arc
length for streamlines. Surfaces and point clouds use `threshold_expr`
instead, since their selection is driven by vertex annotations or
measurements rather than by a single scalar.

Because the criteria live in an attribute, operations that rebuild the
object drop them. This is intended: in most workflows the enclosed
volume, surface, or streamlines object is what gets used, and the caller
is responsible for carrying the decorator along when it matters.

## See also

[`resolve_roi_as`](http://dipterix.org/ieegio/reference/resolve_roi_as.md)
applies the criteria recorded here and converts the region into a chosen
representation.

## Examples

``` r

# ---- Point cloud from a matrix ---------------------------------------
points <- matrix(rnorm(30), ncol = 3)
roi <- as_ieegio_roi(points)
attr(roi, "roi_info")$type
#> [1] "pointcloud"

print(roi)
#> <ieegio ROI [pointcloud]>
#> <ieegio Surface>
#>   Header class: basic_geometry
#>   Geometry : 
#>     # of Vertex     : 10
#>     # of Face index : 0
#>     # of transforms : 0
#>       Transform Targets : 
#> 
#> Contains: `geometry`
#> 

# ---- Streamlines: rows of `NA` separate the tracts -------------------
tracts <- rbind(
  matrix(rnorm(9), ncol = 3),
  NA,
  matrix(rnorm(15), ncol = 3)
)
roi <- as_ieegio_roi(tracts, type = "streamlines")
length(roi)
#> [1] 2

# ---- Image volume with threshold bounds ------------------------------
vox2ras <- matrix(
  c(-1, 0, 0, 5,
    0, 0, 1, -5,
    0, -1, 0, 5,
    0, 0, 0, 1),
  nrow = 4, byrow = TRUE
)
volume <- as_ieegio_roi(
  array(rnorm(1000), c(10, 10, 10)),
  vox2ras = vox2ras,
  threshold_lb = 0.5
)
attr(volume, "roi_info")$threshold_lb
#> [1] 0.5

# The criteria survive a round trip
attr(as_ieegio_roi(volume), "roi_info")$threshold_lb
#> [1] 0.5

# ---- Surface with a threshold expression -----------------------------
vertices <- matrix(rnorm(60), ncol = 3)
faces <- matrix(c(1, 2, 3, 2, 3, 4), ncol = 3, byrow = TRUE)
surface <- as_ieegio_surface(
  vertices,
  faces = faces,
  measurements = data.frame(Curv = rnorm(20))
)

roi <- as_ieegio_roi(surface, threshold_expr = .m1 > 0)
selection <- attr(roi, "roi_info")$threshold_fun(roi)
head(selection)
#> [1]  TRUE  TRUE  TRUE FALSE  TRUE FALSE

```
