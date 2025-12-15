# Convert objects to `'ieegio'` image volumes

Convert array, path, or 'NIfTI' images in other formats to `'ieegio'`
image volume instance

## Usage

``` r
as_ieegio_volume(x, ...)

# S3 method for class 'character'
as_ieegio_volume(x, ...)

# S3 method for class 'ieegio_volume'
as_ieegio_volume(x, ...)

# S3 method for class 'array'
as_ieegio_volume(x, vox2ras = NULL, as_color = is.character(x), ...)

# S3 method for class 'niftiImage'
as_ieegio_volume(x, ...)

# S3 method for class 'nifti'
as_ieegio_volume(x, ...)

# S3 method for class 'ants.core.ants_image.ANTsImage'
as_ieegio_volume(x, ...)
```

## Arguments

- x:

  R object such as array, image path, or objects such as `'RNifti'` or
  `'oro.nifti'` image instances

- ...:

  passed to other methods

- vox2ras:

  a `4x4` 'affine' matrix representing the transform from 'voxel' index
  (column-row-slice) to 'RAS' (right-anterior-superior) coordinate. This
  transform is often called `'xform'`, `'sform'`, `'qform'` in 'NIfTI'
  terms, or `'Norig'` in 'FreeSurfer'

- as_color:

  for converting arrays to volume, whether to treat `x` as array of
  colors; default is true when `x` is a raster matrix ( matrix of color
  strings) and false when `x` is not a character array.

## Value

An `ieegio` volume object; see
[`imaging-volume`](http://dipterix.org/ieegio/reference/imaging-volume.md)

## Examples

``` r

shape <- c(50, 50, 50)
vox2ras <- matrix(
  c(-1, 0, 0, 25,
    0, 0, 1, -25,
    0, -1, 0, 25,
    0, 0, 0, 1),
  nrow = 4, byrow = TRUE
)

# continuous
x <- array(rnorm(125000), shape)

volume <- as_ieegio_volume(x, vox2ras = vox2ras)
plot(volume, zoom = 3, pixel_width = 0.5)


# color rgb(a)
x <- array(
  sample(c("red","blue", "green", "cyan", "yellow"),
         12500, replace = TRUE),
  shape
)
rgb <- as_ieegio_volume(x, vox2ras = vox2ras)
plot(rgb, zoom = 3, pixel_width = 0.5)




# ---- When RNifti package is not available ---------------------------

# Emulate WebAssemply when RNifti is unavailable, using oro.nifti instead
old_opt <- options("ieegio.debug.emscripten" = TRUE)
on.exit({ options(old_opt) }, add = TRUE)

shape <- c(50, 50, 50)
vox2ras <- matrix(
  c(-1, 0, 0, 25,
    0, 0, 1, -25,
    0, -1, 0, 25,
    0, 0, 0, 1),
  nrow = 4, byrow = TRUE
)

# continuous
x <- array(rnorm(125000), shape)

# In WebAssemply, RNifti is not available, using oro.nifti instead
volume <- as_ieegio_volume(x, vox2ras = vox2ras)

stopifnot(volume$type[[1]] == "oro")
#> Error: volume$type[[1]] == "oro" is not TRUE

plot(volume, zoom = 3, pixel_width = 0.5)


# Cleanup: make sure the options are reset
options(old_opt)

```
