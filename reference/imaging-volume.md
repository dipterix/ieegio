# Read and write volume data

Read and write volume data ('MRI', 'CT', etc.) in 'NIfTI' or 'MGH'
formats. Please use `read_volume` and `write_volume` for high-level
function. These functions will call other low-level functions
internally.

## Usage

``` r
read_volume(file, header_only = FALSE, format = c("auto", "nifti", "mgh"), ...)

write_volume(x, con, format = c("auto", "nifti", "mgh"), ...)

io_read_mgz(file, header_only = FALSE)

io_write_mgz(x, con, ...)

# S3 method for class 'ieegio_volume'
io_write_mgz(x, con, ...)

# S3 method for class 'ieegio_mgh'
io_write_mgz(x, con, ...)

# S3 method for class 'nifti'
io_write_mgz(x, con, ...)

# S3 method for class 'niftiImage'
io_write_mgz(x, con, ...)

# S3 method for class 'ants.core.ants_image.ANTsImage'
io_write_mgz(x, con, ...)

# S3 method for class 'array'
io_write_mgz(x, con, vox2ras = NULL, ...)

io_read_nii(
  file,
  method = c("rnifti", "oro", "ants"),
  header_only = FALSE,
  ...
)

io_write_nii(x, con, ...)

# S3 method for class 'ieegio_nifti'
io_write_nii(x, con, ...)

# S3 method for class 'ants.core.ants_image.ANTsImage'
io_write_nii(x, con, ...)

# S3 method for class 'niftiImage'
io_write_nii(x, con, ...)

# S3 method for class 'nifti'
io_write_nii(x, con, gzipped = NA, ...)

# S3 method for class 'ieegio_mgh'
io_write_nii(x, con, ...)

# S3 method for class 'array'
io_write_nii(
  x,
  con,
  vox2ras = NULL,
  datatype_code = NULL,
  xyzt_units = c("NIFTI_UNITS_MM", "NIFTI_UNITS_SEC"),
  intent_code = "NIFTI_INTENT_NONE",
  ...,
  gzipped = NA
)
```

## Format

format of the file; default is auto-detection, other choices are
`'nifti'` and `'mgh'`;

## Arguments

- file:

  file path to read volume data

- header_only:

  whether to read header data only; default is `FALSE`

- format:

  format of the file to be written; choices are `'auto'`, `'nifti'` or
  `'mgh'`; default is to `'auto'` detect the format based on file names,
  which will save as a 'MGH' file when file extension is `'mgz'` or
  `'mgh'`, otherwise 'NIfTI' format. We recommend explicitly setting
  this argument

- ...:

  passed to other methods

- x:

  volume data (such as 'NIfTI' image, array, or 'MGH') to be saved

- con:

  file path to store image

- vox2ras:

  a `4x4` transform matrix from voxel indexing (column, row, slice) to
  scanner (often 'T1-weighted' image) 'RAS' (right-anterior-superior)
  coordinate

- method:

  method to read the file; choices are `'oro'` (using
  [`readNIfTI`](https://rdrr.io/pkg/oro.nifti/man/read_nifti.html)),
  `'rnifti'` (using
  [`readNifti`](https://rdrr.io/pkg/RNifti/man/readNifti.html)), and
  `'ants'` (using
  [`as_ANTsImage`](http://dipterix.org/rpyANTs/reference/as_ANTsImage.md)).

- gzipped:

  for writing `'nii'` data: whether the file needs to be compressed;
  default is inferred from the file name. When the file ends with
  `'nii'`, then no compression is used; otherwise the file will be
  compressed. If the file name does not end with `'nii'` nor `'nii.gz'`,
  then the file extension will be added automatically.

- datatype_code, xyzt_units, intent_code:

  additional flags for 'NIfTI' headers, for advanced users

## Value

Imaging readers return `ieegio_volume` objects. The writers return the
file path to where the file is saved to.

## Examples

``` r

library(ieegio)

nifti_file <- "brain.demosubject.nii.gz"

# Use `ieegio_sample_data(nifti_file)`
#   to download sample data


if( ieegio_sample_data(nifti_file, test = TRUE) ) {

# ---- NIfTI examples ---------------------------------------------

file <- ieegio_sample_data(nifti_file)

# basic read
vol <- read_volume(file)

# voxel to scanner RAS
vol$transforms$vox2ras

# to freesurfer surface
vol$transforms$vox2ras_tkr

# to FSL
vol$transforms$vox2fsl

plot(vol, position = c(10, 0, 30))

# ---- using other methods --------------------------------------
# default
vol <- read_volume(file, method = "rnifti", format = "nifti")
vol$header

# lazy-load nifti
vol2 <- read_volume(file, method = "oro", format = "nifti")
vol2$header

if (FALSE) { # \dontrun{
# requires additional python environment

# Using ANTsPyx
vol3 <- read_volume(file, method = "ants", format = "nifti")
vol3$header

} # }

# ---- write --------------------------------------------------------

# write as NIfTI
f <- tempfile(fileext = ".nii.gz")

write_volume(vol, f, format = "nifti")

# alternative method
write_volume(vol$header, f, format = "nifti")

# write to mgz/mgh
f2 <- tempfile(fileext = ".mgz")

write_volume(vol, f, format = "mgh")

# clean up
unlink(f)
unlink(f2)

# ---- Special case in WebAsssembly --------------------------------
# oro.nifti backend is always used

# Emulate WebAssemply when RNifti is unavailable, using oro.nifti instead
old_opt <- options("ieegio.debug.emscripten" = TRUE)
on.exit({ options(old_opt) }, add = TRUE)


# In WebAssemply, RNifti is not available, using oro.nifti instead
vol <- read_volume(file)

stopifnot(vol$type[[1]] == "oro")

# Cleanup: make sure the options are reset
options(old_opt)

}

```
