# Convert FLIRT transform to world (RAS) coordinates

Converts an FSL FLIRT matrix from FSL scaled-voxel coordinates to world
(RAS) coordinates. Allows partial conversion by specifying only source,
only reference, or both images.

## Usage

``` r
transform_flirt2ras(transform, source = NULL, reference = NULL)
```

## Arguments

- transform:

  an `ieegio_transforms` object with FSL orientation (typically from
  [`io_read_flirt_transform`](http://dipterix.org/ieegio/reference/io_read_flirt_transform.md)),
  or a 4x4 matrix

- source:

  source (moving) image used in FLIRT registration. Can be:

  - A file path to a NIfTI image

  - An `ieegio_volume` object

  - `NULL` to skip source-side conversion

- reference:

  reference (fixed) image used in FLIRT registration. Can be:

  - A file path to a NIfTI image

  - An `ieegio_volume` object

  - `NULL` to skip reference-side conversion

## Value

An `ieegio_transforms` object with updated orientations:

- Both images provided: RAS -\> RAS transform

- Source only: RAS -\> FSL transform (source side converted)

- Reference only: FSL -\> RAS transform (reference side converted)

- Neither: FSL -\> FSL transform (unchanged)

## Details

FSL FLIRT matrices operate in a scaled-voxel coordinate system that
depends on the image geometry. The conversion to world coordinates uses:

`world_transform = ref_vox2ras %*% ref_fsl2vox %*% flirt %*% src_vox2fsl %*% src_ras2vox`

Where:

- `src_ras2vox`: Inverse of source image's voxel-to-RAS matrix

- `src_vox2fsl`: Source voxel-to-FSL coordinate transform

- `flirt`: The original FLIRT matrix

- `ref_fsl2vox`: Inverse of reference voxel-to-FSL transform

- `ref_vox2ras`: Reference image's voxel-to-RAS matrix

The FSL coordinate system uses scaled voxels with possible X-axis flip
depending on the image's `sform` determinant sign.

## See also

[`io_read_flirt_transform`](http://dipterix.org/ieegio/reference/io_read_flirt_transform.md)
for reading FLIRT matrices
[`transform_orientation`](http://dipterix.org/ieegio/reference/transform_orientation.md)
for general orientation transforms

## Examples

``` r
if (FALSE) { # \dontrun{
# Read FLIRT matrix
xfm <- io_read_flirt_transform("source_to_reference.mat")

# Full conversion to RAS coordinates
xfm_ras <- transform_flirt2ras(xfm,
                               source = "source.nii.gz",
                               reference = "reference.nii.gz")

# Partial conversion (reference side only)
xfm_partial <- transform_flirt2ras(xfm, reference = "reference.nii.gz")

# Using ieegio_volume objects
src_vol <- read_volume("source.nii.gz", header_only = TRUE)
ref_vol <- read_volume("reference.nii.gz", header_only = TRUE)
xfm_ras <- transform_flirt2ras(xfm, source = src_vol, reference = ref_vol)
} # }
```
