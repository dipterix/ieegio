# Read FSL FLIRT transformation matrix

Reads a 4x4 affine transformation matrix from an FSL FLIRT output file.
FLIRT matrices operate in FSL scaled-voxel coordinate system and require
source and reference images to convert to world (RAS) coordinates.

## Usage

``` r
io_read_flirt_transform(file, space_from, space_to)
```

## Arguments

- file:

  character string specifying the path to the FLIRT matrix file. This is
  a plain text file containing a 4x4 affine matrix.

- space_from:

  character string or `ieegio_space` object identifying the source
  (moving) space. If missing, will be inferred from the filename using
  BIDS-style `from-<space>` entity.

- space_to:

  character string or `ieegio_space` object identifying the target
  (reference) space. If missing, will be inferred from the filename
  using BIDS-style `to-<space>` entity.

## Value

An `ieegio_transforms` object with:

- data:

  List containing the 4x4 FLIRT matrix

- type:

  `"affine"`

- interpretation:

  `"active"` (FLIRT matrices are point transforms)

- space_from:

  Source space (with `"FSL"` orientation)

- space_to:

  Target space (with `"FSL"` orientation)

- dimension:

  3

## Details

FLIRT matrices operate in FSL scaled-voxel coordinate system, which is:

- Voxel indices multiplied by voxel sizes (`pixdim`)

- X-axis inverted if the image has positive `sform` determinant
  (neurological convention)

The returned transform has `"FSL"` orientation for both source and
target spaces. To convert to world (RAS) coordinates, use
[`transform_flirt2ras`](http://dipterix.org/ieegio/reference/transform_flirt2ras.md)
with the source and/or reference images.

FLIRT matrices are **active** transforms: they map point coordinates
from the source (moving) image space to the reference (fixed) image
space.

## BIDS Support

The function can automatically infer space names from BIDS-compliant
file names:

- `from-<source>`: Source space identifier

- `to-<target>`: Target space identifier

## See also

[`transform_flirt2ras`](http://dipterix.org/ieegio/reference/transform_flirt2ras.md)
for converting to world coordinates
[`io_read_ants_transform`](http://dipterix.org/ieegio/reference/io_read_ants_transform.md)
for reading ANTs format transforms
[`as_ieegio_transform`](http://dipterix.org/ieegio/reference/as_ieegio_transform.md)
for converting objects to transforms

## Examples

``` r
if (FALSE) { # \dontrun{
# Read a FLIRT matrix
xfm <- io_read_flirt_transform("source_to_reference.mat")

# Convert to RAS coordinates (requires source and reference images)
xfm_ras <- transform_flirt2ras(xfm, source = "source.nii.gz",
                                reference = "reference.nii.gz")

# Explicitly specify spaces
xfm <- io_read_flirt_transform(
  "transform.mat",
  space_from = "T1w",
  space_to = "MNI152"
)
} # }
```
