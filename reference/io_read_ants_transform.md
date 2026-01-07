# Read `ANTs` transform file

Reads spatial transformation files in `ANTs` (Advanced Normalization
Tools) format, including affine matrices (`.mat`) and deformation fields
(`.h5`, `.nii.gz`).

## Usage

``` r
io_read_ants_transform(
  file,
  space_from,
  space_to,
  interpretation = c("passive", "active")
)
```

## Arguments

- file:

  character string specifying the path to the transform file. Supported
  formats include:

  - `.mat`: `ITK`/`ANTs` affine transform (4x4 matrix)

  - `.h5`: HDF5 composite transform (may contain affine and/or
    deformation components)

  - `.nii`, `.nii.gz`: Deformation field images

- space_from:

  character string or `ieegio_space` object identifying the source
  space. If missing, will be inferred from the filename using BIDS-style
  `from-<space>` entity (e.g., `"sub-01_from-T1w_to-MNI_xfm.h5"` yields
  `"T1w"`).

- space_to:

  character string or `ieegio_space` object identifying the target
  space. If missing, will be inferred from the filename using BIDS-style
  `to-<space>` entity.

- interpretation:

  character string specifying how to interpret the transform:

  - `"passive"` (default): Axis/coordinate frame transform. Represents
    how coordinate systems relate to each other. This is the typical
    interpretation for brain imaging registration transforms.

  - `"active"`: Point transform. Directly transforms point coordinates
    from source to target space.

## Value

An `ieegio_transforms` object with:

- data:

  List containing the transform data (matrix for affine, `ANTsTransform`
  object for deformation)

- type:

  `"affine"` or `"deformation"`

- interpretation:

  `"active"` or `"passive"`

- space_from:

  Source space (with `"LPS"` orientation for `ANTs`)

- space_to:

  Target space (with `"LPS"` orientation for `ANTs`)

- dimension:

  Spatial dimension (typically 3)

## Details

`ANTs` transforms operate in `LPS` (Left-Posterior-Superior) coordinate
convention. The returned transform object automatically sets orientation
to `"LPS"` for both source and target spaces.

For composite transforms (e.g., `.h5` files containing both affine and
deformation components), the function returns a single transform object.
Use
[`as_ieegio_transform`](http://dipterix.org/ieegio/reference/as_ieegio_transform.md)
with a list to combine multiple transforms.

This function requires the `rpyANTs` package and a configured Python
environment.

## BIDS Support

The function can automatically infer space names from BIDS-compliant
file names:

- `from-<source>`: Source space identifier

- `to-<target>`: Target space identifier

Example: `"sub-01_from-T1w_to-MNI152NLin2009cAsym_mode-image_xfm.h5"`

## See also

[`as_ieegio_transform`](http://dipterix.org/ieegio/reference/as_ieegio_transform.md)
for converting objects to transforms and chaining
[`transform_orientation`](http://dipterix.org/ieegio/reference/transform_orientation.md)
for orientation conversion transforms

## Examples

``` r
if (FALSE) { # \dontrun{
# Read an affine transform
xfm <- io_read_ants_transform("sub-01_from-T1w_to-MNI_xfm.mat")

# Explicitly specify spaces
xfm <- io_read_ants_transform(
  "transform.h5",
  space_from = "native",
  space_to = "MNI152"
)

# Read as active (point) transform
xfm <- io_read_ants_transform(
  "transform.mat",
  interpretation = "active"
)

# Chain multiple transforms
xfm1 <- io_read_ants_transform("from-T1w_to-T2w_xfm.mat")
xfm2 <- io_read_ants_transform("from-T2w_to-MNI_xfm.h5")
combined <- as_ieegio_transform(list(xfm1, xfm2))
} # }
```
