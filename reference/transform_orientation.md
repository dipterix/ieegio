# Create transform between coordinate orientations

Generates an affine transformation to convert coordinates or coordinate
frames between different anatomical orientation conventions (e.g., `RAS`
to `LPS`). Supports all 48 possible 3D orientations including axis
permutations.

## Usage

``` r
transform_orientation(
  space_from,
  orientation_from,
  orientation_to,
  interpretation = c("active", "passive")
)
```

## Arguments

- space_from:

  either an `ieegio_space` object or a character string identifying the
  source space. If provided, `orientation_from` must be omitted
  (orientation is extracted from the space object).

- orientation_from:

  character string specifying the source orientation (e.g., `"RAS"`,
  `"LPS"`). Only used if `space_from` is missing. Must be one of the 48
  valid orientation codes.

- orientation_to:

  character string specifying the target orientation. Must be one of the
  48 valid orientation codes.

- interpretation:

  character string specifying transform interpretation:

  - `"active"` (default): Point transform - transforms point coordinates
    from one orientation to another. Use this when you have coordinates
    in the source orientation and want to convert them.

  - `"passive"`: Axis transform - transforms the coordinate frame/basis
    vectors. This is the transpose of the active transform. Use this
    when transforming reference frames or basis vectors.

## Value

An `ieegio_transforms` object containing a 4x4 affine transformation
matrix

## Details

The function creates orthogonal transformations (rotations and
reflections) to convert between different anatomical coordinate
conventions. For active transforms, the matrix can be directly applied
to homogeneous point coordinates. For passive transforms, the matrix
transforms coordinate axes/frames instead.

Common orientation codes (first 8):

- `RAS`, `LAS`, `LPS`, `RPS`, `LPI`, `RPI`, `LAI`, `RAI` (standard axis
  order)

Extended orientations (40 more) include axis permutations like:

- `PIR`, `AIL`, `SAR`, etc. (permuted axes)

The relationship between active and passive interpretations:
`passive_matrix = t(active_matrix)` for orthogonal transforms.

## Examples

``` r
# Active transform: convert point coordinates from RAS to LPS
trans <- transform_orientation(orientation_from = "RAS",
                               orientation_to = "LPS",
                               interpretation = "active")
trans$data[[1]]  # diag(-1, -1, 1, 1)
#>      [,1] [,2] [,3] [,4]
#> [1,]   -1    0    0    0
#> [2,]    0   -1    0    0
#> [3,]    0    0    1    0
#> [4,]    0    0    0    1

# Apply to a point
point_ras <- c(10, 20, 30, 1)
point_lps <- trans$data[[1]] %*% point_ras

# Using a space object
space <- new_space("scanner", orientation = "RAS")
trans <- transform_orientation(space_from = space,
                               orientation_to = "LPS")

# Passive transform: transform coordinate axes
trans_passive <- transform_orientation(orientation_from = "RAS",
                                      orientation_to = "LPS",
                                      interpretation = "passive")

# With axis permutation
trans <- transform_orientation(orientation_from = "RAS",
                               orientation_to = "PIR")
```
