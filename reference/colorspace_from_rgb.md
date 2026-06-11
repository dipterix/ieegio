# Color space conversion helpers

Convert an n-by-3 RGB integer matrix (values 0-255) to a target color
space, or convert back. All conversions use base-R `grDevices` - no
additional packages required.

## Usage

``` r
colorspace_from_rgb(rgb_mat, to = c("RGB", "sRGB", "HSV", "HCL", "Lab"))

colorspace_to_rgb(mat, from = c("RGB", "sRGB", "HSV", "HCL", "Lab"))
```

## Arguments

- rgb_mat:

  integer matrix with columns R, G, B (values 0-255)

- to, from:

  one of `"RGB"`, `"sRGB"`, `"HSV"`, `"HCL"`, `"Lab"`

- mat:

  color space matrix returned by `colorspace_from_rgb`

## Value

A numeric matrix with 3 columns in the target color space. For
`colorspace_to_rgb`: an integer matrix (0-255).

## Examples

``` r
# RGB primaries as a 3x3 matrix (rows = colors, columns = R, G, B)
rgb_mat <- matrix(c(
  255,   0,   0,
    0, 255,   0,
    0,   0, 255
), ncol = 3, byrow = TRUE)

# Convert to HSV and back
hsv_mat  <- colorspace_from_rgb(rgb_mat, to = "HSV")
rgb_back <- colorspace_to_rgb(hsv_mat, from = "HSV")
print(rgb_back)
#>      red green blue
#> [1,] 255     0    0
#> [2,]   0   255    0
#> [3,]   0     0  255

# Convert to HCL (perceptually uniform)
hcl_mat <- colorspace_from_rgb(rgb_mat, to = "HCL")
print(hcl_mat)
#>              H        C        L
#> [1,]  12.16409 179.3924 53.48418
#> [2,] 128.02415 135.9173 87.63799
#> [3,] 265.75631 130.5509 32.24075
```
