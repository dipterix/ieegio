# Read a color map file

Read a color map or color lookup table from a file. The format is
auto-detected from the file extension and content.

## Usage

``` r
read_colormap(file, type = "auto", ...)
```

## Arguments

- file:

  path to the color map file

- type:

  one of `"auto"` (default), `"fs_lut"`, `"fs_annot"`, `"afni"`,
  `"fsl"`, `"threebrain"`

- ...:

  additional arguments passed to the format-specific reader

## Value

An `ieegio_colormap` or `ieegio_colortable` object.

## Examples

``` r
# Write a colormap to a temp file and read it back
cm <- as_ieegio_colormap(as_ieegio_colortable(data.frame(
  Key = 1:3,
  R = c(255L, 0L, 0L), G = c(0L, 200L, 0L),
  B = c(0L, 0L, 180L), A = c(255L, 255L, 255L)
)))
tmp <- tempfile(fileext = ".json")
write_colormap(cm, tmp)
cm2 <- read_colormap(tmp)
print(cm2)
#> <ieegio Colormap [discrete]>
#>   Colorspace   : RGB
#>   Color stops  : 4  (keys 0 to 3)
#>   Lookup       : 4 labels
unlink(tmp)
```
