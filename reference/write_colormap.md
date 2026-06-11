# Write a color map to file

Write an `ieegio_colormap` or `ieegio_colortable` to disk. The default
format is threeBrain/RAVE JSON, which preserves all information and is
readable by both ieegio and threeBrain.

## Usage

``` r
write_colormap(
  x,
  con,
  format = "threebrain",
  gtype = c("auto", "surface", "volume"),
  ...
)
```

## Arguments

- x:

  an `ieegio_colormap` or `ieegio_colortable`

- con:

  file path to write

- format:

  output format; currently only `"threebrain"` (default) and `"fs_lut"`
  are supported

- gtype:

  geometry type for the threeBrain format: `"auto"` (default; discrete
  maps use `"volume"`, continuous maps use `"surface"`), `"surface"`, or
  `"volume"`; ignored for other formats

- ...:

  additional arguments

## Examples

``` r
# Build a discrete colormap with region labels
ct <- as_ieegio_colortable(data.frame(
  Key = 1:3,
  R = c(255L, 0L, 0L), G = c(0L, 200L, 0L),
  B = c(0L, 0L, 180L), A = c(255L, 255L, 255L)
))
lut <- as_ieegio_lookup(data.frame(
  Key   = 1:3,
  Label = c("Cortex", "White matter", "CSF")
))
cm <- as_ieegio_colormap(ct, lookup = lut)

# Write as threeBrain JSON (default)
tmp_json <- tempfile(fileext = ".json")
write_colormap(cm, tmp_json)

# Write as FreeSurfer LUT
tmp_lut <- tempfile(fileext = ".txt")
write_colormap(cm, tmp_lut, format = "fs_lut")

unlink(c(tmp_json, tmp_lut))
```
