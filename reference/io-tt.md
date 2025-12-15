# Read `'TT'` streamline file

Writer is not implemented yet. Please save as a `'TCK'` file.

## Usage

``` r
io_read_tt(file)
```

## Arguments

- file:

  path to the streamline file

## Value

An
[`imaging-streamlines`](http://dipterix.org/ieegio/reference/imaging-streamlines.md)
instance.

## Examples

``` r

# This example uses sample data, run
# `ieegio_sample_data("streamlines/CNVII_R.trk")` to download

if( ieegio_sample_data("streamlines/CNVII_R.tt.gz", test = TRUE) ) {

  path <- ieegio_sample_data("streamlines/CNVII_R.tt.gz")

  # read
  x <- io_read_tt(path)

  plot(x)

}
#> Using native approach to load the matlab file (supporting MAT 5.0)...

```
