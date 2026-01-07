# Convert to ieegio transform

Generic function to convert various objects into `ieegio_transforms`.

## Usage

``` r
as_ieegio_transform(x, ...)

# S3 method for class 'character'
as_ieegio_transform(x, format = c("ants"), ...)

# S3 method for class 'matrix'
as_ieegio_transform(x, space_from = "", space_to = "", ...)

# S3 method for class 'array'
as_ieegio_transform(x, space_from = "", space_to = "", ...)

# S3 method for class 'list'
as_ieegio_transform(x, ...)

# S3 method for class 'ieegio_transforms'
as_ieegio_transform(x, ...)
```

## Arguments

- x:

  object to convert (character path, matrix, array, list, or existing
  transform)

- ...:

  additional arguments passed to methods

- format:

  character string specifying the file format for character paths.
  Currently supports `"ants"`. Only used for character method.

- space_from:

  source space for matrix/array methods. Default `""` is a wildcard for
  arbitrary space name.

- space_to:

  target space for matrix/array methods. Default `""` is a wildcard for
  arbitrary space name.

## Value

An `ieegio_transforms` object

## Details

Methods available:

- `character`: Reads transform from file (uses `io_read_ants_transform`)

- `matrix`: Creates transform from matrix

- `array`: Creates transform from 2D array

- `list`: Creates transform chain from list of transforms

- `ieegio_transforms`: Returns input unchanged
