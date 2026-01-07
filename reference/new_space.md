# Define a coordinate space

Creates an object representing a coordinate space/reference frame used
in medical imaging. The orientation defines the anatomical meaning of
the coordinate axes.

## Usage

``` r
new_space(name = "", orientation = ORIENTATION_CODES, dimension = 3, ...)
```

## Arguments

- name:

  character string identifying the coordinate space (e.g., `"T1w"`,
  `"MNI152NLin2009cAsym"`, `"scanner"`); default is `""`, a wildcard
  that indicates arbitrary space

- orientation:

  character string specifying the axis orientation convention. Common
  orientations in brain imaging:

  - `"RAS"`: Right-Anterior-Superior (FreeSurfer, NIfTI default)

  - `"LAS"`: Left-Anterior-Superior

  - `"LPS"`: Left-Posterior-Superior (`DICOM`, `ANTs`, `ITK`)

  - `"RPS"`: Right-Posterior-Superior

  - `"LPI"`: Left-Posterior-Inferior

  - `"RPI"`: Right-Posterior-Inferior

  - `"LAI"`: Left-Anterior-Inferior

  - `"RAI"`: Right-Anterior-Inferior

- dimension:

  integer dimension of the space (typically 3 for 3D imaging)

- ...:

  additional attributes to attach to the space object

## Value

An S3 object of class `"ieegio_space"` with attributes `orientation` and
`dimension`

## Details

Orientation codes use three letters to define the positive direction of
the x, y, and z axes respectively:

- First letter (x-axis): **L**eft or **R**ight

- Second letter (y-axis): **A**nterior or **P**osterior

- Third letter (z-axis): **S**uperior or **I**nferior

For example, `"RAS"` means: +x points Right, +y points Anterior (toward
face), +z points Superior (toward top of head).

## Examples

``` r
# FreeSurfer/NIfTI convention
scanner_space <- new_space("scanner", orientation = "RAS")
print(scanner_space)
#> scanner (RAS)

# DICOM/ANTs convention
mni_space <- new_space("MNI152NLin2009cAsym", orientation = "LPS", dimension = 3)
format(mni_space)
#> [1] "MNI152NLin2009cAsym (LPS)"
```
