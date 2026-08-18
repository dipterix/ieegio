# Read or write surface mesh data in `'VTK'` format

Reads and writes the polygon (`'Polys'`) part of a `'vtkPolyData'`
object. Like
[`io_read_vtk_streamlines`](http://dipterix.org/ieegio/reference/io-vtk-streamlines.md),
this reader uses 'Python' `'vtk'` package, and supports `'.vtk'`,
`'.vtp'`, `'.pvtp'`, `'.vtu'`, `'.vtpb'` formats.

## Usage

``` r
io_read_vtk_polys(file, name = basename(file), transform = NULL)

io_write_vtk_polys(x, con, binary = TRUE, transform = 1)
```

## Arguments

- file, con:

  file path to the `'VTK'` file, the format will be inferred from the
  file extension (with default `'.vtk'`)

- name:

  name of the geometry; default is the file name

- transform:

  for `io_read_vtk_polys`, an optional 4 by 4 matrix indicating the
  vertex position to scanner `'RAS'` transform; default is `NULL`
  (identity matrix), i.e. the vertex positions are used as-is. For
  `io_write_vtk_polys`, since `'VTK'` files cannot store any coordinate
  system information, this is the transform applied to the vertex
  positions before writing; the value is either a 4 by 4 matrix, or the
  index or name of the transform stored in the surface object (default
  is `1`, the first transform); use `NULL` to write the vertex positions
  as-is

- x:

  an
  [`imaging-surface`](http://dipterix.org/ieegio/reference/imaging-surface.md)
  object, or anything that can be converted via
  [`as_ieegio_surface`](http://dipterix.org/ieegio/reference/as_ieegio_surface.md)

- binary:

  for legacy `'.vtk'` file only, whether to store the data as binary
  file or 'ASCII' plain text; default is true (binary).

## Value

`io_read_vtk_polys` returns an
[`imaging-surface`](http://dipterix.org/ieegio/reference/imaging-surface.md)
object, while `io_write_vtk_polys` writes the data to file and returns
the file path.

## Details

`'VTK'` allows polygons with arbitrary number of nodes as well as
triangle strips, while `'ieegio'` surface objects only support
triangular faces. Therefore the mesh is triangulated automatically
whenever needed.

`'VTK'` files cannot store the vertex position to scanner `'RAS'`
transform, therefore `io_write_vtk_polys` applies the transform to the
vertex positions before writing, and `io_read_vtk_polys` reads the
vertex positions as-is (with an identity transform).

Vertex-wise attributes stored as point data are imported as well: arrays
with one component become `measurements`, and unsigned character arrays
with three or four components (i.e. `'RGB'` or `'RGBA'` colors) become
the vertex `color`. Other arrays, such as vertex normals, and cell
(face-wise) data are ignored.

## Examples

``` r

# This example shows how to convert a `GIfTI` surface to `VTK`

# run `ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii")`
# to download sample data

geom_file <- "gifti/GzipBase64/sujet01_Lwhite.surf.gii"

if( requireNamespace("rpymat", quietly = TRUE) &&
    dir.exists(rpymat::env_path()) &&
    ieegio_sample_data(geom_file, test = TRUE) ) {

  surface <- read_surface(ieegio_sample_data(geom_file))

  # write to vtk
  tfile <- tempfile(fileext = ".vtk")
  io_write_vtk_polys(surface, con = tfile)

  # read
  vtk_surface <- io_read_vtk_polys(tfile)

  print(vtk_surface)

  # 0 0
  range(surface$geometry$faces - vtk_surface$geometry$faces)

  # the vertex positions are written in the transformed space
  # 0 0
  range(
    surface$geometry$transforms[[1]] %*% surface$geometry$vertices -
      vtk_surface$geometry$vertices
  )

  unlink(tfile)

}

```
