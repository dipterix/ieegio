# Convert other surface formats to `ieegio` surface

Convert other surface formats to `ieegio` surface

## Usage

``` r
as_ieegio_surface(x, ...)

# Default S3 method
as_ieegio_surface(
  x,
  vertices = x,
  faces = NULL,
  face_start = NA,
  transform = NULL,
  vertex_colors = NULL,
  annotation_labels = NULL,
  annotation_values = NULL,
  measurements = NULL,
  time_series_slice_duration = NULL,
  time_series_value = NULL,
  name = NULL,
  ...
)

# S3 method for class 'character'
as_ieegio_surface(x, ...)

# S3 method for class 'ieegio_surface'
as_ieegio_surface(x, ...)

# S3 method for class 'mesh3d'
as_ieegio_surface(x, ...)

# S3 method for class 'fs.surface'
as_ieegio_surface(x, ...)
```

## Arguments

- x:

  R object or file path

- ...:

  passed to default method

- vertices:

  `n` by 3 matrix, each row is a vertex node position

- faces:

  (optional) face index, either zero or one-indexed (`Matlab` and `R`
  start counting from 1 while `C` and `Python` start indices from 0);
  one-index face order is recommended

- face_start:

  (optional) either 0 or 1, indicating whether `faces` is zero or
  one-indexed; default is `NA`, which will check whether the minimum
  value of `faces` is 0. If so, then `faces` will be bumped by 1
  internally

- transform:

  (optional) a 4 by 4 matrix indicating the vertex position to scanner
  `RAS` transform. Default is missing (identity matrix), i.e. the vertex
  positions are already in the scanner `RAS` coordinate system.

- vertex_colors:

  (optional) integer or color (hex) vector indicating the vertex colors

- annotation_labels:

  (optional) a data frame containing at the following columns. Though
  optional, `annotation_labels` must be provided when
  `annotation_values` is provided

  `"Key"`

  :   unique integers to appear in `annotation_values`, indicating the
      key of the annotation label

  `"Label"`

  :   a character vector (strings) of human-readable labels of the
      corresponding key

  `"Color"`

  :   hex string indicating the color of the key/label

- annotation_values:

  (optional) an integer table where each column is a vector of
  annotation key (for example, 'FreeSurfer' segmentation key) and each
  row corresponds to a vertex node

- measurements:

  (optional) a numeric table where each column represents a variable
  (for example, curvature) and each row corresponds to a vertex node.
  Unlike annotations, which is for discrete node values, `measurements`
  is for continuous values

- time_series_slice_duration:

  (optional) a numeric vector indicating the duration of each slice;
  default is `NA`

- time_series_value:

  (optional) a numeric matrix (`n` by `m`) where `n` is the number of
  vertices and `m` is the number of time points, hence each column is a
  time slice and each row is a vertex node.

- name:

  (optional) name of the geometry

## Value

An `ieeg_surface` object; see
[`read_surface`](http://dipterix.org/ieegio/reference/imaging-surface.md)
or 'Examples'.

## Examples

``` r


# ---- Simple usage
# vertices only
dodecahedron_vert <- matrix(
  ncol = 3, byrow = TRUE,
  c(-0.62, -0.62, -0.62, 0.62, -0.62, -0.62, -0.62, 0.62, -0.62,
    0.62, 0.62, -0.62, -0.62, -0.62, 0.62, 0.62, -0.62, 0.62,
    -0.62, 0.62, 0.62, 0.62, 0.62, 0.62, 0.00, -0.38, 1.00,
    0.00, 0.38, 1.00, 0.00, -0.38, -1.00, 0.00, 0.38, -1.00,
    -0.38, 1.00, 0.00, 0.38, 1.00, 0.00, -0.38, -1.00, 0.00,
    0.38, -1.00, 0.00, 1.00, 0.00, -0.38, 1.00, 0.00, 0.38,
    -1.00, 0.00, -0.38, -1.00, 0.00, 0.38)
)

point_cloud <- as_ieegio_surface(dodecahedron_vert)
plot(point_cloud, col = "red")

{"x":{"data3js":{"ticks":[{},{},{}],"ID":"z01q0ouuj6cl1l9y01qj","scene":{"background":{"r":[1],"g":[1],"b":[1],"a":[1]},"rotation":[0,0,0],"zoom":3,"translation":[0,0,0]},"lims":[[-1,1],[-1,1],[-1,1]],"aspect":[1,1,1],"light":[{"type":["light"],"lighttype":"ambient","intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[1,0,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[-1,0,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,1,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,-1,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,0,1],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,0,-1],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}}],"lastID":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],"plot":[{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[1],"g":[0],"b":[0],"a":[1]},"properties":{"mat":"phong","color":{"r":[1],"g":[0],"b":[0],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[-0.62,-0.62,-0.62],"ID":[1]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[1],"g":[0],"b":[0],"a":[1]},"properties":{"mat":"phong","color":{"r":[1],"g":[0],"b":[0],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[0.62,-0.62,-0.62],"ID":[2]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[1],"g":[0],"b":[0],"a":[1]},"properties":{"mat":"phong","color":{"r":[1],"g":[0],"b":[0],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[-0.62,0.62,-0.62],"ID":[3]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[1],"g":[0],"b":[0],"a":[1]},"properties":{"mat":"phong","color":{"r":[1],"g":[0],"b":[0],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[0.62,0.62,-0.62],"ID":[4]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[1],"g":[0],"b":[0],"a":[1]},"properties":{"mat":"phong","color":{"r":[1],"g":[0],"b":[0],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[-0.62,-0.62,0.62],"ID":[5]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[1],"g":[0],"b":[0],"a":[1]},"properties":{"mat":"phong","color":{"r":[1],"g":[0],"b":[0],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[0.62,-0.62,0.62],"ID":[6]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[1],"g":[0],"b":[0],"a":[1]},"properties":{"mat":"phong","color":{"r":[1],"g":[0],"b":[0],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[-0.62,0.62,0.62],"ID":[7]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[1],"g":[0],"b":[0],"a":[1]},"properties":{"mat":"phong","color":{"r":[1],"g":[0],"b":[0],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[0.62,0.62,0.62],"ID":[8]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[1],"g":[0],"b":[0],"a":[1]},"properties":{"mat":"phong","color":{"r":[1],"g":[0],"b":[0],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[0,-0.38,1],"ID":[9]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[1],"g":[0],"b":[0],"a":[1]},"properties":{"mat":"phong","color":{"r":[1],"g":[0],"b":[0],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[0,0.38,1],"ID":[10]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[1],"g":[0],"b":[0],"a":[1]},"properties":{"mat":"phong","color":{"r":[1],"g":[0],"b":[0],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[0,-0.38,-1],"ID":[11]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[1],"g":[0],"b":[0],"a":[1]},"properties":{"mat":"phong","color":{"r":[1],"g":[0],"b":[0],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[0,0.38,-1],"ID":[12]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[1],"g":[0],"b":[0],"a":[1]},"properties":{"mat":"phong","color":{"r":[1],"g":[0],"b":[0],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[-0.38,1,0],"ID":[13]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[1],"g":[0],"b":[0],"a":[1]},"properties":{"mat":"phong","color":{"r":[1],"g":[0],"b":[0],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[0.38,1,0],"ID":[14]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[1],"g":[0],"b":[0],"a":[1]},"properties":{"mat":"phong","color":{"r":[1],"g":[0],"b":[0],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[-0.38,-1,0],"ID":[15]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[1],"g":[0],"b":[0],"a":[1]},"properties":{"mat":"phong","color":{"r":[1],"g":[0],"b":[0],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[0.38,-1,0],"ID":[16]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[1],"g":[0],"b":[0],"a":[1]},"properties":{"mat":"phong","color":{"r":[1],"g":[0],"b":[0],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[1,0,-0.38],"ID":[17]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[1],"g":[0],"b":[0],"a":[1]},"properties":{"mat":"phong","color":{"r":[1],"g":[0],"b":[0],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[1,0,0.38],"ID":[18]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[1],"g":[0],"b":[0],"a":[1]},"properties":{"mat":"phong","color":{"r":[1],"g":[0],"b":[0],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[-1,0,-0.38],"ID":[19]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[1],"g":[0],"b":[0],"a":[1]},"properties":{"mat":"phong","color":{"r":[1],"g":[0],"b":[0],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[-1,0,0.38],"ID":[20]}]},"settings":{"styles":[],"title":"","ID":"z01q0ouuj6cl1l9y01qj"}},"evals":[],"jsHooks":[]}
# with face index
dodecahedron_face <- matrix(
  ncol = 3L, byrow = TRUE,
  c(1, 11, 2, 1, 2, 16, 1, 16, 15, 1, 15, 5, 1, 5, 20, 1, 20, 19,
    1, 19, 3, 1, 3, 12, 1, 12, 11, 2, 11, 12, 2, 12, 4, 2, 4, 17,
    2, 17, 18, 2, 18, 6, 2, 6, 16, 3, 13, 14, 3, 14, 4, 3, 4, 12,
    3, 19, 20, 3, 20, 7, 3, 7, 13, 4, 14, 8, 4, 8, 18, 4, 18, 17,
    5, 9, 10, 5, 10, 7, 5, 7, 20, 5, 15, 16, 5, 16, 6, 5, 6, 9,
    6, 18, 8, 6, 8, 10, 6, 10, 9, 7, 10, 8, 7, 8, 14, 7, 14, 13)
)
mesh <- as_ieegio_surface(dodecahedron_vert,
                          faces = dodecahedron_face)
plot(mesh)

{"x":{"data3js":{"ticks":[{},{},{}],"ID":"2tacy97z8lf0w5bsb1f2","scene":{"background":{"r":[1],"g":[1],"b":[1],"a":[1]},"rotation":[0,0,0],"zoom":3,"translation":[0,0,0]},"lims":[[-1,1],[-1,1],[-1,1]],"aspect":[1,1,1],"light":[{"type":["light"],"lighttype":"ambient","intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[1,0,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[-1,0,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,1,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,-1,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,0,1],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,0,-1],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}}],"lastID":[1],"plot":[{"type":["shape"],"vertices":[[-0.62,-0.62,-0.62],[0.62,-0.62,-0.62],[-0.62,0.62,-0.62],[0.62,0.62,-0.62],[-0.62,-0.62,0.62],[0.62,-0.62,0.62],[-0.62,0.62,0.62],[0.62,0.62,0.62],[0,-0.38,1],[0,0.38,1],[0,-0.38,-1],[0,0.38,-1],[-0.38,1,0],[0.38,1,0],[-0.38,-1,0],[0.38,-1,0],[1,0,-0.38],[1,0,0.38],[-1,0,-0.38],[-1,0,0.38]],"faces":[[0,10,1],[0,1,15],[0,15,14],[0,14,4],[0,4,19],[0,19,18],[0,18,2],[0,2,11],[0,11,10],[1,10,11],[1,11,3],[1,3,16],[1,16,17],[1,17,5],[1,5,15],[2,12,13],[2,13,3],[2,3,11],[2,18,19],[2,19,6],[2,6,12],[3,13,7],[3,7,17],[3,17,16],[4,8,9],[4,9,6],[4,6,19],[4,14,15],[4,15,5],[4,5,8],[5,17,7],[5,7,9],[5,9,8],[6,9,7],[6,7,13],[6,13,12]],"properties":{"mat":"phong","color":{"r":[1],"g":[1],"b":[1],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"ID":[1]}]},"settings":{"styles":[],"title":"","ID":"2tacy97z8lf0w5bsb1f2"}},"evals":[],"jsHooks":[]}
# with vertex colors
mesh <- as_ieegio_surface(dodecahedron_vert,
                          faces = dodecahedron_face,
                          vertex_colors = sample(20))
plot(mesh, name = "color")

{"x":{"data3js":{"ticks":[{},{},{}],"ID":"ps3zlkapx7k1fvy7v2br","scene":{"background":{"r":[1],"g":[1],"b":[1],"a":[1]},"rotation":[0,0,0],"zoom":3,"translation":[0,0,0]},"lims":[[-1,1],[-1,1],[-1,1]],"aspect":[1,1,1],"light":[{"type":["light"],"lighttype":"ambient","intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[1,0,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[-1,0,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,1,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,-1,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,0,1],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,0,-1],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}}],"lastID":[1],"plot":[{"type":["shape"],"vertices":[[-0.62,-0.62,-0.62],[0.62,-0.62,-0.62],[-0.62,0.62,-0.62],[0.62,0.62,-0.62],[-0.62,-0.62,0.62],[0.62,-0.62,0.62],[-0.62,0.62,0.62],[0.62,0.62,0.62],[0,-0.38,1],[0,0.38,1],[0,-0.38,-1],[0,0.38,-1],[-0.38,1,0],[0.38,1,0],[-0.38,-1,0],[0.38,-1,0],[1,0,-0.38],[1,0,0.38],[-1,0,-0.38],[-1,0,0.38]],"faces":[[0,10,1],[0,1,15],[0,15,14],[0,14,4],[0,4,19],[0,19,18],[0,18,2],[0,2,11],[0,11,10],[1,10,11],[1,11,3],[1,3,16],[1,16,17],[1,17,5],[1,5,15],[2,12,13],[2,13,3],[2,3,11],[2,18,19],[2,19,6],[2,6,12],[3,13,7],[3,7,17],[3,17,16],[4,8,9],[4,9,6],[4,6,19],[4,14,15],[4,15,5],[4,5,8],[5,17,7],[5,7,9],[5,9,8],[6,9,7],[6,7,13],[6,13,12]],"properties":{"mat":"phong","color":{"r":[0.6196,0.1333,0.3804,0.9608,0,0,0,0.3804,0.1333,0.8745,0.9608,0.6196,0.3804,0.8039,0.8745,0.8745,0.8039,0.1333,0.1569,0.1569],"g":[0.6196,0.5922,0.8157,0.7804,0,0,0,0.8157,0.5922,0.3255,0.7804,0.6196,0.8157,0.0431,0.3255,0.3255,0.0431,0.5922,0.8863,0.8863],"b":[0.6196,0.902,0.3098,0.0627,0,0,0,0.3098,0.902,0.4196,0.0627,0.6196,0.3098,0.7373,0.4196,0.4196,0.7373,0.902,0.898,0.898],"a":[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"ID":[1]}]},"settings":{"styles":[],"title":"","ID":"ps3zlkapx7k1fvy7v2br"}},"evals":[],"jsHooks":[]}
# with annotations
mesh <- as_ieegio_surface(
  dodecahedron_vert,
  faces = dodecahedron_face,
  annotation_labels = data.frame(
    Key = 1:3,
    Label = c("A", "B", "C"),
    Color = c("red", "green", "blue")
  ),
  annotation_values = data.frame(
    MyVariable = c(rep(1, 7), rep(2, 7), rep(3, 6))
  )
)
plot(mesh, name = "annotations")

{"x":{"data3js":{"ticks":[{},{},{}],"ID":"jzm4mvascsqrllu1ddwg","scene":{"background":{"r":[1],"g":[1],"b":[1],"a":[1]},"rotation":[0,0,0],"zoom":3,"translation":[0,0,0]},"lims":[[-1,1],[-1,1],[-1,1]],"aspect":[1,1,1],"light":[{"type":["light"],"lighttype":"ambient","intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[1,0,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[-1,0,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,1,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,-1,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,0,1],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,0,-1],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}}],"lastID":[1],"plot":[{"type":["shape"],"vertices":[[-0.62,-0.62,-0.62],[0.62,-0.62,-0.62],[-0.62,0.62,-0.62],[0.62,0.62,-0.62],[-0.62,-0.62,0.62],[0.62,-0.62,0.62],[-0.62,0.62,0.62],[0.62,0.62,0.62],[0,-0.38,1],[0,0.38,1],[0,-0.38,-1],[0,0.38,-1],[-0.38,1,0],[0.38,1,0],[-0.38,-1,0],[0.38,-1,0],[1,0,-0.38],[1,0,0.38],[-1,0,-0.38],[-1,0,0.38]],"faces":[[0,10,1],[0,1,15],[0,15,14],[0,14,4],[0,4,19],[0,19,18],[0,18,2],[0,2,11],[0,11,10],[1,10,11],[1,11,3],[1,3,16],[1,16,17],[1,17,5],[1,5,15],[2,12,13],[2,13,3],[2,3,11],[2,18,19],[2,19,6],[2,6,12],[3,13,7],[3,7,17],[3,17,16],[4,8,9],[4,9,6],[4,6,19],[4,14,15],[4,15,5],[4,5,8],[5,17,7],[5,7,9],[5,9,8],[6,9,7],[6,7,13],[6,13,12]],"properties":{"mat":"phong","color":{"r":[1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0],"g":[0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0],"b":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1],"a":[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"ID":[1]}]},"settings":{"styles":[],"title":"Annotation: MyVariable","ID":"jzm4mvascsqrllu1ddwg"}},"evals":[],"jsHooks":[]}
# with measurements
mesh <- as_ieegio_surface(
  dodecahedron_vert,
  faces = dodecahedron_face,
  measurements = data.frame(
    MyVariable = dodecahedron_vert[, 1]
  )
)
plot(mesh, name = "measurements",
     col = c("blue", "gray", "red"))

{"x":{"data3js":{"ticks":[{},{},{}],"ID":"xhmf4fy394wmhhlr59mx","scene":{"background":{"r":[1],"g":[1],"b":[1],"a":[1]},"rotation":[0,0,0],"zoom":3,"translation":[0,0,0]},"lims":[[-1,1],[-1,1],[-1,1]],"aspect":[1,1,1],"light":[{"type":["light"],"lighttype":"ambient","intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[1,0,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[-1,0,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,1,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,-1,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,0,1],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,0,-1],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}}],"lastID":[1],"plot":[{"type":["shape"],"vertices":[[-0.62,-0.62,-0.62],[0.62,-0.62,-0.62],[-0.62,0.62,-0.62],[0.62,0.62,-0.62],[-0.62,-0.62,0.62],[0.62,-0.62,0.62],[-0.62,0.62,0.62],[0.62,0.62,0.62],[0,-0.38,1],[0,0.38,1],[0,-0.38,-1],[0,0.38,-1],[-0.38,1,0],[0.38,1,0],[-0.38,-1,0],[0.38,-1,0],[1,0,-0.38],[1,0,0.38],[-1,0,-0.38],[-1,0,0.38]],"faces":[[0,10,1],[0,1,15],[0,15,14],[0,14,4],[0,4,19],[0,19,18],[0,18,2],[0,2,11],[0,11,10],[1,10,11],[1,11,3],[1,3,16],[1,16,17],[1,17,5],[1,5,15],[2,12,13],[2,13,3],[2,3,11],[2,18,19],[2,19,6],[2,6,12],[3,13,7],[3,7,17],[3,17,16],[4,8,9],[4,9,6],[4,6,19],[4,14,15],[4,15,5],[4,5,8],[5,17,7],[5,7,9],[5,9,8],[6,9,7],[6,7,13],[6,13,12]],"properties":{"mat":"phong","color":{"r":[0.2784,0.902,0.2784,0.902,0.2784,0.902,0.2784,0.902,0.7412,0.7412,0.7412,0.7412,0.4588,0.8392,0.4588,0.8392,1,1,0,0],"g":[0.2784,0.2784,0.2784,0.2784,0.2784,0.2784,0.2784,0.2784,0.7412,0.7412,0.7412,0.7412,0.4588,0.4588,0.4588,0.4588,0,0,0,0],"b":[0.902,0.2784,0.902,0.2784,0.902,0.2784,0.902,0.2784,0.7451,0.7451,0.7451,0.7451,0.8392,0.4588,0.8392,0.4588,0,0,1,1],"a":[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"ID":[1]}]},"settings":{"styles":[],"title":"Measurement: MyVariable","ID":"xhmf4fy394wmhhlr59mx"}},"evals":[],"jsHooks":[]}

```
