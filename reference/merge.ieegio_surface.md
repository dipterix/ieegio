# Merge two `'ieegio'` surfaces

Either merge surface objects by attributes or merge geometries

## Usage

``` r
# S3 method for class 'ieegio_surface'
merge(
  x,
  y,
  ...,
  merge_type = c("attribute", "geometry"),
  merge_space = c("model", "world"),
  transform_index = 1,
  verbose = TRUE
)
```

## Arguments

- x, y, ...:

  `'ieegio'` surface objects, see
  [`as_ieegio_surface`](http://dipterix.org/ieegio/reference/as_ieegio_surface.md)
  or
  [`read_surface`](http://dipterix.org/ieegio/reference/imaging-surface.md).
  Object `x` must contain geometry information.

- merge_type:

  type of merge:

  `"attribute"`

  :   merge `y,...` into x by attributes such as color, measurements,
      annotations, or time-series data, assuming `x,y,...` all refer to
      the same geometry, hence the underlying number of vertices should
      be the same.

  `"geometry"`

  :   merge `y,...` into x by geometry; this requires the surfaces to
      merge have geometries and cannot be only surface attributes. Two
      mesh objects will be merged into one, and face index will be
      re-calculated. The merge happens in transformed space, Notice the
      attributes will be ignored and eventually discarded during merge.

- merge_space:

  space to merge the geometries; only used when `merge_type` is
  `"geometry"`. Default is to directly merge the surfaces in `"model"`
  space, i.e. assuming the surfaces share the same transform;
  alternatively, if the model to world transforms are different, users
  can choose to merge in `"world"` space, then all the surfaces will be
  transformed into world space and mapped back to the model space in `x`

- transform_index:

  which local-to-world transform to use when merging geometries in the
  world space; default is the first transform for each surface object.
  The transform list can be obtained from `surface$geometry$transforms`
  and `transform_index` indicates the index of the transform matrices.
  The length of `transform_index` can be either 1 (same for all
  surfaces) or the length of all the surfaces, (i.e. length of
  `list(x,y,...)`), when the index needs to be set for each surface
  respectively. If any index is set to `NA`, then it means no transform
  is to be applied and that surface will be merged assuming its model
  space is the world space.

- verbose:

  whether to verbose the messages

## Value

A merged surface object

## Examples

``` r

# Construct example geometry
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

dodecahedron_face <- matrix(
  ncol = 3L, byrow = TRUE,
  c(1, 11, 2, 1, 2, 16, 1, 16, 15, 1, 15, 5, 1, 5, 20, 1, 20, 19,
    1, 19, 3, 1, 3, 12, 1, 12, 11, 2, 11, 12, 2, 12, 4, 2, 4, 17,
    2, 17, 18, 2, 18, 6, 2, 6, 16, 3, 13, 14, 3, 14, 4, 3, 4, 12,
    3, 19, 20, 3, 20, 7, 3, 7, 13, 4, 14, 8, 4, 8, 18, 4, 18, 17,
    5, 9, 10, 5, 10, 7, 5, 7, 20, 5, 15, 16, 5, 16, 6, 5, 6, 9,
    6, 18, 8, 6, 8, 10, 6, 10, 9, 7, 10, 8, 7, 8, 14, 7, 14, 13)
)

x0 <- as_ieegio_surface(dodecahedron_vert, faces = dodecahedron_face)

plot(x0)

{"x":{"data3js":{"ticks":[{},{},{}],"ID":"v0gn2a4feggyp6rn0w3a","scene":{"background":{"r":[1],"g":[1],"b":[1],"a":[1]},"rotation":[0,0,0],"zoom":3,"translation":[0,0,0]},"lims":[[-1,1],[-1,1],[-1,1]],"aspect":[1,1,1],"light":[{"type":["light"],"lighttype":"ambient","intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[1,0,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[-1,0,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,1,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,-1,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,0,1],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,0,-1],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}}],"lastID":[1],"plot":[{"type":["shape"],"vertices":[[-0.62,-0.62,-0.62],[0.62,-0.62,-0.62],[-0.62,0.62,-0.62],[0.62,0.62,-0.62],[-0.62,-0.62,0.62],[0.62,-0.62,0.62],[-0.62,0.62,0.62],[0.62,0.62,0.62],[0,-0.38,1],[0,0.38,1],[0,-0.38,-1],[0,0.38,-1],[-0.38,1,0],[0.38,1,0],[-0.38,-1,0],[0.38,-1,0],[1,0,-0.38],[1,0,0.38],[-1,0,-0.38],[-1,0,0.38]],"faces":[[0,10,1],[0,1,15],[0,15,14],[0,14,4],[0,4,19],[0,19,18],[0,18,2],[0,2,11],[0,11,10],[1,10,11],[1,11,3],[1,3,16],[1,16,17],[1,17,5],[1,5,15],[2,12,13],[2,13,3],[2,3,11],[2,18,19],[2,19,6],[2,6,12],[3,13,7],[3,7,17],[3,17,16],[4,8,9],[4,9,6],[4,6,19],[4,14,15],[4,15,5],[4,5,8],[5,17,7],[5,7,9],[5,9,8],[6,9,7],[6,7,13],[6,13,12]],"properties":{"mat":"phong","color":{"r":[1],"g":[1],"b":[1],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"ID":[1]}]},"settings":{"styles":[],"title":"","ID":"v0gn2a4feggyp6rn0w3a"}},"evals":[],"jsHooks":[]}

# ---- merge by attributes -----------------------------------

# point-cloud but with vertex measurements
y1 <- as_ieegio_surface(
  dodecahedron_vert,
  measurements = data.frame(MyVariable = dodecahedron_vert[, 1]),
  transform = diag(c(2,1,0.5,1))
)

plot(y1)

{"x":{"data3js":{"ticks":[{},{},{}],"ID":"88vjxad9h1vfx473cgw0","scene":{"background":{"r":[1],"g":[1],"b":[1],"a":[1]},"rotation":[0,0,0],"zoom":3,"translation":[0,0,0]},"lims":[[-2,2],[-1,1],[-0.5,0.5]],"aspect":[1,1,1],"light":[{"type":["light"],"lighttype":"ambient","intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[1,0,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[-1,0,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,1,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,-1,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,0,1],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,0,-1],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}}],"lastID":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],"plot":[{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[0.1882],"g":[0.1882],"b":[0.1882],"a":[1]},"properties":{"mat":"phong","color":{"r":[0.1882],"g":[0.1882],"b":[0.1882],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[-1.24,-0.62,-0.31],"ID":[1]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[0.8118],"g":[0.8118],"b":[0.8118],"a":[1]},"properties":{"mat":"phong","color":{"r":[0.8118],"g":[0.8118],"b":[0.8118],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[1.24,-0.62,-0.31],"ID":[2]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[0.1882],"g":[0.1882],"b":[0.1882],"a":[1]},"properties":{"mat":"phong","color":{"r":[0.1882],"g":[0.1882],"b":[0.1882],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[-1.24,0.62,-0.31],"ID":[3]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[0.8118],"g":[0.8118],"b":[0.8118],"a":[1]},"properties":{"mat":"phong","color":{"r":[0.8118],"g":[0.8118],"b":[0.8118],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[1.24,0.62,-0.31],"ID":[4]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[0.1882],"g":[0.1882],"b":[0.1882],"a":[1]},"properties":{"mat":"phong","color":{"r":[0.1882],"g":[0.1882],"b":[0.1882],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[-1.24,-0.62,0.31],"ID":[5]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[0.8118],"g":[0.8118],"b":[0.8118],"a":[1]},"properties":{"mat":"phong","color":{"r":[0.8118],"g":[0.8118],"b":[0.8118],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[1.24,-0.62,0.31],"ID":[6]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[0.1882],"g":[0.1882],"b":[0.1882],"a":[1]},"properties":{"mat":"phong","color":{"r":[0.1882],"g":[0.1882],"b":[0.1882],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[-1.24,0.62,0.31],"ID":[7]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[0.8118],"g":[0.8118],"b":[0.8118],"a":[1]},"properties":{"mat":"phong","color":{"r":[0.8118],"g":[0.8118],"b":[0.8118],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[1.24,0.62,0.31],"ID":[8]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[0.498],"g":[0.498],"b":[0.498],"a":[1]},"properties":{"mat":"phong","color":{"r":[0.498],"g":[0.498],"b":[0.498],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[0,-0.38,0.5],"ID":[9]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[0.498],"g":[0.498],"b":[0.498],"a":[1]},"properties":{"mat":"phong","color":{"r":[0.498],"g":[0.498],"b":[0.498],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[0,0.38,0.5],"ID":[10]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[0.498],"g":[0.498],"b":[0.498],"a":[1]},"properties":{"mat":"phong","color":{"r":[0.498],"g":[0.498],"b":[0.498],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[0,-0.38,-0.5],"ID":[11]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[0.498],"g":[0.498],"b":[0.498],"a":[1]},"properties":{"mat":"phong","color":{"r":[0.498],"g":[0.498],"b":[0.498],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[0,0.38,-0.5],"ID":[12]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[0.3098],"g":[0.3098],"b":[0.3098],"a":[1]},"properties":{"mat":"phong","color":{"r":[0.3098],"g":[0.3098],"b":[0.3098],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[-0.76,1,0],"ID":[13]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[0.6902],"g":[0.6902],"b":[0.6902],"a":[1]},"properties":{"mat":"phong","color":{"r":[0.6902],"g":[0.6902],"b":[0.6902],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[0.76,1,0],"ID":[14]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[0.3098],"g":[0.3098],"b":[0.3098],"a":[1]},"properties":{"mat":"phong","color":{"r":[0.3098],"g":[0.3098],"b":[0.3098],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[-0.76,-1,0],"ID":[15]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[0.6902],"g":[0.6902],"b":[0.6902],"a":[1]},"properties":{"mat":"phong","color":{"r":[0.6902],"g":[0.6902],"b":[0.6902],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[0.76,-1,0],"ID":[16]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[1],"g":[1],"b":[1],"a":[1]},"properties":{"mat":"phong","color":{"r":[1],"g":[1],"b":[1],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[2,0,-0.19],"ID":[17]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[1],"g":[1],"b":[1],"a":[1]},"properties":{"mat":"phong","color":{"r":[1],"g":[1],"b":[1],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[2,0,0.19],"ID":[18]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[0],"g":[0],"b":[0],"a":[1]},"properties":{"mat":"phong","color":{"r":[0],"g":[0],"b":[0],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[-2,0,-0.19],"ID":[19]},{"type":["point"],"shape":["sphere"],"size":[1],"fill":{"r":[0],"g":[0],"b":[0],"a":[1]},"properties":{"mat":"phong","color":{"r":[0],"g":[0],"b":[0],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"position":[-2,0,0.19],"ID":[20]}]},"settings":{"styles":[],"title":"Measurement: MyVariable","ID":"88vjxad9h1vfx473cgw0"}},"evals":[],"jsHooks":[]}
# the geometry of `y1` will be discarded and only attributes
# (in this case, measurements:MyVariable) will be merged to `x`

z1 <- merge(x0, y1, merge_type = "attribute")
#> Merging geometry attributes, assuming all the surface objects have the same number of vertices.

plot(z1)

{"x":{"data3js":{"ticks":[{},{},{}],"ID":"q96etsd3l30jmunot5b7","scene":{"background":{"r":[1],"g":[1],"b":[1],"a":[1]},"rotation":[0,0,0],"zoom":3,"translation":[0,0,0]},"lims":[[-1,1],[-1,1],[-1,1]],"aspect":[1,1,1],"light":[{"type":["light"],"lighttype":"ambient","intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[1,0,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[-1,0,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,1,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,-1,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,0,1],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,0,-1],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}}],"lastID":[1],"plot":[{"type":["shape"],"vertices":[[-0.62,-0.62,-0.62],[0.62,-0.62,-0.62],[-0.62,0.62,-0.62],[0.62,0.62,-0.62],[-0.62,-0.62,0.62],[0.62,-0.62,0.62],[-0.62,0.62,0.62],[0.62,0.62,0.62],[0,-0.38,1],[0,0.38,1],[0,-0.38,-1],[0,0.38,-1],[-0.38,1,0],[0.38,1,0],[-0.38,-1,0],[0.38,-1,0],[1,0,-0.38],[1,0,0.38],[-1,0,-0.38],[-1,0,0.38]],"faces":[[0,10,1],[0,1,15],[0,15,14],[0,14,4],[0,4,19],[0,19,18],[0,18,2],[0,2,11],[0,11,10],[1,10,11],[1,11,3],[1,3,16],[1,16,17],[1,17,5],[1,5,15],[2,12,13],[2,13,3],[2,3,11],[2,18,19],[2,19,6],[2,6,12],[3,13,7],[3,7,17],[3,17,16],[4,8,9],[4,9,6],[4,6,19],[4,14,15],[4,15,5],[4,5,8],[5,17,7],[5,7,9],[5,9,8],[6,9,7],[6,7,13],[6,13,12]],"properties":{"mat":"phong","color":{"r":[0.1882,0.8118,0.1882,0.8118,0.1882,0.8118,0.1882,0.8118,0.498,0.498,0.498,0.498,0.3098,0.6902,0.3098,0.6902,1,1,0,0],"g":[0.1882,0.8118,0.1882,0.8118,0.1882,0.8118,0.1882,0.8118,0.498,0.498,0.498,0.498,0.3098,0.6902,0.3098,0.6902,1,1,0,0],"b":[0.1882,0.8118,0.1882,0.8118,0.1882,0.8118,0.1882,0.8118,0.498,0.498,0.498,0.498,0.3098,0.6902,0.3098,0.6902,1,1,0,0],"a":[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"ID":[1]}]},"settings":{"styles":[],"title":"Measurement: MyVariable","ID":"q96etsd3l30jmunot5b7"}},"evals":[],"jsHooks":[]}
# ---- merge by geometry ----------------------------------------

y2 <- as_ieegio_surface(
  dodecahedron_vert + 4, faces = dodecahedron_face,
  transform = diag(c(2, 1, 0.5, 1))
)

plot(y2)

{"x":{"data3js":{"ticks":[{},{},{}],"ID":"kz3dlt41zrdte0flm2qo","scene":{"background":{"r":[1],"g":[1],"b":[1],"a":[1]},"rotation":[0,0,0],"zoom":3,"translation":[0,0,0]},"lims":[[6,10],[3,5],[1.5,2.5]],"aspect":[1,1,1],"light":[{"type":["light"],"lighttype":"ambient","intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[1,0,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[-1,0,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,1,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,-1,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,0,1],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,0,-1],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}}],"lastID":[1],"plot":[{"type":["shape"],"vertices":[[6.76,3.38,1.69],[9.24,3.38,1.69],[6.76,4.62,1.69],[9.24,4.62,1.69],[6.76,3.38,2.31],[9.24,3.38,2.31],[6.76,4.62,2.31],[9.24,4.62,2.31],[8,3.62,2.5],[8,4.38,2.5],[8,3.62,1.5],[8,4.38,1.5],[7.24,5,2],[8.76,5,2],[7.24,3,2],[8.76,3,2],[10,4,1.81],[10,4,2.19],[6,4,1.81],[6,4,2.19]],"faces":[[0,10,1],[0,1,15],[0,15,14],[0,14,4],[0,4,19],[0,19,18],[0,18,2],[0,2,11],[0,11,10],[1,10,11],[1,11,3],[1,3,16],[1,16,17],[1,17,5],[1,5,15],[2,12,13],[2,13,3],[2,3,11],[2,18,19],[2,19,6],[2,6,12],[3,13,7],[3,7,17],[3,17,16],[4,8,9],[4,9,6],[4,6,19],[4,14,15],[4,15,5],[4,5,8],[5,17,7],[5,7,9],[5,9,8],[6,9,7],[6,7,13],[6,13,12]],"properties":{"mat":"phong","color":{"r":[1],"g":[1],"b":[1],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"ID":[1]}]},"settings":{"styles":[],"title":"","ID":"kz3dlt41zrdte0flm2qo"}},"evals":[],"jsHooks":[]}
# merge directly in model space: transform matrix of `y2` will be ignored
z2 <- merge(x0, y2, merge_type = "geometry", merge_space = "model")
#> Merging geometries directly without checking transforms (assuming the transforms are the same)

plot(z2)

{"x":{"data3js":{"ticks":[{},{},{}],"ID":"7ehicbbhfyxpqfe6s3c6","scene":{"background":{"r":[1],"g":[1],"b":[1],"a":[1]},"rotation":[0,0,0],"zoom":3,"translation":[0,0,0]},"lims":[[-1,5],[-1,5],[-1,5]],"aspect":[1,1,1],"light":[{"type":["light"],"lighttype":"ambient","intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[1,0,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[-1,0,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,1,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,-1,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,0,1],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,0,-1],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}}],"lastID":[1],"plot":[{"type":["shape"],"vertices":[[-0.62,-0.62,-0.62],[0.62,-0.62,-0.62],[-0.62,0.62,-0.62],[0.62,0.62,-0.62],[-0.62,-0.62,0.62],[0.62,-0.62,0.62],[-0.62,0.62,0.62],[0.62,0.62,0.62],[0,-0.38,1],[0,0.38,1],[0,-0.38,-1],[0,0.38,-1],[-0.38,1,0],[0.38,1,0],[-0.38,-1,0],[0.38,-1,0],[1,0,-0.38],[1,0,0.38],[-1,0,-0.38],[-1,0,0.38],[3.38,3.38,3.38],[4.62,3.38,3.38],[3.38,4.62,3.38],[4.62,4.62,3.38],[3.38,3.38,4.62],[4.62,3.38,4.62],[3.38,4.62,4.62],[4.62,4.62,4.62],[4,3.62,5],[4,4.38,5],[4,3.62,3],[4,4.38,3],[3.62,5,4],[4.38,5,4],[3.62,3,4],[4.38,3,4],[5,4,3.62],[5,4,4.38],[3,4,3.62],[3,4,4.38]],"faces":[[0,10,1],[0,1,15],[0,15,14],[0,14,4],[0,4,19],[0,19,18],[0,18,2],[0,2,11],[0,11,10],[1,10,11],[1,11,3],[1,3,16],[1,16,17],[1,17,5],[1,5,15],[2,12,13],[2,13,3],[2,3,11],[2,18,19],[2,19,6],[2,6,12],[3,13,7],[3,7,17],[3,17,16],[4,8,9],[4,9,6],[4,6,19],[4,14,15],[4,15,5],[4,5,8],[5,17,7],[5,7,9],[5,9,8],[6,9,7],[6,7,13],[6,13,12],[20,30,21],[20,21,35],[20,35,34],[20,34,24],[20,24,39],[20,39,38],[20,38,22],[20,22,31],[20,31,30],[21,30,31],[21,31,23],[21,23,36],[21,36,37],[21,37,25],[21,25,35],[22,32,33],[22,33,23],[22,23,31],[22,38,39],[22,39,26],[22,26,32],[23,33,27],[23,27,37],[23,37,36],[24,28,29],[24,29,26],[24,26,39],[24,34,35],[24,35,25],[24,25,28],[25,37,27],[25,27,29],[25,29,28],[26,29,27],[26,27,33],[26,33,32]],"properties":{"mat":"phong","color":{"r":[1],"g":[1],"b":[1],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"ID":[1]}]},"settings":{"styles":[],"title":"","ID":"7ehicbbhfyxpqfe6s3c6"}},"evals":[],"jsHooks":[]}
# merge x, y2 in the world space where transforms will be respected
z3 <- merge(x0, y2, merge_type = "geometry", merge_space = "world")
#> Merging geometries in the transformed world space indicated by `transform_index` list.

plot(z3)

{"x":{"data3js":{"ticks":[{},{},{}],"ID":"mtac63ly4q5h37f30or4","scene":{"background":{"r":[1],"g":[1],"b":[1],"a":[1]},"rotation":[0,0,0],"zoom":3,"translation":[0,0,0]},"lims":[[-1,10],[-1,5],[-1,2.5]],"aspect":[1,1,1],"light":[{"type":["light"],"lighttype":"ambient","intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[1,0,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[-1,0,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,1,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,-1,0],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,0,1],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}},{"type":["light"],"lighttype":"directional","position":[0,0,-1],"intensity":0.3,"color":{"r":[1],"g":[1],"b":[1],"a":[1]}}],"lastID":[1],"plot":[{"type":["shape"],"vertices":[[-0.62,-0.62,-0.62],[0.62,-0.62,-0.62],[-0.62,0.62,-0.62],[0.62,0.62,-0.62],[-0.62,-0.62,0.62],[0.62,-0.62,0.62],[-0.62,0.62,0.62],[0.62,0.62,0.62],[0,-0.38,1],[0,0.38,1],[0,-0.38,-1],[0,0.38,-1],[-0.38,1,0],[0.38,1,0],[-0.38,-1,0],[0.38,-1,0],[1,0,-0.38],[1,0,0.38],[-1,0,-0.38],[-1,0,0.38],[6.76,3.38,1.69],[9.24,3.38,1.69],[6.76,4.62,1.69],[9.24,4.62,1.69],[6.76,3.38,2.31],[9.24,3.38,2.31],[6.76,4.62,2.31],[9.24,4.62,2.31],[8,3.62,2.5],[8,4.38,2.5],[8,3.62,1.5],[8,4.38,1.5],[7.24,5,2],[8.76,5,2],[7.24,3,2],[8.76,3,2],[10,4,1.81],[10,4,2.19],[6,4,1.81],[6,4,2.19]],"faces":[[0,10,1],[0,1,15],[0,15,14],[0,14,4],[0,4,19],[0,19,18],[0,18,2],[0,2,11],[0,11,10],[1,10,11],[1,11,3],[1,3,16],[1,16,17],[1,17,5],[1,5,15],[2,12,13],[2,13,3],[2,3,11],[2,18,19],[2,19,6],[2,6,12],[3,13,7],[3,7,17],[3,17,16],[4,8,9],[4,9,6],[4,6,19],[4,14,15],[4,15,5],[4,5,8],[5,17,7],[5,7,9],[5,9,8],[6,9,7],[6,7,13],[6,13,12],[20,30,21],[20,21,35],[20,35,34],[20,34,24],[20,24,39],[20,39,38],[20,38,22],[20,22,31],[20,31,30],[21,30,31],[21,31,23],[21,23,36],[21,36,37],[21,37,25],[21,25,35],[22,32,33],[22,33,23],[22,23,31],[22,38,39],[22,39,26],[22,26,32],[23,33,27],[23,27,37],[23,37,36],[24,28,29],[24,29,26],[24,26,39],[24,34,35],[24,35,25],[24,25,28],[25,37,27],[25,27,29],[25,29,28],[26,29,27],[26,27,33],[26,33,32]],"properties":{"mat":"phong","color":{"r":[1],"g":[1],"b":[1],"a":[1]},"opacity":1,"xpd":true,"lwd":1,"transparent":false,"frontSide":true,"backSide":true,"shininess":30},"ID":[1]}]},"settings":{"styles":[],"title":"","ID":"mtac63ly4q5h37f30or4"}},"evals":[],"jsHooks":[]}

```
