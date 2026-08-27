# Detect whether two regions of interest overlap

Reports, for every unit of `y`, whether it comes within `radius` of `x`,
and hands back `y` with that answer written onto it. The test is
asymmetric: `x` is indexed once and `y` is streamed against it, so the
results line up with `y`, and the larger or repeatedly reused region
belongs in `x`.

## Usage

``` r
detect_roi_overlap(
  x,
  y,
  mode_x = c("auto", "volume", "pointcloud", "streamlines", "surface"),
  mode_y = c("auto", "volume", "pointcloud", "streamlines", "surface"),
  radius = 0,
  early_stop = FALSE,
  include_interior = FALSE
)

# S3 method for class 'ieegio_roi_overlap_result'
print(x, ...)
```

## Arguments

- x, y:

  the two regions, or anything
  [`as_ieegio_roi`](http://dipterix.org/ieegio/reference/as_ieegio_roi.md)
  accepts; each is put through
  [`resolve_roi_as`](http://dipterix.org/ieegio/reference/resolve_roi_as.md)
  before being tested

- mode_x, mode_y:

  representation to resolve each region into first: `"auto"` (default,
  keep whatever the region already is), `"volume"`, `"pointcloud"`,
  `"surface"`, or `"streamlines"`

- radius:

  distance tolerance; an overlap is reported when the distance is at
  most `radius`. Default is `0`, meaning literal contact. A volume
  raises this to a floor of its own; see 'Details'

- early_stop:

  whether to stop measuring a unit at its first overlapping element
  instead of keeping the closest one; default is `FALSE`. Every unit is
  answered either way; see 'Details'

- include_interior:

  whether geometry lying strictly inside a closed `x` counts as
  overlapping even when it never comes within `radius` of the surface
  itself; default is `FALSE`. Only a surface has an interior, so when
  `x` resolves to anything else this is reported and dropped rather than
  raised as an error, and the test falls back to contact alone. The
  setting that was actually used is recorded in
  `collision_detection$summary$include_interior`

- ...:

  ignored, present for compatibility with `print`

## Value

An `"ieegio_roi_overlap_result"` list with

- `overlapped`:

  whether the two regions overlap at all, taking each as a whole.

- `hit_ratio`:

  the ratio, between 0 and 1, of the units of `y` that overlap `x`.
  Units that could not be tested are left out of both sides rather than
  counted as misses, and a region with nothing testable in it gives `0`
  rather than `NaN`. A surface reports two, `c(vertex = , face = )`, a
  face-level answer and its vertex-level reduction being different
  numbers.

- `early_stop`:

  the setting actually used.

- `annotated`:

  the resolved `y`, annotated as above.

- `collision_detection`:

  the raw
  [`vcg_detect_collision`](https://dipterix.org/ravetools/reference/vcg_detect_collision.html)
  output, untouched.

- `mode_x`, `mode_y`:

  the representations the two regions resolved to, which is what was
  tested, rather than the arguments, which may both have said `"auto"`.

## Details

Answers are reported per *unit* of `y`, which is coarser than a vertex
or a point: a point for a point cloud, a voxel center for a volume, a
**face** for a surface, and a **whole tract** for streamlines. A surface
of 642 vertices and 1280 faces therefore yields 1280 answers, and a
bundle of 20 tracts yields 20, however many points each holds.

**Both regions must already share one coordinate space.** Nothing here
transforms them into a common one: a volume contributes its `vox2ras`,
streamlines their `header$vox2ras`, and a surface whichever transform
`geometry$transforms` lists first, which for a `GIFTI` carrying several
is not necessarily the scanner one. Comparing a region in `"MNI152"`
against one in `"ScannerAnat"` returns confident nonsense rather than an
error.

`radius` has a lower bound that the regions themselves set. ravetools
has no volume mode, so a volume is tested as its voxel centers, and each
center then stands in for a voxel it can no longer describe. The radius
actually used is `max(radius, inflation_x, inflation_y)`, where a
volume's inflation is half its voxel diagonal and every other
representation inflates by zero - a point cloud is genuinely points, and
surfaces and streamlines are carried exactly. Two volumes therefore
floor at the larger of their two, a volume against anything else floors
at its own, and a pair without a volume leaves `radius` untouched.

`early_stop` chooses only which element inside a unit gets reported, not
how many units are looked at: `FALSE` measures every element and keeps
the closest, `TRUE` stops at the first that overlaps. Both a hit and a
distance come back either way. Since a point and a face are each their
own unit, the setting changes nothing for them, and matters only for
streamlines, where a tract's reported distance becomes that of its first
overlapping segment rather than its nearest one.

Tracts that are not lines are dropped before the test: a tract needs at
least two points to have any segment, and points with missing
coordinates are removed first. A region can therefore report fewer
tracts than it was built with.

## Annotating `y`

`annotated` is the resolved `y`, in whatever `mode_y` asked for,
carrying the result in that representation's own idiom:

- point cloud, surface:

  two vertex measurements, `Overlap` (`1` or `0`) and `distance` (the
  distance, or `NA` where clear). A surface is answered per face, so
  both are read at the vertices: a vertex overlaps if any face meeting
  there does, and takes the nearest of them.

- streamlines:

  the same two as per-tract *properties*, one value each, since a unit
  is a whole tract.

- volume:

  voxel values on the region's own grid and `vox2ras`. A volume cannot
  hold `NA` - it becomes `0`, which a distance field would read as exact
  contact - so two negative sentinels keep the three states apart,
  neither of which a measured distance can take: `-2` outside the region
  and never tested, `-1` inside it but not overlapping, and `>= 0`
  overlapping, the value being the distance. Thresholding at `>= -1`
  therefore recovers the region's own mask, and at `>= 0` the part of it
  that overlaps.

## See also

[`as_ieegio_roi`](http://dipterix.org/ieegio/reference/as_ieegio_roi.md)
records how a region is derived, and
[`resolve_roi_as`](http://dipterix.org/ieegio/reference/resolve_roi_as.md)
applies that description; this function consumes the result of both.

## Examples

``` r

if (interactive()) {

# ---- Two spheres, offset so that they partly overlap -----------------
sphere <- ravetools::vcg_sphere()
x <- as_ieegio_roi(sphere)

sphere$vb[1:2, ] <- sphere$vb[1:2, ] + 1
y <- as_ieegio_roi(sphere)

result <- detect_roi_overlap(x, y)
result

# `y` is answered per face, and carries the answer at its vertices
length(result$collision_detection$hit_unit)
head(result$annotated$measurements$data_table)

# ---- The same question asked of a volume -----------------------------
mask <- array(0, c(20, 20, 20))
mask[8:13, 8:13, 8:13] <- 1
vox2ras <- rbind(cbind(diag(1, 3), c(-10, -10, -10)), c(0, 0, 0, 1))
region <- as_ieegio_roi(mask, vox2ras = vox2ras, threshold_lb = 0.5)

result <- detect_roi_overlap(x, region, mode_y = "volume")
result$hit_ratio

# `>= -1` is the region itself, `>= 0` the part of it that overlaps
values <- result$annotated[]
c(region = sum(values >= -1), overlapping = sum(values >= 0))

}
```
