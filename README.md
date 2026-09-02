
# `ieegio`: IO toolbox for Intracranial Electroencephalography

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/ieegio)](https://CRAN.R-project.org/package=ieegio)
[![r-universe](https://dipterix.r-universe.dev/badges/ieegio)](https://dipterix.r-universe.dev/ieegio)
[![R-CMD-check](https://github.com/dipterix/ieegio/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dipterix/ieegio/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `ieegio` is to provide integrated toolbox for common file formats used in intracranial Electroencephalography (iEEG) and deep-brain stimulation (DBS). Besides reading and writing files, `ieegio` also maps data between volumes and surfaces, chains coordinate transforms, and compares regions of interest. Currently the package is under active development.


## Installation

Install the released version from `CRAN`:

``` r
install.packages("ieegio")
```

Or the development version from [r-universe](https://dipterix.r-universe.dev/ieegio):

``` r
install.packages("ieegio", repos = "https://dipterix.r-universe.dev")
```

Or from [GitHub](https://github.com/dipterix/ieegio):

``` r
# install.packages("pak")
pak::pak("dipterix/ieegio")
```

`ieegio` provides additional sample data. To download them:

``` r
# list all the sample data
sample_names <- ieegio::ieegio_sample_data()

lapply(sample_names, ieegio::ieegio_sample_data)
```

All examples below use these sample files.

## Electrophysiology

Read a recording, then pull a channel out by number or by label:

``` r
library(ieegio)

edf <- read_edf(ieegio_sample_data("edfPlusD.edf"))

# channel metadata as a data frame
head(edf$get_channel_table())

# by number, or by label
channel <- edf$get_channel(1)
channel <- edf$get_channel("squarewave")
```

To write a recording, build the channels with `as_edf_channel`. A channel is
either a signal or an annotation table:

``` r
signal <- sin(seq(0, 10, by = 0.01))

channels <- list(
  as_edf_channel(signal, channel_num = 1, sample_rate = 200, label = "sine"),
  as_edf_channel(
    data.frame(timestamp = c(0, 5), comments = c("start", "end")),
    channel_num = 2
  )
)

path <- tempfile(fileext = ".edf")
write_edf(channels = channels, con = path)
```

See the [`ephys` article](https://dipterix.org/ieegio/articles/read-ephys.html) for more.

## Volumes and surfaces

`read_volume` and `read_surface` are the universal entry points; the format is
inferred from the file name. Volumes carry a `header`, the `data`, and a
`transforms` list mapping voxel index to other coordinate systems.

``` r
volume <- read_volume(ieegio_sample_data("brain.demosubject.nii.gz"))

# index with `[`, plot anatomical slices
volume[128, , ]
plot(volume, position = c(-50, -10, 15), which = "coronal")
```

Surface files each hold one kind of data, and `merge` combines them into a
single object:

``` r
geometry    <- read_surface(ieegio_sample_data("gifti/icosahedron3d/geometry.gii"))
measurement <- read_surface(ieegio_sample_data("gifti/icosahedron3d/rand.gii"))

merged <- merge(geometry, measurement)
plot(merged, name = list("measurements", 1))
```

See the [imaging article](https://dipterix.org/ieegio/articles/read-imaging-data.html) for more.

## Surface annotations from `AFNI`/`SUMA`

`NIML` datasets (`*.niml.dset`) are read by `read_surface`, which decides
between labels and continuous values from the dataset itself. Sample data
includes matching `std.141` geometry and annotations:

``` r
geometry <- read_surface(
  ieegio_sample_data("gifti/std.141.lh.inf_200.gii"))

annotation <- read_surface(
  ieegio_sample_data("niml/std.141.lh.aparc.a2009s.annot.niml.dset"))

merge(geometry, annotation)
#> <ieegio Surface>
#>   Header class: basic_geometry
#>   Geometry :
#>     # of Vertex     : 198812
#>     # of Face index : 397620
#>   Annotations: `node label`
#>     # of labels: 76
#>
#> Contains: `geometry`, `annotations`
```

`io_read_niml` is the low-level reader if you need the raw element tree. All
`NIML` storage forms are supported (plain text, binary and `base64` in either
byte order, optionally `gzip` compressed).

## Color maps

`read_colormap` handles `FreeSurfer` lookup tables, `.annot` color tables,
`AFNI` label tables, `FSL` color maps, and `threeBrain`/`RAVE` `JSON`:

``` r
cmap <- read_colormap(
  ieegio_sample_data("niml/rh.std.141.Glasser_HCP.lbl.niml.dset"))
cmap
#> <ieegio Colormap [discrete]>
#>   Colorspace   : RGB
#>   Color stops  : 181  (keys 0 to 180)
#>   Lookup       : 181 labels

# write as a FreeSurfer lookup table
write_colormap(cmap, tempfile(fileext = ".txt"), format = "fs_lut")
```

## Regions of interest

A region of interest is described first and computed later. `as_ieegio_roi`
records the criteria, `resolve_roi_as` carries them out and hands back geometry
in world (`RAS`) coordinates, and `detect_roi_overlap` compares two regions.

``` r
atlas <- read_volume(ieegio_sample_data("atlases/YBA/YBA690.nii.gz"))

# describe: which voxels count as the region
roi <- as_ieegio_roi(atlas, threshold_lb = 1, threshold_ub = 5)

# compute: turn the description into geometry
resolve_roi_as(roi, "pointcloud")
#> <ieegio ROI [pointcloud]>
#> ... # of Vertex : 3495
```

Because both sides are resolved to world coordinates first, regions of
different kinds can be compared directly. Here the whole atlas is tested
against a facial-nerve tract bundle:

``` r
tracts <- read_streamlines(
  ieegio_sample_data("streamlines/CNVII_R.trk"), half_voxel_offset = TRUE)

detect_roi_overlap(as_ieegio_roi(atlas, threshold_lb = 1), tracts, radius = 2)
#> <ieegio ROI overlap: `x` and `y` overlap>
#>   x: volume
#>   y: streamlines (annotated)
#>   Units of `y` overlapping `x`: 53.7%
#>   Early stop: FALSE
```

The result also carries the annotated tracts back, so each streamline knows
whether it hit the region.

## Coordinate spaces and transforms

Name a space with `new_space`, then move a surface into it. Transforms can be
read from `ANTs` or `FSL FLIRT` files, or chained with `as_ieegio_transform`.

``` r
mni <- new_space("MNI152", orientation = "RAS")

surface <- read_surface(ieegio_sample_data("gifti/icosahedron3d/geometry.gii"))
surface_to_surface(surface, space_from = "scanner", space_to = mni,
                   transform = diag(1, 4))
#> ...
#>     # of transforms : 1
#>       Transform Targets : MNI152
```

| Purpose | Function |
|:--|:--|
| Define a coordinate space | `new_space` |
| Build or chain transforms | `as_ieegio_transform` |
| Read `ANTs` affine / displacement field | `io_read_ants_transform` |
| Read `FSL FLIRT` matrix | `io_read_flirt_transform` |
| Convert `FLIRT` to world (`RAS`) | `transform_flirt2ras` |
| Transform between orientations | `transform_orientation` |
| Move a surface between spaces | `surface_to_surface` |

## Mapping and editing volumes

``` r
# volume mask or atlas label to a smoothed mesh
volume_to_surface(atlas, threshold_lb = 1, threshold_ub = 5)

# down-sample or super-sample
resample_volume(volume, new_dim = c(128, 128, 128))

# burn electrode contacts at RAS positions
burn_volume(volume, ras_position = rbind(c(-50, -10, 15), c(-40, -10, 15)),
            col = c("red", "green"), radius = 2)
```

`burn_curve` does the same for a trajectory, such as a depth (`sEEG`) lead or a
`DBS` shaft, given start and end positions.

## Supported formats

The following formats have been (or will be) supported. Please feel free to make a wish-list by posting an issue in this repository.

### Electrophysiology ([examples](https://dipterix.org/ieegio/articles/read-ephys.html))

| Name                                    | Common Extensions                       | Read            | Write            |
|:----------------------------------------|:---------------------------------------:|:---------------:|:----------------:|
| EDF(+)/BDF(+) (European Data Format)    | `*.edf`                                 | `read_edf`      | `write_edf`      |
| BrainVision                             | `*.vhdr`, `*.vmrk`, `*.eeg`, `*.dat`    | `read_brainvis` |       |
| BCI2000                                 | `*.dat`                                 | `read_bci2000`  |       |
| NEV/NSx (BlackRock neural-event/signal) | `*.nev`, `*.ns1`, `*.ns2`, ..., `*.ns6` | `read_nsx`      |       |
| NWB (Neurodata Without Borders)         | `*.nwb`                                 | `read_nwb`      | *     |
| MEF                                     | `*.mef`                                 |                 |       |

`*` - `NWB` format only has low-level support due to its format nature. Please see examples with `help(read_nwb)`

### Imaging ([examples](https://dipterix.org/ieegio/articles/read-imaging-data.html))

| Name                                                   | Common Extensions          | Read                           | Write                           |
|:-------------------------------------------------------|:--------------------------:|:------------------------------:|:-------------------------------:|
| NIfTI (Neuroimaging Informatics Technology Initiative) | `*.nii[.gz]`               | `read_volume` (`io_read_nii`)  | `write_volume` (`io_write_nii`) |
| MGH/MGZ (Massachusetts General Hospital format)        | `*.mgh`, `*.mgz`           | `read_volume` (`io_read_mgz`)  | `write_volume` (`io_write_mgz`) |
| GIfTI (Geometry format under the NIfTI)                | `*.gii[.gz]`               | `read_surface` (`io_read_gii`) | `write_surface` (`io_write_gii`) |
| FreeSurfer surface                                     | e.g. `*h.pial`, `*h.white` | `read_surface` (`io_read_fs(type="geometry")`)  | `write_surface(format="freesurfer", type="geometry")` |
| FreeSurfer node values/weights                         | e.g. `*h.curv`, `*h.sulc`  | `read_surface` (`io_read_fs(type="measurements")`)  | `write_surface(format="freesurfer", type="measurements")` |
| Surface annotation                                     | `*.annot`                  | `read_surface` (`io_read_fs(type="annotations")`)  | `write_surface(format="freesurfer", type="annotations")` |
| `AFNI`/`SUMA` `NIML` surface data                      | `*.niml.dset`              | `read_surface` (`io_read_niml`) |  |
| `VTK` poly-data (as surface mesh)                      | `*.vtk`, `*.vtp`, `*.pvtp`, `*.vtu`, `*.vtpb` | `read_surface` (`io_read_vtk_polys`)  | `write_surface` (`io_write_vtk_polys`) ** |
| Other mesh formats                                     | `*.stl`, `*.ply`, `*.obj`, `*.off`, `*.mz3` | `read_surface` (via `freesurferformats`)  | `write_surface` (`*.stl` only) |
| `TRK` streamlines                                      | `*.trk`, `*.trk.gz`        | `read_streamlines` (`io_read_trk`)  | `write_streamlines` (`io_write_trk`) |
| `TCK` streamlines                                      | `*.tck`                    | `read_streamlines` (`io_read_tck`)  | `write_streamlines` (`io_write_tck`) |
| `TT` (`DSI-Studio`) streamlines                        | `*.tt`, `*.tt.gz`          | `read_streamlines` (`io_read_tt`)  |  |
| `VTK` poly-data (as streamlines)                       | `*.vtk`, `*.vtp`           | `read_streamlines` (`io_read_vtk_streamlines`) *** | `write_streamlines` (`io_write_vtk_streamlines`) |

`**` - `VTK` surfaces can be read from all five extensions, but only written as
legacy `*.vtk`, `XML` `*.vtp`, or `HDF5` `*.vtpb`.

`***` - `read_streamlines` infers `*.vtk` and `*.vtp` from the file name; call
`io_read_vtk_streamlines` directly for `*.pvtp` and `*.vtpb`.
`write_streamlines` also writes `*.vtpb` and `*.h5`.

The `VTK` readers and writers use the `Python` `vtk` package.

### Color maps

| Name                        | Common Extensions   | Read             | Write             |
|:----------------------------|:-------------------:|:----------------:|:-----------------:|
| `FreeSurfer` lookup table   | `*.txt`             | `read_colormap`  | `write_colormap(format="fs_lut")` |
| `FreeSurfer` annotation     | `*.annot`           | `read_colormap`  |  |
| `AFNI` label table          | `*.niml.dset`, `*.lut` | `read_colormap` |  |
| `FSL` color map / `VEST`    | `*.cmap`, `*.lut`   | `read_colormap`  |  |
| `threeBrain`/`RAVE`         | `*.json`            | `read_colormap`  | `write_colormap` (default) |

### Meta data

| Name        | Common Extensions |      Read      |      Write      |
|:------------|:-----------------:|:--------------:|:---------------:|
| Matlab      | `*.mat`           | `io_read_mat`  | `io_write_mat`  |
| HDF5        | `*.h5`            | `io_read_h5`   | `io_write_h5`   |
| YAML        | `*.yml`, `*.yaml` | `io_read_yaml` | `io_write_yaml` |
| JSON        | `*.json`          | `io_read_json` | `io_write_json` |
| INI         | `*.ini`           | `io_read_ini`  |                 |
| FST         | `*.fst`           | `io_read_fst`  | `io_write_fst`  |

The `HDF5` back-end is resolved at run time, choosing among `readNSx`,
`h5lite`, `hdf5r`, and `h5py`, so no single one of them is required. `LazyH5`
and `LazyFST` read `HDF5` and `FST` files lazily, and `convert_fst_to_csv` and
`convert_fst_to_hdf5` convert between the formats.
