
# `ieegio`: IO toolbox for Intracranial Electroencephalography

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/ieegio)](https://CRAN.R-project.org/package=ieegio)
[![r-universe](https://dipterix.r-universe.dev/badges/ieegio)](https://dipterix.r-universe.dev/ieegio)
[![R-CMD-check](https://github.com/dipterix/ieegio/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dipterix/ieegio/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of `ieegio` is to provide integrated toolbox for common file formats used in intracranial Electroencephalography (iEEG) and deep-brain stimulation (DBS). Currently the package is under active development.


## Installation

You can install the development version of `ieegio` from [GitHub](https://github.com/) with:

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

## List 1: read formatted `ephys` data ([examples](https://dipterix.org/ieegio/articles/read-ephys.html))

The following formats have been (or will be) supported. Please feel free to make a wish-list by posting an issue in this repository.

**Electrophysiology**

| Name                                    | Common Extensions                       | Read            | Write |
|:----------------------------------------|:---------------------------------------:|:---------------:|:-----:|
| EDF(+)/BDF(+) (European Data Format)    | `*.edf`                                 | `read_edf`      |       |
| BrainVision                             | `*.vhdr`, `*.vmrk`, `*.eeg`, `*.dat`    | `read_brainvis` |       |
| BCI2000                                 | `*.dat`                                 | `read_bci2000`  |       |
| NEV/NSx (BlackRock neural-event/signal) | `*.nev`, `*.ns1`, `*.ns2`, ..., `*.ns6` | `read_nsx`      |       |
| NWB (Neurodata Without Borders)         | `*.nwb`                                 | `read_nwb`      | *     |
| MEF                                     | `*.mef`                                 |                 |       |

`*` - `NWB` format only has low-level support due to its format nature. Please see examples with `help(read_nwb)`


## List 2: read imaging data ([examples](https://dipterix.org/ieegio/articles/read-imaging-data.html))

**Imaging**

| Name                                                   | Common Extensions          | Read                           | Write                           |
|:-------------------------------------------------------|:--------------------------:|:------------------------------:|:-------------------------------:|
| NIfTI (Neuroimaging Informatics Technology Initiative) | `*.nii[.gz]`               | `read_volume` (`io_read_nii`)  | `write_volume` (`io_write_nii`) |
| MGH/MGZ (Massachusetts General Hospital format)        | `*.mgh`, `*.mgz`           | `read_volume` (`io_read_mgz`)  | `write_volume` (`io_write_mgz`) |
| GIfTI (Geometry format under the NIfTI)                | `*.gii[.gz]`               | `read_surface` (`io_read_gii`) | `write_surface` (`io_write_gii`) |
| FreeSurfer surface                                     | e.g. `*h.pial`, `*h.white` | `read_surface` (`io_read_fs(type="geometry")`)  | `write_surface(format="freesurfer", type="geometry")` |
| FreeSurfer node values/weights                         | e.g. `*h.curv`, `*h.sulc`  | `read_surface` (`io_read_fs(type="measurements")`)  | `write_surface(format="freesurfer", type="measurements")` |
| Surface annotation                                     | `*.annot`                  | `read_surface` (`io_read_fs(type="annotations")`)  | `write_surface(format="freesurfer", type="annotations")` |


## List 3: common file formats used for meta data


| Name        | Common Extensions |      Read      |      Write      |
|:------------|:-----------------:|:--------------:|:---------------:|
| Matlab      | `*.mat`           | `io_read_mat`  | `io_write_mat`  |
| HDF5        | `*.h5`            | `io_read_h5`   | `io_write_h5`   |
| YAML        | `*.yml`, `*.yaml` | `io_read_yaml` | `io_write_yaml` |
| JSON        | `*.json`          | `io_read_json` | `io_write_json` |
| INI         | `*.ini`           | `io_read_ini`  |                 |
| FST         | `*.fst`           | `io_read_fst`  | `io_write_fst`  |


