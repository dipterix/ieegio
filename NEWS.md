# ieegio 0.0.4

* Added merge support for both volume and surface objects; for surface objects, users can choose whether to merge the objects in model or world space
* Added dependency `ravetools` to help speed up sample volumes using `C++`
* Fixed missing transform issue in writing to `GIfTI` format
* Fixed `write_surface` not working when writing to `FreeSurfer` measurement (curvature) format
* Fixed a typo that caused setting volume data resulting in errors
* `as_ieegio_surface` does not require vertices

# ieegio 0.0.3

* Cleaned `h5py` loader
* `mat_to_quaternion` always returns non-negative `w` now
* Added generics `dim` and `length` to volume object
* Fixed annotation issues in `EDF` format
* Time selection is double-sided using greater equal and less equal signs
* Added more sample data
* Supports colored (`RGB[A]`) formats
* Fixed color intensity when `cal_min` and `cal_max` is set
* Using `r3js` for visualizing surfaces
* Added `resample_volume` to sample images (currently only nearest neighbor)
* Added `burn_volume` to burn contacts (positions given in the image native `RAS` coordinate) with any given colors
* Fixed `io_h5_names` returning wrong names when using with `h5py`
* Exported internal class `LazyH5` for down-stream packages
* Added `as_ieegio_volume` and `as_ieegio_surface` to enclose volume or surface data
* Added a flag to disable using `Python`


# ieegio 0.0.2

* Added `h5py` as an alternative back-end support
* Fixed a bug where images are not loaded correctly due to image format being ignored
* Fixed `Torig` (index to `tkr` coordinate transform) issue
* Fixed a global variable issue
* Fixed a bug where file name is not respected


# ieegio 0.0.1

* Initial CRAN submission.
