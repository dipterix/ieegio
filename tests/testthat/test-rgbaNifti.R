testthat::test_that("NIfTI with RGBA format", {
  testthat::skip_if(system.file(package = "RNifti") == "")

  rgba_file <- system.file("extdata", "example_rgb.nii.gz", package="RNifti")
  y <- RNifti::readNifti(rgba_file)
  x <- ieegio::read_volume(rgba_file)

  rgba <- RNifti::channels(y[36, 63, 30])
  testthat::expect_identical(x[36, 63, 30], grDevices::rgb(rgba[[1]], rgba[[2]], rgba[[3]], maxColorValue = 255))

  rgba <- RNifti::channels(y)

  col <- grDevices::rgb(rgba[,,,1], rgba[,,,2], rgba[,,,3], maxColorValue = 255)
  testthat::expect_identical(as.vector(x[]), col)

})
