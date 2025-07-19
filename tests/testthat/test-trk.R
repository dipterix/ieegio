test_that("io-trk", {

  testthat::skip_if_not(ieegio_sample_data("streamlines/CNVII_R.trk", test = TRUE))

  path <- ieegio_sample_data("streamlines/CNVII_R.trk")
  tfile <- tempfile(fileext = ".trk")

  # read
  x <- io_read_trk(path)

  # write
  io_write_trk(x, tfile)

  # compare two files
  testthat::expect_equal(file.size(path), file.size(tfile))

  src_raw <- readBin(path, "raw", n = file.size(path))
  dst_raw <- readBin(tfile, "raw", n = file.size(tfile))

  equal_raw <- src_raw == dst_raw

  # Some reserved information are removed
  testthat::expect_true(all(equal_raw[-c(945:947)]))

  unlink(tfile)

})
