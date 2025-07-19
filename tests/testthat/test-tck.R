testthat::test_that("tck IO", {

  testthat::skip_if_not(ieegio_sample_data("streamlines/CNVII_R.tck", test = TRUE))

  tck_path <- ieegio::ieegio_sample_data("streamlines/CNVII_R.tck")

  tck_src <- io_read_tck(tck_path)

  tfile <- tempfile(fileext = ".tck")
  on.exit({ unlink(tfile) })

  io_write_tck(x = tck_src, con = tfile, datatype = tck_src$header$datatype)

  testthat::expect_identical(
    digest::digest(file = tfile),
    digest::digest(file = tck_path)
  )
})
