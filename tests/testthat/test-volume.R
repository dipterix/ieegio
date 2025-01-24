library(testthat)

test_that("io_nii", {

  f <- tempfile(fileext = ".nii")
  f2 <- tempfile(fileext = ".nii")
  f3 <- tempfile(fileext = ".nii.gz")
  f4 <- tempfile(fileext = ".mgz")

  on.exit({
    unlink(f)
    unlink(f2)
    unlink(f3)
    unlink(f4)
  })

  tmp <- array(rnorm(60), dim = c(3,4,5))

  vox2ras <- matrix(
    nrow = 4, byrow = TRUE,
    c(
      -1, 0, 0, 0.8,
      0, 0, 1, -3.1,
      0, 1, 0, 3.2,
      0, 0, 0, 1
    )
  )

  # io_write_nii.array
  write_volume(tmp, con = f, vox2ras = vox2ras)

  v <- io_read_nii(f)
  expect_true(inherits(v, "ieegio_nifti"))
  expect_true(inherits(v, "ieegio_rnifti"))

  expect_equal(v$shape, dim(tmp))
  expect_equal(v$transforms$vox2ras, vox2ras, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(v$transforms$vox2ras_tkr, get_vox2ras_tkr(vox2ras, v$shape/2), tolerance = 1e-4, ignore_attr = TRUE)

  unlink(f2)
  io_write_nii(v, con = f2)
  expect_true(file.exists(f2))
  unlink(f3)
  io_write_nii(v, con = f3)
  expect_true(file.exists(f3))


  # io_write_nii.niftiImage
  v <- read_volume(file = f, method = 'oro')

  expect_true(inherits(v, "ieegio_nifti"))
  expect_true(inherits(v, "ieegio_oro"))
  expect_equal(v$shape, dim(tmp))
  expect_equal(v$transforms$vox2ras, vox2ras, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(v$transforms$vox2ras_tkr, get_vox2ras_tkr(vox2ras, v$shape/2), tolerance = 1e-4, ignore_attr = TRUE)

  unlink(f2)
  io_write_nii(v, con = f2)
  expect_true(file.exists(f2))
  unlink(f3)
  io_write_nii(v, con = f3)
  expect_true(file.exists(f3))

  unlink(f2)
  io_write_nii.niftiImage(v$header, con = f2)
  expect_true(file.exists(f2))
  unlink(f3)
  io_write_nii.niftiImage(v$header, con = f3)
  expect_true(file.exists(f3))


  # io_write_nii.nifti
  v <- read_volume(file = f, method = 'oro')

  expect_true(inherits(v, "ieegio_nifti"))
  expect_true(inherits(v, "ieegio_oro"))
  expect_equal(v$shape, dim(tmp))
  expect_equal(v$transforms$vox2ras, vox2ras, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(v$transforms$vox2ras_tkr, get_vox2ras_tkr(vox2ras, v$shape/2), tolerance = 1e-4, ignore_attr = TRUE)

  unlink(f2)
  io_write_nii(v, con = f2)
  expect_true(file.exists(f2))
  unlink(f3)
  io_write_nii(v, con = f3)
  expect_true(file.exists(f3))

  unlink(f2)
  io_write_nii.nifti(v$header, con = f2)
  expect_true(file.exists(f2))
  unlink(f3)
  io_write_nii.nifti(v$header, con = f3)
  expect_true(file.exists(f3))

  # io_write_mgz
  unlink(f4)
  write_volume(v, f4)
  expect_true(file.exists(f4))
  v2 <- read_volume(file = f4)
  expect_true(inherits(v2, "ieegio_mgh"))
  expect_true(inherits(v2, "ieegio_mgz"))
  expect_equal(v2$shape[1:3], dim(tmp))
  expect_equal(v2$transforms$vox2ras, vox2ras, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(v2$transforms$vox2ras_tkr, get_vox2ras_tkr(vox2ras, v$shape/2), tolerance = 1e-4, ignore_attr = TRUE)

  # ANTs
  testthat::skip_on_cran()
  testthat::skip_on_bioc()
  testthat::skip_if(nzchar(Sys.getenv("IEEGIO_NO_PYTHON", unset = "")))
  skip_if_not(rpyANTs::ants_available("ants"), message = "ANTs is not installed")
  v <- read_volume(file = f, method = 'ants')

  expect_true(inherits(v, "ieegio_nifti"))
  expect_true(inherits(v, "ieegio_antspy"))
  expect_equal(v$shape, dim(tmp))
  expect_equal(v$transforms$vox2ras, vox2ras, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(v$transforms$vox2ras_tkr, get_vox2ras_tkr(vox2ras, v$shape/2), tolerance = 1e-4, ignore_attr = TRUE)

  unlink(f2)
  io_write_nii(v, con = f2)
  expect_true(file.exists(f2))
  unlink(f3)
  io_write_nii(v, con = f3)
  expect_true(file.exists(f3))

  unlink(f2)
  io_write_nii.ants.core.ants_image.ANTsImage(v$header, con = f2)
  expect_true(file.exists(f2))
  unlink(f3)
  io_write_nii.ants.core.ants_image.ANTsImage(v$header, con = f3)
  expect_true(file.exists(f3))

})

test_that("io_mgz", {

  f <- tempfile(fileext = ".mgz")
  f2 <- tempfile(fileext = ".mgz")
  f3 <- tempfile(fileext = ".nii")

  on.exit({
    unlink(f)
    unlink(f2)
    unlink(f3)
  })

  tmp <- array(rnorm(60), dim = c(3,4,5))

  vox2ras <- matrix(
    nrow = 4, byrow = TRUE,
    c(
      -1, 0, 0, 0.8,
      0, 0, 1, -3.1,
      0, 1, 0, 3.2,
      0, 0, 0, 1
    )
  )

  # io_write_nii.array
  write_volume(tmp, con = f, vox2ras = vox2ras)

  v <- io_read_mgz(f)
  expect_true(inherits(v, "ieegio_mgh"))
  expect_true(inherits(v, "ieegio_mgz"))

  expect_equal(v$shape[1:3], dim(tmp))
  expect_equal(v$transforms$vox2ras, vox2ras, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(v$transforms$vox2ras_tkr, get_vox2ras_tkr(vox2ras, v$shape/2), tolerance = 1e-4, ignore_attr = TRUE)

  unlink(f2)
  io_write_mgz(v, con = f2)
  expect_true(file.exists(f2))

  # io_write_nii.ieegio_mgh
  unlink(f3)
  io_write_nii(v, con = f3)
  expect_true(file.exists(f2))

  # io_write_nii.niftiImage
  v2 <- read_volume(file = f3)

  expect_true(inherits(v2, "ieegio_nifti"))
  expect_true(inherits(v2, "ieegio_rnifti"))
  expect_equal(v2$shape, dim(tmp))
  expect_equal(v2$transforms$vox2ras, vox2ras, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(v2$transforms$vox2ras_tkr, get_vox2ras_tkr(vox2ras, v$shape/2), tolerance = 1e-4, ignore_attr = TRUE)

})

