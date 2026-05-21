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

  tmp <- array(rnorm(60), dim = c(3, 4, 5))

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
  expect_equal(
    v$transforms$vox2ras_tkr,
    get_vox2ras_tkr(vox2ras, v$shape / 2),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  unlink(f2)
  io_write_nii(v, con = f2)
  expect_true(file.exists(f2))
  unlink(f3)
  io_write_nii(v, con = f3)
  expect_true(file.exists(f3))


  # io_write_nii.niftiImage
  v <- read_volume(file = f, method = "oro")

  expect_true(inherits(v, "ieegio_nifti"))
  expect_true(inherits(v, "ieegio_oro"))
  expect_equal(v$shape, dim(tmp))
  expect_equal(v$transforms$vox2ras, vox2ras, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(
    v$transforms$vox2ras_tkr,
    get_vox2ras_tkr(vox2ras, v$shape / 2),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

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
  v <- read_volume(file = f, method = "oro")

  expect_true(inherits(v, "ieegio_nifti"))
  expect_true(inherits(v, "ieegio_oro"))
  expect_equal(v$shape, dim(tmp))
  expect_equal(v$transforms$vox2ras, vox2ras, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(
    v$transforms$vox2ras_tkr,
    get_vox2ras_tkr(vox2ras, v$shape / 2),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

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
  expect_equal(
    v2$transforms$vox2ras_tkr,
    get_vox2ras_tkr(vox2ras, v$shape / 2),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  # ANTs
  testthat::skip_on_cran()
  testthat::skip_on_bioc()
  testthat::skip_if(nzchar(Sys.getenv("IEEGIO_NO_PYTHON", unset = "")))
  skip_if_not(rpyANTs::ants_available("ants"), message = "ANTs is not installed")
  v <- read_volume(file = f, method = "ants")

  expect_true(inherits(v, "ieegio_nifti"))
  expect_true(inherits(v, "ieegio_antspy"))
  expect_equal(v$shape, dim(tmp))
  expect_equal(v$transforms$vox2ras, vox2ras, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(
    v$transforms$vox2ras_tkr,
    get_vox2ras_tkr(vox2ras, v$shape / 2),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

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

  tmp <- array(rnorm(60), dim = c(3, 4, 5))

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
  expect_equal(
    v$transforms$vox2ras_tkr,
    get_vox2ras_tkr(vox2ras, v$shape / 2),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

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
  expect_equal(
    v2$transforms$vox2ras_tkr,
    get_vox2ras_tkr(vox2ras, v$shape / 2),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

})

# Helper: reconstruct the qform 4x4 matrix from the NIfTI quaternion + qfac,
# mirroring the spec formula (nifti_quatern_to_mat44) used by all readers.
reconstruct_qform <- function(quaternion, vox2ras) {
  b <- quaternion[["x"]]   # quatern_b
  c <- quaternion[["y"]]   # quatern_c
  d <- quaternion[["z"]]   # quatern_d
  m33 <- vox2ras[1:3, 1:3]
  dx  <- sqrt(sum(m33[, 1]^2))
  dy  <- sqrt(sum(m33[, 2]^2))
  dz  <- sqrt(sum(m33[, 3]^2))
  qfac <- sign(det(m33))   # stored in pixdim[0]
  a_sq <- max(0, 1 - b * b - c * c - d * d)
  a <- sqrt(a_sq)
  zd <- dz * qfac           # qfac flips z back for left-handed matrices
  R <- matrix(0, 4, 4)
  R[1, 1] <- (a * a + b * b - c * c - d * d) * dx
  R[1, 2] <- 2 * (b * c - a * d) * dy
  R[1, 3] <- 2 * (b * d + a * c) * zd
  R[2, 1] <- 2 * (b * c + a * d) * dx
  R[2, 2] <- (a * a + c * c - b * b - d * d) * dy
  R[2, 3] <- 2 * (c * d - a * b) * zd
  R[3, 1] <- 2 * (b * d - a * c) * dx
  R[3, 2] <- 2 * (c * d + a * b) * dy
  R[3, 3] <- (a * a + d * d - c * c - b * b) * zd
  R[4, 4] <- 1
  R[1, 4] <- vox2ras[1, 4]
  R[2, 4] <- vox2ras[2, 4]
  R[3, 4] <- vox2ras[3, 4]
  R
}

test_that("mat_to_quaternion round-trip: right-handed (det > 0)", {
  # Typical neurological NIfTI orientation: det > 0
  vox2ras_rh <- matrix(
    nrow = 4, byrow = TRUE,
    c(
       2,  0,  0, -90,
       0,  2,  0, -126,
       0,  0,  2,  -72,
       0,  0,  0,    1
    )
  )
  expect_gt(det(vox2ras_rh[1:3, 1:3]), 0)

  q <- ieegio:::mat_to_quaternion(vox2ras_rh)
  recovered <- reconstruct_qform(q, vox2ras_rh)
  expect_equal(recovered, vox2ras_rh, tolerance = 1e-6, ignore_attr = TRUE)
})

test_that("mat_to_quaternion round-trip: left-handed (det < 0)", {
  # Typical radiological NIfTI orientation: det < 0 (x-axis flipped)
  vox2ras_lh <- matrix(
    nrow = 4, byrow = TRUE,
    c(
      -2,  0,  0,  90,
       0,  2,  0, -126,
       0,  0,  2,  -72,
       0,  0,  0,    1
    )
  )
  expect_lt(det(vox2ras_lh[1:3, 1:3]), 0)

  q <- ieegio:::mat_to_quaternion(vox2ras_lh)
  recovered <- reconstruct_qform(q, vox2ras_lh)
  expect_equal(recovered, vox2ras_lh, tolerance = 1e-6, ignore_attr = TRUE)
})

test_that("mat_to_quaternion round-trip: oblique left-handed", {
  # Oblique left-handed: columns are orthogonal, det < 0
  # Constructed as: rotation in xy-plane with x-axis flipped, scaled by voxel sizes
  theta <- pi / 6
  cos_t <- cos(theta)
  sin_t <- sin(theta)
  vox2ras_oblique_lh <- matrix(
    nrow = 4, byrow = TRUE,
    c(
      -1.5 * cos_t, 2.0 * sin_t, 0, 80,
       1.5 * sin_t, 2.0 * cos_t, 0, -100,
       0,          0,          2,  -60,
       0,          0,          0,    1
    )
  )
  # Verify it is indeed left-handed
  expect_lt(det(vox2ras_oblique_lh[1:3, 1:3]), 0)

  q <- ieegio:::mat_to_quaternion(vox2ras_oblique_lh)
  recovered <- reconstruct_qform(q, vox2ras_oblique_lh)
  expect_equal(recovered, vox2ras_oblique_lh, tolerance = 1e-6, ignore_attr = TRUE)
})

test_that("NIfTI write/read round-trip: left-handed vox2ras", {
  # Radiological (left-handed) vox2ras: the case that was broken before the fix
  vox2ras_lh <- matrix(
    nrow = 4, byrow = TRUE,
    c(
      -2,  0,  0,  90,
       0,  2,  0, -126,
       0,  0,  2,  -72,
       0,  0,  0,    1
    )
  )
  expect_lt(det(vox2ras_lh[1:3, 1:3]), 0)

  f <- tempfile(fileext = ".nii")
  on.exit(unlink(f))

  tmp <- array(rnorm(3 * 4 * 5), dim = c(3, 4, 5))
  write_volume(tmp, con = f, vox2ras = vox2ras_lh)

  v <- io_read_nii(f)
  expect_equal(v$transforms$vox2ras, vox2ras_lh, tolerance = 1e-4, ignore_attr = TRUE)

  # Verify qform and sform agree (both must encode the same orientation)
  hdr <- v$header
  sform <- matrix(c(hdr$srow_x, hdr$srow_y, hdr$srow_z, 0, 0, 0, 1), nrow = 4, byrow = TRUE)
  q <- c(x = hdr$quatern_b, y = hdr$quatern_c, z = hdr$quatern_d)
  qfac <- hdr$pixdim[[1]]
  # pixdim for qfac is the first element; reconstruct using full vox2ras spacings
  qform <- reconstruct_qform(q, vox2ras_lh)
  expect_equal(qform[1:3, 1:3], sform[1:3, 1:3], tolerance = 1e-4, ignore_attr = TRUE)
})

