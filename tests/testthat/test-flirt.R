library(testthat)

# ---- Helper to create a temporary FLIRT matrix file ----
create_temp_flirt_file <- function(matrix = diag(4)) {
  tmp <- tempfile(fileext = ".mat")
  write.table(matrix, tmp, row.names = FALSE, col.names = FALSE)
  tmp
}

# ---- io_read_flirt_transform tests ----

test_that("io_read_flirt_transform reads 4x4 identity matrix", {
  flirt_file <- create_temp_flirt_file(diag(4))
  on.exit(unlink(flirt_file))

  xfm <- io_read_flirt_transform(flirt_file)

  expect_s3_class(xfm, "ieegio_transforms")
  expect_equal(xfm$type, "affine")
  expect_equal(xfm$interpretation, "active")
  expect_equal(attr(xfm$space_from, "orientation"), "FSL")
  expect_equal(attr(xfm$space_to, "orientation"), "FSL")
  expect_equal(xfm$data[[1]], diag(4))
})

test_that("io_read_flirt_transform reads non-identity matrix", {
  # Create a rotation + translation matrix
  test_mat <- matrix(c(
    0.9, -0.1, 0.0, 10.0,
    0.1, 0.9, 0.0, -5.0,
    0.0, 0.0, 1.0, 2.0,
    0.0, 0.0, 0.0, 1.0
  ), nrow = 4, byrow = TRUE)

  flirt_file <- create_temp_flirt_file(test_mat)
  on.exit(unlink(flirt_file))

  xfm <- io_read_flirt_transform(flirt_file)

  expect_equal(xfm$data[[1]], test_mat, tolerance = 1e-10)
})

test_that("io_read_flirt_transform infers BIDS spaces from filename", {
  test_mat <- diag(4)

  # Create file with BIDS-style naming
  tmp_dir <- tempdir()
  bids_file <- file.path(tmp_dir, "sub-01_from-T1w_to-MNI152_xfm.mat")
  write.table(test_mat, bids_file, row.names = FALSE, col.names = FALSE)
  on.exit(unlink(bids_file))

  xfm <- io_read_flirt_transform(bids_file)

  expect_equal(as.character(xfm$space_from), "T1w")
  expect_equal(as.character(xfm$space_to), "MNI152")
})

test_that("io_read_flirt_transform accepts explicit space names", {
  flirt_file <- create_temp_flirt_file(diag(4))
  on.exit(unlink(flirt_file))

  xfm <- io_read_flirt_transform(flirt_file,
                                  space_from = "native",
                                  space_to = "standard")

  expect_equal(as.character(xfm$space_from), "native")
  expect_equal(as.character(xfm$space_to), "standard")
})

test_that("io_read_flirt_transform errors on invalid matrix", {
  # Create a 3x3 matrix (invalid)
  tmp <- tempfile(fileext = ".mat")
  write.table(diag(3), tmp, row.names = FALSE, col.names = FALSE)
  on.exit(unlink(tmp))

  expect_error(io_read_flirt_transform(tmp), "4x4")
})

# ---- transform_orientation with FSL tests ----

test_that("transform_orientation FSL to FSL returns identity", {
  xfm <- transform_orientation(orientation_from = "FSL",
                               orientation_to = "FSL")

  expect_equal(xfm$data[[1]], diag(4))
  expect_equal(attr(xfm$space_from, "orientation"), "FSL")
  expect_equal(attr(xfm$space_to, "orientation"), "FSL")
})

test_that("transform_orientation FSL to/from RAS requires image", {
  expect_error(
    transform_orientation(orientation_from = "FSL", orientation_to = "RAS"),
    "image.*required"
  )

  expect_error(
    transform_orientation(orientation_from = "RAS", orientation_to = "FSL"),
    "image.*required"
  )
})

test_that("transform_orientation FSL to RAS with image", {
  skip_if_not_installed("RNifti")

  # Use sample NIfTI file
  nii_file <- ieegio_sample_data("brain.demosubject.nii.gz")
  skip_if(!file.exists(nii_file), "Sample NIfTI file not available")

  xfm <- transform_orientation(orientation_from = "FSL",
                               orientation_to = "RAS",
                               image = read_volume(nii_file, header_only = TRUE, method = "oro"))

  expect_s3_class(xfm, "ieegio_transforms")
  expect_equal(attr(xfm$space_from, "orientation"), "FSL")
  expect_equal(attr(xfm$space_to, "orientation"), "RAS")

  # Should be a 4x4 matrix

  expect_equal(dim(xfm$data[[1]]), c(4L, 4L))
})

test_that("transform_orientation RAS to FSL with image", {
  skip_if_not_installed("RNifti")

  nii_file <- ieegio_sample_data("brain.demosubject.nii.gz")
  skip_if(!file.exists(nii_file), "Sample NIfTI file not available")

  xfm <- transform_orientation(orientation_from = "RAS",
                               orientation_to = "FSL",
                               image = read_volume(nii_file, header_only = TRUE, method = "oro"))

  expect_equal(attr(xfm$space_from, "orientation"), "RAS")
  expect_equal(attr(xfm$space_to, "orientation"), "FSL")
})

test_that("transform_orientation FSL to LPS chains correctly", {
  skip_if_not_installed("RNifti")

  nii_file <- ieegio_sample_data("brain.demosubject.nii.gz")
  skip_if(!file.exists(nii_file), "Sample NIfTI file not available")

  # FSL -> LPS should go through RAS internally
  xfm <- transform_orientation(orientation_from = "FSL",
                               orientation_to = "LPS",
                               image = read_volume(nii_file, header_only = TRUE, method = "oro"))

  expect_equal(attr(xfm$space_from, "orientation"), "FSL")
  expect_equal(attr(xfm$space_to, "orientation"), "LPS")
})

# ---- transform_flirt2ras tests ----

test_that("transform_flirt2ras with no images returns unchanged",
{
  flirt_file <- create_temp_flirt_file(diag(4))
  on.exit(unlink(flirt_file))

  xfm <- io_read_flirt_transform(flirt_file)
  result <- transform_flirt2ras(xfm)

  # Should be unchanged (FSL -> FSL)
  expect_equal(result$data[[1]], diag(4))
  expect_equal(attr(result$space_from, "orientation"), "FSL")
  expect_equal(attr(result$space_to, "orientation"), "FSL")
})

test_that("transform_flirt2ras with source only", {
  skip_if_not_installed("RNifti")

  nii_file <- ieegio_sample_data("brain.demosubject.nii.gz")
  skip_if(!file.exists(nii_file), "Sample NIfTI file not available")

  flirt_file <- create_temp_flirt_file(diag(4))
  on.exit(unlink(flirt_file))

  xfm <- io_read_flirt_transform(flirt_file)
  result <- transform_flirt2ras(xfm, source = read_volume(nii_file, header_only = TRUE, method = "oro"))

  # Source side converted: RAS -> FSL
  expect_equal(attr(result$space_from, "orientation"), "RAS")
  expect_equal(attr(result$space_to, "orientation"), "FSL")
})

test_that("transform_flirt2ras with reference only", {
  skip_if_not_installed("RNifti")

  nii_file <- ieegio_sample_data("brain.demosubject.nii.gz")
  skip_if(!file.exists(nii_file), "Sample NIfTI file not available")

  flirt_file <- create_temp_flirt_file(diag(4))
  on.exit(unlink(flirt_file))

  xfm <- io_read_flirt_transform(flirt_file)
  result <- transform_flirt2ras(xfm, reference = read_volume(nii_file, header_only = TRUE, method = "oro"))

  # Reference side converted: FSL -> RAS
  expect_equal(attr(result$space_from, "orientation"), "FSL")
  expect_equal(attr(result$space_to, "orientation"), "RAS")
})

test_that("transform_flirt2ras with both images", {
  skip_if_not_installed("RNifti")

  nii_file <- ieegio_sample_data("brain.demosubject.nii.gz")
  skip_if(!file.exists(nii_file), "Sample NIfTI file not available")

  flirt_file <- create_temp_flirt_file(diag(4))
  on.exit(unlink(flirt_file))

  xfm <- io_read_flirt_transform(flirt_file)
  result <- transform_flirt2ras(xfm, source = read_volume(nii_file, header_only = TRUE, method = "oro"), reference = read_volume(nii_file, header_only = TRUE, method = "oro"))

  # Both sides converted: RAS -> RAS
  expect_equal(attr(result$space_from, "orientation"), "RAS")
  expect_equal(attr(result$space_to, "orientation"), "RAS")
})

test_that("transform_flirt2ras accepts matrix input", {
  skip_if_not_installed("RNifti")

  nii_file <- ieegio_sample_data("brain.demosubject.nii.gz")
  skip_if(!file.exists(nii_file), "Sample NIfTI file not available")

  # Pass a raw matrix instead of ieegio_transforms
  result <- transform_flirt2ras(diag(4), source = read_volume(nii_file, header_only = TRUE, method = "oro"), reference = read_volume(nii_file, header_only = TRUE, method = "oro"))

  expect_s3_class(result, "ieegio_transforms")
  expect_equal(attr(result$space_from, "orientation"), "RAS")
  expect_equal(attr(result$space_to, "orientation"), "RAS")
})

test_that("transform_flirt2ras accepts ieegio_volume objects", {
  skip_if_not_installed("RNifti")

  nii_file <- ieegio_sample_data("brain.demosubject.nii.gz")
  skip_if(!file.exists(nii_file), "Sample NIfTI file not available")

  vol <- read_volume(nii_file, header_only = TRUE, method = "oro")

  flirt_file <- create_temp_flirt_file(diag(4))
  on.exit(unlink(flirt_file))

  xfm <- io_read_flirt_transform(flirt_file)
  result <- transform_flirt2ras(xfm, source = vol, reference = vol)

  expect_equal(attr(result$space_from, "orientation"), "RAS")
  expect_equal(attr(result$space_to, "orientation"), "RAS")
})

# ---- Integration tests ----

test_that("FLIRT identity transform preserves coordinates (RAS round-trip)", {
  skip_if_not_installed("RNifti")

  nii_file <- ieegio_sample_data("brain.demosubject.nii.gz")
  skip_if(!file.exists(nii_file), "Sample NIfTI file not available")

  # Identity FLIRT matrix with same source and reference should give
  # identity when converted to RAS
  flirt_file <- create_temp_flirt_file(diag(4))
  on.exit(unlink(flirt_file))

  xfm <- io_read_flirt_transform(flirt_file)
  xfm_ras <- transform_flirt2ras(xfm, source = read_volume(nii_file, header_only = TRUE, method = "oro"), reference = read_volume(nii_file, header_only = TRUE, method = "oro"))

  # The resulting matrix should be close to identity
  # (exact identity if source == reference and FLIRT matrix is identity)
  expect_equal(xfm_ras$data[[1]], diag(4), tolerance = 1e-10)
})

test_that("as_ieegio_transform.character with format='flirt'", {
  flirt_file <- create_temp_flirt_file(diag(4))
  on.exit(unlink(flirt_file))

  xfm <- as_ieegio_transform(flirt_file, format = "flirt")

  expect_s3_class(xfm, "ieegio_transforms")
  expect_equal(attr(xfm$space_from, "orientation"), "FSL")
  expect_equal(attr(xfm$space_to, "orientation"), "FSL")
})
