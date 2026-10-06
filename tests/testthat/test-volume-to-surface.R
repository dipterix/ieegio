# volume_to_surface: decimation of large surfaces and the choice of smoother,
# on a 30^3 cross of three bars (a surface of a few thousand vertices)

cross_volume <- function() {
  volume <- array(0, dim = rep(30, 3))
  volume[11:20, 11:20, 3:28] <- 1
  volume[3:28, 11:20, 11:20] <- 1
  volume[11:20, 3:28, 11:20] <- 1
  volume
}

surface_vertices <- function(surf) {
  surf$geometry$vertices[1:3, , drop = FALSE]
}

test_that("volume_to_surface decimates a surface with more than max_vertices", {
  skip_if_not(package_installed("ravetools"))
  skip_if_not(is.function(asNamespace("ravetools")$vcg_decimate))
  volume <- cross_volume()

  # lambda < 0 turns smoothing off, leaving decimation alone
  full <- volume_to_surface(volume, vox2ras = diag(1, 4), lambda = -1)
  n_full <- ncol(surface_vertices(full))

  capped <- volume_to_surface(volume, vox2ras = diag(1, 4), lambda = -1,
                              max_vertices = n_full / 4)
  n_capped <- ncol(surface_vertices(capped))
  expect_lt(n_capped, n_full / 3)
  expect_gt(n_capped, n_full / 6)

  # at or under the cap the surface is untouched
  same <- volume_to_surface(volume, vox2ras = diag(1, 4), lambda = -1,
                            max_vertices = n_full)
  expect_equal(surface_vertices(same), surface_vertices(full))
})

test_that("volume_to_surface smooths at full resolution before decimating", {
  skip_if_not(package_installed("ravetools"))
  skip_if_not(is.function(asNamespace("ravetools")$vcg_decimate))
  volume <- cross_volume()
  raw <- ravetools::vcg_isosurface(volume, threshold_lb = 0.5,
                                   vox_to_ras = diag(1, 4))
  cap <- ncol(raw$vb) / 4

  # the Laplacian works in mesh steps: the same lambda on a decimated mesh
  # smooths several times more in millimeters, so the order matters
  smoothed <- ravetools::vcg_smooth_implicit(
    raw, lambda = 0.2, use_mass_matrix = TRUE, fix_border = TRUE,
    use_cot_weight = FALSE, degree = 2)
  expected <- ravetools::vcg_decimate(smoothed, ratio = cap / ncol(raw$vb))

  surf <- volume_to_surface(volume, vox2ras = diag(1, 4), max_vertices = cap)
  expect_equal(surface_vertices(surf), expected$vb[1:3, ])
})

test_that("volume_to_surface decimates first when full-resolution smoothing fails", {
  skip_if_not(package_installed("ravetools"))
  skip_if_not(is.function(asNamespace("ravetools")$vcg_decimate))
  volume <- cross_volume()
  raw <- ravetools::vcg_isosurface(volume, threshold_lb = 0.5,
                                   vox_to_ras = diag(1, 4))
  cap <- ncol(raw$vb) / 4

  # stands in for a surface above the memory limit of vcg_smooth_implicit,
  # which takes millions of vertices to reach for real
  real_smooth <- ravetools::vcg_smooth_implicit
  local_mocked_bindings(
    vcg_smooth_implicit = function(mesh, ...) {
      if (ncol(mesh$vb) >= ncol(raw$vb)) stop("needs about 9 GiB of memory")
      real_smooth(mesh, ...)
    },
    .package = "ravetools"
  )
  expected <- real_smooth(
    ravetools::vcg_decimate(raw, ratio = cap / ncol(raw$vb)),
    lambda = 0.2, use_mass_matrix = TRUE, fix_border = TRUE,
    use_cot_weight = FALSE, degree = 2)

  expect_message(
    surf <- volume_to_surface(volume, vox2ras = diag(1, 4), max_vertices = cap),
    "decimating it first"
  )
  expect_equal(surface_vertices(surf), expected$vb[1:3, ])
})

test_that("volume_to_surface smooths explicitly with mris_smooth when asked", {
  skip_if_not(package_installed("ravetools"))
  skip_if_not(is.function(asNamespace("ravetools")$mris_smooth))
  volume <- cross_volume()

  explicit <- volume_to_surface(volume, vox2ras = diag(1, 4),
                                smooth_method = "explicit",
                                smooth_iterations = 5)
  raw <- ravetools::vcg_isosurface(volume, threshold_lb = 0.5,
                                   vox_to_ras = diag(1, 4))
  expected <- ravetools::mris_smooth(raw, niterations = 5)
  expect_equal(surface_vertices(explicit), expected$vb[1:3, ])

  implicit <- volume_to_surface(volume, vox2ras = diag(1, 4))
  expect_false(isTRUE(all.equal(surface_vertices(implicit),
                                surface_vertices(explicit))))

  expect_error(volume_to_surface(volume, vox2ras = diag(1, 4),
                                 smooth_method = "unknown"))
})
