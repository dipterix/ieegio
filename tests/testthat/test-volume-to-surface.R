# volume_to_surface: decimation of large surfaces and the choice of smoother,
# on a 30^3 cross of three bars (a surface of a few thousand vertices), and a
# thin slab (a surface of more than 20000 vertices)

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

test_that("volume_to_surface decimates before smoothing when over max_vertices", {
  skip_if_not(package_installed("ravetools"))
  skip_if_not(is.function(asNamespace("ravetools")$vcg_decimate))
  volume <- cross_volume()
  raw <- ravetools::vcg_isosurface(volume, threshold_lb = 0.5,
                                   vox_to_ras = diag(1, 4))
  cap <- ncol(raw$vb) / 4

  # the Laplacian works in mesh steps: the same lambda on a decimated mesh
  # smooths several times more in millimeters, so the order matters. Implicit
  # smoothing needs memory that grows quickly with the surface, so a surface
  # above the cap is decimated first, then smoothed once
  expected <- ravetools::vcg_smooth_implicit(
    ravetools::vcg_decimate(raw, ratio = cap / ncol(raw$vb)),
    lambda = 0.2, use_mass_matrix = TRUE, fix_border = TRUE,
    use_cot_weight = FALSE, degree = 2)

  surf <- volume_to_surface(volume, vox2ras = diag(1, 4), max_vertices = cap)
  expect_equal(surface_vertices(surf), expected$vb[1:3, ])
})

test_that("volume_to_surface smooths only small surfaces without vcg_decimate", {
  skip_if_not(package_installed("ravetools"))

  # stands in for ravetools 0.3.2 and earlier, whose implicit smoother
  # crashes on large surfaces and which cannot decimate
  if (is.function(asNamespace("ravetools")$vcg_decimate)) {
    local_mocked_bindings(vcg_decimate = NULL, .package = "ravetools")
  }

  # small surface: smoothed at full resolution, the cap cannot apply
  volume <- cross_volume()
  raw <- ravetools::vcg_isosurface(volume, threshold_lb = 0.5,
                                   vox_to_ras = diag(1, 4))
  expected <- ravetools::vcg_smooth_implicit(
    raw, lambda = 0.2, use_mass_matrix = TRUE, fix_border = TRUE,
    use_cot_weight = FALSE, degree = 2)
  surf <- volume_to_surface(volume, vox2ras = diag(1, 4),
                            max_vertices = ncol(raw$vb) / 4)
  expect_equal(surface_vertices(surf), expected$vb[1:3, ])

  # large surface: not smoothed, with a warning
  slab <- array(0, dim = c(110, 110, 8))
  slab[3:108, 3:108, 3:6] <- 1
  raw <- ravetools::vcg_isosurface(slab, threshold_lb = 0.5,
                                   vox_to_ras = diag(1, 4))
  expect_gt(ncol(raw$vb), 20000)
  expect_warning(
    surf <- volume_to_surface(slab, vox2ras = diag(1, 4)),
    "no smoothing is applied"
  )
  expect_equal(surface_vertices(surf), raw$vb[1:3, ])

  # nothing to warn about when smoothing is turned off
  expect_no_warning(volume_to_surface(slab, vox2ras = diag(1, 4), lambda = -1))
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

  # explicit smoothing needs memory linear in the surface: never decimated
  capped <- volume_to_surface(volume, vox2ras = diag(1, 4),
                              smooth_method = "explicit",
                              smooth_iterations = 5,
                              max_vertices = ncol(raw$vb) / 4)
  expect_equal(surface_vertices(capped), expected$vb[1:3, ])

  implicit <- volume_to_surface(volume, vox2ras = diag(1, 4))
  expect_false(isTRUE(all.equal(surface_vertices(implicit),
                                surface_vertices(explicit))))

  expect_error(volume_to_surface(volume, vox2ras = diag(1, 4),
                                 smooth_method = "unknown"))
})

test_that("volume_to_surface falls back to vcg_smooth_explicit without mris_smooth", {
  skip_if_not(package_installed("ravetools"))

  # stands in for ravetools older than 0.3.0
  if (is.function(asNamespace("ravetools")$mris_smooth)) {
    local_mocked_bindings(mris_smooth = NULL, .package = "ravetools")
  }
  volume <- cross_volume()
  raw <- ravetools::vcg_isosurface(volume, threshold_lb = 0.5,
                                   vox_to_ras = diag(1, 4))
  expected <- ravetools::vcg_smooth_explicit(raw, type = "laplace",
                                             iteration = 5L)

  expect_warning(
    surf <- volume_to_surface(volume, vox2ras = diag(1, 4),
                              smooth_method = "explicit",
                              smooth_iterations = 5,
                              max_vertices = ncol(raw$vb) / 4),
    "vcg_smooth_explicit"
  )
  expect_equal(surface_vertices(surf), expected$vb[1:3, ])
})

test_that("volume_to_surface returns the iso-surface as is with smooth_method none", {
  skip_if_not(package_installed("ravetools"))
  volume <- cross_volume()
  raw <- ravetools::vcg_isosurface(volume, threshold_lb = 0.5,
                                   vox_to_ras = diag(1, 4))

  surf <- volume_to_surface(volume, vox2ras = diag(1, 4),
                            smooth_method = "none",
                            max_vertices = ncol(raw$vb) / 4)
  expect_equal(surface_vertices(surf), raw$vb[1:3, ])
})
