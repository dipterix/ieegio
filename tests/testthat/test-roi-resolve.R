make_box_volume_roi <- function(threshold_lb = 0.5) {
  arr <- array(0, c(10, 10, 10))
  arr[3:5, 3:5, 3:5] <- 1
  vox2ras <- rbind(cbind(diag(2, 3), c(-10, -10, -10)), c(0, 0, 0, 1))
  as_ieegio_roi(arr, vox2ras = vox2ras, threshold_lb = threshold_lb)
}

make_tetrahedron_roi <- function() {
  vertices <- matrix(
    c(0, 0, 0,
      1, 0, 0,
      0, 1, 0,
      0, 0, 1),
    ncol = 3, byrow = TRUE
  )
  faces <- matrix(
    c(1, 2, 3,
      1, 2, 4,
      1, 3, 4,
      2, 3, 4),
    ncol = 3, byrow = TRUE
  )
  surface <- as_ieegio_surface(
    vertices,
    faces = faces,
    measurements = data.frame(Curv = c(1, 1, 1, -1))
  )
  as_ieegio_roi(surface, threshold_expr = .m1 > 0)
}

make_streamlines_roi <- function(...) {
  short_tract <- cbind(seq(0, 1, length.out = 5), 0, 0)
  long_tract <- cbind(seq(0, 10, length.out = 21), 5, 0)
  streamlines <- as_ieegio_streamlines(
    list(short_tract, long_tract),
    vox2ras = diag(1, 4)
  )
  as_ieegio_roi(streamlines, ...)
}

test_that("resolve_roi_as: `auto` preserves the representation and clears the criteria", {

  rois <- list(
    volume = make_box_volume_roi(),
    pointcloud = as_ieegio_roi(matrix(rnorm(30), ncol = 3)),
    surface = make_tetrahedron_roi(),
    streamlines = make_streamlines_roi(threshold_lb = 2)
  )

  for (type in names(rois)) {
    resolved <- resolve_roi_as(rois[[type]], "auto")

    expect_equal(attr(resolved, "roi_info")$type, type)
    expect_s3_class(resolved, "ieegio_roi")

    # the criteria were applied, so they must not survive into a second pass
    roi_info <- attr(resolved, "roi_info")
    expect_null(roi_info$threshold_lb)
    expect_null(roi_info$threshold_ub)
    expect_null(roi_info$threshold_fun)

    # only a point cloud reports how much space each of its points stands in for
    if (type == "pointcloud") {
      expect_named(roi_info$point_radius, c("min", "median", "q95", "max"))
    } else {
      expect_null(roi_info$point_radius)
    }
  }
})

test_that("resolve_roi_as: volume thresholding and voxel radius", {

  roi <- make_box_volume_roi()
  vox2ras <- roi$transforms$vox2ras

  points <- resolve_roi_as(roi, "pointcloud")
  coords <- t(points$geometry$vertices[seq_len(3), , drop = FALSE])

  # the 3x3x3 block, mapped through `vox2ras`
  expected_ijk <- as.matrix(expand.grid(2:4, 2:4, 2:4))
  expected <- t((vox2ras %*% rbind(t(expected_ijk), 1))[seq_len(3), , drop = FALSE])

  expect_equal(nrow(coords), 27L)
  expect_equal(coords[order(coords[, 1], coords[, 2], coords[, 3]), ],
               expected[order(expected[, 1], expected[, 2], expected[, 3]), ],
               ignore_attr = TRUE)

  # 2mm isotropic grid: the balls overlap at half the spacing, and cover the
  # voxel only out at half the diagonal
  radius <- attr(points, "roi_info")$point_radius
  expect_equal(radius$min, 1)
  expect_equal(radius$max, sqrt(12) / 2)

  # resolving back to a volume keeps the mask, and a volume is not a point cloud
  volume <- resolve_roi_as(roi, "volume")
  expect_equal(sum(volume[]), 27)
  expect_null(attr(volume, "roi_info")$point_radius)
})

test_that("point_radius: an overlap radius and a ladder of covering radii", {

  # ---- volume: a voxel is a box, so covering it reaches the corner ----
  arr <- array(0, c(10, 10, 10))
  arr[3:5, 3:5, 3:5] <- 1
  sizes <- c(1, 1.5, 2)
  vox2ras <- rbind(cbind(diag(sizes), c(-5, -5, -5)), c(0, 0, 0, 1))
  volume <- as_ieegio_roi(arr, vox2ras = vox2ras, threshold_lb = 0.5)

  radius <- attr(resolve_roi_as(volume, "pointcloud"), "roi_info")$point_radius
  expect_equal(radius$min, min(sizes) / 2)
  expect_equal(radius$max, sqrt(sum(sizes^2)) / 2)

  # covering the corner takes more than half the largest spacing, which is
  # exactly what the axis-wise reading misses
  expect_true(radius$max > max(sizes) / 2)

  # each rung really does cover what it claims. A finer grid than the one the
  # radii were derived from keeps this honest.
  distances <- as.vector(box_center_distances(sizes, n = 200L))
  expect_equal(mean(distances <= radius$median), 0.5, tolerance = 1e-3)
  expect_equal(mean(distances <= radius$q95), 0.95, tolerance = 1e-3)
  expect_equal(mean(distances <= radius$max), 1)

  # `min` asks a different question and so is not ordered against the rest: for
  # isotropic voxels the median distance falls just below the overlap radius
  isotropic <- roi_voxel_point_radius(diag(c(2, 2, 2, 1)))
  expect_true(isotropic$median < isotropic$min)

  # ---- streamlines: between samples the tract is one-dimensional ----
  streamlines <- as_ieegio_streamlines(
    list(cbind(seq(0, 1, by = 0.25), 0, 0), cbind(seq(0, 2, by = 0.5), 1, 0)),
    vox2ras = diag(1, 4)
  )
  expect_equal(
    attr(resolve_roi_as(as_ieegio_roi(streamlines), "pointcloud"), "roi_info")$point_radius,
    list(min = 0.125, median = 0.1875, q95 = 0.25, max = 0.25)
  )

  # ---- surface: three right faces and one equilateral one ----
  vertices <- matrix(
    c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1), ncol = 3, byrow = TRUE)
  faces <- matrix(
    c(1, 2, 3, 1, 2, 4, 1, 3, 4, 2, 3, 4), ncol = 3, byrow = TRUE)
  surface <- as_ieegio_surface(vertices, faces = faces)

  # the three right faces are covered by half their hypotenuse; the equilateral
  # face is acute and needs its circumcircle, which reaches farther
  covering <- c(rep(sqrt(2) / 2, 3), sqrt(2) / sqrt(3))
  expect_equal(
    attr(resolve_roi_as(as_ieegio_roi(surface), "pointcloud"), "roi_info")$point_radius,
    list(
      min = sqrt(2) / 2,
      median = unname(stats::quantile(covering, 0.5)),
      q95 = unname(stats::quantile(covering, 0.95)),
      max = sqrt(2) / sqrt(3)
    )
  )

  # half the longest edge would have stopped short of covering the mesh
  expect_true(sqrt(2) / sqrt(3) > sqrt(2) / 2)

  # ---- bare points have no neighborhood of their own ----
  expect_equal(
    attr(
      resolve_roi_as(as_ieegio_roi(matrix(rnorm(30), ncol = 3)), "pointcloud"),
      "roi_info"
    )$point_radius,
    list(min = 0, median = 0, q95 = 0, max = 0)
  )

  # a degenerate or absent grid yields no radius at all
  expect_equal(roi_voxel_point_radius(diag(c(1, 1, 0, 1))), empty_point_radius())
  expect_equal(roi_voxel_point_radius(NULL), empty_point_radius())
})

test_that("box_center_distances: a representative sample of the box", {

  # the sampled octant reproduces the known mean distance from the center of a
  # unit cube, so its quantiles stand in for the whole box
  expect_equal(mean(box_center_distances(c(1, 1, 1))), 0.4803, tolerance = 1e-4)

  # distances run from the center out to the corner
  distances <- box_center_distances(c(1, 1.5, 2))
  expect_true(max(distances) < sqrt(sum(c(1, 1.5, 2)^2)) / 2)
  expect_true(min(distances) > 0)

  # scaling the box scales every distance linearly
  expect_equal(
    box_center_distances(c(2, 3, 4)),
    box_center_distances(c(1, 1.5, 2)) * 2
  )

  # the default grid is fine enough that the quantiles have converged
  coarse <- stats::quantile(box_center_distances(c(1, 1.5, 2)), c(0.5, 0.95))
  fine <- stats::quantile(box_center_distances(c(1, 1.5, 2), n = 200L), c(0.5, 0.95))
  expect_equal(unname(coarse), unname(fine), tolerance = 1e-3)
})

test_that("roi_triangle_radii: the smallest circle covering each triangle", {

  covering <- function(points) {
    roi_triangle_radii(rbind(t(points), 1), matrix(1:3, ncol = 1))
  }

  # acute: the circumcircle, `side / sqrt(3)` for an equilateral triangle
  equilateral <- rbind(c(0, 0, 0), c(1, 0, 0), c(0.5, sqrt(3) / 2, 0))
  expect_equal(covering(equilateral), 1 / sqrt(3))

  # right: the two branches meet here, the hypotenuse being a diameter
  expect_equal(covering(rbind(c(0, 0, 0), c(3, 0, 0), c(0, 4, 0))), 2.5)

  # obtuse: half the longest side, the circumcenter having left the triangle
  expect_equal(covering(rbind(c(0, 0, 0), c(10, 0, 0), c(5, 0.5, 0))), 5)

  # degenerate shapes must not divide by a zero area
  expect_equal(covering(rbind(c(0, 0, 0), c(1, 0, 0), c(2, 0, 0))), 1)
  expect_equal(covering(rbind(c(0, 0, 0), c(1, 0, 0), c(0.5, 1e-9, 0))), 0.5)
  expect_equal(covering(rbind(c(1, 1, 1), c(1, 1, 1), c(1, 1, 1))), 0)

  # a mesh of acute triangles needs more than half its longest edge
  ravetools <- check_ravetools_flag()
  skip_if(isFALSE(ravetools) || !is.function(ravetools$vcg_max_edge_length))

  mesh <- ravetools$vcg_sphere()
  surface <- as_ieegio_surface(
    t(mesh$vb[seq_len(3), ]), faces = t(mesh$it), face_start = 1L)
  radii <- roi_triangle_radii(roi_surface_vertices(surface), roi_surface_faces(surface))

  expect_length(radii, ncol(mesh$it))
  expect_false(anyNA(radii))
  expect_true(max(radii) > ravetools$vcg_max_edge_length(mesh) / 2)
})

test_that("resolve_roi_as: surface threshold drops faces with a rejected corner", {

  roi <- make_tetrahedron_roi()
  resolved <- resolve_roi_as(roi, "surface")

  # vertex 4 fails `.m1 > 0`, so only the face made of vertices 1, 2, 3 survives
  expect_equal(ncol(resolved$geometry$vertices), 3L)
  expect_equal(ncol(resolved$geometry$faces), 1L)
  expect_equal(sort(as.integer(resolved$geometry$faces)), 1:3)
})

test_that("resolve_roi_as: faces are read through `face_start`, not assumed 1-based", {

  roi <- make_tetrahedron_roi()
  reference <- resolve_roi_as(roi, "surface")

  # `face_start` is documented as either 0 or 1. Nothing in the package builds
  # the 0-based form today, but every other face-consuming function
  # re-normalizes on read, and an unshifted 0 index would shear `remap[faces]`.
  variants <- list("zero based" = 0L, "unset" = NULL)

  for (nm in names(variants)) {
    shifted <- roi
    shifted$geometry$faces <- shifted$geometry$faces - 1L
    shifted$geometry$face_start <- variants[[nm]]

    resolved <- resolve_roi_as(shifted, "surface")
    expect_equal(resolved$geometry$faces, reference$geometry$faces, info = nm)
    expect_equal(resolved$geometry$vertices, reference$geometry$vertices, info = nm)
  }
})

test_that("resolve_roi_as: geometry transforms are baked into world coordinates", {

  transform <- rbind(cbind(diag(c(2, 3, 4)), c(1, 2, 3)), c(0, 0, 0, 1))
  vertices <- matrix(rnorm(30), ncol = 3)
  roi <- as_ieegio_roi(as_ieegio_surface(vertices, transform = transform))

  resolved <- resolve_roi_as(roi, "pointcloud")

  expect_equal(
    t(resolved$geometry$vertices[seq_len(3), , drop = FALSE]),
    t((transform %*% rbind(t(vertices), 1))[seq_len(3), , drop = FALSE]),
    ignore_attr = TRUE
  )
})

test_that("resolve_roi_as: point cloud rasterizes and reads back within a voxel", {

  points <- as.matrix(expand.grid(
    seq(-3, 3, by = 1),
    seq(-2, 2, by = 1),
    seq(0, 4, by = 1)
  ))
  roi <- as_ieegio_roi(points)

  volume <- resolve_roi_as(roi, "volume", resolution = 0.5)
  expect_equal(sum(volume[]), nrow(points))

  # every point lands on its own voxel, including the ones on the maximum
  # index plane along each axis
  back <- resolve_roi_as(volume, "pointcloud")
  coords <- t(back$geometry$vertices[seq_len(3), , drop = FALSE])

  expect_equal(nrow(coords), nrow(points))
  expect_equal(apply(coords, 2L, range), apply(points, 2L, range), ignore_attr = TRUE)
})

test_that("resolve_roi_as: streamlines are filtered by arc length", {

  roi <- make_streamlines_roi(threshold_lb = 2)

  resolved <- resolve_roi_as(roi, "streamlines")
  expect_length(resolved, 1L)

  # coordinates are already in world space, so the rebuilt object must not
  # carry a transform that would be applied a second time
  expect_equal(resolved$header$vox2ras, diag(1, 4))
  expect_equal(resolved[[1]]$coords[, 1], seq(0, 10, length.out = 21))

  # the same filter applies whichever representation is asked for
  points <- resolve_roi_as(roi, "pointcloud")
  expect_equal(ncol(points$geometry$vertices), 21L)

  # both tracts survive when only an upper bound rejects the long one
  expect_length(resolve_roi_as(make_streamlines_roi(threshold_ub = 2), "streamlines"), 1L)
  expect_length(resolve_roi_as(make_streamlines_roi(), "streamlines"), 2L)
})

test_that("resolve_roi_as: a non-finite bound selects nothing rather than NA", {

  # `as_ieegio_roi.ieegio_roi` stores `...` verbatim, so a bound can reach the
  # resolvers as `NA` even though the constructors drop one
  streamlines <- as_ieegio_roi(make_streamlines_roi(), threshold_lb = NA)
  expect_true(is.na(attr(streamlines, "roi_info")$threshold_lb))
  expect_length(resolve_roi_as(streamlines, "streamlines"), 2L)
  expect_equal(ncol(resolve_roi_as(streamlines, "pointcloud")$geometry$vertices), 26L)

  # an unbounded volume falls back to "any voxel above zero"
  arr <- array(c(-1, 0, 1, 2, 3, 4, 5, 6), c(2, 2, 2))
  volume <- as_ieegio_roi(
    as_ieegio_roi(arr, vox2ras = diag(1, 4)), threshold_lb = -Inf)
  expect_equal(sum(resolve_roi_as(volume, "volume")[]), 6)
})

test_that("resolve_roi_as: a sparse tract is drawn as a continuous line", {

  # two points ten apart: sampling the stored vertices alone would leave a gap
  streamlines <- as_ieegio_streamlines(
    list(rbind(c(0, 0, 0), c(10, 0, 0))),
    vox2ras = diag(1, 4)
  )
  volume <- resolve_roi_as(as_ieegio_roi(streamlines), "volume", resolution = 0.5)

  expect_equal(sum(volume[]), 21)
})

test_that("resolve_roi_as: streamlines become closed tubes", {

  roi <- make_streamlines_roi(threshold_lb = 2)
  n_sides <- 8L
  n_points <- 21L

  tube <- resolve_roi_as(roi, "surface", tube_radius = 1, tube_sides = n_sides)

  expect_equal(ncol(tube$geometry$vertices), n_points * n_sides + 2L)
  expect_equal(
    ncol(tube$geometry$faces),
    n_sides * 2L * (n_points - 1L) + 2L * n_sides
  )

  vertices <- t(tube$geometry$vertices[seq_len(3), , drop = FALSE])
  expect_false(anyNA(vertices))

  # the tract runs along `x` at y = 5, z = 0, so every ring vertex (the two cap
  # centers excluded) sits exactly one unit from that axis
  rings <- vertices[seq_len(n_points * n_sides), , drop = FALSE]
  radial <- sqrt((rings[, 2] - 5)^2 + rings[, 3]^2)
  expect_equal(radial, rep(1, n_points * n_sides))
})

test_that("tube_mesh_from_polylines: the frame follows a curve without twisting", {

  angle <- seq(0, pi / 2, length.out = 25)
  tract <- cbind(cos(angle) * 10, sin(angle) * 10, 0)
  n_sides <- 6L

  surface <- tube_mesh_from_polylines(list(tract), radius = 1, n_sides = n_sides)
  vertices <- t(surface$geometry$vertices[seq_len(3), , drop = FALSE])

  # each ring must stay in the plane normal to the curve, so its vertices are
  # exactly `radius` from the point that generated them
  centers <- tract[rep(seq_len(nrow(tract)), each = n_sides), , drop = FALSE]
  rings <- vertices[seq_len(nrow(tract) * n_sides), , drop = FALSE]
  expect_equal(sqrt(rowSums((rings - centers)^2)), rep(1, nrow(centers)))

  # a twist-free frame keeps consecutive rings aligned: corresponding vertices
  # advance by roughly the spacing of the curve itself, never by a full ring
  first_ring <- rings[seq_len(n_sides), , drop = FALSE]
  second_ring <- rings[n_sides + seq_len(n_sides), , drop = FALSE]
  step <- sqrt(sum((tract[2, ] - tract[1, ])^2))
  expect_true(all(sqrt(rowSums((second_ring - first_ring)^2)) < step * 2))
})

test_that("tube_mesh_from_polylines: degenerate tracts are dropped", {

  # a single point, a repeated point, and missing coordinates carry no direction
  surface <- tube_mesh_from_polylines(
    list(
      matrix(c(0, 0, 0), ncol = 3),
      rbind(c(1, 1, 1), c(1, 1, 1)),
      rbind(c(0, 0, 0), c(NA, NA, NA))
    ),
    radius = 1
  )

  expect_equal(ncol(surface$geometry$vertices), 1L)
  expect_null(surface$geometry$faces)

  expect_error(tube_mesh_from_polylines(list(), radius = 0), "positive")
})

test_that("resolve_roi_as: unconvertible requests fail loudly", {

  roi <- as_ieegio_roi(matrix(rnorm(9), ncol = 3))
  expect_error(resolve_roi_as(roi, "streamlines"), "streamlines")

  # nothing left after thresholding, so there is no extent to rasterize. A
  # volume keeps its own grid in the same situation and simply comes back empty.
  surface <- as_ieegio_surface(
    matrix(rnorm(12), ncol = 3),
    measurements = data.frame(Curv = rep(0, 4))
  )
  empty <- as_ieegio_roi(surface, threshold_expr = .m1 > 100)
  expect_error(resolve_roi_as(empty, "volume"), "empty")

  vox2ras <- rbind(cbind(diag(1, 3), c(-2, -2, -2)), c(0, 0, 0, 1))
  empty_volume <- as_ieegio_roi(
    array(0, c(5, 5, 5)), vox2ras = vox2ras, threshold_lb = 100)
  expect_equal(sum(resolve_roi_as(empty_volume, "volume")[]), 0)
})

test_that("resolve_roi_as_volume: an explicit grid retargets the mask", {

  arr <- array(0, c(12, 14, 16))
  arr[4:9, 5:10, 6:12] <- 1
  vox2ras_old <- rbind(cbind(diag(c(1, 1.5, 2)), c(-6, -10, -16)), c(0, 0, 0, 1))
  vox2ras_new <- rbind(cbind(diag(0.5, 3), c(-6, -10, -16)), c(0, 0, 0, 1))

  roi <- as_ieegio_roi(arr, vox2ras = vox2ras_old, threshold_lb = 0.5)
  fine <- resolve_roi_as(roi, "volume", dim = c(24, 42, 64), vox2ras = vox2ras_new)

  expect_equal(dim(fine)[seq_len(3)], c(24L, 42L, 64L))
  expect_equal(fine$transforms$vox2ras, vox2ras_new, ignore_attr = TRUE)

  # the same physical box: nearest-neighbor resampling reproduces its volume up
  # to the half-voxel boundary of each grid, not to the voxel count
  expect_equal(
    sum(fine[]) * 0.5^3,
    6 * 6 * 7 * (1 * 1.5 * 2),
    tolerance = 0.1
  )
  expect_null(attr(fine, "roi_info")$point_radius)
})

test_that("resample_volume_vox2ras: the fallback matches ravetools", {

  ravetools <- check_ravetools_flag()
  skip_if(isFALSE(ravetools) || !is.function(ravetools$resample_3d_volume))

  arr <- array(0L, c(12, 14, 16))
  arr[4:9, 5:10, 6:12] <- 1L
  vox2ras_old <- rbind(cbind(diag(c(1, 1.5, 2)), c(-6, -10, -16)), c(0, 0, 0, 1))
  vox2ras_new <- rbind(cbind(diag(0.5, 3), c(-6, -10, -16)), c(0, 0, 0, 1))
  new_dim <- c(24, 42, 64)

  with_ravetools <- resample_volume_vox2ras(
    arr, new_dim, vox2ras_old, vox2ras_new, na_fill = 0L)

  Sys.setenv(IEEGIO_NO_RAVETOOLS = "1")
  on.exit({ Sys.unsetenv("IEEGIO_NO_RAVETOOLS") }, add = TRUE)
  naive <- resample_volume_vox2ras(
    arr, new_dim, vox2ras_old, vox2ras_new, na_fill = 0L)
  Sys.unsetenv("IEEGIO_NO_RAVETOOLS")

  expect_equal(as.integer(naive), as.integer(with_ravetools))
})

test_that("resolve_roi_as: a filled surface encloses more than its shell", {

  ravetools <- check_ravetools_flag()
  skip_if(isFALSE(ravetools) || !is.function(ravetools$fill_surface))

  sphere <- ravetools$vcg_sphere()
  surface <- as_ieegio_surface(
    t(sphere$vb[seq_len(3), ]) * 10,
    faces = t(sphere$it),
    face_start = 1L
  )
  roi <- as_ieegio_roi(surface)

  shell <- resolve_roi_as(roi, "volume", resolution = 1)
  filled <- resolve_roi_as(roi, "volume", resolution = 1, fill_surface = TRUE)

  expect_true(sum(filled[]) > sum(shell[]) * 4)

  # a sphere of radius 10 holds roughly 4189 unit voxels; the closing that
  # `fill_surface` performs dilates it a little
  expect_true(sum(filled[]) > 4000 && sum(filled[]) < 6000)
})

test_that("resolve_roi_as: a point cloud can be wrapped in a surface", {

  ravetools <- check_ravetools_flag()
  skip_if(isFALSE(ravetools) || !is.function(ravetools$vcg_isosurface))

  sphere <- ravetools$vcg_sphere()
  roi <- as_ieegio_roi(t(sphere$vb[seq_len(3), ]) * 10)

  surface <- resolve_roi_as(roi, "surface", resolution = 1)

  expect_equal(attr(surface, "roi_info")$type, "surface")
  expect_true(ncol(surface$geometry$faces) > 0)

  radius <- sqrt(colSums(surface$geometry$vertices[seq_len(3), , drop = FALSE]^2))
  expect_true(all(radius > 8 & radius < 12))
})
