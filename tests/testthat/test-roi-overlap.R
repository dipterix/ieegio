skip_without_collision <- function() {
  ravetools <- check_ravetools_flag()
  skip_if(isFALSE(ravetools) || !is.function(ravetools$vcg_detect_collision))
  ravetools
}

# A sphere of radius 5 centred on the origin, so distances are known exactly
sphere_roi <- function(radius = 5) {
  ravetools <- check_ravetools_flag()
  mesh <- ravetools$vcg_sphere()
  as_ieegio_roi(as_ieegio_surface(
    t(mesh$vb[seq_len(3), ]) * radius,
    faces = t(mesh$it),
    face_start = 1L
  ))
}

# A box mask, `size` mm isotropic, spanning the origin
box_volume_roi <- function(size = 1) {
  arr <- array(0, c(20, 20, 20))
  arr[5:15, 5:15, 5:15] <- 1
  vox2ras <- rbind(cbind(diag(size, 3), rep(-10 * size, 3)), c(0, 0, 0, 1))
  as_ieegio_roi(arr, vox2ras = vox2ras, threshold_lb = 0.5)
}

tract_roi <- function() {
  as_ieegio_roi(as_ieegio_streamlines(
    list(
      cbind(seq(-9, 9, by = 1), 0, 0),    # runs through the sphere
      cbind(seq(-9, 9, by = 1), 40, 0)    # far away, never overlaps
    ),
    vox2ras = diag(1, 4)
  ))
}

test_that("detect_roi_overlap: every pairing of representations runs", {

  skip_without_collision()

  regions <- list(
    surface = sphere_roi(),
    pointcloud = as_ieegio_roi(rbind(c(0, 0, 0), c(5, 0, 0), c(50, 0, 0))),
    volume = box_volume_roi(),
    streamlines = tract_roi()
  )

  expected_class <- c(
    surface = "ieegio_surface",
    pointcloud = "ieegio_surface",
    volume = "ieegio_volume",
    streamlines = "ieegio_streamlines"
  )

  for (name_x in names(regions)) {
    for (name_y in names(regions)) {
      label <- sprintf("x = %s, y = %s", name_x, name_y)

      result <- detect_roi_overlap(regions[[name_x]], regions[[name_y]])

      expect_s3_class(result, "ieegio_roi_overlap_result")
      expect_named(
        result,
        c("overlapped", "hit_ratio", "early_stop", "annotated",
          "collision_detection", "mode_x", "mode_y"),
        info = label
      )
      expect_s3_class(result$annotated, expected_class[[name_y]])
      expect_s3_class(result$annotated, "ieegio_roi")

      # the resolved types, not the arguments, which both said "auto"
      expect_equal(result$mode_x, name_x, info = label)
      expect_equal(result$mode_y, name_y, info = label)

      expect_true(all(result$hit_ratio >= 0 & result$hit_ratio <= 1), info = label)

      # `collision_detection` is the raw output, and its unit count is what the
      # annotation was written against
      summary_y <- result$collision_detection$summary$y
      expect_length(result$collision_detection$hit_unit, summary_y$n_units)
    }
  }
})

test_that("detect_roi_overlap: results are one per unit of y", {

  skip_without_collision()

  surface <- sphere_roi()
  resolved <- resolve_roi_as(surface, "surface")
  n_face <- ncol(resolved$geometry$faces)
  n_vertex <- ncol(resolved$geometry$vertices)
  expect_true(n_face > n_vertex)

  # a mesh's unit is a FACE, but measurements are per vertex
  result <- detect_roi_overlap(box_volume_roi(), surface, mode_y = "surface")
  expect_length(result$collision_detection$hit_unit, n_face)
  expect_equal(nrow(result$annotated$measurements$data_table), n_vertex)

  # so a surface reports both ratios, and the face one is the raw unit ratio
  expect_named(result$hit_ratio, c("vertex", "face"))
  expect_equal(
    unname(result$hit_ratio[["face"]]),
    mean(result$collision_detection$hit_unit)
  )

  # a point cloud's unit is a point
  points <- rbind(c(0, 0, 0), c(5, 0, 0), c(6, 0, 0), c(50, 0, 0))
  result <- detect_roi_overlap(surface, as_ieegio_roi(points), radius = 1.5)
  expect_length(result$collision_detection$hit_unit, nrow(points))
  expect_equal(result$hit_ratio, mean(result$collision_detection$hit_unit))

  # a streamline's unit is a whole tract, so there is one answer per tract and
  # nothing is written along the points
  result <- detect_roi_overlap(surface, tract_roi(), mode_y = "streamlines")
  annotated <- result$annotated
  expect_length(result$collision_detection$hit_unit, 2L)
  expect_length(annotated$data, 2L)
  expect_equal(annotated$header$property_names, c("Overlap", "distance"))
  expect_null(annotated$data[[1]]$scalars)

  # only the tract passing through the sphere overlaps it
  expect_equal(unname(annotated$data[[1]]$properties[["Overlap"]]), 1)
  expect_equal(unname(annotated$data[[2]]$properties[["Overlap"]]), 0)
  expect_false(is.na(annotated$data[[1]]$properties[["distance"]]))
  expect_true(is.na(annotated$data[[2]]$properties[["distance"]]))
  expect_equal(result$hit_ratio, 0.5)
})

test_that("detect_roi_overlap: `Overlap` and `distance` agree with the hits", {

  skip_without_collision()

  # the sphere has radius 5, so these points sit at 0 and 1 from its surface
  points <- as_ieegio_roi(rbind(c(0, 0, 0), c(5, 0, 0), c(6, 0, 0), c(50, 0, 0)))
  result <- detect_roi_overlap(sphere_roi(), points, radius = 1.5)

  expect_equal(result$collision_detection$hit_unit, c(FALSE, TRUE, TRUE, FALSE))

  measurements <- result$annotated$measurements$data_table
  expect_named(measurements, c("Overlap", "distance"))
  expect_equal(measurements$Overlap, c(0, 1, 1, 0))
  expect_equal(measurements$distance, c(NA, 0, 1, NA))

  # the origin is inside, and only counts once the interior is included
  centre <- as_ieegio_roi(rbind(c(0, 0, 0)))
  expect_false(detect_roi_overlap(sphere_roi(), centre)$overlapped)
  expect_true(
    detect_roi_overlap(sphere_roi(), centre, include_interior = TRUE)$overlapped)
})

test_that("detect_roi_overlap: only a volume inflates the radius", {

  skip_without_collision()

  coarse <- box_volume_roi(size = 2)
  fine <- box_volume_roi(size = 1)
  surface <- sphere_roi()
  points <- as_ieegio_roi(rbind(c(0, 0, 0)))

  radius_of <- function(roi) {
    side <- roi_overlap_side(roi, "auto")
    side$inflation
  }

  # half the voxel diagonal, for 2mm and 1mm isotropic grids
  expect_equal(radius_of(coarse), sqrt(12) / 2)
  expect_equal(radius_of(fine), sqrt(3) / 2)

  # nothing else loses extent on the way to `ravetools`
  expect_equal(radius_of(surface), 0)
  expect_equal(radius_of(points), 0)
  expect_equal(radius_of(tract_roi()), 0)

  # the inflation is a lower bound, so a larger caller radius simply wins
  expect_equal(max(0, radius_of(coarse), radius_of(fine)), sqrt(12) / 2)
  expect_equal(max(0, radius_of(coarse), radius_of(surface)), sqrt(12) / 2)
  expect_equal(max(0, radius_of(surface), radius_of(surface)), 0)
  expect_equal(max(7, radius_of(coarse), radius_of(surface)), 7)

  # and it changes the answer: a bare point 1mm outside the sphere is clear,
  # but the same location as a 2mm voxel reaches far enough to touch, because
  # half that voxel's diagonal is 1.73mm
  outside <- as_ieegio_roi(rbind(c(6, 0, 0)))
  expect_false(detect_roi_overlap(sphere_roi(), outside)$overlapped)

  # voxel [14, 11, 11] on a 2mm grid starting at -20 sits at exactly (6, 0, 0)
  near_miss <- array(0, c(20, 20, 20))
  near_miss[14, 11, 11] <- 1
  near_volume <- as_ieegio_roi(
    near_miss,
    vox2ras = rbind(cbind(diag(2, 3), rep(-20, 3)), c(0, 0, 0, 1)),
    threshold_lb = 0.5
  )
  expect_equal(
    t(resolve_roi_as(near_volume, "pointcloud")$geometry$vertices[seq_len(3), ]),
    matrix(c(6, 0, 0), nrow = 1)
  )
  expect_true(detect_roi_overlap(sphere_roi(), near_volume)$overlapped)
})

test_that("detect_roi_overlap: a volume is painted back onto its own grid", {

  skip_without_collision()

  vox2ras <- rbind(cbind(diag(c(1, 1.5, 2)), c(-10, -14, -18)), c(0, 0, 0, 1))
  arr <- array(0, c(20, 20, 20))
  arr[5:15, 5:15, 5:15] <- 1
  volume <- as_ieegio_roi(arr, vox2ras = vox2ras, threshold_lb = 0.5)

  result <- detect_roi_overlap(sphere_roi(), volume, mode_y = "volume")
  annotated <- result$annotated

  expect_s3_class(annotated, "ieegio_volume")
  expect_equal(dim(annotated)[seq_len(3)], c(20L, 20L, 20L))
  expect_equal(annotated$transforms$vox2ras, vox2ras, ignore_attr = TRUE)

  # `NA` cannot survive into a volume, so the three states are told apart by two
  # negative sentinels that a measured value can never take
  values <- annotated[]
  mask_index <- which(arr > 0.5)

  hit <- result$collision_detection$hit_unit
  expect_equal(sum(values == -2), length(arr) - length(mask_index))
  expect_equal(sum(values == -1), sum(!hit))
  expect_equal(sum(values >= 0), sum(hit))

  # the documented thresholds: `>= -1` is the region, `>= 0` is the overlap
  expect_equal(which(values >= -1), mask_index)
  expect_equal(
    sort(values[values >= 0]),
    sort(result$collision_detection$representation$distance)
  )

  # exact contact is a distance of 0, and stays distinguishable from a voxel
  # that was merely never measured
  expect_true(sum(values == 0) > 0)

  # and the volume survives a `NIfTI` round trip with both thresholds intact
  path <- tempfile(fileext = ".nii.gz")
  on.exit({ unlink(path) }, add = TRUE)
  write_volume(annotated, path)
  restored <- read_volume(path)[]
  expect_equal(which(restored >= -1), mask_index)
  expect_equal(sum(restored >= 0), sum(values >= 0))
})

test_that("detect_roi_overlap: `early_stop` answers every unit either way", {

  skip_without_collision()

  # Two overlapping spheres, so hundreds of faces really do overlap. The old
  # `early_stop` abandoned the whole mesh at its first hit and reported one of
  # them; scanning is now per unit, so both settings must find them all.
  sphere <- check_ravetools_flag()$vcg_sphere()
  x <- as_ieegio_roi(sphere)
  sphere$vb[seq_len(2), ] <- sphere$vb[seq_len(2), ] + 1
  y <- as_ieegio_roi(sphere)

  full <- detect_roi_overlap(x, y, mode_x = "volume", early_stop = FALSE)
  early <- detect_roi_overlap(x, y, mode_x = "volume", early_stop = TRUE)

  expect_true(sum(full$collision_detection$hit_unit) > 100)
  expect_false(anyNA(full$collision_detection$hit_unit))

  # a face is its own unit, so the setting changes nothing at all for a surface
  expect_identical(
    full$collision_detection$hit_unit,
    early$collision_detection$hit_unit
  )
  expect_equal(full$hit_ratio, early$hit_ratio)
  expect_equal(
    full$annotated$measurements$data_table,
    early$annotated$measurements$data_table
  )

  # the flag is reported back as it was used
  expect_false(full$early_stop)
  expect_true(early$early_stop)

  # points behave the same way, being their own units too
  points <- as_ieegio_roi(rbind(c(0, 0, 0), c(5, 0, 0), c(5, 0, 0)))
  expect_identical(
    detect_roi_overlap(sphere_roi(), points, early_stop = FALSE)$collision_detection$hit_unit,
    detect_roi_overlap(sphere_roi(), points, early_stop = TRUE)$collision_detection$hit_unit
  )
})

test_that("detect_roi_overlap: a tract's gaps do not split it into two units", {

  skip_without_collision()

  # any row that is not fully finite is a separator to `ravetools`, so an
  # internal gap would silently make one tract into two units and shift every
  # annotation after it
  broken <- cbind(seq(-9, 9, by = 1), 0, 0)
  broken[10, ] <- NA
  streamlines <- as_ieegio_roi(as_ieegio_streamlines(
    list(broken, cbind(seq(-9, 9, by = 1), 40, 0)),
    vox2ras = diag(1, 4)
  ))

  result <- detect_roi_overlap(sphere_roi(), streamlines)

  expect_equal(result$collision_detection$summary$y$n_units, 2L)
  expect_equal(result$collision_detection$hit_unit, c(TRUE, FALSE))
  expect_length(result$annotated$data, 2L)
  expect_equal(result$hit_ratio, 0.5)
})

test_that("detect_roi_overlap: degenerate tracts never reach the test", {

  skip_without_collision()

  empty <- cbind(c(NA, NA), c(NA, NA), c(NA, NA))
  hits <- cbind(seq(-9, 9, by = 1), 0, 0)
  misses <- cbind(seq(-9, 9, by = 1), 40, 0)

  # resolving drops whatever the gap removal leaves empty, so only the two real
  # tracts are ever tested and the ratio is over those
  streamlines <- as_ieegio_roi(as_ieegio_streamlines(
    list(hits, empty, misses), vox2ras = diag(1, 4)))
  result <- detect_roi_overlap(sphere_roi(), streamlines)

  expect_equal(result$collision_detection$hit_unit, c(TRUE, FALSE))
  expect_length(result$annotated$data, 2L)
  expect_equal(result$hit_ratio, 0.5)

  # a region left with no tract at all still answers, rather than erroring or
  # ratioing to `NaN`
  nothing <- as_ieegio_roi(as_ieegio_streamlines(
    list(empty, empty), vox2ras = diag(1, 4)))
  result <- detect_roi_overlap(sphere_roi(), nothing)

  expect_false(result$overlapped)
  expect_length(result$collision_detection$hit_unit, 0L)
  expect_length(result$annotated$data, 0L)
  expect_equal(result$hit_ratio, 0)
})

test_that("sanitize_streamlines: only tracts that are lines survive", {

  item <- function(coords, scalars = NULL) {
    list(
      coords = coords,
      num_points = if (is.matrix(coords)) { nrow(coords) } else { 0L },
      scalars = scalars
    )
  }
  kept <- function(...) { length(sanitize_streamlines(list(item(...)))) }

  # a clean tract passes through untouched
  clean <- sanitize_streamlines(list(item(cbind(0:4, 0, 0))))
  expect_length(clean, 1L)
  expect_equal(nrow(clean[[1]]$coords), 5L)
  expect_equal(clean[[1]]$num_points, 5L)

  # a gap is closed over, and `num_points` follows the rows that survived
  gapped <- sanitize_streamlines(list(item(
    rbind(c(0, 0, 0), c(1, 0, 0), c(NA, NA, NA), c(3, 0, 0)))))
  expect_length(gapped, 1L)
  expect_equal(nrow(gapped[[1]]$coords), 3L)
  expect_equal(gapped[[1]]$num_points, 3L)
  expect_false(anyNA(gapped[[1]]$coords))

  # removing rows can leave one point, or none, and neither is a line. These
  # are the two cases a bare row filter returns as degenerate tracts.
  expect_equal(kept(rbind(c(0, 0, 0), c(NA, NA, NA), c(NA, NA, NA))), 0L)
  expect_equal(kept(rbind(c(NA, NA, NA), c(NA, NA, NA))), 0L)

  # a single point is not a line whether or not it is finite
  expect_equal(kept(rbind(c(7, 7, 7))), 0L)
  expect_equal(kept(rbind(c(NA, NA, NA))), 0L)

  # nor is anything that cannot be read as coordinates at all
  expect_equal(kept(NULL), 0L)
  expect_equal(kept(seq_len(6)), 0L)
  expect_equal(kept(cbind(0:4, 0)), 0L)

  # per-point scalars follow the surviving rows, and leave with a dropped tract
  scaled <- sanitize_streamlines(list(item(
    rbind(c(0, 0, 0), c(1, 0, 0), c(NA, NA, NA), c(3, 0, 0)),
    scalars = cbind(a = 1:4)
  )))
  expect_equal(nrow(scaled[[1]]$scalars), 3L)
  expect_equal(unname(scaled[[1]]$scalars[, "a"]), c(1, 2, 4))
  expect_equal(
    length(sanitize_streamlines(list(item(
      rbind(c(NA, NA, NA), c(NA, NA, NA)), scalars = cbind(a = 1:2))))),
    0L
  )

  # an empty region stays empty rather than erroring
  expect_equal(sanitize_streamlines(list()), list())
})

test_that("resolve_roi_as: a stub tract reaches none of the resolvers", {

  # the case that motivated the single helper: a real tract beside a
  # single-point one. Every resolver must see only the real tract.
  real <- cbind(seq(0, 4, by = 1), 0, 0)
  roi <- as_ieegio_roi(as_ieegio_streamlines(
    list(real, rbind(c(7, 7, 7))), vox2ras = diag(1, 4)))

  points <- resolve_roi_as(roi, "pointcloud")
  vertices <- t(points$geometry$vertices[seq_len(3), , drop = FALSE])
  expect_equal(nrow(vertices), nrow(real))
  expect_false(anyNA(vertices))
  expect_equal(vertices, real, ignore_attr = TRUE)

  expect_length(resolve_roi_as(roi, "streamlines"), 1L)

  # and an `NA` stub never leaks a missing vertex into a point cloud
  roi <- as_ieegio_roi(as_ieegio_streamlines(
    list(real, rbind(c(NA, NA, NA))), vox2ras = diag(1, 4)))
  points <- resolve_roi_as(roi, "pointcloud")
  expect_equal(ncol(points$geometry$vertices), nrow(real))
  expect_false(anyNA(points$geometry$vertices))

  # the volume rasterizes the real tract alone
  volume <- resolve_roi_as(roi, "volume", resolution = 0.5)
  expect_true(sum(volume[]) > 0)
  expect_false(anyNA(volume[]))
})

test_that("as_ieegio_streamlines: `sanitize` trims on read, and only there", {

  good <- cbind(seq(0, 4, by = 1), 0, 0)
  streamlines <- as_ieegio_streamlines(
    list(good, rbind(c(9, 9, 9))), vox2ras = diag(1, 4))

  path <- tempfile(fileext = ".trk")
  on.exit({ unlink(path) }, add = TRUE)
  write_streamlines(streamlines, path)

  # the low-level reader is untouched, which is what keeps the byte-level
  # round trip in `test-trk.R` honest
  verbatim <- io_read_trk(path, half_voxel_offset = FALSE)
  expect_length(verbatim$data, 2L)
  expect_equal(verbatim$header$n_count, 2L)

  # the default drops the stub, and the header count follows
  trimmed <- read_streamlines(path, half_voxel_offset = FALSE)
  expect_length(trimmed, 1L)
  expect_equal(trimmed$header$n_count, 1L)
  expect_equal(nrow(trimmed[[1]]$coords), nrow(good))

  # and the file can still be read exactly as written
  whole <- read_streamlines(path, half_voxel_offset = FALSE, sanitize = FALSE)
  expect_length(whole, 2L)
  expect_equal(whole$header$n_count, 2L)
})

test_that("resolve_roi_as: streamline gaps are removed and stubs dropped", {

  good <- cbind(seq(0, 4, by = 1), 0, 0)
  gappy <- good
  gappy[3, ] <- NA
  orphan <- rbind(c(9, 9, 9), c(NA, NA, NA))
  empty <- cbind(c(NA, NA), c(NA, NA), c(NA, NA))

  roi <- as_ieegio_roi(as_ieegio_streamlines(
    list(good, gappy, orphan, empty), vox2ras = diag(1, 4)))
  expect_length(roi, 4L)

  resolved <- resolve_roi_as(roi, "streamlines")

  # the gap is removed and the tract closes over it; a tract left with one
  # point or none is not a line and is dropped
  expect_length(resolved, 2L)
  expect_equal(vapply(resolved[], function(item) { nrow(item$coords) }, 0L),
               c(5L, 4L))
  expect_false(anyNA(do.call("rbind", lapply(resolved[], function(item) {
    item$coords
  }))))

  # `num_points` follows the rows that survived
  expect_equal(
    vapply(resolved$data, function(item) { item$num_points }, 0L),
    c(5L, 4L)
  )
})

test_that("resolve_roi_as: per-point scalars follow the removed rows", {

  gappy <- cbind(seq(0, 4, by = 1), 0, 0)
  gappy[3, ] <- NA

  roi <- as_ieegio_roi(as_ieegio_streamlines(
    list(list(coords = gappy, num_points = 5L,
              scalars = cbind(a = 1:5, b = 6:10), properties = c(p = 1))),
    vox2ras = diag(1, 4)
  ))
  resolved <- resolve_roi_as(roi, "streamlines")

  item <- resolved[[1]]
  expect_equal(nrow(item$coords), 4L)
  expect_equal(nrow(item$scalars), 4L)
  expect_equal(unname(item$scalars[, "a"]), c(1, 2, 4, 5))
  expect_equal(unname(item$scalars[, "b"]), c(6, 7, 9, 10))
})

test_that("resolve_roi_as: a gap no longer poisons the arc-length filter", {

  # one missing coordinate used to make the whole tract's length `NA`, which
  # then failed every bound it was compared against
  gappy <- cbind(seq(0, 4, by = 1), 0, 0)
  gappy[3, ] <- NA
  short <- cbind(seq(0, 1, by = 0.5), 5, 0)

  streamlines <- as_ieegio_streamlines(list(gappy, short), vox2ras = diag(1, 4))

  # closing over the gap leaves a length of 4, so the tract survives a bound
  # that the short one does not
  expect_length(
    resolve_roi_as(as_ieegio_roi(streamlines, threshold_lb = 3.5), "streamlines"),
    1L
  )
  expect_length(
    resolve_roi_as(as_ieegio_roi(streamlines), "streamlines"),
    2L
  )
})

test_that("print.ieegio_roi_overlap_result: reports the two types and the ratio", {

  skip_without_collision()

  points <- as_ieegio_roi(rbind(c(0, 0, 0), c(5, 0, 0), c(50, 0, 0)))
  output <- capture.output(print(detect_roi_overlap(sphere_roi(), points)))

  expect_match(output[[1]], "overlap")
  expect_true(any(grepl("x: surface", output, fixed = TRUE)))
  expect_true(any(grepl("y: pointcloud", output, fixed = TRUE)))
  expect_true(any(grepl("33.3%", output, fixed = TRUE)))
  expect_true(any(grepl("Early stop: FALSE", output, fixed = TRUE)))

  # a surface names both of its ratios
  output <- capture.output(print(detect_roi_overlap(points, sphere_roi())))
  expect_true(any(grepl("vertex", output, fixed = TRUE)))
  expect_true(any(grepl("face", output, fixed = TRUE)))

  # and a region that does not touch says so
  far <- as_ieegio_roi(rbind(c(500, 0, 0)))
  output <- capture.output(print(detect_roi_overlap(sphere_roi(), far)))
  expect_match(output[[1]], "do not overlap")
})

test_that("detect_roi_overlap: unusable arguments fail loudly", {

  skip_without_collision()

  expect_error(
    detect_roi_overlap(sphere_roi(), sphere_roi(), radius = -1),
    "non-negative"
  )
  expect_error(
    detect_roi_overlap(sphere_roi(), sphere_roi(), radius = c(1, 2)),
    "single"
  )
})

test_that("detect_roi_overlap: `include_interior` falls back off a non-surface", {

  skip_without_collision()

  # only a surface has an interior, so asking a volume for one is answered
  # without it rather than refused
  expect_message(
    result <- detect_roi_overlap(
      box_volume_roi(), sphere_roi(), include_interior = TRUE),
    "surface"
  )

  # and the setting that was actually used is on the record
  expect_false(result$collision_detection$summary$include_interior)

  # the answer is the one `include_interior = FALSE` would have given
  quiet <- detect_roi_overlap(
    box_volume_roi(), sphere_roi(), include_interior = FALSE)
  expect_identical(
    result$collision_detection$hit_unit,
    quiet$collision_detection$hit_unit
  )

  # a surface `x` still gets the interior test, and it still changes the answer
  centre <- as_ieegio_roi(rbind(c(0, 0, 0)))
  expect_silent(
    inside <- detect_roi_overlap(sphere_roi(), centre, include_interior = TRUE))
  expect_true(inside$overlapped)
  expect_true(inside$collision_detection$summary$include_interior)
  expect_false(detect_roi_overlap(sphere_roi(), centre)$overlapped)
})

test_that("roi_faces_to_vertices: the nearest incident face speaks for a vertex", {

  # two triangles sharing the edge 2-3
  faces <- cbind(c(1L, 2L, 3L), c(2L, 3L, 4L))

  # a distance takes the minimum over the faces meeting at the vertex
  expect_equal(
    roi_faces_to_vertices(c(5, 2), faces, n_vertex = 4L, reduce = min),
    c(5, 2, 2, 2)
  )

  # a mask takes any hit at all
  expect_equal(
    roi_faces_to_vertices(c(0, 1), faces, n_vertex = 4L, reduce = max),
    c(0, 1, 1, 1)
  )

  # unevaluated faces are left out of the reduction rather than counted as
  # misses; a vertex touched by nothing evaluated stays unknown
  expect_equal(
    roi_faces_to_vertices(c(NA, 3), faces, n_vertex = 4L, reduce = min),
    c(NA, 3, 3, 3)
  )
  expect_equal(
    roi_faces_to_vertices(c(NA, NA), faces, n_vertex = 4L, reduce = min),
    rep(NA_real_, 4)
  )

  # a vertex with no face at all
  expect_equal(
    roi_faces_to_vertices(c(1, 1), faces, n_vertex = 6L, reduce = min),
    c(1, 1, 1, 1, NA, NA)
  )
})

test_that("as_ieegio_streamlines: scalars are counted per column, not per value", {

  # `scalars` is a `num_points x n_scalars` matrix, so its length counts values
  streamlines <- as_ieegio_streamlines(
    list(
      list(coords = cbind(0:4, 0, 0), num_points = 5L,
           scalars = cbind(a = rnorm(5), b = rnorm(5)), properties = c(p = 1)),
      list(coords = cbind(0:2, 1, 0), num_points = 3L,
           scalars = cbind(a = rnorm(3), b = rnorm(3)), properties = c(p = 2))
    ),
    vox2ras = diag(1, 4)
  )

  expect_equal(streamlines$header$n_scalars, 2L)
  expect_equal(streamlines$header$scalar_names, c("a", "b"))

  # properties are one value each, and are named as properties
  expect_equal(streamlines$header$n_properties, 1L)
  expect_true(all(grepl("^Property", streamlines$header$property_names)))

  # a tract carrying neither still reports zero of each
  plain <- as_ieegio_streamlines(list(cbind(0:4, 0, 0)), vox2ras = diag(1, 4))
  expect_equal(plain$header$n_scalars, 0L)
  expect_equal(plain$header$n_properties, 0L)
})

test_that("as_ieegio_volume: a non-finite value forces a floating point type", {

  # `Inf - round(Inf)` is `NaN`, which used to make the integer test return `NA`
  # and error out whenever every finite value happened to be a whole number
  arr <- array(Inf, c(4, 4, 4))
  arr[seq_len(5)] <- c(0, 1, 0, 1, 1)

  volume <- as_ieegio_volume(arr, vox2ras = diag(1, 4))
  expect_equal(sum(is.infinite(volume[])), 59L)
  expect_equal(sum(volume[] == 1, na.rm = TRUE), 3L)

  # an array with nothing finite in it is still convertible
  expect_s3_class(
    as_ieegio_volume(array(Inf, c(3, 3, 3)), vox2ras = diag(1, 4)),
    "ieegio_volume"
  )

  # an all-finite array is unaffected by that guard
  whole <- array(seq_len(64), c(4, 4, 4))
  expect_equal(as_ieegio_volume(whole, vox2ras = diag(1, 4))[], whole,
               ignore_attr = TRUE)
})
