
# Overlap detects if y is within radius of x, it's asym algorithm
# The radius needs to be inflated for algorithms in ravetools
# the radius for ravetools depends on the mode:
# for volume (v) vs v, min of radius is max(rx, ry), since the comparison in ravetools will be points vs points: you need to convert volumes to pointcloud for comparison
# v vs points, it's rx (or ry for x=p vs y=v). since the volume will be converted to points
# v vs streamlines, rx, same reason
# v vs surface, rx. same reason
# p vs p is 0, p vs streamlines is 0, p vs surface is zero
# streamlines vs streamlines is 0
# streamlines vs surface is 0
# surface vs surface is 0
# rx/ry is the max point radius
# Reason: ravetools::vcg_detect_collision does not handle volume, so you need to convert volumes to points for collision detection,

# Return types: depending on mode_y
# the annotated y should have mode_y, not the original y (counter example if you use original y: y can be a character and resolve_roi_as reads in the file as ROI)
# The only unsupported by ravetools is volume. If mode_y is volume, then annotated y should be volume with the same vox2ras as the underlying y ROI

# ---- One side of the test -----------------------------------------------------

# Reduce a region to something `ravetools` understands, together with the
# tolerance that reduction costs. Only a volume loses anything: `ravetools` has
# no volume mode, so it is collided as its voxel centers, and `inflation` is the
# radius each of those centers stands in for. Point clouds are genuinely points,
# and surfaces and streamlines are carried exactly, so all three inflate by zero.
roi_overlap_side <- function(x, mode) {
  roi <- resolve_roi_as(x, mode)
  type <- attr(roi, "roi_info")$type

  volume <- NULL
  inflation <- 0
  items <- NULL

  if (type == "volume") {
    volume <- roi
    roi <- resolve_roi_as(roi, "pointcloud")
    inflation <- attr(roi, "roi_info")$point_radius$max %||% 0
  }

  switch(
    type,
    "streamlines" = {
      # `ravetools` reads any row that is not fully finite as a separator, so a
      # tract carrying an internal gap would silently become two units and shift
      # every annotation after it. Read the tracts once, here, and carry the
      # list forward: the annotation writes back onto this same list rather than
      # re-reading `roi[]`, so the two cannot drift apart.
      items <- sanitize_streamlines(roi[])

      # a separator after every tract, the last one included: a trailing
      # separator closes the chain before it rather than opening a new one, so
      # the units stay one-to-one with the tracts
      geometry <- roi_bind_points(lapply(items, function(item) {
        rbind(item$coords[, seq_len(3), drop = FALSE], NA_real_)
      }))
      collision_mode <- "segments"
      n_units <- length(items)
    },
    "surface" = {
      # `ensure_mesh3d` reads an `ieegio_surface` directly, and a resolved
      # surface carries no transform that could be applied a second time
      geometry <- roi
      collision_mode <- "mesh"
      n_units <- ncol(roi$geometry$faces)
    },
    {
      # a point cloud, or a volume already reduced to its voxel centers
      geometry <- t(roi$geometry$vertices[seq_len(3), , drop = FALSE])
      collision_mode <- "points"
      n_units <- nrow(geometry)
    }
  )

  list(
    roi = roi,
    volume = volume,
    type = type,
    geometry = geometry,
    mode = collision_mode,
    n_units = n_units,
    # the tracts the geometry was built from; `NULL` for every other type
    items = items,
    inflation = inflation
  )
}

# ---- Reporting the result -----------------------------------------------------

# A per-face result read at the vertices. A vertex belongs to every face that
# lists it, so `reduce` decides which of them speaks for it: the nearest one for
# a distance, any hit at all for a mask. `NA` faces were never evaluated and are
# left out of the reduction rather than counted as misses.
roi_faces_to_vertices <- function(values, faces, n_vertex, reduce) {
  vertex_values <- rep(NA_real_, n_vertex)
  if (!length(faces)) {
    return(vertex_values)
  }

  # `faces` is 3 x m and unrolls column-major, so each face's value repeats
  # three times alongside its three corners
  vertex_index <- as.vector(faces)
  face_values <- rep(values, each = 3L)

  keep <- !is.na(face_values)
  if (any(keep)) {
    reduced <- tapply(face_values[keep], vertex_index[keep], reduce)
    vertex_values[as.integer(names(reduced))] <- as.numeric(reduced)
  }
  vertex_values
}

# Ratio of the units of `y` that overlap `x`. Units that could not be tested -
# an empty tract, say - are left out of both sides rather than counted as
# misses, and a set with nothing testable in it ratios to zero rather than
# `NaN`.
roi_hit_ratio <- function(hit) {
  testable <- sum(!is.na(hit))
  if (!testable) {
    return(0)
  }
  sum(hit, na.rm = TRUE) / testable
}

# Write the result back onto `y`, in whatever idiom its representation uses.
# `hit` and `distance` both carry one value per unit of `y`.
roi_annotate_overlap <- function(side, hit, distance) {
  roi <- side$roi
  overlapped <- hit %in% TRUE

  switch(
    side$type,
    "volume" = {
      # the points are exactly the voxel centers, so their indices come back
      # through the volume's own `ras2vox`
      volume <- side$volume
      vox2ras <- roi_volume_vox2ras(volume)
      dm <- dim(volume)[seq_len(3)]

      # A volume cannot carry `NA`: `as_ieegio_volume` turns it into 0, which in
      # a distance field would be indistinguishable from exact contact. Two
      # negative sentinels keep the three states apart, and because a measured
      # value is never negative they also make the volume directly thresholdable:
      #
      #   -2         outside the region, never tested
      #   -1         inside the region, but no overlap found
      #   >= 0       overlapping: the distance to `x`
      #
      # so `>= -1` recovers the region's own mask and `>= 0` the overlap.
      annotated <- array(-2, dim = dm)

      if (nrow(side$geometry)) {
        ijk <- round(
          solve(vox2ras) %*% rbind(t(side$geometry), 1)
        )[seq_len(3), , drop = FALSE] + 1L
        index <- colSums((ijk - 1L) * cumprod(c(1, dm))[seq_len(3)]) + 1L

        painted <- rep(-1, length(index))
        painted[overlapped] <- distance[overlapped]
        annotated[index] <- painted
      }

      cal_max <- suppressWarnings(max(annotated[annotated >= 0]))
      if (!is.finite(cal_max)) {
        cal_max <- 0
      }
      annotated <- as_ieegio_volume(
        annotated,
        vox2ras = vox2ras,
        cal_max = cal_max,

        # Using -1 and -2 for special meaning but the value min and max
        # should be 0 to max
        cal_min = 0
      )
    },
    "streamlines" = {
      # a unit is a whole tract, so the result is a per-tract property rather
      # than anything laid along the points. These are the very tracts the
      # geometry was built from, so a unit and an item are the same thing.
      items <- lapply(seq_along(side$items), function(ii) {
        item <- side$items[[ii]]
        item$properties <- c(
          item$properties[
            !names(item$properties) %in% c("Overlap", "distance")
          ],
          Overlap = as.numeric(overlapped[[ii]]),
          distance = distance[[ii]]
        )
        item
      })

      # a region whose tracts were all dropped as degenerate leaves nothing to
      # read a name off, so the names are stated rather than discovered
      if (length(items)) {
        property_names <- names(items[[1]]$properties)
      } else {
        property_names <- c("Overlap", "distance")
      }

      # coordinates are already in world space, hence the identity `vox2ras`
      annotated <- as_ieegio_streamlines.default(
        x = items,
        vox2ras = diag(1, 4),
        scalar_names = roi$header$scalar_names,
        property_names = property_names
      )
    },
    "surface" = {
      # reported per face, so both columns are read at the vertices: a vertex
      # overlaps if any face meeting there does, and takes the nearest of them
      faces <- roi_surface_faces(roi)
      n_vertex <- ncol(roi$geometry$vertices)

      annotated <- as_ieegio_surface(
        t(roi$geometry$vertices[seq_len(3), , drop = FALSE]),
        faces = t(faces),
        face_start = 1L,
        measurements = data.frame(
          Overlap = roi_faces_to_vertices(
            values = as.numeric(overlapped),
            faces = faces, n_vertex = n_vertex, reduce = max
          ),
          distance = roi_faces_to_vertices(
            values = distance,
            faces = faces, n_vertex = n_vertex, reduce = min
          )
        )
      )
    },
    {
      annotated <- as_ieegio_surface(
        t(roi$geometry$vertices[seq_len(3), , drop = FALSE]),
        measurements = data.frame(
          Overlap = as.numeric(overlapped),
          distance = distance
        )
      )
    }
  )

  finalize_resolved_roi(annotated, type = side$type)
}

# ---- The test -----------------------------------------------------------------

# Reports, for every unit of `y`, whether it comes within `radius` of `x`. The
# test is asymmetric: `x` is indexed once and `y` is queried against it, so the
# results line up with `y` and the larger region belongs in `x`.
#
# A unit is `ravetools`' own notion, and it is coarser than an element: a point
# for a point cloud or a volume's voxel centers, a face for a surface, and a
# whole tract for streamlines.
#
# Both regions must already be in one coordinate space. Nothing here transforms
# them: a volume contributes its `vox2ras`, streamlines their `header$vox2ras`,
# and a surface whichever transform `geometry$transforms` lists first, which for
# a `GIFTI` carrying several is not necessarily the scanner one.
#
# `early_stop` only chooses which element inside a unit gets reported: `FALSE`
# measures them all and keeps the closest, `TRUE` stops at the first one found
# to hit. Every unit is answered either way, and both a hit and a distance come
# back either way. Since a point and a face are each their own unit, the setting
# changes nothing for them, and matters only for streamlines - where a tract's
# reported distance is then that of its first overlapping segment rather than
# its nearest one.
#
# `hit` is `NA` for a unit with nothing to test, such as a tract left empty or
# holding a single point. Surfaces, point clouds, and streamlines keep that `NA`
# as it stands, since an `R` table holds it fine. A volume cannot -
# `as_ieegio_volume` turns `NA` into 0, which a distance field would read as
# exact contact - so it uses two negative sentinels instead, which a measured
# value can never take:
#
#   -2     outside the region, never tested
#   -1     inside the region, but no overlap
#   >= 0   overlapping: the distance to `x`
#
# Thresholding the annotated volume at `>= -1` therefore recovers the region's
# own mask, and at `>= 0` the voxels that overlap.
detect_roi_overlap <- function(
    x, y,
    mode_x = c("auto", "volume", "pointcloud", "streamlines", "surface"),
    mode_y = c("auto", "volume", "pointcloud", "streamlines", "surface"),
    radius = 0,
    early_stop = FALSE,
    include_interior = FALSE) {

  mode_x <- match.arg(mode_x)
  mode_y <- match.arg(mode_y)

  ravetools <- check_ravetools_flag()
  if (isFALSE(ravetools) || !is.function(ravetools$vcg_detect_collision)) {
    stop(
      "`detect_roi_overlap` requires the `ravetools` package. Please install ",
      "it to detect overlaps between regions of interest."
    )
  }

  radius <- as.double(radius)
  if (length(radius) != 1 || !isTRUE(is.finite(radius)) || radius < 0) {
    stop("`detect_roi_overlap`: `radius` must be a single non-negative number")
  }
  early_stop <- isTRUE(as.logical(early_stop))
  include_interior <- isTRUE(as.logical(include_interior))

  side_x <- roi_overlap_side(x, mode_x)
  side_y <- roi_overlap_side(y, mode_y)

  if (include_interior && side_x$mode != "mesh") {
    stop(
      "`detect_roi_overlap`: `include_interior = TRUE` counts whatever lies ",
      "inside `x`, so `x` must resolve to a surface; it resolved to ",
      sQuote(side_x$type), "."
    )
  }

  # the inflation is a lower bound on the tolerance rather than something added
  # to it: it only gives a volume back the extent it lost becoming points
  radius <- max(radius, side_x$inflation, side_y$inflation)

  collision <- ravetools$vcg_detect_collision(
    x = side_x$geometry,
    y = side_y$geometry,
    mode_x = side_x$mode,
    mode_y = side_y$mode,
    radius = radius,
    # "whole" would stop at the first hit anywhere and report no units at all,
    # so it is never used here
    test_level = if (early_stop) { "unit" } else { "element" },
    include_interior = include_interior
  )

  n_units <- collision$summary$y$n_units
  if (!isTRUE(n_units == side_y$n_units)) {
    stop(
      "`detect_roi_overlap`: `ravetools` reported ", n_units, " units for `y`, ",
      "but it was built with ", side_y$n_units, ". The two must agree for the ",
      "result to be written back onto the right part of the region."
    )
  }

  hit <- collision$hit_unit

  # `representation` lists the hits only, keyed by the unit they belong to
  distance <- rep(NA_real_, n_units)
  representation <- collision$representation
  if (nrow(representation)) {
    distance[representation$unit] <- representation$distance
  }

  if (side_y$type == "surface") {
    # a face-level ratio and its vertex-level reduction are different numbers,
    # so a surface reports both
    vertex_hit <- roi_faces_to_vertices(
      values = as.numeric(hit %in% TRUE),
      faces = roi_surface_faces(side_y$roi),
      n_vertex = ncol(side_y$roi$geometry$vertices),
      reduce = max
    )
    hit_ratio <- c(
      vertex = roi_hit_ratio(vertex_hit),
      face = roi_hit_ratio(hit)
    )
  } else {
    hit_ratio <- roi_hit_ratio(hit)
  }

  structure(
    list(
      overlapped = collision$collide,
      hit_ratio = hit_ratio,
      early_stop = early_stop,
      annotated = roi_annotate_overlap(side_y, hit = hit, distance = distance),
      collision_detection = collision,
      mode_x = side_x$type,
      mode_y = side_y$type
    ),
    class = "ieegio_roi_overlap_result"
  )
}

#' @export
print.ieegio_roi_overlap_result <- function(x, ...) {
  if (isTRUE(x$overlapped)) {
    verdict <- "overlap"
  } else {
    verdict <- "do not overlap"
  }

  ratio <- sprintf("%s %.1f%%", names(x$hit_ratio), x$hit_ratio * 100)
  if (!length(names(x$hit_ratio))) {
    ratio <- sprintf("%.1f%%", x$hit_ratio * 100)
  }

  cat(
    c(
      sprintf("<ieegio ROI overlap: `x` and `y` %s>", verdict),
      sprintf("  x: %s", x$mode_x),
      sprintf("  y: %s (annotated)", x$mode_y),
      sprintf("  Units of `y` overlapping `x`: %s", paste(ratio, collapse = ", ")),
      sprintf("  Early stop: %s", x$early_stop),
      ""
    ),
    sep = "\n"
  )

  invisible(x)
}
