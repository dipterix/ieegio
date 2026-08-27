# Build closed triangular tubes ("generalized cylinders") around polylines.
# Streamlines are curves without thickness, so any operation that needs a solid
# body (collision detection, rendering, filling) has to give them one first.

vec3_cross <- function(a, b) {
  c(
    a[[2]] * b[[3]] - a[[3]] * b[[2]],
    a[[3]] * b[[1]] - a[[1]] * b[[3]],
    a[[1]] * b[[2]] - a[[2]] * b[[1]]
  )
}

# Returns `NULL` rather than `NaN` for a zero-length vector so that callers are
# forced to decide what a degenerate direction should mean
vec3_normalize <- function(x) {
  norm <- sqrt(sum(x^2))
  if (!isTRUE(norm > 0)) {
    return(NULL)
  }
  x / norm
}

# Seed vector for the tube frame: cross the tangent with whichever axis it is
# least aligned with, which keeps the cross product well conditioned
tube_frame_seed <- function(tangent) {
  axis <- c(0, 0, 0)
  axis[[which.min(abs(tangent))]] <- 1
  vec3_normalize(vec3_cross(tangent, axis))
}

# Rodrigues rotation carrying `u` along with the rotation that takes unit vector
# `from` to unit vector `to`. Transporting the frame this way (rather than
# re-deriving it at every point) is what keeps the tube from twisting.
vec3_rotate_between <- function(u, from, to) {
  axis <- vec3_cross(from, to)
  sin_theta <- sqrt(sum(axis^2))
  if (!isTRUE(sin_theta > 1e-12)) {
    # `from` and `to` are parallel; there is nothing to rotate
    return(u)
  }
  cos_theta <- sum(from * to)
  axis <- axis / sin_theta
  u * cos_theta + vec3_cross(axis, u) * sin_theta +
    axis * sum(axis * u) * (1 - cos_theta)
}

# Unit tangents along a polyline: central difference on the interior, one-sided
# at the two ends
polyline_tangents <- function(points) {
  n <- nrow(points)
  tangents <- matrix(0, nrow = n, ncol = 3)
  tangents[1, ] <- points[2, ] - points[1, ]
  tangents[n, ] <- points[n, ] - points[n - 1L, ]
  if (n > 2) {
    tangents[seq(2, n - 1L), ] <-
      points[seq(3, n), , drop = FALSE] - points[seq_len(n - 2L), , drop = FALSE]
  }

  previous <- NULL
  for (ii in seq_len(n)) {
    unit <- vec3_normalize(tangents[ii, ])
    if (is.null(unit)) {
      # a doubling-back segment has no direction of its own; keep going the way
      # the curve was already headed
      unit <- previous %||% c(0, 0, 1)
    }
    tangents[ii, ] <- unit
    previous <- unit
  }
  tangents
}

# Length of each segment joining consecutive points, in the unit of the
# coordinates themselves
polyline_segment_lengths <- function(coords) {
  n <- nrow(coords)
  if (n < 2) {
    return(numeric(0L))
  }
  coords <- coords[, seq_len(3), drop = FALSE]
  sqrt(rowSums((coords[-1, , drop = FALSE] - coords[-n, , drop = FALSE])^2))
}

# Arc length of a polyline
polyline_length <- function(coords) {
  sum(polyline_segment_lengths(coords))
}

# Insert evenly spaced samples so no gap between consecutive points exceeds
# `spacing`. This is how a polyline gets "drawn" into a voxel grid: sampling the
# stored vertices alone leaves holes wherever they sit more than a voxel apart.
densify_polyline <- function(points, spacing) {
  n <- nrow(points)
  if (n < 2 || !isTRUE(spacing > 0)) {
    return(points)
  }
  starts <- points[-n, , drop = FALSE]
  deltas <- points[-1, , drop = FALSE] - starts
  n_sub <- pmax(ceiling(sqrt(rowSums(deltas^2)) / spacing), 1L)

  filled <- lapply(seq_len(n - 1L), function(ii) {
    # the closing sample of each segment is the opening sample of the next one,
    # so it is dropped here and the final point is appended once at the end
    fractions <- seq(0, 1, length.out = n_sub[[ii]] + 1L)[seq_len(n_sub[[ii]])]
    matrix(starts[ii, ], nrow = length(fractions), ncol = 3, byrow = TRUE) +
      outer(fractions, deltas[ii, ])
  })
  rbind(do.call("rbind", filled), points[n, , drop = FALSE])
}

# Drop missing rows and repeated points; a zero-length segment carries no
# tangent and would corrupt the frame
polyline_clean <- function(points) {
  if (!is.matrix(points)) {
    points <- as.matrix(points)
  }
  if (ncol(points) < 3) {
    return(NULL)
  }
  points <- points[, seq_len(3), drop = FALSE]
  storage.mode(points) <- "double"
  dimnames(points) <- NULL

  points <- points[rowSums(is.na(points)) == 0, , drop = FALSE]
  n <- nrow(points)
  if (n < 2) {
    return(NULL)
  }

  duplicated_point <-
    rowSums(abs(points[-1, , drop = FALSE] - points[-n, , drop = FALSE])) == 0
  points <- points[c(TRUE, !duplicated_point), , drop = FALSE]
  if (nrow(points) < 2) {
    return(NULL)
  }
  points
}

# Vertices are returned as an `nx3` matrix and faces as an `mx3` matrix of
# 1-based vertex indices, ready for `as_ieegio_surface`
tube_mesh_single <- function(points, radius, n_sides, capped) {
  n <- nrow(points)
  tangents <- polyline_tangents(points)

  theta <- seq(0, 2 * pi, length.out = n_sides + 1L)[seq_len(n_sides)]
  cos_theta <- cos(theta)
  sin_theta <- sin(theta)

  vertices <- matrix(NA_real_, nrow = n * n_sides, ncol = 3)
  u <- tube_frame_seed(tangents[1, ])

  for (ii in seq_len(n)) {
    if (ii > 1) {
      u <- vec3_rotate_between(u, tangents[ii - 1L, ], tangents[ii, ])
      # re-orthogonalize: floating point drift accumulates over a long tract and
      # would slowly tilt the rings out of the normal plane
      u <- vec3_normalize(u - tangents[ii, ] * sum(u * tangents[ii, ]))
      if (is.null(u)) {
        u <- tube_frame_seed(tangents[ii, ])
      }
    }
    v <- vec3_cross(tangents[ii, ], u)
    ring <- outer(cos_theta, u) + outer(sin_theta, v)
    vertices[seq_len(n_sides) + (ii - 1L) * n_sides, ] <-
      matrix(points[ii, ], nrow = n_sides, ncol = 3, byrow = TRUE) + radius * ring
  }

  # Two triangles per side of every band between consecutive rings, wound so the
  # normals point away from the axis
  side <- seq_len(n_sides)
  side_next <- side %% n_sides + 1L
  bands <- lapply(seq_len(n - 1L), function(ii) {
    lower <- (ii - 1L) * n_sides
    upper <- ii * n_sides
    rbind(
      cbind(lower + side, lower + side_next, upper + side_next),
      cbind(lower + side, upper + side_next, upper + side)
    )
  })
  faces <- do.call("rbind", bands)

  if (capped) {
    # Close both ends with a triangle fan around an extra center vertex, so the
    # tube is a solid body rather than an open tube
    center_first <- n * n_sides + 1L
    center_last <- n * n_sides + 2L
    vertices <- rbind(vertices, points[1, ], points[n, ])
    last <- (n - 1L) * n_sides
    faces <- rbind(
      faces,
      # the opening cap faces backwards, hence the reversed winding
      cbind(side_next, side, center_first),
      cbind(last + side, last + side_next, center_last)
    )
  }

  storage.mode(faces) <- "integer"
  list(vertices = vertices, faces = faces)
}

tube_mesh_from_polylines <- function(tracts, radius, n_sides = 6L, capped = TRUE) {
  n_sides <- max(as.integer(n_sides), 3L)
  radius <- as.double(radius)[[1]]
  if (!isTRUE(radius > 0)) {
    stop("`tube_mesh_from_polylines`: `radius` must be a single positive number")
  }

  meshes <- drop_nulls(lapply(tracts, function(points) {
    points <- polyline_clean(points)
    if (is.null(points)) {
      return(NULL)
    }
    tube_mesh_single(points, radius = radius, n_sides = n_sides, capped = capped)
  }))

  if (!length(meshes)) {
    # degrade the same way `volume_to_surface` does on an empty mask
    return(as_ieegio_surface(matrix(c(0, 0, 0), ncol = 3)))
  }

  n_vertices <- vapply(meshes, function(mesh) { nrow(mesh$vertices) }, 0L)
  offsets <- cumsum(c(0L, n_vertices))

  vertices <- do.call("rbind", lapply(meshes, "[[", "vertices"))
  faces <- do.call("rbind", lapply(seq_along(meshes), function(ii) {
    meshes[[ii]]$faces + offsets[[ii]]
  }))

  as_ieegio_surface(vertices, faces = faces, face_start = 1L)
}
