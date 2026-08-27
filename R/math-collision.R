
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

#' @name detect_roi_overlap
#' @title Detect whether two regions of interest overlap
#' @description
#' Reports, for every unit of \code{y}, whether it comes within \code{radius} of
#' \code{x}, and hands back \code{y} with that answer written onto it. The test
#' is asymmetric: \code{x} is indexed once and \code{y} is streamed against it,
#' so the results line up with \code{y}, and the larger or repeatedly reused
#' region belongs in \code{x}.
#'
#' @param x,y the two regions, or anything \code{\link{as_ieegio_roi}} accepts;
#' each is put through \code{\link{resolve_roi_as}} before being tested
#' @param mode_x,mode_y representation to resolve each region into first:
#' \code{"auto"} (default, keep whatever the region already is),
#' \code{"volume"}, \code{"pointcloud"}, \code{"surface"}, or
#' \code{"streamlines"}
#' @param radius distance tolerance; an overlap is reported when the distance is
#' at most \code{radius}. Default is \code{0}, meaning literal contact. A volume
#' raises this to a floor of its own; see 'Details'
#' @param early_stop whether to stop measuring a unit at its first overlapping
#' element instead of keeping the closest one; default is \code{FALSE}. Every
#' unit is answered either way; see 'Details'
#' @param include_interior whether geometry lying strictly inside a closed
#' \code{x} counts as overlapping even when it never comes within \code{radius}
#' of the surface itself; default is \code{FALSE}. Only a surface has an
#' interior, so when \code{x} resolves to anything else this is reported and
#' dropped rather than raised as an error, and the test falls back to contact
#' alone. The setting that was actually used is recorded in
#' \code{collision_detection$summary$include_interior}
#' @param ... ignored, present for compatibility with \code{print}
#'
#' @details
#' Answers are reported per \emph{unit} of \code{y}, which is coarser than a
#' vertex or a point: a point for a point cloud, a voxel center for a volume, a
#' \strong{face} for a surface, and a \strong{whole tract} for streamlines. A
#' surface of 642 vertices and 1280 faces therefore yields 1280 answers, and a
#' bundle of 20 tracts yields 20, however many points each holds.
#'
#' \strong{Both regions must already share one coordinate space.} Nothing here
#' transforms them into a common one: a volume contributes its \code{vox2ras},
#' streamlines their \code{header$vox2ras}, and a surface whichever transform
#' \code{geometry$transforms} lists first, which for a \verb{GIFTI} carrying
#' several is not necessarily the scanner one. Comparing a region in
#' \code{"MNI152"} against one in \code{"ScannerAnat"} returns confident
#' nonsense rather than an error.
#'
#' \code{radius} has a lower bound that the regions themselves set.
#' \pkg{ravetools} has no volume mode, so a volume is tested as its voxel
#' centers, and each center then stands in for a voxel it can no longer
#' describe. The radius actually used is
#' \code{max(radius, inflation_x, inflation_y)}, where a volume's inflation is
#' half its voxel diagonal and every other representation inflates by zero -
#' a point cloud is genuinely points, and surfaces and streamlines are carried
#' exactly. Two volumes therefore floor at the larger of their two, a volume
#' against anything else floors at its own, and a pair without a volume leaves
#' \code{radius} untouched.
#'
#' \code{early_stop} chooses only which element inside a unit gets reported, not
#' how many units are looked at: \code{FALSE} measures every element and keeps
#' the closest, \code{TRUE} stops at the first that overlaps. Both a hit and a
#' distance come back either way. Since a point and a face are each their own
#' unit, the setting changes nothing for them, and matters only for streamlines,
#' where a tract's reported distance becomes that of its first overlapping
#' segment rather than its nearest one.
#'
#' Tracts that are not lines are dropped before the test: a tract needs at least
#' two points to have any segment, and points with missing coordinates are
#' removed first. A region can therefore report fewer tracts than it was built
#' with.
#'
#' @section Annotating \code{y}:
#'
#' \code{annotated} is the resolved \code{y}, in whatever \code{mode_y} asked
#' for, carrying the result in that representation's own idiom:
#'
#' \describe{
#'   \item{point cloud, surface}{two vertex measurements, \code{Overlap}
#'   (\code{1} or \code{0}) and \code{distance} (the distance, or \code{NA}
#'   where clear). A surface is answered per face, so both are read at the
#'   vertices: a vertex overlaps if any face meeting there does, and takes the
#'   nearest of them.}
#'   \item{streamlines}{the same two as per-tract \emph{properties}, one value
#'   each, since a unit is a whole tract.}
#'   \item{volume}{voxel values on the region's own grid and \code{vox2ras}. A
#'   volume cannot hold \code{NA} - it becomes \code{0}, which a distance field
#'   would read as exact contact - so two negative sentinels keep the three
#'   states apart, neither of which a measured distance can take:
#'   \code{-2} outside the region and never tested, \code{-1} inside it but not
#'   overlapping, and \code{>= 0} overlapping, the value being the distance.
#'   Thresholding at \code{>= -1} therefore recovers the region's own mask, and
#'   at \code{>= 0} the part of it that overlaps.}
#' }
#'
#' @returns An \code{"ieegio_roi_overlap_result"} list with
#' \describe{
#'   \item{\code{overlapped}}{whether the two regions overlap at all, taking
#'   each as a whole.}
#'   \item{\code{hit_ratio}}{the ratio, between 0 and 1, of the units of
#'   \code{y} that overlap \code{x}. Units that could not be tested are left out
#'   of both sides rather than counted as misses, and a region with nothing
#'   testable in it gives \code{0} rather than \code{NaN}. A surface reports two,
#'   \code{c(vertex = , face = )}, a face-level answer and its vertex-level
#'   reduction being different numbers.}
#'   \item{\code{early_stop}}{the setting actually used.}
#'   \item{\code{annotated}}{the resolved \code{y}, annotated as above.}
#'   \item{\code{collision_detection}}{the raw
#'   \code{\link[ravetools]{vcg_detect_collision}} output, untouched.}
#'   \item{\code{mode_x}, \code{mode_y}}{the representations the two regions
#'   resolved to, which is what was tested, rather than the arguments, which may
#'   both have said \code{"auto"}.}
#' }
#'
#' @seealso \code{\link{as_ieegio_roi}} records how a region is derived, and
#' \code{\link{resolve_roi_as}} applies that description; this function consumes
#' the result of both.
#'
#' @examples
#'
#' if (interactive()) {
#'
#' # ---- Two spheres, offset so that they partly overlap -----------------
#' sphere <- ravetools::vcg_sphere()
#' x <- as_ieegio_roi(sphere)
#'
#' sphere$vb[1:2, ] <- sphere$vb[1:2, ] + 1
#' y <- as_ieegio_roi(sphere)
#'
#' result <- detect_roi_overlap(x, y)
#' result
#'
#' # `y` is answered per face, and carries the answer at its vertices
#' length(result$collision_detection$hit_unit)
#' head(result$annotated$measurements$data_table)
#'
#' # ---- The same question asked of a volume -----------------------------
#' mask <- array(0, c(20, 20, 20))
#' mask[8:13, 8:13, 8:13] <- 1
#' vox2ras <- rbind(cbind(diag(1, 3), c(-10, -10, -10)), c(0, 0, 0, 1))
#' region <- as_ieegio_roi(mask, vox2ras = vox2ras, threshold_lb = 0.5)
#'
#' result <- detect_roi_overlap(x, region, mode_y = "volume")
#' result$hit_ratio
#'
#' # `>= -1` is the region itself, `>= 0` the part of it that overlaps
#' values <- result$annotated[]
#' c(region = sum(values >= -1), overlapping = sum(values >= 0))
#'
#' }
#'
#' @export
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
    # There is no inside to be in unless `x` is a closed surface. The overlap
    # itself is still perfectly well defined without the interior test, so give
    # that answer rather than refusing to answer at all - but say so, since the
    # result is not the one that was asked for.
    message(
      "`detect_roi_overlap`: `include_interior` needs `x` to resolve to a ",
      "surface to have an interior at all, and it resolved to ",
      sQuote(side_x$type), ". Continuing without the interior test; only ",
      "contact within `radius` counts."
    )
    include_interior <- FALSE
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

#' @rdname detect_roi_overlap
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
