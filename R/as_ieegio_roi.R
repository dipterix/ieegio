
new_roi <- function(x, type = c("volume", "surface", "pointcloud", "streamlines"), ...) {

  type <- match.arg(type)

  switch(
    type,
    "volume" = {
      x <- as_ieegio_volume(x, ...)
    },
    "surface" = {
      x <- as_ieegio_surface(x, ...)
    },
    "streamlines" = {
      x <- as_ieegio_streamlines(x, ...)
    },
    {
      # pointcloud - surface without face
      x <- as_ieegio_surface(x, ...)
    }
  )

  # `ieegio_roi` must come first: `UseMethod` stops at the first class that has
  # a method, so appending it would make `as_ieegio_roi.ieegio_roi` and
  # `print.ieegio_roi` unreachable behind `ieegio_volume`/`ieegio_surface`/
  # `ieegio_streamlines`. `NextMethod` still falls through to those.
  class(x) <- unique(c("ieegio_roi", class(x)))

  # `.auto_fix` resolves "surface" against "pointcloud" from the face count, so
  # callers can always ask for "surface" and still get the correct label
  edit_roi_info(x, type = type, .auto_fix = TRUE)
}

# Bound-based constructors shared by the object methods and by
# `as_ieegio_roi.character`, so that a threshold given alongside a file path is
# recorded in `roi_info` instead of leaking into the reader through `...`
new_volume_roi <- function(x, threshold_lb = NA, threshold_ub = NA, ...) {
  x <- new_roi(x = x, type = "volume", ...)

  if (!isTRUE(is.finite(threshold_lb))) {
    threshold_lb <- NULL
  }
  if (!isTRUE(is.finite(threshold_ub))) {
    threshold_ub <- NULL
  }

  edit_roi_info(x = x, type = "volume", threshold_lb = threshold_lb, threshold_ub = threshold_ub)
}

new_streamlines_roi <- function(x, threshold_lb = NA, threshold_ub = NA, ...) {
  x <- new_roi(x = x, type = "streamlines", ...)

  if (!isTRUE(is.finite(threshold_lb))) {
    threshold_lb <- NULL
  }
  if (!isTRUE(is.finite(threshold_ub))) {
    threshold_ub <- NULL
  }

  edit_roi_info(x = x, type = "streamlines", threshold_lb = threshold_lb, threshold_ub = threshold_ub)
}

edit_roi_info <- function(x, ..., .auto_fix = FALSE, .clean = FALSE) {

  needs_update <- ...length() > 0

  roi_info <- as.list(attr(x, "roi_info"))

  if (.clean) {
    # The selection criteria have already been applied to `x`, so the object now
    # *is* the region. Carrying them forward would make a second pass threshold
    # the region again, against data that no longer holds the original values.
    # `point_radius` goes the same way: it measures one particular resolved
    # representation and says nothing about the next one.
    stale <- c("threshold_lb", "threshold_ub", "threshold_fun", "point_radius")
    stale <- stale[stale %in% names(roi_info)]
    if (length(stale)) {
      needs_update <- TRUE
      roi_info[stale] <- NULL
    }
  }

  roi_info <- utils::modifyList(roi_info, list(...))
  roi_type <- roi_info$type

  if (.auto_fix || !isTRUE(roi_type %in% c("volume", "surface", "pointcloud", "streamlines"))) {
    if (inherits(x, "ieegio_volume")) {
      roi_type <- "volume"
    } else if (inherits(x, "ieegio_surface")) {
      if (length(x$geometry$faces)) {
        roi_type <- "surface"
      } else {
        roi_type <- "pointcloud"
      }
    } else if (inherits(x, "ieegio_streamlines")) {
      roi_type <- "streamlines"
    } else {
      roi_type <- "pointcloud"
    }
    if (!identical(roi_info$type, roi_type)) {
      needs_update <- TRUE
      roi_info$type <- roi_type
    }
  }

  if (needs_update) {
    attr(x, "roi_info") <- roi_info
  }

  x
}

# Builds the threshold closure in its own frame so that the resulting function
# only retains `threshold_expr` and `env`. Creating the closure inline inside
# `as_ieegio_roi.ieegio_surface` would capture that method's frame, which holds
# the entire surface (vertices, faces, annotations) alongside it.
roi_threshold_fun <- function(threshold_expr, env) {
  force(threshold_expr)
  force(env)
  function(x) {
    annot_table <- x$annotations$data_table
    measu_table <- x$measurements$data_table
    if (is.data.frame(annot_table) && ncol(annot_table) > 0) {
      annot1 <- annot_table[[1]]
    } else {
      annot1 <- NULL
    }

    if (is.data.frame(measu_table) && ncol(measu_table) > 0) {
      measu1 <- measu_table[[1]]
    } else {
      measu1 <- NULL
    }
    eval(
      expr = threshold_expr,
      envir = list(
        .x = x,
        .a = annot_table,
        .m = measu_table,
        .a1 = annot1,
        .m1 = measu1
      ),
      enclos = env
    )
  }
}

#' @name as_ieegio_roi
#' @title Convert objects to \code{'ieegio'} region of interest
#' @description
#' Marks an image volume, surface, point cloud, or streamlines object as a
#' region of interest (\verb{ROI}). The returned object is the underlying
#' \code{'ieegio'} instance with an extra \code{"ieegio_roi"} class and an
#' \code{"roi_info"} attribute; see 'Details'.
#'
#' @param x R object or file path to convert; when \code{x} is a character
#' vector of file paths, only the first element is used and the remaining
#' elements are ignored
#' @param type type of the region of interest. For file paths, the choices are
#' \code{"auto"} (default, inferred from the file extension), \code{"volume"},
#' \code{"surface"}, and \code{"streamlines"}; for matrices and data frames,
#' the choices are \code{"pointcloud"} (default) and \code{"streamlines"}.
#' There is no \code{"pointcloud"} choice for file paths: a point cloud on disk
#' is simply a surface file that carries no face index, so the file is read as
#' a surface and the label is corrected afterwards from the face count.
#' @param threshold_lb,threshold_ub lower and upper bounds selecting the parts
#' that belong to the region of interest; default is \code{NA} (no bound).
#' Non-finite bounds are dropped. For image volumes the bounds select voxels by
#' intensity; for streamlines they select tracts by arc length, in the unit of
#' the world coordinate system
#' @param threshold_expr expression selecting the vertices that belong to the
#' region of interest, evaluated with \code{.x} bound to the surface,
#' \code{.a} and \code{.m} bound to the annotation and measurement tables, and
#' \code{.a1} and \code{.m1} bound to the first column of those tables; default
#' is \code{NULL} (no selection)
#' @param quoted whether \code{threshold_expr} has been quoted already; default
#' is \code{FALSE}
#' @param env environment in which \code{threshold_expr} is evaluated; default
#' is the calling frame
#' @param ... passed to the underlying \code{\link{as_ieegio_volume}},
#' \code{\link{as_ieegio_surface}}, or \code{\link{as_ieegio_streamlines}}
#' methods
#'
#' @details
#' A region of interest is a decorator describing \emph{how} to derive the
#' region from the object it wraps; it does not alter the underlying data. The
#' criteria are stored in the \code{"roi_info"} attribute, a list with the
#' resolved \code{type} plus, where applicable, \code{threshold_lb},
#' \code{threshold_ub}, or \code{threshold_fun}. Applying them is left to the
#' functions that consume a region of interest, and those functions document
#' the behavior themselves; \code{\link{resolve_roi_as}} is the general one,
#' applying the criteria and returning the region itself.
#'
#' The two bounds are interpreted against whatever quantity is natural for the
#' enclosed object: voxel intensity for an image volume, and tract arc length
#' for streamlines. Surfaces and point clouds use \code{threshold_expr}
#' instead, since their selection is driven by vertex annotations or
#' measurements rather than by a single scalar.
#'
#' Because the criteria live in an attribute, operations that rebuild the
#' object drop them. This is intended: in most workflows the enclosed volume,
#' surface, or streamlines object is what gets used, and the caller is
#' responsible for carrying the decorator along when it matters.
#'
#' @returns \code{as_ieegio_roi} returns the converted \code{'ieegio'} object
#' with \code{"ieegio_roi"} prepended to its class and an \code{"roi_info"}
#' attribute. \code{print} returns the object invisibly.
#'
#' @seealso \code{\link{resolve_roi_as}} applies the criteria recorded here and
#' converts the region into a chosen representation.
#'
#' @examples
#'
#' # ---- Point cloud from a matrix ---------------------------------------
#' points <- matrix(rnorm(30), ncol = 3)
#' roi <- as_ieegio_roi(points)
#' attr(roi, "roi_info")$type
#'
#' print(roi)
#'
#' # ---- Streamlines: rows of `NA` separate the tracts -------------------
#' tracts <- rbind(
#'   matrix(rnorm(9), ncol = 3),
#'   NA,
#'   matrix(rnorm(15), ncol = 3)
#' )
#' roi <- as_ieegio_roi(tracts, type = "streamlines")
#' length(roi)
#'
#' # ---- Image volume with threshold bounds ------------------------------
#' vox2ras <- matrix(
#'   c(-1, 0, 0, 5,
#'     0, 0, 1, -5,
#'     0, -1, 0, 5,
#'     0, 0, 0, 1),
#'   nrow = 4, byrow = TRUE
#' )
#' volume <- as_ieegio_roi(
#'   array(rnorm(1000), c(10, 10, 10)),
#'   vox2ras = vox2ras,
#'   threshold_lb = 0.5
#' )
#' attr(volume, "roi_info")$threshold_lb
#'
#' # The criteria survive a round trip
#' attr(as_ieegio_roi(volume), "roi_info")$threshold_lb
#'
#' # ---- Surface with a threshold expression -----------------------------
#' vertices <- matrix(rnorm(60), ncol = 3)
#' faces <- matrix(c(1, 2, 3, 2, 3, 4), ncol = 3, byrow = TRUE)
#' surface <- as_ieegio_surface(
#'   vertices,
#'   faces = faces,
#'   measurements = data.frame(Curv = rnorm(20))
#' )
#'
#' roi <- as_ieegio_roi(surface, threshold_expr = .m1 > 0)
#' selection <- attr(roi, "roi_info")$threshold_fun(roi)
#' head(selection)
#'
#'
#' @export
as_ieegio_roi <- function(x, ...) {
  UseMethod("as_ieegio_roi")
}

# ---- Naive classes -----------------------------------------------------------

#' @rdname as_ieegio_roi
#' @export
as_ieegio_roi.default <- function(x, ...) {
  stop("Unknown conversion to `ieegio` region of interest from class: ", paste(class(x), collapse = ", "))
}

#' @rdname as_ieegio_roi
#' @export
as_ieegio_roi.ieegio_roi <- function(x, ...) {
  edit_roi_info(x = x, ..., .auto_fix = FALSE, .clean = FALSE)
}

#' @rdname as_ieegio_roi
#' @export
as_ieegio_roi.character <- function(x, type = c("auto", "volume", "surface", "streamlines"), ...) {

  type <- match.arg(type)

  if (!length(x)) {
    stop("`as_ieegio_roi`: `x` must be a file path")
  }

  # Only the first path is read; any remaining element is ignored by design
  x <- x[[1]]

  if (is.na(x) || !nzchar(x)) {
    stop("`as_ieegio_roi`: `x` must be a non-missing file path")
  }

  if (type == "auto") {
    # `path_ext` already strips compound archive suffixes such as `tar.gz`, but
    # keeps a plain wrapper, e.g. `brain.nii.gz` resolves to `nii.gz`
    ext <- tolower(path_ext(x))
    ext <- sub("\\.(gz|bz2|xz|zip)$", "", ext)

    type <- switch(
      ext,
      "mgz" = ,
      "mgh" = ,
      "nii" = ,
      "hdr" = ,
      "img" = { "volume" },
      "tck" = ,
      "trk" = ,
      "tt" = { "streamlines" },
      "vtu" = { "surface" },
      "vtk" = ,
      "vtp" = ,
      "pvtp" = ,
      "vtpb" = { "vtk" },
      { "surface" }
    )
  }

  if (type == "vtk") {
    # A `VTK` poly-data file stores either a polygon mesh or streamlines and the
    # extension cannot tell the two apart. Read as streamlines first, and fall
    # back to a surface mesh when the file carries no tract.
    obj <- tryCatch({
      streamlines <- io_read_vtk_streamlines(x)
      if (length(streamlines)) { streamlines } else { NULL }
    }, error = function(e) { NULL })

    if (is.null(obj)) {
      obj <- as_ieegio_surface(x, ...)
    }
    return(as_ieegio_roi(obj, ...))
  }

  switch(
    type,
    "volume" = { new_volume_roi(x = x, ...) },
    "streamlines" = { new_streamlines_roi(x = x, ...) },
    { new_roi(x, type = type, ...) }
  )
}

# ---- Streamlines, point-clouds -----------------------------------------------

#' @rdname as_ieegio_roi
#' @export
as_ieegio_roi.ieegio_streamlines <- function(x, threshold_lb = NA, threshold_ub = NA, ...) {
  new_streamlines_roi(x = x, threshold_lb = threshold_lb, threshold_ub = threshold_ub, ...)
}

#' @rdname as_ieegio_roi
#' @export
as_ieegio_roi.list <- function(x, threshold_lb = NA, threshold_ub = NA, ...) {
  message(
    "`as_ieegio_roi`: reading the input list as streamlines. Please call ",
    "`as_ieegio_roi(as_ieegio_streamlines(x))` when the intent is ambiguous."
  )
  new_streamlines_roi(x = x, threshold_lb = threshold_lb, threshold_ub = threshold_ub, ...)
}

#' @rdname as_ieegio_roi
#' @export
as_ieegio_roi.matrix <- function(x, type = c("pointcloud", "streamlines"), ...) {
  type <- match.arg(type)

  nc <- ncol(x)

  if (nc == 1) {
    # column matrix
    x <- t(x)
    nc <- ncol(x)
  }

  if (nc != 3) {
    stop("Input `x` must be a matrix with three columns")
  }

  switch(
    type,
    "pointcloud" = {
      # `new_roi` demotes "surface" to "pointcloud" when there is no face
      new_roi(x = x, type = "surface", ...)
    },
    {
      row_idx <- which(rowSums(is.na(x)) > 0)
      row_idx <- sort(unique(c(0L, row_idx, nrow(x) + 1L)))

      tracts <- lapply(seq_len(length(row_idx) - 1), function(ii) {
        row_begin <- row_idx[[ii]] + 1L
        row_end <- row_idx[[ii + 1]] - 1L
        if (row_begin <= row_end) {
          x[seq.int(row_begin, row_end), , drop = FALSE]
        } else {
          NULL
        }
      })

      tracts <- drop_nulls(tracts)
      new_streamlines_roi(x = tracts, ...)
    }
  )
}

#' @rdname as_ieegio_roi
#' @export
as_ieegio_roi.data.frame <- function(x, type = c("pointcloud", "streamlines"), ...) {
  type <- match.arg(type)

  is_numeric_column <- vapply(x, is.numeric, FUN.VALUE = logical(1L))
  if (!all(is_numeric_column)) {
    stop("Input `x` must be a data frame whose columns are all numeric")
  }

  as_ieegio_roi.matrix(x = as.matrix(x), type = type, ...)
}

#' @rdname as_ieegio_roi
#' @export
as_ieegio_roi.numeric <- function(x, ...) {
  if (length(x) %% 3 != 0) {
    stop("Input `x` must have a length that is a multiple of three (x, y, z per point)")
  }
  x <- matrix(x, ncol = 3, byrow = TRUE)
  as_ieegio_roi.matrix(x = x, ...)
}

# Registered explicitly: an object carrying an actual `class = "integer"`
# attribute never reaches `as_ieegio_roi.numeric`, because the implicit
# "numeric" class only applies when there is no class attribute at all.
#' @rdname as_ieegio_roi
#' @export
as_ieegio_roi.integer <- as_ieegio_roi.numeric

# ---- Volume classes ----------------------------------------------------------

#' @rdname as_ieegio_roi
#' @export
as_ieegio_roi.array <- function(x, ...) {
  dm <- dim(x)
  ldm <- length(dm)
  if (ldm < 2) {
    as_ieegio_roi.numeric(x = x, ...)
  } else if (ldm == 2) {
    as_ieegio_roi.matrix(x = x, ...)
  } else {
    as_ieegio_roi.ieegio_volume(x = x, ...)
  }
}

#' @rdname as_ieegio_roi
#' @export
as_ieegio_roi.ieegio_volume <- function(x, threshold_lb = NA, threshold_ub = NA, ...) {
  new_volume_roi(x = x, threshold_lb = threshold_lb, threshold_ub = threshold_ub, ...)
}

#' @rdname as_ieegio_roi
#' @export
as_ieegio_roi.niftiImage <- as_ieegio_roi.ieegio_volume

#' @rdname as_ieegio_roi
#' @export
as_ieegio_roi.nifti <- as_ieegio_roi.ieegio_volume

#' @rdname as_ieegio_roi
#' @export
as_ieegio_roi.ants.core.ants_image.ANTsImage <- as_ieegio_roi.ieegio_volume

# ---- Surface classes ---------------------------------------------------------

#' @rdname as_ieegio_roi
#' @export
as_ieegio_roi.ieegio_surface <- function(
    x, threshold_expr = NULL, quoted = FALSE, env = parent.frame(), ...) {
  x <- as_ieegio_surface(x, ...)
  if (length(x$geometry$faces)) {
    type <- "surface"
  } else {
    type <- "pointcloud"
  }

  x <- new_roi(x = x, type = type, ...)

  # threshold_expr is threshold on the annotations or measurements
  # with .a representing x$annotations$data_table or NULL
  # and .m representing x$measurements$data_table or NULL
  # .a1 is x$annotations$data_table[[1]] or NULL
  # and .m1 is x$measurements$data_table[[1]] or NULL
  if (!quoted) {
    threshold_expr <- substitute(threshold_expr)
  }

  if (is.null(threshold_expr)) {
    threshold_fun <- NULL
  } else {
    threshold_fun <- roi_threshold_fun(threshold_expr = threshold_expr, env = env)
  }

  edit_roi_info(x = x, type = type, threshold_fun = threshold_fun)
}

#' @rdname as_ieegio_roi
#' @export
as_ieegio_roi.mesh3d <- as_ieegio_roi.ieegio_surface

#' @rdname as_ieegio_roi
#' @export
as_ieegio_roi.fs.surface <- as_ieegio_roi.ieegio_surface

# ---- Display -----------------------------------------------------------------

#' @rdname as_ieegio_roi
#' @export
print.ieegio_roi <- function(x, ...) {
  roi_info <- as.list(attr(x, "roi_info"))

  details <- NULL
  if (length(roi_info$threshold_lb) == 1) {
    details <- c(details, sprintf("lower bound %g", roi_info$threshold_lb))
  }
  if (length(roi_info$threshold_ub) == 1) {
    details <- c(details, sprintf("upper bound %g", roi_info$threshold_ub))
  }
  if (is.function(roi_info$threshold_fun)) {
    details <- c(details, "expression filter")
  }
  if (length(details)) {
    details <- sprintf(": %s", paste(details, collapse = ", "))
  } else {
    details <- ""
  }

  cat(sprintf("<ieegio ROI [%s]%s>\n", roi_info$type, details))

  # falls through to print.ieegio_volume/print.ieegio_surface/
  # print.ieegio_streamlines, which is why `ieegio_roi` is prepended
  NextMethod()

  invisible(x)
}



# Resolve: apply threshold, transform, and possibly convert to other types
#
# `as_ieegio_roi` records *how* to derive a region without touching the data;
# these functions are the other half, applying those criteria and handing back a
# plain object in the requested representation. Every resolved object is in
# world ('RAS') coordinates: geometry transforms are baked into the coordinates
# and the stored transform is reset to the identity, so two resolved regions can
# always be compared directly.

# ---- Shared accessors --------------------------------------------------------

roi_volume_vox2ras <- function(x) {
  vox2ras <- x$transforms$vox2ras
  if (!is.matrix(vox2ras)) {
    vox2ras <- diag(1, 4)
  }
  vox2ras
}

roi_surface_transform <- function(x) {
  transform <- x$geometry$transforms
  if (length(transform)) {
    transform <- transform[[1]]
  }
  if (!is.matrix(transform)) {
    transform <- diag(1, 4)
  }
  transform
}

# `point_radius` describes the ball to draw around each point of a resolved
# point cloud, as a ladder of how much of the original object the union of those
# balls swallows:
#
#   min     the smallest radius that covers any one cell of the source, i.e.
#           the point at which neighboring balls start to overlap
#   median  covers at least half of the object
#   q95     covers at least ninety-five percent of it
#   max     covers all of it, its corners and midpoints included
#
# What a "cell" is depends on the source: a voxel box for a volume, the segment
# between two samples along a tract, the triangle between three vertices of a
# mesh. In two and three dimensions, covering a cell reaches past half its
# longest side - out to half the voxel diagonal, or to a triangle's circumcircle.
#
# `min` answers a different question from the other three, so it is not ordered
# against them: for an isotropic volume `median` (about 0.48 of the voxel side)
# falls just below `min` (half the side), because making two balls overlap asks
# for more than covering the typical point.
POINT_RADIUS_PROBS <- c(0.5, 0.95)

empty_point_radius <- function() {
  list(min = 0, median = 0, q95 = 0, max = 0)
}

point_radius_summary <- function(radii) {
  radii <- radii[is.finite(radii)]
  if (!length(radii)) {
    return(empty_point_radius())
  }
  quantiles <- unname(stats::quantile(radii, POINT_RADIUS_PROBS))
  list(
    min = min(radii),
    median = quantiles[[1]],
    q95 = quantiles[[2]],
    max = max(radii)
  )
}

# Voxel size along each of the three axes, the same way `burn_volume` measures it
roi_voxel_sizes <- function(vox2ras) {
  if (!is.matrix(vox2ras)) {
    return(numeric(0L))
  }
  sqrt(colSums(vox2ras[seq_len(3), seq_len(3), drop = FALSE]^2))
}

# Distances from an arbitrary point of a box to the box center, sampled on a
# regular grid. A single octant is representative of the whole box by symmetry,
# so the quantiles of this sample are the quantiles of the box.
box_center_distances <- function(sizes, n = 64L) {
  squares <- lapply(sizes / 2, function(half) {
    ((seq_len(n) - 0.5) / n * half)^2
  })
  sqrt(outer(outer(squares[[1]], squares[[2]], "+"), squares[[3]], "+"))
}

# A voxel is a three-dimensional box rather than a gap along a line, so covering
# it reaches to the corner - half the diagonal - not merely half the spacing.
roi_voxel_point_radius <- function(vox2ras) {
  sizes <- roi_voxel_sizes(vox2ras)
  sizes <- sizes[is.finite(sizes) & sizes > 0]
  if (length(sizes) != 3) {
    return(empty_point_radius())
  }
  quantiles <- unname(
    stats::quantile(box_center_distances(sizes), POINT_RADIUS_PROBS))
  list(
    min = min(sizes) / 2,
    median = quantiles[[1]],
    q95 = quantiles[[2]],
    max = sqrt(sum(sizes^2)) / 2
  )
}

# Radius of the smallest circle covering each triangle, which is what it takes
# for balls on the three corners to swallow the face between them. Half the
# longest edge only suffices when the triangle is obtuse or right, so that the
# longest edge is a diameter; an acute triangle reaches farther, out to its
# circumcircle. This is the same corner-reaching argument as the voxel diagonal.
roi_triangle_radii <- function(vertices, faces) {
  if (!length(faces) || nrow(faces) < 3) {
    return(numeric(0L))
  }
  coords <- vertices[seq_len(3), , drop = FALSE]

  side_length <- function(i, j) {
    sqrt(colSums((
      coords[, faces[i, ], drop = FALSE] - coords[, faces[j, ], drop = FALSE]
    )^2))
  }
  s1 <- side_length(1, 2)
  s2 <- side_length(2, 3)
  s3 <- side_length(3, 1)

  # sorted descending: the obtuse test reads off the longest side, and the
  # stable form of Heron's formula below needs this ordering
  a <- pmax(s1, s2, s3)
  cc <- pmin(s1, s2, s3)
  b <- s1 + s2 + s3 - a - cc

  # obtuse or right: the longest side is a diameter of the covering circle. The
  # two branches agree at the right angle, where the circumradius is exactly
  # half the hypotenuse, so the result is continuous across the test.
  radii <- a / 2

  acute <- a^2 < b^2 + cc^2
  if (any(acute)) {
    a_acute <- a[acute]
    b_acute <- b[acute]
    c_acute <- cc[acute]

    # Kahan's rearrangement of Heron's formula. The textbook version cancels
    # catastrophically on needle-thin triangles, which a decimated mesh has
    # plenty of; the `pmax` guards the rounding noise that can push the product
    # a hair below zero.
    area <- 0.25 * sqrt(pmax(
      (a_acute + (b_acute + c_acute)) *
        (c_acute - (a_acute - b_acute)) *
        (c_acute + (a_acute - b_acute)) *
        (a_acute + (b_acute - c_acute)),
      0
    ))

    circumradius <- a_acute * b_acute * c_acute / (4 * area)
    # zero area means the three points are collinear, and then the longest side
    # is the covering diameter after all
    degenerate <- !is.finite(circumradius)
    circumradius[degenerate] <- a_acute[degenerate] / 2

    radii[acute] <- circumradius
  }
  radii
}

# Zero/one integer mask of the voxels inside the region, ready to be used as
# volume data directly. Only the leading 3D frame takes part: the extra
# dimensions of a 4D volume hold time points or color channels, neither of which
# is part of the region geometry.
roi_volume_mask <- function(x, roi_info) {
  dm <- dim(x)[c(1, 2, 3)]
  x_data <- x[drop = FALSE]
  x_data <- array(x_data[seq_len(prod(dm))], dim = dm)

  if (!isTRUE(is.finite(roi_info$threshold_lb))) {
    roi_info$threshold_lb <- NULL
  }

  if (!isTRUE(is.finite(roi_info$threshold_ub))) {
    roi_info$threshold_ub <- NULL
  }

  if (length(roi_info$threshold_lb) || length(roi_info$threshold_ub)) {
    mask <- x_data >= (roi_info$threshold_lb %||% -Inf) &
      x_data <= (roi_info$threshold_ub %||% Inf)
  } else {
    mask <- x_data > 0
  }
  mask[is.na(mask)] <- FALSE
  mode(mask) <- "integer"
  mask
}

# Which vertices the region keeps; `TRUE` everywhere when no expression was given
roi_vertex_selection <- function(x, roi_info) {
  n_vertex <- ncol(x$geometry$vertices)
  if (!is.function(roi_info$threshold_fun)) {
    return(rep(TRUE, n_vertex))
  }
  sel <- roi_info$threshold_fun(x = x)
  if (is.logical(sel)) {
    sel <- rep_len(sel, n_vertex)
    sel[is.na(sel)] <- FALSE
  } else {
    re <- rep(FALSE, n_vertex)
    sel <- as.integer(sel)
    sel <- sel[!is.na(sel) & sel > 0 & sel <= n_vertex]
    if (length(sel)) {
      re[sel] <- TRUE
    }
    sel <- re
  }
  sel
}

# Vertices in world space, as the 4xn matrix `ieegio` stores internally
roi_surface_vertices <- function(x) {
  vertices <- x$geometry$vertices
  if (nrow(vertices) == 3) {
    vertices <- rbind(vertices, 1)
  }
  roi_surface_transform(x) %*% vertices
}

# Faces as 1-based indices. `face_start` is documented as either 0 or 1, and
# every other face-consuming function in the package re-normalizes on read
# rather than trusting the stored value, so this one does too: a 0 index would
# silently drop elements out of `remap[faces]` and shear the whole matrix.
roi_surface_faces <- function(x) {
  faces <- x$geometry$faces
  if (!length(faces)) {
    return(NULL)
  }
  face_start <- x$geometry$face_start
  if (!length(face_start) || is.na(face_start) || !is.numeric(face_start)) {
    face_start <- min(faces, na.rm = TRUE)
  }
  if (face_start != 1) {
    faces <- faces - face_start + 1L
    storage.mode(faces) <- "integer"
  }
  faces
}

# Keep the selected vertices and only those faces whose three corners all
# survive, remapping the face indices onto the reduced vertex set
subset_surface_geometry <- function(vertices, faces, sel) {
  if (all(sel)) {
    return(list(vertices = vertices, faces = faces))
  }
  remap <- rep(NA_integer_, ncol(vertices))
  remap[sel] <- seq_len(sum(sel))
  vertices <- vertices[, sel, drop = FALSE]

  if (length(faces)) {
    faces <- matrix(remap[faces], nrow = nrow(faces))
    faces <- faces[, colSums(is.na(faces)) == 0, drop = FALSE]
    if (!ncol(faces)) {
      faces <- NULL
    }
  }
  list(vertices = vertices, faces = faces)
}

# Keep only the tracts that are actually lines, and only the points that are
# actually coordinates. Takes and returns the item list that
# `[.ieegio_streamlines` produces, in three steps:
#
#   1. a tract with fewer than two points is dropped - it has no segment;
#   2. rows that are not fully finite are removed, along with the per-point
#      `scalars` sitting on them, so the two stay aligned;
#   3. a tract left with fewer than two points is dropped as well.
#
# The third step is the one that matters: removing rows can leave one point, or
# none, and such a tract is no more a line than it was before the removal.
#
# A gap is closed over rather than split around: a streamline with a missing
# sample stays one streamline. That is the opposite of what a non-finite row
# means to `ravetools`, where it separates two chains, which is exactly why
# these rows must not survive downstream.
sanitize_streamlines <- function(items) {
  items <- lapply(items, function(item) {
    coords <- item$coords

    # nothing that cannot be read as a line survives at all
    if (!is.matrix(coords) || ncol(coords) < 3 || nrow(coords) < 2) {
      return(NULL)
    }

    keep <- rowSums(!is.finite(coords[, seq_len(3), drop = FALSE])) == 0
    if (!all(keep)) {
      if (sum(keep) < 2) {
        return(NULL)
      }
      item$coords <- coords[keep, , drop = FALSE]
      if (is.matrix(item$scalars)) {
        item$scalars <- item$scalars[keep, , drop = FALSE]
      }
      item$num_points <- nrow(item$coords)
    }
    item
  })

  drop_nulls(items)
}

# Streamlines that pass the arc-length filter. `[.ieegio_streamlines` applies
# `header$vox2ras`, so the coordinates come back in world space already.
roi_streamline_items <- function(x, roi_info) {
  # Gaps are not geometry, and they poison the arc length below - one missing
  # coordinate makes a whole tract's length `NA`, which then fails every bound.
  items <- sanitize_streamlines(x[])

  # `as_ieegio_roi.ieegio_roi` stores whatever it is handed, so a bound may well
  # arrive as `NA`; comparing against it would select `NA` tracts rather than
  # none. Same guard as `roi_volume_mask`.
  threshold_lb <- roi_info$threshold_lb
  if (!isTRUE(is.finite(threshold_lb))) {
    threshold_lb <- NULL
  }

  threshold_ub <- roi_info$threshold_ub
  if (!isTRUE(is.finite(threshold_ub))) {
    threshold_ub <- NULL
  }

  if (length(items) && (length(threshold_lb) || length(threshold_ub))) {
    tract_lengths <- vapply(items, function(item) {
      polyline_length(item$coords)
    }, 0)
    items <- items[
      tract_lengths >= (threshold_lb %||% -Inf) &
        tract_lengths <= (threshold_ub %||% Inf)
    ]
  }
  items
}

roi_streamline_tracts <- function(x, roi_info) {
  lapply(roi_streamline_items(x, roi_info), function(item) {
    item$coords[, seq_len(3), drop = FALSE]
  })
}

# `rbind` on an empty list gives `NULL`; every rasterizing path needs an nx3
# matrix instead so the downstream matrix algebra stays well defined
roi_bind_points <- function(points_list) {
  points <- do.call("rbind", points_list)
  if (!is.matrix(points) || ncol(points) < 3) {
    return(matrix(numeric(0L), ncol = 3))
  }
  points[, seq_len(3), drop = FALSE]
}

# Stamp the resolved representation. `new_roi` reconstructs the object so the
# `ieegio_roi` class is genuinely present (patching the attribute alone would
# leave a bare volume or surface), and `.clean` discards the criteria that have
# just been applied so a second pass cannot threshold the region again.
finalize_resolved_roi <- function(x, type, ...) {
  x <- new_roi(x = x, type = type)
  edit_roi_info(x = x, ..., .clean = TRUE)
}

# ---- Resolvers ---------------------------------------------------------------

resolve_roi_as_pointcloud <- function(x, ...) {
  x <- as_ieegio_roi(x)
  roi_info <- attr(x, "roi_info")

  # bare points have no neighborhood of their own, hence a radius of zero
  point_radius <- empty_point_radius()

  switch(
    roi_info$type,
    "volume" = {
      vox2ras <- roi_volume_vox2ras(x)
      point_radius <- roi_voxel_point_radius(vox2ras)

      # `which` rejects a non-logical argument, so the integer mask is compared
      # rather than passed straight through
      ijk <- which(roi_volume_mask(x, roi_info) > 0, arr.ind = TRUE)
      if (nrow(ijk)) {
        points <- vox2ras %*% t(cbind(ijk - 1L, 1))
        points <- t(points[seq_len(3), , drop = FALSE])
      } else {
        points <- matrix(numeric(0L), ncol = 3)
      }
      x <- as_ieegio_surface(points)
    },
    "streamlines" = {
      tracts <- roi_streamline_tracts(x, roi_info)
      # between two samples a tract is one-dimensional, so the covering radius
      # is simply half the segment
      point_radius <- point_radius_summary(
        unlist(lapply(tracts, polyline_segment_lengths)) / 2)
      x <- as_ieegio_surface(roi_bind_points(tracts))
    },
    {
      # surface or point cloud: threshold the vertices and bring them into the
      # world space defined by the geometry transform
      sel <- roi_vertex_selection(x, roi_info)
      geometry <- subset_surface_geometry(
        vertices = roi_surface_vertices(x),
        faces = roi_surface_faces(x),
        sel = sel
      )
      # a bare point cloud keeps no faces, hence no triangles and a zero radius
      point_radius <- point_radius_summary(
        roi_triangle_radii(geometry$vertices, geometry$faces))
      x <- as_ieegio_surface(t(geometry$vertices[seq_len(3), , drop = FALSE]))
    }
  )

  finalize_resolved_roi(x, type = "pointcloud", point_radius = point_radius)
}

resolve_roi_as_volume <- function(
    x, dim = NULL, vox2ras = NULL, resolution = 0.5, fill_surface = FALSE, ...) {
  x <- as_ieegio_roi(x)
  roi_info <- attr(x, "roi_info")

  if (roi_info$type == "volume") {
    vox2ras0 <- roi_volume_vox2ras(x)
    x_data <- roi_volume_mask(x, roi_info)

    if (!is.null(dim) || !is.null(vox2ras)) {
      dim <- dim %||% dim(x_data)
      vox2ras <- vox2ras %||% vox2ras0

      x_data <- resample_volume_vox2ras(
        x_data,
        new_dim = dim,
        vox2ras_old = vox2ras0,
        vox2ras_new = vox2ras,
        na_fill = 0L,
        interpolation = "nearest"
      )
      vox2ras0 <- vox2ras
    }

    volume <- as_ieegio_volume(x_data, vox2ras = vox2ras0, cal_max = 1)
    return(finalize_resolved_roi(volume, type = "volume"))
  }

  # Everything else is rasterized from its world-space point set
  surface <- NULL

  if (roi_info$type == "streamlines") {
    tracts <- roi_streamline_tracts(x, roi_info)
    # A tract is a continuous curve: sampling only the stored vertices leaves
    # gaps wherever consecutive points sit more than a voxel apart
    tracts <- lapply(tracts, densify_polyline, spacing = resolution / 2)
    points <- roi_bind_points(tracts)
    fill_surface <- FALSE
  } else {
    sel <- roi_vertex_selection(x, roi_info)
    geometry <- subset_surface_geometry(
      vertices = roi_surface_vertices(x),
      faces = roi_surface_faces(x),
      sel = sel
    )
    points <- t(geometry$vertices[seq_len(3), , drop = FALSE])

    if (fill_surface) {
      faces <- geometry$faces
      if (!length(faces)) {
        stop(
          "`resolve_roi_as_volume`: `fill_surface = TRUE` needs a closed mesh, ",
          "but this region of interest has no faces left after thresholding."
        )
      }
      # `ravetools::fill_surface` re-applies `geometry$transforms`, so the mesh
      # is handed over with the transform already baked in and dropped
      surface <- as_ieegio_surface(points, faces = t(faces), face_start = 1L)
    }
  }

  if (!nrow(points)) {
    stop(
      "`resolve_roi_as_volume`: the region of interest is empty, there is ",
      "nothing to rasterize. Please check the threshold criteria."
    )
  }

  bbox <- apply(points, 2L, range, na.rm = TRUE)

  if (!is.matrix(vox2ras)) {
    # 'RAS'-aligned isotropic grid, with a two-voxel margin so that the outermost
    # points are not sitting on the very edge of the volume
    vox2ras <- rbind(
      cbind(diag(resolution, 3), bbox[1, ] - 2 * resolution),
      c(0, 0, 0, 1)
    )
  }
  ras2vox <- solve(vox2ras)

  if (!length(dim)) {
    # The grid may be rotated with respect to the bounding box, so the shape is
    # taken from the eight corners rather than from the extent along each axis
    corners <- as.matrix(expand.grid(bbox[, 1], bbox[, 2], bbox[, 3]))
    corners_ijk <- ras2vox %*% rbind(t(corners), 1)
    dim <- ceiling(apply(corners_ijk[seq_len(3), , drop = FALSE], 1L, max)) + 3L
    dim <- pmax(dim, 2L)
  }
  dim <- as.integer(dim[c(1, 2, 3)])

  if (fill_surface) {
    ravetools <- check_ravetools_flag()
    if (isFALSE(ravetools) || !is.function(ravetools$fill_surface)) {
      stop(
        "`resolve_roi_as_volume`: `fill_surface = TRUE` requires the ",
        "`ravetools` package. Please install it, or set `fill_surface = FALSE`."
      )
    }

    # `fill_surface` closes the mesh morphologically and needs empty voxels all
    # around it to do so; the two-voxel margin used for plain rasterizing leaves
    # it no room and the closing then floods the whole volume. It also insists
    # on a cubic grid, sized by a single `resolution`.
    pad <- 16L
    fill_side <- max(dim) + 2L * pad
    fill_vox2ras <- vox2ras
    fill_vox2ras[seq_len(3), 4] <-
      (vox2ras %*% c(-pad, -pad, -pad, 1))[seq_len(3)]

    filled <- ravetools$fill_surface(
      surface = surface,
      inflate = 0,
      resolution = fill_side,
      IJK2RAS = fill_vox2ras
    )

    volume <- as_ieegio_volume(filled$volume, vox2ras = filled$IJK2RAS, cal_max = 1)
    return(finalize_resolved_roi(volume, type = "volume"))
  }

  idx <- round(ras2vox %*% rbind(t(points), 1))[seq_len(3), , drop = FALSE] + 1L
  keep <- colSums(is.na(idx) | idx < 1L | idx > dim) == 0
  idx <- idx[, keep, drop = FALSE]

  x_data <- array(0L, dim = dim)
  if (ncol(idx)) {
    x_data[colSums((idx - 1L) * cumprod(c(1, dim))[seq_len(3)]) + 1L] <- 1L
  }

  volume <- as_ieegio_volume(x_data, vox2ras = vox2ras, cal_max = 1)
  finalize_resolved_roi(volume, type = "volume")
}

resolve_roi_as_surface <- function(
    x, lambda = 0, resolution = 0.5, tube_radius = NULL, tube_sides = 6L, ...) {
  x <- as_ieegio_roi(x)
  roi_info <- attr(x, "roi_info")

  if (roi_info$type == "surface") {
    sel <- roi_vertex_selection(x, roi_info)
    geometry <- subset_surface_geometry(
      vertices = roi_surface_vertices(x),
      faces = roi_surface_faces(x),
      sel = sel
    )
    faces <- geometry$faces
    if (length(faces)) {
      faces <- t(faces)
    } else {
      faces <- NULL
    }
    surface <- as_ieegio_surface(
      t(geometry$vertices[seq_len(3), , drop = FALSE]),
      faces = faces,
      face_start = 1L
    )
  } else if (roi_info$type == "streamlines") {
    # a curve has no surface of its own; give each tract a tube body
    surface <- tube_mesh_from_polylines(
      tracts = roi_streamline_tracts(x, roi_info),
      radius = tube_radius %||% resolution,
      n_sides = tube_sides
    )
  } else {
    # point cloud: rasterize, then walk the iso-surface of the mask
    volume <- resolve_roi_as_volume(x, resolution = resolution, ...)
    surface <- volume_to_surface(volume = volume, lambda = lambda, threshold_lb = 0.5)
  }

  finalize_resolved_roi(surface, type = "surface")
}

resolve_roi_as_streamlines <- function(x, ...) {
  x <- as_ieegio_roi(x)
  roi_info <- attr(x, "roi_info")

  # x can only be ieegio_streamline because we do not have other straightforward
  # conversion
  if (!identical(roi_info$type, "streamlines")) {
    stop(
      "`resolve_roi_as`: cannot resolve a region of interest of type ",
      sQuote(roi_info$type), " as streamlines. Only streamlines carry the ",
      "tract connectivity that this representation needs; there is no ",
      "straightforward conversion from the other types."
    )
  }

  # The filters keep or drop whole tracts, so the per-point scalars and the
  # per-tract properties still line up and are carried over. `sanitize_streamlines`
  # has already dropped anything that is not a line.
  items <- roi_streamline_items(x, roi_info)

  # `roi_streamline_items` returns world coordinates, hence the identity
  # `vox2ras` rather than the original one, which would be applied a second time
  streamlines <- as_ieegio_streamlines.default(
    x = items,
    vox2ras = diag(1, 4),
    scalar_names = x$header$scalar_names,
    property_names = x$header$property_names
  )

  finalize_resolved_roi(streamlines, type = "streamlines")
}

#' @title Resolve a region of interest into a chosen representation
#' @description
#' Applies the criteria recorded by \code{\link{as_ieegio_roi}} and converts the
#' result into the requested geometry. Where \code{as_ieegio_roi} only describes
#' \emph{how} to derive a region, \code{resolve_roi_as} carries that description
#' out and hands back an object that \emph{is} the region.
#'
#' @param x a region of interest, or anything \code{\link{as_ieegio_roi}}
#' accepts; non-\verb{ROI} inputs are converted first, with no criteria attached
#' @param mode representation to resolve into: \code{"auto"} (default, keep the
#' representation the region already has), \code{"volume"}, \code{"pointcloud"},
#' \code{"surface"}, or \code{"streamlines"}
#' @param ... tuning arguments for the chosen \code{mode}; see 'Details'
#'
#' @details
#' The resolved object is always in world ('RAS') coordinates: geometry
#' transforms are applied to the coordinates and the stored transform is reset
#' to the identity, so two resolved regions can be compared directly. The
#' criteria are cleared from the \code{"roi_info"} attribute afterwards, since
#' they have been spent and the data no longer holds the values they described.
#'
#' Not every pair of representations is convertible. Streamlines carry tract
#' connectivity that nothing else does, so \code{mode = "streamlines"} accepts
#' only a streamlines region and fails otherwise. The remaining conversions all
#' succeed, though some lose information: a mesh resolved as a point cloud keeps
#' its vertices and drops its faces.
#'
#' Arguments in \code{...} depend on \code{mode}:
#' \describe{
#'   \item{\code{"volume"}}{\code{dim} and \code{vox2ras} give the target grid
#'   explicitly; otherwise one is built along the world axes at \code{resolution}
#'   spacing (default \code{0.5}) around the region. \code{fill_surface} fills
#'   the interior of a closed mesh rather than only marking the voxels it passes
#'   through, and needs the \pkg{ravetools} package.}
#'   \item{\code{"surface"}}{\code{lambda} smooths the surface extracted from a
#'   point cloud or volume, which also needs \pkg{ravetools}. For streamlines,
#'   \code{tube_radius} and \code{tube_sides} set the thickness and the number of
#'   sides of the tube built around each tract.}
#'   \item{\code{"pointcloud"}, \code{"streamlines"}}{no tuning arguments.}
#' }
#'
#' A resolved point cloud gains a \code{point_radius} entry in its
#' \code{"roi_info"}, describing the ball to draw around each of its points as
#' \code{min}, \code{median}, \code{q95}, and \code{max}. The last three say what
#' radius covers at least half, at least ninety-five percent, and all of the
#' original region; \code{min} instead is the radius at which balls on
#' neighboring points begin to overlap. No other representation carries it.
#'
#' @returns The resolved region: an \code{'ieegio'} volume, surface, or
#' streamlines object carrying the \code{"ieegio_roi"} class and an
#' \code{"roi_info"} attribute whose \code{type} reports the representation.
#'
#' @seealso \code{\link{as_ieegio_roi}} records the criteria that this function
#' applies.
#'
#' @examples
#'
#' # ---- A volume becomes the point cloud of its voxel centers -----------
#' vox2ras <- rbind(cbind(diag(2, 3), c(-10, -10, -10)), c(0, 0, 0, 1))
#' mask <- array(0, c(10, 10, 10))
#' mask[3:5, 3:5, 3:5] <- 1
#'
#' roi <- as_ieegio_roi(mask, vox2ras = vox2ras, threshold_lb = 0.5)
#'
#' # ROI is a volume, resolved as pointcloud
#' points <- resolve_roi_as(roi, "pointcloud")
#' print(points)
#'
#' # covering the voxel reaches its corner, not merely half its spacing
#' point_radius <- attr(points, "roi_info")$point_radius
#'
#' print(point_radius)
#'
#' # Plot the point cloud as spheres with radius of `max`, the union of
#' # the spheres covers the entire mask
#' plot(points, cex = point_radius$max)
#'
#' # ---- Points are sampled back onto a voxel grid -----------------------
#' box <- as.matrix(expand.grid(-3:3, -2:2, 0:4))
#' volume <- resolve_roi_as(as_ieegio_roi(box), "volume", resolution = 0.5)
#' dim(volume)
#' sum(volume[])
#'
#' plot(volume, zoom = 10)
#'
#' # ---- A tract has no thickness, so it is given a tube body ------------
#' tract <- cbind(seq(0, 10, by = 0.5), rnorm(21), rnorm(21))
#' roi <- as_ieegio_roi(tract, type = "streamlines")
#' plot(roi)
#'
#' tube <- resolve_roi_as(roi, "surface", tube_radius = 0.2, tube_sides = 8)
#'
#' plot(tube)
#'
#' # ---- A threshold expression trims a mesh -----------------------------
#' vertices <- matrix(
#'   c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1),
#'   ncol = 3, byrow = TRUE
#' )
#' faces <- matrix(
#'   c(1, 2, 3, 1, 2, 4, 1, 3, 4, 2, 3, 4),
#'   ncol = 3, byrow = TRUE
#' )
#' surface <- as_ieegio_surface(
#'   vertices,
#'   faces = faces,
#'   measurements = data.frame(Curv = c(1, 1, 1, -1))
#' )
#' roi <- as_ieegio_roi(surface, threshold_expr = .m1 > 0)
#'
#' # only the face whose three corners all survive is kept
#' trimmed <- resolve_roi_as(roi, "surface")
#' ncol(trimmed$geometry$faces)
#'
#' # `auto` keeps whatever representation the region already has
#' attr(resolve_roi_as(roi, "auto"), "roi_info")$type
#'
#' @export
resolve_roi_as <- function(
    x, mode = c("auto", "volume", "pointcloud", "streamlines", "surface"), ...) {

  x <- as_ieegio_roi(x)
  roi_info <- attr(x, "roi_info")

  mode <- match.arg(mode)

  if (mode == "auto") {
    mode <- roi_info$type
  }

  fn_name <- sprintf("resolve_roi_as_%s", mode)

  do.call(fn_name, c(list(x = x), list(...)))
}

