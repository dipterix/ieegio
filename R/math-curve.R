
#' Burn a curve trajectory into a volume
#' @description
#' Burn density values along a \verb{Catmull-Rom} spline trajectory into a numeric
#' volume. The curve is sampled at sub-voxel resolution and voxels within
#' \code{thickness} of the trajectory are assigned density values.
#'
#' @param image volume providing the coordinate space; see
#'   \code{\link{imaging-volume}}
#' @param curve a \code{ravetools_curve} object (see
#'   \code{\link[ravetools]{catmull_rom_3d}}), or an \eqn{n \times 3} numeric matrix
#'   of key points in 'RAS' world coordinates. If a matrix is supplied,
#'   \code{\link[ravetools]{catmull_rom_3d}} is called automatically (requires
#'   the \pkg{ravetools} package).
#' @param thickness tube radius (half-width) around the curve in world ('RAS')
#'   coordinates (millimeters). Can be a positive scalar or a function of
#'   \code{t} in \code{[0, 1]} that returns a positive scalar, with the same
#'   call convention as \code{curve$get_point(t)}.
#' @param density value to burn into voxels within the tube. Can be a numeric
#'   scalar or a function of \code{t} in \code{[0, 1]} that returns a numeric
#'   scalar.
#' @param reshape whether to reshape the image before burning; identical
#'   semantics to \code{\link{burn_volume}}: \code{FALSE} (default, keep
#'   original resolution), \code{TRUE} (double each dimension), a single
#'   positive number (isotropic new size), or a length-3 integer vector.
#' @param antialias_type how to handle voxels at the edge of the tube.
#'   One of \code{"reduced"} (default), \code{"ignore"}, \code{"fill"}, or
#'   \code{"threshold"}:
#'   \describe{
#'     \item{\code{"ignore"}}{Only voxels whose center is strictly within
#'       \code{thickness} are burned at full density.}
#'     \item{\code{"fill"}}{Any voxel that overlaps the tube (center within
#'       \code{thickness + half_voxel_diagonal}) is fully burned.}
#'     \item{\code{"threshold"}}{Voxels are burned at full density if more than
#'       50\% of their volume (approximated by 8 corners at ±0.5 voxel) is
#'       within \code{thickness}; otherwise skipped.}
#'     \item{\code{"reduced"}}{Density is scaled proportionally by the fraction
#'       of the voxel (8-corner approximation) that lies within
#'       \code{thickness}.}
#'   }
#' @param n_samples number of points to sample along the curve. Default
#'   \code{NULL} computes \code{ceiling(arc_length / min_voxel_size * 3)}
#'   (3x oversampling), with a minimum of 10.
#' @param merge how to combine density values when multiple curve samples
#'   illuminate the same voxel. One of \code{"mean"} (default, average of
#'   all contributing samples), \code{"max"} (maximum value), or
#'   \code{"min"} (minimum value).
#' @param ... passed to \code{\link{as_ieegio_volume}}, useful when
#'   \code{image} is an array.
#' @returns A numeric \code{ieegio_volume} with the same spatial extent as
#'   \code{image} (or the reshaped extent). Background voxels contain \code{0}.
#'   When multiple curve samples illuminate the same voxel, values are combined
#'   according to \code{merge}.
#'
#' @examples
#'
#' if (interactive()) {
#'
#' dm <- c(50, 50, 50)
#' image <- as_ieegio_volume(
#'   array(0, dm),
#'   vox2ras = rbind(cbind(diag(1, 3), -dm / 2),
#'                   c(0, 0, 0, 1))
#' )
#'
#' # Three-point trajectory (Catmull-Rom from matrix)
#' key_pts <- rbind(c(-50, 0, 0), c(0, 20, 2), c(-10, 0, -1))
#'
#' curve <- ravetools::catmull_rom_3d(key_pts)
#'
#' plot(curve, use_rgl = FALSE)
#'
#' # Constant thickness and density
#' burned <- burn_curve(image, key_pts, thickness = 3, density = 100)
#'
#' plot(burned, zoom = 3, position = c(0, 0, 0), which = "axial")
#'
#' # Varying thickness and density along the curve
#' burned2 <- burn_curve(
#'   image, curve,
#'   thickness      = function(t) 2 + t * 4,
#'   density        = function(t) 100 * (1 - t) + 100,
#'   antialias_type = "reduced"
#' )
#' plot(burned2, zoom = 3, position = c(0, 0, 0), which = "axial")
#'
#' }
#'
#'
#' @export
burn_curve <- function(image, curve, thickness = 1, density = 1,
                       reshape = FALSE,
                       antialias_type = c("reduced", "ignore", "fill", "threshold"),
                       merge = c("mean", "max", "min"),
                       n_samples = NULL, ...) {

  # DIPSAUS DEBUG START
  # dm <- c(20, 20, 20)
  # image <- as_ieegio_volume(
  #   array(0, dm),
  #   vox2ras = rbind(cbind(diag(1, 3), -dm / 2), c(0, 0, 0, 1))
  # )
  # key_pts <- rbind(c(-5, 0, 0), c(0, 3, 0), c(5, 0, 0))
  # ravetools <- check_ravetools_flag()
  # curve <- ravetools$catmull_rom_3d(key_pts)
  # thickness <- 1.5
  # density   <- 100
  # reshape   <- FALSE
  # antialias_type <- "reduced"
  # n_samples <- NULL

  antialias_type <- match.arg(antialias_type)
  merge          <- match.arg(merge)

  # ------------------------------------------------------------------
  # 1.  Coerce curve to ravetools_curve
  # ------------------------------------------------------------------
  if (inherits(curve, "ravetools_curve")) {
    # use as-is
  } else if (is.numeric(curve) && is.matrix(curve) &&
             ncol(curve) == 3 && nrow(curve) >= 2) {
    ravetools <- check_ravetools_flag()
    if (isFALSE(ravetools) || !is.function(ravetools$catmull_rom_3d)) {
      stop(paste0(
        "Package 'ravetools' is required to fit a Catmull-Rom spline from ",
        "an nx3 matrix. Please install it, or supply a 'ravetools_curve' ",
        "object directly."
      ))
    }
    curve <- ravetools$catmull_rom_3d(curve)
  } else {
    stop(
      "`curve` must be a 'ravetools_curve' object or an nx3 numeric matrix ",
      "of key points (n >= 2)."
    )
  }

  # ------------------------------------------------------------------
  # 2.  Normalise thickness / density to functions of t in [0, 1]
  # ------------------------------------------------------------------
  if (is.numeric(thickness) && length(thickness) == 1L) {
    thickness_val <- as.double(thickness)
    thickness_fn  <- function(t) thickness_val
  } else if (is.function(thickness)) {
    thickness_fn <- thickness
  } else {
    stop("`thickness` must be a positive scalar or a function of t.")
  }

  if (is.numeric(density) && length(density) == 1L) {
    density_val <- as.double(density)
    density_fn  <- function(t) density_val
  } else if (is.function(density)) {
    density_fn <- density
  } else {
    stop("`density` must be a numeric scalar or a function of t.")
  }

  # ------------------------------------------------------------------
  # 3.  Image setup and optional reshaping (mirrors burn_volume)
  # ------------------------------------------------------------------
  image   <- as_ieegio_volume(image, ...)
  shape   <- dim(image)[c(1, 2, 3)]
  vox2ras <- image$transforms$vox2ras

  if (!isFALSE(reshape)) {
    stopifnot(isTRUE(reshape) || (
      is.numeric(reshape) &&
        length(reshape) %in% c(1, 3) &&
        all(reshape > 0)
    ))
    if (isTRUE(reshape)) {
      reshape <- shape * 2L
    } else if (length(reshape) == 1) {
      reshape <- c(reshape, reshape, reshape)
    }
    burn_vox2ras <- resample_vox2ras(vox2ras = vox2ras, old_dim = shape, new_dim = reshape)
    burn_shape   <- as.integer(reshape)
  } else {
    burn_vox2ras <- vox2ras
    burn_shape   <- shape
  }

  burn_ras2vox    <- solve(burn_vox2ras)
  burn_voxel_sizes <- sqrt(colSums((burn_vox2ras[1:3, 1:3])^2))
  burn_cum_shape  <- c(1L, cumprod(burn_shape))

  # ------------------------------------------------------------------
  # 4.  Curve sampling
  # ------------------------------------------------------------------
  arc_length <- sum(curve$segment_lengths)

  if (is.null(n_samples)) {
    n_samples <- max(10L, ceiling(arc_length / min(burn_voxel_sizes) * 5))
  }
  n_samples <- as.integer(n_samples)

  t_vals <- seq(0, 1, length.out = n_samples)
  pts    <- curve$get_points(n_samples)           # n_samples × 3 (x, y, z)

  r_vals <- vapply(t_vals, thickness_fn, numeric(1L))
  d_vals <- vapply(t_vals, density_fn,   numeric(1L))

  # ------------------------------------------------------------------
  # 5.  Global search table (IJK offsets spanning the largest tube)
  # ------------------------------------------------------------------
  max_r <- max(r_vals, na.rm = TRUE)

  half_diag <- sqrt(sum(burn_voxel_sizes^2)) / 2   # used by "fill" inside loop
  search_r  <- if (antialias_type == "fill") max_r + half_diag else max_r

  nvox <- ceiling(search_r / burn_voxel_sizes)
  search_table <- t(as.matrix(expand.grid(
    i = seq(-nvox[[1]], nvox[[1]]),
    j = seq(-nvox[[2]], nvox[[2]]),
    k = seq(-nvox[[3]], nvox[[3]])
  )))
  dimnames(search_table) <- NULL   # strip names to speed up arithmetic

  # ------------------------------------------------------------------
  # 6.  Sub-voxel 8-corner table for "threshold" / "reduced"
  # ------------------------------------------------------------------
  # corners_ijk_offsets: 3×8, each column is a (di, dj, dk) corner offset
  corners_ijk_offsets <- t(as.matrix(expand.grid(
    di = c(-0.5, 0.5),
    dj = c(-0.5, 0.5),
    dk = c(-0.5, 0.5)
  )))
  dimnames(corners_ijk_offsets) <- NULL

  # ------------------------------------------------------------------
  # 7.  Initialise output array
  # ------------------------------------------------------------------
  arr <- array(if (merge == "min") Inf else 0, dim = burn_shape)
  if (merge == "mean") {
    cnt <- array(0L, dim = burn_shape)
  }

  # ------------------------------------------------------------------
  # 8.  Main burn loop over curve samples
  # ------------------------------------------------------------------
  for (ii in seq_len(n_samples)) {
    r_ii <- r_vals[[ii]]
    d_ii <- d_vals[[ii]]

    if (!isTRUE(r_ii > 0) || is.na(d_ii) || anyNA(pts[ii, ])) next

    pos_ii   <- as.double(pts[ii, ])                       # RAS  3-vector
    vox_pos  <- (burn_ras2vox %*% c(pos_ii, 1))[1:3]     # fractional IJK
    vox_pos0 <- round(vox_pos)                             # nearest voxel IJK

    # IJK of all candidate voxels: 0-based, 3 × n_search
    cands0 <- search_table + vox_pos0

    # RAS displacement from each candidate voxel *center* to pos_ii
    # (reuse burn_vox2ras rotation: RAS_disp = R * (IJK_cand - IJK_frac))
    cand_ras_disp <- burn_vox2ras[1:3, 1:3] %*% (cands0 - vox_pos)  # 3 × n_search
    dist_center   <- sqrt(colSums(cand_ras_disp^2))

    # ---- filter candidates and compute per-voxel values ----
    if (antialias_type == "ignore") {

      keep <- dist_center <= r_ii
      if (!any(keep)) next
      cands0 <- cands0[, keep, drop = FALSE]
      values <- rep(d_ii, sum(keep))

    } else if (antialias_type == "fill") {

      keep <- dist_center <= r_ii + half_diag
      if (!any(keep)) next
      cands0 <- cands0[, keep, drop = FALSE]
      values <- rep(d_ii, sum(keep))

    } else {
      # "threshold" or "reduced": sub-voxel 8-corner approximation
      keep <- dist_center <= r_ii
      if (!any(keep)) next
      cands0 <- cands0[, keep, drop = FALSE]
      n_cand <- ncol(cands0)

      # IJK displacement of each candidate voxel center from vox_pos
      # Vectorised over all 8 corners at once:
      #   replicate each candidate 8 times; add each of the 8 corners
      offsets_rep <- cands0[, rep(seq_len(n_cand), each = 8L), drop = FALSE] - vox_pos
      corners_rep <- corners_ijk_offsets[, rep(seq_len(8L), times = n_cand), drop = FALSE]

      # Convert combined IJK offsets to RAS and compute distances
      combined_ras  <- burn_vox2ras[1:3, 1:3] %*% (offsets_rep + corners_rep)  # 3 × (8*n_cand)
      dist_corners  <- sqrt(colSums(combined_ras^2))           # 8*n_cand

      # Fraction of corners within radius: collapse 8 rows per candidate
      frac <- colMeans(matrix(dist_corners <= r_ii, nrow = 8L))  # n_cand

      if (antialias_type == "threshold") {
        values <- ifelse(frac >= 0.5, d_ii, 0)
      } else {   # "reduced"
        values <- d_ii * frac
      }
    }

    # ---- remove out-of-bounds candidates (mirrors burn_volume) ----
    cands0[cands0 < 0 | cands0 >= burn_shape] <- NA
    valid <- !is.na(colSums(cands0))
    if (!any(valid)) next
    cands0 <- cands0[, valid, drop = FALSE]
    values <- values[valid]

    # ---- linear indices and merge-density update ----
    burn_idx <- colSums(cands0 * burn_cum_shape[1:3]) + 1L
    if (merge == "mean") {
      arr[burn_idx] <- arr[burn_idx] + values
      cnt[burn_idx] <- cnt[burn_idx] + 1L
    } else if (merge == "max") {
      arr[burn_idx] <- pmax(arr[burn_idx], values)
    } else {
      arr[burn_idx] <- pmin(arr[burn_idx], values)
    }
  }

  # ------------------------------------------------------------------
  # 8b. Post-loop: finalise merge
  # ------------------------------------------------------------------
  if (merge == "mean") {
    burned <- cnt > 0L
    arr[burned] <- arr[burned] / cnt[burned]
  } else if (merge == "min") {
    arr[is.infinite(arr)] <- 0
  }

  # ------------------------------------------------------------------
  # 9.  Wrap in ieegio_volume and return (numeric, not color)
  # ------------------------------------------------------------------
  as_ieegio_volume(arr, vox2ras = burn_vox2ras)
}
