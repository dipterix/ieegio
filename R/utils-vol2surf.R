#' Create smooth surface from volume mask or data
#'
#' @param volume volume object or path to the NIfTI volume files, see
#' \code{as_ieegio_volume} for details
#' @param ... passed to \code{as_ieegio_volume}
#' @param lambda,degree smooth parameters; see
#' \code{\link[ravetools]{vcg_smooth_implicit}} for details. To disable
#' smoothing, set \code{lambda} to negative or \code{NA}
#' @param threshold_lb,threshold_ub threshold of volume, see
#' \code{\link[ravetools]{vcg_isosurface}}; default is any voxel value above 0.5
#' @param smooth_method \code{"implicit"} (default) smooths with
#' \code{\link[ravetools]{vcg_smooth_implicit}} using \code{lambda} and
#' \code{degree}; \code{"explicit"} smooths with
#' \code{\link[ravetools]{mris_smooth}} instead, repeated neighbor averaging
#' whose memory grows only linearly with the surface (with a \pkg{ravetools}
#' version that does not have \code{mris_smooth}, the \code{"laplace"} type
#' of \code{\link[ravetools]{vcg_smooth_explicit}} is used);
#' \code{"none"} returns the surface without smoothing
#' @param smooth_iterations number of averaging rounds when
#' \code{smooth_method} is \code{"explicit"}; default is \code{10}
#' @param max_vertices used only when \code{smooth_method} is
#' \code{"implicit"}, whose memory grows quickly with the surface size:
#' surfaces with more vertices than this are reduced to about this many with
#' \code{ravetools::vcg_decimate()} before smoothing, which removes vertices
#' from flat regions first and keeps the shape; default is \code{500000}.
#' Because the smoothing works in mesh steps, the same \code{lambda} and
#' \code{degree} smooth a reduced surface more; use a larger value or
#' \code{Inf} to smooth at full resolution. With a \pkg{ravetools} version
#' that does not have \code{vcg_decimate}, surfaces with more than
#' \code{20000} vertices are not smoothed, since the implicit smoothing of
#' those versions can crash on large surfaces
#'
#' @returns A \code{as_ieegio_surface} object; the surface is
#' transformed into anatomical space defined by the volume.
#'
#' @examples
#'
#'
#' # toy example; in practice, use tha path to the volume
#' volume <- array(0, dim = rep(30, 3))
#' volume[11:20, 11:20, 3:28] <- 1
#' volume[3:28, 11:20, 11:20] <- 1
#' volume[11:20, 3:28, 11:20] <- 1
#' vox2ras <- diag(1, 4)
#'
#' surf <- volume_to_surface(volume, vox2ras = vox2ras)
#'
#' if(interactive()) {
#'   plot(surf)
#' }
#'
#'
#' @export
volume_to_surface <- function(
    volume, lambda = 0.2, degree = 2, threshold_lb = 0.5, threshold_ub = NA,
    smooth_method = c("implicit", "explicit", "none"), smooth_iterations = 10L,
    max_vertices = 500000, ...) {

  smooth_method <- match.arg(smooth_method)

  # DIPSAUS DEBUG START
  # volume <- "~/rave_data/raw_dir/yael_demo_001/rave-imaging/fs/mri/ct_in_t1.nii.gz"
  # lambda = 0.2
  # degree = 2
  # threshold_lb = 0.5
  # threshold_ub = NA
  volume <- ieegio::as_ieegio_volume(x = volume, ...)

  vol_dim <- dim(volume)

  vox_to_ras <- volume$transforms[[1]]

  if (length(vol_dim) < 3) {
    vol_dim <- c(vol_dim, 1, 1, 1)[seq_len(3)]
    volume <- array(volume[], dim = vol_dim)
  } else if (length(vol_dim) > 3) {
    vol_dim <- vol_dim[seq_len(3)]
    volume <- array(volume$data[seq_len(prod(vol_dim))], dim = vol_dim)
  } else {
    volume <- array(volume[], dim = vol_dim)
  }

  if (is.na(threshold_lb)) { threshold_lb <- 0 }
  if (is.na(threshold_ub)) {
    nvox <- sum(volume > threshold_lb)
  } else {
    nvox <- sum(volume > threshold_lb & volume < threshold_ub)
  }

  if (nvox == 0) {
    # empty surface
    return(ieegio::as_ieegio_surface(matrix(c(0, 0, 0), ncol = 3)))
  }

  # Mesh
  mesh <- ravetools::vcg_isosurface(
    volume = volume,
    threshold_lb = threshold_lb,
    threshold_ub = threshold_ub,
    vox_to_ras = vox_to_ras
  )

  if (length(mesh$vb) < 9 && length(mesh$it) < 3) {
    return(ieegio::as_ieegio_surface(mesh, transform = diag(1, 4)))
  }

  # For compatibility, since some functions are not available in the old versions
  ravetools <- asNamespace("ravetools")
  mris_smooth <- ravetools$mris_smooth
  vcg_decimate <- ravetools$vcg_decimate

  if (smooth_method == "explicit" && !is.function(mris_smooth)) {
    warning("Explicit smooth is requested but package `ravetools` version is too low to have `mris_smooth`; using `vcg_smooth_explicit` instead.")
  }

  smooth <- function(mesh) {
    if (smooth_method == "none" || !length(mesh$it) || !length(mesh$vb)) {
      return(mesh)
    }
    if (smooth_method == "explicit") {
      if (is.function(mris_smooth)) {
        mesh <- mris_smooth(mesh, niterations = as.integer(smooth_iterations))
      } else {
        mesh <- ravetools::vcg_smooth_explicit(
          mesh,
          type = "laplace",
          iteration = as.integer(smooth_iterations)
        )
      }
    } else if (isTRUE(lambda > 0)) {
      mesh <- ravetools::vcg_smooth_implicit(
        mesh,
        lambda = lambda,
        use_mass_matrix = TRUE,
        fix_border = TRUE,
        use_cot_weight = FALSE,
        degree = degree
      )
    }
    mesh
  }

  # An iso-surface carries one vertex per voxel boundary, millions for a
  # whole-brain mask at sub-millimeter resolution, finer than the voxels
  # resolve. Implicit smoothing solves a linear system whose memory grows
  # with the surface (several GB for millions of vertices), so above
  # `max_vertices` the surface is decimated (flat regions first) before it is
  # smoothed. The 'Laplacian' works in mesh steps, so the same `lambda` and
  # `degree` smooth a decimated mesh more in millimeters; users who want
  # full-resolution smoothing raise `max_vertices`. Explicit smoothing (and
  # no smoothing) needs memory linear in the surface and is never decimated.
  # `ravetools` without `vcg_decimate` (0.3.2 and earlier) has an implicit
  # smoother that crashes R with a segmentation fault on large surfaces, so
  # there only surfaces up to `legacy_smooth_limit` vertices are smoothed
  n_vertices <- ncol(mesh$vb)
  max_vertices <- as.numeric(max_vertices)[[1]]
  legacy_smooth_limit <- 20000

  if (smooth_method == "implicit" && length(mesh$it)) {
    if (is.function(vcg_decimate)) {
      if (isTRUE(n_vertices > max_vertices)) {
        # Decimate is required before smoothing
        mesh <- vcg_decimate(mesh, ratio = max_vertices / n_vertices)
      }
      mesh <- smooth(mesh)
    } else if (n_vertices <= legacy_smooth_limit) {
      mesh <- smooth(mesh)
    } else if (isTRUE(lambda > 0)) {
      warning(sprintf(
        paste0(
          "The surface has %d vertices, more than %d that the installed ",
          "`ravetools` can smooth safely without `vcg_decimate`; no ",
          "smoothing is applied. Please update `ravetools`."
        ),
        n_vertices, legacy_smooth_limit
      ))
    }
  } else {
    mesh <- smooth(mesh)
  }

  if (length(mesh$vb) < 9 && length(mesh$it) < 3) {
    return(ieegio::as_ieegio_surface(mesh, transform = diag(1, 4)))
  }

  mesh <- ravetools::vcg_update_normals(mesh)

  # ravetools::rgl_view({
  #   ravetools::rgl_call("shade3d", mesh, col = 'red')
  # })

  surf <- ieegio::as_ieegio_surface(mesh, transform = diag(1, 4))

  surf
}
