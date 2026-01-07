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
    ...) {

  # DIPSAUS DEBUG START
  # volume <- "~/rave_data/raw_dir/yael_demo_001/rave-imaging/fs/mri/ct_in_t1.nii.gz"
  # lambda = 0.2
  # degree = 2
  # threshold_lb = 0.5
  # threshold_ub = NA
  volume <- ieegio::as_ieegio_volume(x = volume, ...)

  vol_dim <- dim(volume)

  vox_to_ras <- volume$transforms[[1]]

  if(length(vol_dim) < 3) {
    vol_dim <- c(vol_dim, 1, 1, 1)[seq_len(3)]
    volume <- array(volume[], dim = vol_dim)
  } else if (length(vol_dim) > 3) {
    vol_dim <- vol_dim[seq_len(3)]
    volume <- array(volume$data[seq_len(prod(vol_dim))], dim = vol_dim)
  } else {
    volume <- array(volume[], dim = vol_dim)
  }

  if(is.na(threshold_lb)) { threshold_lb <- 0 }
  if(is.na(threshold_ub)) {
    nvox <- sum(volume > threshold_lb)
  } else {
    nvox <- sum(volume > threshold_lb & volume < threshold_ub)
  }

  if(nvox == 0) {
    # empty surface
    return(ieegio::as_ieegio_surface(matrix(c(0,0,0), ncol = 3)))
  }

  # Mesh
  mesh <- ravetools::vcg_isosurface(
    volume = volume,
    threshold_lb = threshold_lb,
    threshold_ub = threshold_ub,
    vox_to_ras = vox_to_ras
  )

  if(length(mesh$vb) < 9 && length(mesh$it) < 3) {
    return(ieegio::as_ieegio_surface(mesh, transform = diag(1, 4)))
  }

  # smooth
  if( isTRUE( lambda > 0 ) ) {
    mesh <- ravetools::vcg_smooth_implicit(
      mesh,
      lambda = lambda,
      use_mass_matrix = TRUE,
      fix_border = TRUE,
      use_cot_weight = FALSE,
      degree = degree
    )
  }

  if(length(mesh$vb) < 9 && length(mesh$it) < 3) {
    return(ieegio::as_ieegio_surface(mesh, transform = diag(1, 4)))
  }

  mesh <- ravetools::vcg_update_normals(mesh)

  # ravetools::rgl_view({
  #   ravetools::rgl_call("shade3d", mesh, col = 'red')
  # })

  surf <- ieegio::as_ieegio_surface(mesh, transform = diag(1, 4))

  surf
}
