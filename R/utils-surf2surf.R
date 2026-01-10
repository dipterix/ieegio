#' Transform surface between coordinate spaces
#'
#' Transforms surface vertex positions from one coordinate space or orientation
#' to another, optionally applying an additional custom transform.
#'
#' @param surface an \code{ieegio_surface} object or file path; see
#'   \code{\link{as_ieegio_surface}} for valid inputs
#' @param space_from source coordinate space; either an \code{ieegio_space}
#'   object (from \code{\link{new_space}}) or a character string; default
#'   is empty string
#' @param space_to target coordinate space; either an \code{ieegio_space}
#'   object or a character string; default is empty string
#' @param transform optional 4x4 affine transformation matrix or
#'   \code{ieegio_transforms} object to apply; see
#'   \code{\link{as_ieegio_transform}}
#'
#' @returns A transformed \code{ieegio_surface} object with updated vertex
#'   positions and transform metadata
#'
#' @details
#' The function handles orientation changes (e.g., \code{"RAS"} to \code{"LPS"})
#' and optional custom transforms. It creates a transform chain consisting of:
#' an affine (orientation alignment from source), the custom transform, and
#' a post-affine (final orientation alignment to target).
#'
#' If the provided transform has a \code{"passive"} interpretation, it is
#' automatically converted to an \code{"active"} interpretation before
#' being applied to the vertex coordinates.
#'
#' @seealso
#' \code{\link{as_ieegio_surface}} for creating surface objects,
#' \code{\link{new_space}} for defining coordinate spaces,
#' \code{\link{transform_orientation}} for orientation transforms,
#' \code{\link{volume_to_surface}} for creating surfaces from volumes
#'
#' @examples
#'
#' library(ieegio)
#'
#' # geometry
#' geom_file <- "gifti/GzipBase64/sujet01_Lwhite.surf.gii"
#'
#' if(ieegio_sample_data(geom_file, test = TRUE)) {
#'
#' surf_ras <- read_surface(ieegio_sample_data(geom_file))
#' plot(surf_ras)
#'
#' # ---- Change axis orientation ------------------
#' # convert from RAS orientation to LPS
#' surf_lps <- surface_to_surface(
#'   surf_ras,
#'   space_from = new_space("", orientation = "RAS"),
#'   space_to = new_space("", orientation = "LPS")
#' )
#' plot(surf_lps)
#'
#' # validate
#' lps_verts <- diag(c(-1, -1, 1, 1)) %*% surf_ras$geometry$vertices
#' range(surf_lps$geometry$vertices - lps_verts)
#'
#' # ---- Apply transforms ------------------
#' transform <- matrix(
#'   byrow = TRUE, nrow = 4,
#'   c(
#'     0.5, 0, 0.3, 1,
#'     0, -1, 0.2, 2,
#'     0, 0.7, -0.5, 4,
#'     0, 0, 0, 1
#'   )
#' )
#' surf_stretch <- surface_to_surface(surf_ras, transform = transform)
#' plot(surf_stretch)
#'
#' # validate
#' stretch_verts <- transform %*% surf_ras$geometry$vertices
#' range(surf_stretch$geometry$vertices - stretch_verts)
#'
#'
#' }
#' @export
surface_to_surface <- function(surface, space_from = "", space_to = "", transform = NULL) {

  # DIPSAUS DEBUG START
  # surface <- ieegio_sample_data('gifti/GzipBase64/sujet01_Lwhite.surf.gii')
  # space_from = ieegio::new_space("T1wACPC", orientation = "RAS")
  # space_to = ieegio::new_space("T1wNative", orientation = "LPS")
  # transform = structure(list(data = list(structure(c(
  #   0.999772369861603, 0.0145429726690054, 0.0156085658818483, 0,
  #   -0.0109670050442219, 0.977920234203339, -0.208690464496613, 0,
  #   -0.0182989109307528, 0.208471775054932, 0.977857172489166, 0,
  #   0.912206709384918, -25.4448070526123, -13.4114465713501, 1),
  #   dim = c(4L, 4L))), type = "affine", interpretation = "passive",
  #   space_from = structure(
  #     "T1wNative",
  #     orientation = "LPS",
  #     dimension = 3L,
  #     class = "ieegio_space"
  #   ), space_to = structure(
  #     "T1wACPC",
  #     orientation = "LPS",
  #     dimension = 3L,
  #     class = "ieegio_space"
  #   ), dimension = 3L), class = c("ieegio_transform_affine", "ieegio_transforms"))

  surface <- as_ieegio_surface(surface)

  vertices <- surface$geometry$vertices
  if(isTRUE(nrow(vertices) == 3)) {
    dimnames(vertices) <- NULL
    vertices <- rbind(vertices, 1)
  }

  if(length(surface$geometry$transforms)) {
    vertices <- surface$geometry$transforms[[1]] %*% surface$geometry$vertices
  }

  if(!inherits(space_from, "ieegio_space")) {
    space_from <- ieegio::new_space(name = space_from)
  }
  if(!inherits(space_to, "ieegio_space")) {
    space_to <- ieegio::new_space(name = space_to)
  }

  transform <- ieegio::as_ieegio_transform(transform)

  pre_affine <- new_transform(
    diag(4),
    type = "affine",
    space_from = space_from,
    space_to = space_from,  # Same space - identity transform
    interpretation = "active"
  )

  post_affine <- new_transform(
    diag(4),
    type = "affine",
    space_from = space_to,
    space_to = space_to,  # Same space - identity transform
    interpretation = "active"
  )

  chained_transform <- new_transform_chain(pre_affine, transform, post_affine, interpretation = "active")

  vertices_t <- apply_transform_to_points(t(vertices), transform = chained_transform)

  surface$geometry$vertices[1:3, ] <- t(vertices_t[, 1:3, drop = FALSE])

  # The transform is now undefined
  space_to <- as.character(chained_transform$space_to)
  if(!nzchar(space_to)) {
    space_to <- "Unknown"
  }

  surface$geometry$transforms <- structure(
    list(
      structure(diag(1, 4),
                source_space = "Unknown",
                target_space = space_to)
    ),
    names = space_to
  )

  surface

  #
  # orientation_from <- attr(accumulated$space_to, "orientation")
  # orientation_to <- attr(next_transform$space_from, "orientation")
  #
  # # Merge consecutive affine matrices at boundary
  # last_idx <- length(accumulated$data)
  # last_data_is_matrix <- is.matrix(accumulated$data[[last_idx]])
  #
  # if(!is.null(orientation_from) && !is.null(orientation_to) && orientation_from != orientation_to) {
  #   # Create orientation transform
  #   orientation_xform <- transform_orientation(
  #     space_from = accumulated$space_to,
  #     orientation_to = orientation_to,
  #     interpretation = chainable_analysis$interpretation
  #   )
  #   # Append orientation transform matrix to accumulated data
  #   if(last_data_is_matrix) {
  #     accumulated$data[[last_idx]] <- orientation_xform$data[[1]] %*% accumulated$data[[last_idx]]
  # new_transform_chain()
  # surface$geometry$vertices

}
