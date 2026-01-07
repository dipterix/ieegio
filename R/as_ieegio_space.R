#' Define a coordinate space
#'
#' Creates an object representing a coordinate space/reference frame used in
#' medical imaging. The orientation defines the anatomical meaning of the
#' coordinate axes.
#'
#' @param name character string identifying the coordinate space (e.g.,
#'   \code{"T1w"}, \code{"MNI152NLin2009cAsym"}, \code{"scanner"});
#'   default is \code{""}, a wildcard that indicates arbitrary space
#' @param orientation character string specifying the axis orientation
#'   convention. Common orientations in brain imaging:
#'   \itemize{
#'     \item \code{"RAS"}: Right-Anterior-Superior (FreeSurfer, NIfTI default)
#'     \item \code{"LAS"}: Left-Anterior-Superior
#'     \item \code{"LPS"}: Left-Posterior-Superior (\code{DICOM}, \code{ANTs}, \code{ITK})
#'     \item \code{"RPS"}: Right-Posterior-Superior
#'     \item \code{"LPI"}: Left-Posterior-Inferior
#'     \item \code{"RPI"}: Right-Posterior-Inferior
#'     \item \code{"LAI"}: Left-Anterior-Inferior
#'     \item \code{"RAI"}: Right-Anterior-Inferior
#'   }
#' @param dimension integer dimension of the space (typically 3 for 3D imaging)
#' @param ... additional attributes to attach to the space object
#'
#' @returns An S3 object of class \code{"ieegio_space"} with attributes
#'   \code{orientation} and \code{dimension}
#'
#' @details
#' Orientation codes use three letters to define the positive direction of the
#' x, y, and z axes respectively:
#' \itemize{
#'   \item First letter (x-axis): \strong{L}eft or \strong{R}ight
#'   \item Second letter (y-axis): \strong{A}nterior or \strong{P}osterior
#'   \item Third letter (z-axis): \strong{S}uperior or \strong{I}nferior
#' }
#'
#' For example, \code{"RAS"} means: +x points Right, +y points Anterior
#' (toward face), +z points Superior (toward top of head).
#'
#' @examples
#' # FreeSurfer/NIfTI convention
#' scanner_space <- new_space("scanner", orientation = "RAS")
#' print(scanner_space)
#'
#' # DICOM/ANTs convention
#' mni_space <- new_space("MNI152NLin2009cAsym", orientation = "LPS", dimension = 3)
#' format(mni_space)
#'
#' @export
new_space <- function(name = "", orientation = ORIENTATION_CODES,
                      dimension = 3, ...) {
  name <- as.character(unclass(name))
  if(length(name) != 1 || is.na(name)) {
    stop("Invalid space name")
  }
  orientation <- match.arg(orientation)
  structure(name, orientation = orientation, dimension = as.integer(dimension), ..., class = "ieegio_space")
}

#' @export
format.ieegio_space <- function(x, ...) {
  x_ <- unclass(x)
  sprintf("%s (%s)", x_, attr(x, "orientation"))
}

#' @export
print.ieegio_space <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}

new_space_internal <- function(space, orientation, dimension) {
  if(missing(orientation) || is.null(orientation)) {
    orientation <- attr(space, "orientation")
    if(length(orientation) != 1) {
      orientation <- "RAS"
    }
  }
  if(missing(dimension) || is.null(dimension)) {
    dimension <- attr(space, "dimension")
    if(length(dimension) != 1) {
      dimension <- 3
    }
  }
  new_space(space, orientation = orientation, dimension = dimension)
}

# Internal helper: create active transform matrix between orientations
orientation_transform <- function(from, to) {
  from <- match.arg(from, ORIENTATION_CODES)
  to <- match.arg(to, ORIENTATION_CODES)

  # Parse orientation codes
  from_axes <- strsplit(from, "")[[1]]
  to_axes <- strsplit(to, "")[[1]]

  # Map each axis letter to its opposite
  axis_opposites <- c(
    "R" = "L", "L" = "R",
    "A" = "P", "P" = "A",
    "S" = "I", "I" = "S"
  )

  # Initialize 4x4 matrix (active transform for points)
  mat <- matrix(0, nrow = 4, ncol = 4)
  mat[4, 4] <- 1  # Homogeneous coordinate

  # For each axis in the target orientation, find where it comes from
  # This is an ACTIVE transform: it transforms point coordinates
  for(i in 1:3) {
    to_axis <- to_axes[i]
    opposite_axis <- axis_opposites[to_axis]

    # Find which source axis matches (same direction or opposite)
    if(to_axis %in% from_axes) {
      from_pos <- which(from_axes == to_axis)
      mat[i, from_pos] <- 1
    } else if(opposite_axis %in% from_axes) {
      from_pos <- which(from_axes == opposite_axis)
      mat[i, from_pos] <- -1
    } else {
      stop(sprintf("Unable to map axis '%s' in orientation '%s'", to_axis, to))
    }
  }

  mat
}
