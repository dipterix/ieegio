infer_transform_spaces <- function(x) {

  fname <- basename(x)

  entities <- strsplit(fname, "_", fixed = TRUE)[[1]]

  from_entity <- entities[startsWith(tolower(entities), "from-")]
  if(length(from_entity)) {
    from_entity <- gsub("^from\\-", "", from_entity[[1]])
  } else {
    from_entity <- ""
  }

  to_entity <- entities[startsWith(tolower(entities), "to-")]
  if(length(to_entity)) {
    to_entity <- gsub("^to\\-", "", to_entity[[1]])
  } else {
    to_entity <- ""
  }

  list(
    # passive transform - coordinate/reference frame transform
    # ieegio surface uses active transform/point transform
    interpretation = "passive",
    space_from = from_entity,
    space_to = to_entity
  )

}

#' Create transform between coordinate orientations
#'
#' Generates an affine transformation to convert coordinates or coordinate
#' frames between different anatomical orientation conventions (e.g., \code{RAS} to \code{LPS}).
#' Supports all 48 possible 3D orientations including axis permutations, plus
#' FSL scaled-voxel coordinates (which require an image for conversion).
#'
#' @param space_from either an \code{ieegio_space} object or a character string
#'   identifying the source space. If provided, \code{orientation_from} must
#'   be omitted (orientation is extracted from the space object).
#' @param orientation_from character string specifying the source orientation
#'   (e.g., \code{"RAS"}, \code{"LPS"}, \code{"FSL"}). Only used if \code{space_from} is
#'   missing. Must be one of the 48 valid orientation codes plus \code{"FSL"}.
#' @param orientation_to character string specifying the target orientation.
#'   Must be one of the 48 valid orientation codes plus \code{"FSL"}.
#' @param interpretation character string specifying transform interpretation:
#'   \itemize{
#'     \item \code{"active"} (default): Point transform - transforms point
#'       coordinates from one orientation to another. Use this when you have
#'       coordinates in the source orientation and want to convert them.
#'     \item \code{"passive"}: Axis transform - transforms the coordinate
#'       frame/basis vectors. This is the transpose of the active transform.
#'       Use this when transforming reference frames or basis vectors.
#'   }
#' @param image optional image for FSL coordinate conversion. Required when
#'   either \code{orientation_from} or \code{orientation_to} is \code{"FSL"}
#'   (but not both). Can be a file path or an \code{ieegio_volume} object.
#'
#' @returns An \code{ieegio_transforms} object containing a 4x4 affine
#'   transformation matrix
#'
#' @details
#' The function creates orthogonal transformations (rotations and reflections)
#' to convert between different anatomical coordinate conventions. For active
#' transforms, the matrix can be directly applied to homogeneous point coordinates.
#' For passive transforms, the matrix transforms coordinate axes/frames instead.
#'
#' Common orientation codes (first 8):
#' \itemize{
#'   \item \code{RAS}, \code{LAS}, \code{LPS}, \code{RPS}, \code{LPI}, \code{RPI}, \code{LAI}, \code{RAI} (standard axis order)
#' }
#'
#' Extended orientations (40 more) include axis permutations like:
#' \itemize{
#'   \item \code{PIR}, \code{AIL}, \code{SAR}, etc. (permuted axes)
#' }
#'
#' \strong{FSL Coordinates:}
#' When \code{"FSL"} orientation is involved, an image is required for conversion
#' because FSL coordinates are image-dependent (scaled voxels with possible X-flip).
#' Three scenarios are supported:
#' \itemize{
#'   \item \code{FSL -> FSL}: Identity transform (no image needed)
#'   \item \code{FSL -> RAS/other}: Uses \code{vox2ras \%*\% fsl2vox} from image
#'   \item \code{RAS/other -> FSL}: Uses \code{vox2fsl \%*\% ras2vox} from image
#' }
#'
#' The relationship between active and passive interpretations:
#' \code{passive_matrix = t(active_matrix)} for orthogonal transforms.
#'
#' @examples
#' # Active transform: convert point coordinates from RAS to LPS
#' trans <- transform_orientation(orientation_from = "RAS",
#'                                orientation_to = "LPS",
#'                                interpretation = "active")
#' trans$data[[1]]  # diag(-1, -1, 1, 1)
#'
#' # Apply to a point
#' point_ras <- c(10, 20, 30, 1)
#' point_lps <- trans$data[[1]] %*% point_ras
#'
#' # Using a space object
#' space <- new_space("scanner", orientation = "RAS")
#' trans <- transform_orientation(space_from = space,
#'                                orientation_to = "LPS")
#'
#' # Passive transform: transform coordinate axes
#' trans_passive <- transform_orientation(orientation_from = "RAS",
#'                                       orientation_to = "LPS",
#'                                       interpretation = "passive")
#'
#' # With axis permutation
#' trans <- transform_orientation(orientation_from = "RAS",
#'                                orientation_to = "PIR")
#'
#' \dontrun{
#' # FSL to RAS conversion (requires image)
#' trans_fsl2ras <- transform_orientation(orientation_from = "FSL",
#'                                        orientation_to = "RAS",
#'                                        image = "brain.nii.gz")
#' }
#'
#' @export
transform_orientation <- function(space_from, orientation_from, orientation_to,
                                  interpretation = c("active", "passive"),
                                  image = NULL) {

  if(missing(space_from)) {
    orientation_from <- match.arg(orientation_from, ORIENTATION_CODES)
    space_from <- ""
    dimension <- 3L
  } else {
    if(!missing(orientation_from)) {
      stop("Only one of 'space_from' or 'orientation_from' is allowed, not both")
    }
    orientation_from <- match.arg(attr(space_from, "orientation"), ORIENTATION_CODES)
    dimension <- c(as.integer(attr(space_from, "dimension")), 3L)
    dimension <- dimension[is.finite(dimension) & dimension > 0][[1]]
  }
  orientation_to <- match.arg(orientation_to, ORIENTATION_CODES)
  interpretation <- match.arg(interpretation)

  # Handle FSL coordinate cases
  fsl_from <- (orientation_from == "FSL")
  fsl_to <- (orientation_to == "FSL")

  if(fsl_from && fsl_to) {
    # FSL to FSL: identity transform
    mat <- diag(4)
  } else if(fsl_from || fsl_to) {
    # One side is FSL: need image for conversion
    if(is.null(image)) {
      stop("'image' is required when converting between FSL and other orientations")
    }

    # Load image if it's a path
    if(is.character(image)) {
      image <- read_volume(image, header_only = TRUE)
    }
    if(!inherits(image, "ieegio_volume")) {
      stop("'image' must be a file path or ieegio_volume object")
    }

    # Extract image info
    shape <- dim(image$data)[1:3]
    vox2ras <- image$transforms$vox2ras
    pixdim <- sqrt(colSums(vox2ras[1:3, 1:3]^2))
    vox2fsl <- get_vox2fsl(shape, pixdim, vox2ras)

    if(fsl_from) {
      # FSL -> RAS (or other world orientation)
      # First convert FSL to RAS, then RAS to target orientation
      fsl2ras <- vox2ras %*% solve(vox2fsl)

      if(orientation_to == "RAS") {
        mat <- fsl2ras
      } else {
        # Chain: FSL -> RAS -> target
        ras2target <- orientation_transform("RAS", orientation_to)
        mat <- ras2target %*% fsl2ras
      }
    } else {
      # RAS (or other) -> FSL
      # First convert source to RAS, then RAS to FSL
      ras2fsl <- vox2fsl %*% solve(vox2ras)

      if(orientation_from == "RAS") {
        mat <- ras2fsl
      } else {
        # Chain: source -> RAS -> FSL
        source2ras <- orientation_transform(orientation_from, "RAS")
        mat <- ras2fsl %*% source2ras
      }
    }
  } else {
    # Standard orientation conversion (no FSL)
    mat <- orientation_transform(orientation_from, orientation_to)
  }

  # If passive interpretation, use inverse (transpose for orthogonal matrices)
  if(interpretation == "passive") {
    mat <- t(mat)
  }

  new_transform(
    data = mat,
    type = "affine",
    space_from = new_space(space_from, orientation = orientation_from, dimension = dimension),
    space_to = new_space(space_from, orientation = orientation_to, dimension = dimension),
    dimension = dimension,
    interpretation = interpretation
  )

}

new_transform <- function(data,
                          type = c("affine", "deformation"),
                          space_from, space_to,
                          dimension = 3,
                          interpretation = c("active", "passive")) {
  # interpretation + matrix + space mapping + dimension

  type <- match.arg(type)
  interpretation <- match.arg(interpretation)
  dimension <- as.integer(dimension)

  if(type == "affine") {
    data <- as.matrix(data)
    nr <- nrow(data)
    nc <- ncol(data)
    if(!isTRUE(nr == nc && nr == (dimension + 1))) {
      nc <- dimension + 1
      stop(sprintf("Affine transform is mal-formed: must be a %d x %d matrix",
                   nc, nc))
    }
  }

  structure(
    list(
      data = list(data),
      type = type,
      interpretation = interpretation,
      space_from = new_space_internal(space_from, dimension = dimension),
      space_to = new_space_internal(space_to, dimension = dimension),
      dimension = as.integer(dimension)
    ),
    class = c(sprintf("ieegio_transform_%s", type), "ieegio_transforms")
  )
}

# Internal helper: invert a transform
# @param method "matrix" inverts the transformation matrices only (reverses order and computes matrix inverse)
#               "direction" swaps space_from/space_to only
#               "interpretation" toggles interpretation (active <-> passive) only
invert_transform <- function(transform, method = c("matrix", "direction", "interpretation")) {
  method <- match.arg(method)

  switch(
    method,
    "matrix" = {
      # Invert matrices only (reverse order and compute matrix inverses)
      if(transform$type == "deformation") {
        stop("Cannot invert deformation transforms with method='matrix' (not implemented)")
      }
      transform$data <- lapply(rev(transform$data), function(mat) { solve(mat) })
    },
    "direction" = {
      # Swap spaces only
      tmp <- transform$space_from
      transform$space_from <- transform$space_to
      transform$space_to <- tmp
    },
    "interpretation" = {
      # Toggle interpretation only
      transform$interpretation <- switch(
        transform$interpretation,
        "active" = "passive",
        "passive" = "active"
      )
    }
  )

  transform
}

# Internal helper: check if two transforms can be chained
# Returns list with:
#     chainable (logical),
#     space_from, space_to,
#     interpretation
are_transforms_chainable <- function(transform1, transform2, expected_interpretation = NA) {

  # Extract atomic values upfront - discard transform objects after extraction
  t1_interpretation <- transform1$interpretation
  t2_interpretation <- transform2$interpretation
  t1_space_from <- unclass(transform1$space_from)
  t1_space_to <- unclass(transform1$space_to)
  t2_space_from <- unclass(transform2$space_from)
  t2_space_to <- unclass(transform2$space_to)
  t1_is_affine <- (transform1$type == "affine")
  t2_is_affine <- (transform2$type == "affine")

  # Make sure interpretation is consistent
  if(is.na(expected_interpretation)) {
    if(!t2_is_affine) {
      # passive interpretation (for now deformation is always passive)
      expected_interpretation <- t2_interpretation
    } else {
      expected_interpretation <- t1_interpretation
    }
  }

  if(expected_interpretation != t1_interpretation) {
    if( t1_is_affine ) {
      # switch both interpretation and swap space_from & space_to
      # passive transform from A to B is the active transform from B to A
      t1_interpretation <- expected_interpretation
      tmp <- t1_space_from
      t1_space_from <- t1_space_to
      t1_space_to <- tmp
    } else {
      return(list(
        chainable = FALSE,
        reason = "Cannot chain: transform1 is non-invertible deformation with wrong interpretation or space mapping"
      ))
    }
  }

  if(expected_interpretation != t2_interpretation) {
    if( t2_is_affine ) {
      # switch both interpretation and swap space_from & space_to
      # passive transform from A to B is the active transform from B to A
      t2_interpretation <- expected_interpretation
      tmp <- t2_space_from
      t2_space_from <- t2_space_to
      t2_space_to <- tmp
    } else {
      return(list(
        chainable = FALSE,
        reason = "Cannot chain: transform2 is non-invertible deformation with wrong interpretation or space mapping"
      ))
    }
  }


  # After alignment, both should have expected_interpretation
  # Now just check if spaces are compatible
  # Use wildcard matching: empty string matches any space
  if(t1_space_to == "" || t2_space_from == "" || t1_space_to == t2_space_from) {
    return(list(
      chainable = TRUE,
      space_from = t1_space_from,
      space_to = t2_space_to,
      interpretation = expected_interpretation
    ))
  } else {
    return(list(
      chainable = FALSE,
      reason = sprintf("Cannot chain: space mismatch from %s to %s", t1_space_to, t2_space_from)
    ))
  }
}

#' Convert to ieegio transform
#'
#' Generic function to convert various objects into \code{ieegio_transforms}.
#'
#' @param x object to convert (character path, matrix, array, list, or existing transform)
#' @param ... additional arguments passed to methods
#' @param format character string specifying the file format for character paths.
#'   Supports \code{"ants"} (default) for ANTs format and \code{"flirt"} for
#'   FSL FLIRT format. Only used for character method.
#' @param space_from source space for matrix/array methods. Default \code{""} is
#'   a wildcard for arbitrary space name.
#' @param space_to target space for matrix/array methods. Default \code{""} is
#'   a wildcard for arbitrary space name.
#'
#' @returns An \code{ieegio_transforms} object
#'
#' @details
#' Methods available:
#' \itemize{
#'   \item \code{character}: Reads transform from file (uses \code{io_read_ants_transform}
#'     or \code{io_read_flirt_transform} depending on \code{format})
#'   \item \code{matrix}: Creates transform from matrix
#'   \item \code{array}: Creates transform from 2D array
#'   \item \code{list}: Creates transform chain from list of transforms
#'   \item \code{ieegio_transforms}: Returns input unchanged
#' }
#'
#' @export
as_ieegio_transform <- function(x, ...) {
  UseMethod("as_ieegio_transform")
}

#' @rdname as_ieegio_transform
#' @export
as_ieegio_transform.NULL <- function(x, space_from = "", space_to = "", ...) {
  return(new_transform(
    data = diag(4),
    type = "affine",
    space_from = space_from,
    space_to = space_to,
    ...
  ))
}

#' @rdname as_ieegio_transform
#' @export
as_ieegio_transform.character <- function(x, format = c("ants", "flirt"), ...) {
  format <- match.arg(format)
  switch(
    format,
    "ants" = io_read_ants_transform(x, ...),
    "flirt" = io_read_flirt_transform(x, ...),
    stop("Unknown transform format: ", format)
  )
}

#' @rdname as_ieegio_transform
#' @export
as_ieegio_transform.matrix <- function(x, space_from = "", space_to = "", ...) {

  if(identical(space_from, "")) {
    space_from <- attr(x, "source_space")
    if(length(space_from) != 1) {
      space_from <- ""
    }
  }

  if(identical(space_to, "")) {
    space_to <- attr(x, "source_space")
    if(length(space_to) != 1) {
      space_to <- ""
    }
  }

  new_transform(data = x, space_from = space_from, space_to = space_to, ...)
}

#' @rdname as_ieegio_transform
#' @export
as_ieegio_transform.array <- function(x, space_from = "", space_to = "", ...) {
  if(length(dim(x)) != 2) {
    stop("Array must be 2-dimensional to convert to transform")
  }
  new_transform(data = x, space_from = space_from, space_to = space_to, ...)
}

#' @rdname as_ieegio_transform
#' @export
as_ieegio_transform.list <- function(x, ...) {
  # Placeholder: will call new_transform_chain when implemented
  new_transform_chain(.list = x)
}

#' @rdname as_ieegio_transform
#' @export
as_ieegio_transform.ieegio_transforms <- function(x, ...) {
  if(...length() > 0) {
    return(new_transform_chain(x, ...))
  } else {
    # Return as-is to avoid recursive chaining
    return(x)
  }
}

#' @export
format.ieegio_transforms <- function(x, ...) {
  str <- c(
    "<ieegio transform>",
    sprintf("  Type: %s", x$type),
    sprintf("  Interpretation: %s transform (%s)", ifelse(x$interpretation == "passive", "axis", "coordinate"), x$interpretation),
    sprintf("  From space: %s", format(x$space_from)),
    sprintf("  To space: %s", format(x$space_to))
  )
  str
}

#' @export
print.ieegio_transforms <- function(x, ...) {
  cat(format(x), "", sep = "\n")
  return(invisible(x))
}

#' Read \code{ANTs} transform file
#'
#' Reads spatial transformation files in \code{ANTs} (Advanced Normalization Tools)
#' format, including affine matrices (\code{.mat}) and deformation fields
#' (\code{.h5}, \code{.nii.gz}).
#'
#' @param file character string specifying the path to the transform file.
#'   Supported formats include:
#'   \itemize{
#'     \item \code{.mat}: \code{ITK}/\code{ANTs} affine transform (4x4 matrix)
#'     \item \code{.h5}: HDF5 composite transform (may contain affine and/or
#'       deformation components)
#'     \item \code{.nii}, \code{.nii.gz}: Deformation field images
#'   }
#' @param space_from character string or \code{ieegio_space} object identifying
#'   the source space. If missing, will be inferred from the filename using
#'   BIDS-style \code{from-<space>} entity (e.g.,
#'   \code{"sub-01_from-T1w_to-MNI_xfm.h5"} yields \code{"T1w"}).
#' @param space_to character string or \code{ieegio_space} object identifying
#'   the target space. If missing, will be inferred from the filename using
#'   BIDS-style \code{to-<space>} entity.
#' @param interpretation character string specifying how to interpret the
#'   transform:
#'   \itemize{
#'     \item \code{"passive"} (default): Axis/coordinate frame transform.
#'       Represents how coordinate systems relate to each other. This is the
#'       typical interpretation for brain imaging registration transforms.
#'     \item \code{"active"}: Point transform. Directly transforms point
#'       coordinates from source to target space.
#'   }
#'
#' @returns An \code{ieegio_transforms} object with:
#'   \describe{
#'     \item{data}{List containing the transform data (matrix for affine,
#'       \code{ANTsTransform} object for deformation)}
#'     \item{type}{\code{"affine"} or \code{"deformation"}}
#'     \item{interpretation}{\code{"active"} or \code{"passive"}}
#'     \item{space_from}{Source space (with \code{"LPS"} orientation for \code{ANTs})}
#'     \item{space_to}{Target space (with \code{"LPS"} orientation for \code{ANTs})}
#'     \item{dimension}{Spatial dimension (typically 3)}
#'   }
#'
#' @details
#' \code{ANTs} transforms operate in \code{LPS} (Left-Posterior-Superior) coordinate
#' convention. The returned transform object automatically sets orientation
#' to \code{"LPS"} for both source and target spaces.
#'
#' For composite transforms (e.g., \code{.h5} files containing both affine
#' and deformation components), the function returns a single transform object.
#' Use \code{\link{as_ieegio_transform}} with a list to combine multiple transforms.
#'
#' This function requires the \code{rpyANTs} package and a configured Python
#' environment.
#'
#' @section BIDS Support:
#' The function can automatically infer space names from BIDS-compliant
#' file names:
#' \itemize{
#'   \item \code{from-<source>}: Source space identifier
#'   \item \code{to-<target>}: Target space identifier
#' }
#' Example: \code{"sub-01_from-T1w_to-MNI152NLin2009cAsym_mode-image_xfm.h5"}
#'
#' @examples
#' \dontrun{
#' # Read an affine transform
#' xfm <- io_read_ants_transform("sub-01_from-T1w_to-MNI_xfm.mat")
#'
#' # Explicitly specify spaces
#' xfm <- io_read_ants_transform(
#'   "transform.h5",
#'   space_from = "native",
#'   space_to = "MNI152"
#' )
#'
#' # Read as active (point) transform
#' xfm <- io_read_ants_transform(
#'   "transform.mat",
#'   interpretation = "active"
#' )
#'
#' # Chain multiple transforms
#' xfm1 <- io_read_ants_transform("from-T1w_to-T2w_xfm.mat")
#' xfm2 <- io_read_ants_transform("from-T2w_to-MNI_xfm.h5")
#' combined <- as_ieegio_transform(list(xfm1, xfm2))
#' }
#'
#' @seealso
#' \code{\link{as_ieegio_transform}} for converting objects to transforms and chaining
#' \code{\link{transform_orientation}} for orientation conversion transforms
#'
#' @export
io_read_ants_transform <- function(file, space_from, space_to, interpretation = c("passive", "active")) {

  interpretation <- match.arg(interpretation)

  # DIPSAUS DEBUG START
  # file <- '/Users/dipterix/PennNeurosurgery Dropbox/Dipterix W/Share_with_ZJ/test_for_ZJ/sub-OCD2_QSIPrep/anat_output_from_QSI_fslcomp_objects_in_this_space/sub-OCD2_from-T1wACPC_to-T1wNative_mode-image_xfm.mat'
  # interpretation = "auto"

  if(missing(space_from) || missing(space_to)) {
    inferred_space <- infer_transform_spaces(file)
    if(missing(space_from)) {
      space_from <- inferred_space$space_from
    }
    if(missing(space_to)) {
      space_to <- inferred_space$space_to
    }
  }

  check_py_flag()
  if(!rpyANTs::ants_available(module = "ants")) {
    if(dir.exists(rpymat::env_path())) {
      rpyANTs::install_ants(python_ver = "auto")
    } else {
      rpyANTs::install_ants()
    }
  }

  data <- rpyANTs::as_ANTsTransform(file)

  transform_type <- as.character(data$transform_type)

  AFFINE_TRANSFORM_TYPES <- asNamespace("rpyANTs")$AFFINE_TRANSFORM_TYPES
  if (transform_type %in% AFFINE_TRANSFORM_TYPES) {
    type <- "affine"
  } else {
    type <- "deformation"
  }

  dimension <- rpyANTs::py_to_r(data$dimension)

  space_from <- new_space(space_from, orientation = "LPS", dimension = dimension)
  space_to <- new_space(space_to, orientation = "LPS", dimension = dimension)

  new_transform(
    data,
    type = type,
    space_from = space_from,
    space_to = space_to,
    dimension = dimension,
    interpretation = interpretation
  )
}


#' Read FSL FLIRT transformation matrix
#'
#' Reads a 4x4 affine transformation matrix from an FSL FLIRT output file.
#' FLIRT matrices operate in FSL scaled-voxel coordinate system and require
#' source and reference images to convert to world (RAS) coordinates.
#'
#' @param file character string specifying the path to the FLIRT matrix file.
#'   This is a plain text file containing a 4x4 affine matrix.
#' @param space_from character string or \code{ieegio_space} object identifying
#'   the source (moving) space. If missing, will be inferred from the filename
#'   using BIDS-style \code{from-<space>} entity.
#' @param space_to character string or \code{ieegio_space} object identifying
#'   the target (reference) space. If missing, will be inferred from the
#'   filename using BIDS-style \code{to-<space>} entity.
#'
#' @returns An \code{ieegio_transforms} object with:
#'   \describe{
#'     \item{data}{List containing the 4x4 FLIRT matrix}
#'     \item{type}{\code{"affine"}}
#'     \item{interpretation}{\code{"active"} (FLIRT matrices are point transforms)}
#'     \item{space_from}{Source space (with \code{"FSL"} orientation)}
#'     \item{space_to}{Target space (with \code{"FSL"} orientation)}
#'     \item{dimension}{3}
#'   }
#'
#' @details
#' FLIRT matrices operate in FSL scaled-voxel coordinate system, which is:
#' \itemize{
#'   \item Voxel indices multiplied by voxel sizes (\code{pixdim})
#'   \item X-axis inverted if the image has positive \code{sform} determinant
#'     (neurological convention)
#' }
#'
#' The returned transform has \code{"FSL"} orientation for both source and
#' target spaces. To convert to world (RAS) coordinates, use
#' \code{\link{transform_flirt2ras}} with the source and/or reference images.
#'
#' FLIRT matrices are \strong{active} transforms: they map point coordinates
#' from the source (moving) image space to the reference (fixed) image space.
#'
#' @section BIDS Support:
#' The function can automatically infer space names from BIDS-compliant
#' file names:
#' \itemize{
#'   \item \code{from-<source>}: Source space identifier
#'   \item \code{to-<target>}: Target space identifier
#' }
#'
#' @examples
#' \dontrun{
#' # Read a FLIRT matrix
#' xfm <- io_read_flirt_transform("source_to_reference.mat")
#'
#' # Convert to RAS coordinates (requires source and reference images)
#' xfm_ras <- transform_flirt2ras(xfm, source = "source.nii.gz",
#'                                 reference = "reference.nii.gz")
#'
#' # Explicitly specify spaces
#' xfm <- io_read_flirt_transform(
#'   "transform.mat",
#'   space_from = "T1w",
#'   space_to = "MNI152"
#' )
#' }
#'
#' @seealso
#' \code{\link{transform_flirt2ras}} for converting to world coordinates
#' \code{\link{io_read_ants_transform}} for reading ANTs format transforms
#' \code{\link{as_ieegio_transform}} for converting objects to transforms
#'
#' @export
io_read_flirt_transform <- function(file, space_from, space_to) {

  # Infer spaces from filename if not provided
  if(missing(space_from) || missing(space_to)) {
    inferred_space <- infer_transform_spaces(file)
    if(missing(space_from)) {
      space_from <- inferred_space$space_from
    }
    if(missing(space_to)) {
      space_to <- inferred_space$space_to
    }
  }

  # Read the 4x4 matrix from plain text file
  flirt_matrix <- as.matrix(utils::read.table(file, header = FALSE))
  dimnames(flirt_matrix) <- NULL

  # Validate matrix dimensions

  if(!identical(dim(flirt_matrix), c(4L, 4L))) {
    stop(sprintf(
      "FLIRT matrix must be 4x4, got %dx%d",
      nrow(flirt_matrix), ncol(flirt_matrix)
    ))
  }

  dimension <- 3L

  # FSL uses its own scaled-voxel coordinate system
  space_from <- new_space(space_from, orientation = "FSL", dimension = dimension)
  space_to <- new_space(space_to, orientation = "FSL", dimension = dimension)

  # FLIRT matrices are active transforms (source -> reference point mapping)
  new_transform(
    data = flirt_matrix,
    type = "affine",
    space_from = space_from,
    space_to = space_to,
    dimension = dimension,
    interpretation = "active"
  )
}


#' Convert FLIRT transform to world (RAS) coordinates
#'
#' Converts an FSL FLIRT matrix from FSL scaled-voxel coordinates to world
#' (RAS) coordinates. Allows partial conversion by specifying only source,
#' only reference, or both images.
#'
#' @param transform an \code{ieegio_transforms} object with FSL orientation
#'   (typically from \code{\link{io_read_flirt_transform}}), or a 4x4 matrix
#' @param source source (moving) image used in FLIRT registration. Can be:
#'   \itemize{
#'     \item A file path to a NIfTI image
#'     \item An \code{ieegio_volume} object
#'     \item \code{NULL} to skip source-side conversion
#'   }
#' @param reference reference (fixed) image used in FLIRT registration. Can be:
#'   \itemize{
#'     \item A file path to a NIfTI image
#'     \item An \code{ieegio_volume} object
#'     \item \code{NULL} to skip reference-side conversion
#'   }
#'
#' @returns An \code{ieegio_transforms} object with updated orientations:
#'   \itemize{
#'     \item Both images provided: RAS -> RAS transform
#'     \item Source only: RAS -> FSL transform (source side converted)
#'     \item Reference only: FSL -> RAS transform (reference side converted)
#'     \item Neither: FSL -> FSL transform (unchanged)
#'   }
#'
#' @details
#' FSL FLIRT matrices operate in a scaled-voxel coordinate system that depends

#' on the image geometry. The conversion to world coordinates uses:
#'
#' \code{world_transform = ref_vox2ras \%*\% ref_fsl2vox \%*\% flirt \%*\% src_vox2fsl \%*\% src_ras2vox}
#'
#' Where:
#' \itemize{
#'   \item \code{src_ras2vox}: Inverse of source image's voxel-to-RAS matrix
#'   \item \code{src_vox2fsl}: Source voxel-to-FSL coordinate transform
#'   \item \code{flirt}: The original FLIRT matrix
#'   \item \code{ref_fsl2vox}: Inverse of reference voxel-to-FSL transform
#'   \item \code{ref_vox2ras}: Reference image's voxel-to-RAS matrix
#' }
#'
#' The FSL coordinate system uses scaled voxels with possible X-axis flip
#' depending on the image's \code{sform} determinant sign.
#'
#' @examples
#' \dontrun{
#' # Read FLIRT matrix
#' xfm <- io_read_flirt_transform("source_to_reference.mat")
#'
#' # Full conversion to RAS coordinates
#' xfm_ras <- transform_flirt2ras(xfm,
#'                                source = "source.nii.gz",
#'                                reference = "reference.nii.gz")
#'
#' # Partial conversion (reference side only)
#' xfm_partial <- transform_flirt2ras(xfm, reference = "reference.nii.gz")
#'
#' # Using ieegio_volume objects
#' src_vol <- read_volume("source.nii.gz", header_only = TRUE)
#' ref_vol <- read_volume("reference.nii.gz", header_only = TRUE)
#' xfm_ras <- transform_flirt2ras(xfm, source = src_vol, reference = ref_vol)
#' }
#'
#' @seealso
#' \code{\link{io_read_flirt_transform}} for reading FLIRT matrices
#' \code{\link{transform_orientation}} for general orientation transforms
#'
#' @export
transform_flirt2ras <- function(transform, source = NULL, reference = NULL) {

  # Convert transform to ieegio_transforms if needed
  if(is.matrix(transform)) {
    transform <- as_ieegio_transform(transform)
    transform$space_from <- new_space(
      transform$space_from, orientation = "FSL", dimension = 3L)
    transform$space_to <- new_space(
      transform$space_to, orientation = "FSL", dimension = 3L)
  } else {
    transform <- as_ieegio_transform(transform)
  }

  # Get the current matrix
  flirt_matrix <- transform$data[[1]]
  if(!is.matrix(flirt_matrix)) {
    stop("transform_flirt2ras only works with affine transforms")
  }

  result_matrix <- flirt_matrix
  orientation_from <- attr(transform$space_from, "orientation")
  orientation_to <- attr(transform$space_to, "orientation")

  # Helper to load image and extract needed info
  get_image_info <- function(img) {
    if(is.character(img)) {
      img <- read_volume(img, header_only = TRUE)
    }
    if(!inherits(img, "ieegio_volume")) {
      stop("Image must be a file path or ieegio_volume object")
    }
    shape <- dim(img$data)[1:3]
    vox2ras <- img$transforms$vox2ras
    pixdim <- sqrt(colSums(vox2ras[1:3, 1:3]^2))
    list(
      shape = shape,
      pixdim = pixdim,
      vox2ras = vox2ras
    )
  }

  # Process source image (right side of transform chain)
  if(!is.null(source)) {
    src_info <- get_image_info(source)
    src_vox2fsl <- get_vox2fsl(src_info$shape, src_info$pixdim, src_info$vox2ras)
    src_ras2vox <- solve(src_info$vox2ras)

    # Apply: flirt %*% src_vox2fsl %*% src_ras2vox
    result_matrix <- result_matrix %*% src_vox2fsl %*% src_ras2vox
    orientation_from <- "RAS"
  }

  # Process reference image (left side of transform chain)
  if(!is.null(reference)) {
    ref_info <- get_image_info(reference)
    ref_vox2fsl <- get_vox2fsl(ref_info$shape, ref_info$pixdim, ref_info$vox2ras)
    ref_fsl2vox <- solve(ref_vox2fsl)
    ref_vox2ras <- ref_info$vox2ras

    # Apply: ref_vox2ras %*% ref_fsl2vox %*% result_matrix
    result_matrix <- ref_vox2ras %*% ref_fsl2vox %*% result_matrix
    orientation_to <- "RAS"
  }

  # Update transform with new matrix and orientations
  new_transform(
    data = result_matrix,
    type = "affine",
    space_from = new_space(transform$space_from, orientation = orientation_from, dimension = 3L),
    space_to = new_space(transform$space_to, orientation = orientation_to, dimension = 3L),
    dimension = 3L,
    interpretation = transform$interpretation
  )
}


# Create a chain of transforms
new_transform_chain <- function(..., .list = NULL, interpretation = c("unset", "passive", "active")) {

  interpretation <- match.arg(interpretation)
  if(interpretation == "unset") {
    interpretation <- NA
  }

  # Get transform list from both ... and .list
  transform_list <- c(list(...), .list)

  # Filter out NULLs
  transform_list <- drop_nulls(transform_list)

  # If empty, return identity transform (wildcard space, RAS to RAS)
  if(length(transform_list) == 0) {
    return(new_transform(
      data = diag(4),
      type = "affine",
      space_from = "",
      space_to = "",
      dimension = 3,
      interpretation = if(is.na(interpretation)) "active" else interpretation
    ))
  }

  # Convert each item to ieegio_transform (handles various input types)
  transform_list <- lapply(transform_list, as_ieegio_transform)

  # If single transform, adjust interpretation if needed
  if(length(transform_list) == 1) {
    transform <- transform_list[[1]]

    # If interpretation was explicitly specified and differs, invert the transform
    if(!is.na(interpretation) && transform$interpretation != interpretation) {
      if(transform$type == "deformation") {
        # Deformation: swap direction + toggle interpretation (using equivalence)
        transform <- invert_transform(
          invert_transform(transform, method = "direction"),
          method = "interpretation"
        )
      } else {
        # Affine: invert matrix + toggle interpretation
        transform <- invert_transform(
          invert_transform(transform, method = "matrix"),
          method = "interpretation"
        )
      }
    }

    return(transform)
  }

  # Determine chain interpretation from first transform if not specified
  if(is.na(interpretation)) {
    interpretation <- transform_list[[1]]$interpretation
  }

  # Helper: check if spaces are compatible for chaining
  spaces_match <- function(space1, space2, allow_wildcard = TRUE) {
    # Wildcards match anything
    if(space1 == "" || space2 == "") return(allow_wildcard)
    return(space1 == space2)
  }

  # Helper: determine primary inversion method for interpretation change
  inversion_method <- function(trans) {
    # Returns the primary method (matrix or direction)
    if(trans$type == "deformation") {
      return("direction")
    }
    return("matrix")
  }

  # Validate and process transforms using Reduce
  result <- Reduce(function(accumulated, next_transform) {

    # First iteration
    if(is.null(accumulated)) {

      # Align next_transform to target interpretation
      # This makes sure accumulated always has consistent `interpretation`
      if(next_transform$interpretation != interpretation) {
        method <- inversion_method(next_transform)
        next_transform <- invert_transform(
          invert_transform(next_transform, method = method),
          method = "interpretation"
        )
      }

      return(next_transform)
    }

    # Validate: Check dimension compatibility
    if(accumulated$dimension != next_transform$dimension) {
      stop(sprintf(
        "Dimension mismatch: previous transform has dimension %d, next has %d",
        accumulated$dimension, next_transform$dimension
      ))
    }

    # Align next_transform to target interpretation
    next_transform_inverse_method <- inversion_method(next_transform)
    if(next_transform$interpretation != interpretation) {
      next_transform <- invert_transform(
        invert_transform(next_transform, method = next_transform_inverse_method),
        method = "interpretation"
      )
    }

    # Verify spaces are compatible for chaining
    accum_space_to <- unclass(accumulated$space_to)
    next_space_from <- unclass(next_transform$space_from)

    if(!spaces_match(accum_space_to, next_space_from)) {
      if(next_transform_inverse_method == "matrix" &&
         spaces_match(accum_space_to, unclass(next_transform$space_to), allow_wildcard = FALSE)) {
        # Backwards transform recovery: accumulated ends where next_transform ends
        # Double-invert to create forward transform (matrix inversion + direction swap)
        next_transform <- invert_transform(
          invert_transform(next_transform, method = "matrix"),
          method = "direction"
        )
      } else {
        stop(sprintf(
          "Cannot chain transforms: space mismatch (accumulated ends at '%s', next starts at '%s')",
          accum_space_to, next_space_from
        ))
      }
    }

    # Check orientation compatibility and insert orientation transform if needed
    orientation_from <- attr(accumulated$space_to, "orientation")
    orientation_to <- attr(next_transform$space_from, "orientation")

    # Merge consecutive affine matrices at boundary
    last_idx <- length(accumulated$data)
    last_data_is_matrix <- is.matrix(accumulated$data[[last_idx]])

    if(!is.null(orientation_from) && !is.null(orientation_to) && orientation_from != orientation_to) {
      # Create orientation transform
      orientation_xform <- transform_orientation(
        space_from = accumulated$space_to,
        orientation_to = orientation_to,
        interpretation = interpretation
      )
      # Append orientation transform matrix to accumulated data
      if(last_data_is_matrix) {
        accumulated$data[[last_idx]] <- orientation_xform$data[[1]] %*% accumulated$data[[last_idx]]
      } else {
        accumulated$data <- c(accumulated$data, orientation_xform$data)
        last_idx <- last_idx + 1
        last_data_is_matrix <- TRUE
      }

      # Update accumulated's space_to orientation to match
      accumulated$space_to <- new_space(
        accumulated$space_to,
        orientation = orientation_to,
        dimension = accumulated$dimension
      )
    }

    if(last_data_is_matrix &&
       is.matrix(next_transform$data[[1]])) {
      # Multiply matrices and replace last element
      accumulated$data[[last_idx]] <- next_transform$data[[1]] %*% accumulated$data[[last_idx]]
      # Concatenate remaining data from next_transform (skip first element)
      if(length(next_transform$data) > 1) {
        accumulated$data <- c(accumulated$data, next_transform$data[-1])
      }
    } else {
      # Concatenate all data
      accumulated$data <- c(accumulated$data, next_transform$data)
    }

    # Update accumulated metadata
    accumulated$space_to <- next_transform$space_to
    accumulated$interpretation <- interpretation

    # Update type: "deformation" if any non-matrix, otherwise "affine"
    if( accumulated$type == "affine" && next_transform$type == "affine" ) {
      accumulated$type <- "affine"
    } else {
      accumulated$type <- "deformation"
    }

    class(accumulated) <- c(sprintf("ieegio_transform_%s", accumulated$type), "ieegio_transforms")
    accumulated

  }, transform_list, init = NULL)

  result
}




apply_transform_to_points <- function(points, transform) {

  # Ensure transform is active interpretation for point transformation
  # This uses new_transform_chain to properly handle both affines and deformations
  transform <- new_transform_chain(.list = list(transform), interpretation = "active")

  dimension <- transform$dimension

  if(length(points) %in% c(dimension, dimension + 1)) {
    points <- matrix(points, nrow = 1)
  } else {
    points <- as.matrix(points)
  }
  nc <- ncol(points)
  stopifnot(is.numeric(points) && nc %in% c(dimension, dimension + 1))
  dimnames(points) <- NULL

  if(ncol(points) == dimension) {
    points <- cbind(points, 1)
  } else {
    points[, dimension + 1] <- 1
  }

  for(item in transform$data) {
    if(is.matrix(item)) {
      points <- points %*% t(item)
    } else {
      .NotYetImplemented()
    }
  }

  return(points)

}
