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
#' Supports all 48 possible 3D orientations including axis permutations.
#'
#' @param space_from either an \code{ieegio_space} object or a character string
#'   identifying the source space. If provided, \code{orientation_from} must
#'   be omitted (orientation is extracted from the space object).
#' @param orientation_from character string specifying the source orientation
#'   (e.g., \code{"RAS"}, \code{"LPS"}). Only used if \code{space_from} is
#'   missing. Must be one of the 48 valid orientation codes.
#' @param orientation_to character string specifying the target orientation.
#'   Must be one of the 48 valid orientation codes.
#' @param interpretation character string specifying transform interpretation:
#'   \itemize{
#'     \item \code{"active"} (default): Point transform - transforms point
#'       coordinates from one orientation to another. Use this when you have
#'       coordinates in the source orientation and want to convert them.
#'     \item \code{"passive"}: Axis transform - transforms the coordinate
#'       frame/basis vectors. This is the transpose of the active transform.
#'       Use this when transforming reference frames or basis vectors.
#'   }
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
#' @export
transform_orientation <- function(space_from, orientation_from, orientation_to, interpretation = c("active", "passive")) {

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

  # Get active transform matrix (applies to point coordinates)
  mat <- orientation_transform(orientation_from, orientation_to)

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
invert_transform <- function(transform, lazy = TRUE) {
  if(transform$type == "deformation") {
    stop("Cannot invert deformation transforms yet (not implemented)")
  }

  if( lazy ) {
    new_interpretation <- switch(
      transform$interpretation,
      "active" = "passive",
      "passive" = "active"
    )

    transform$interpretation <- new_interpretation

  } else {

    transform$data <- lapply(rev(transform$data), function(mat) { solve(mat) })
    tmp <- transform$space_from
    transform$space_from <- transform$space_to
    transform$space_to <- tmp

  }

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
#'   Currently supports \code{"ants"}. Only used for character method.
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
#'   \item \code{character}: Reads transform from file (uses \code{io_read_ants_transform})
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
as_ieegio_transform.character <- function(x, format = c("ants"), ...) {
  format <- match.arg(format)

  switch(
    format,
    "ants" = io_read_ants_transform(x, ...),
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



# Create a chain of transforms
new_transform_chain <- function(..., .list = NULL, expected_interpretation = c("unset", "passive", "active")) {

  expected_interpretation <- match.arg(expected_interpretation)
  if(expected_interpretation == "unset") {
    expected_interpretation <- NA
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
      interpretation = "active"
    ))
  }

  # Convert each item to ieegio_transform (handles various input types)
  transform_list <- lapply(transform_list, as_ieegio_transform)

  # If single transform, return as-is
  if(length(transform_list) == 1) {
    return(transform_list[[1]])
  }

  # Validate and process transforms using Reduce
  result <- Reduce(function(accumulated, next_transform) {

    # First iteration
    if(is.null(accumulated)) {
      return(next_transform)
    }

    # Validate: Check dimension compatibility
    if(accumulated$dimension != next_transform$dimension) {
      stop(sprintf(
        "Dimension mismatch: previous transform has dimension %d, next has %d",
        accumulated$dimension, next_transform$dimension
      ))
    }

    chainable_analysis <- are_transforms_chainable(accumulated, next_transform, expected_interpretation)

    if(!chainable_analysis$chainable) {
      stop(chainable_analysis$reason)
    }

    # Apply inversions if needed
    if(accumulated$interpretation != chainable_analysis$interpretation) {
      # swap both space mapping and interpretation to avoid inversing the matrix
      tmp <- accumulated$space_to
      accumulated$space_to <- accumulated$space_from
      accumulated$space_from <- tmp
      accumulated$interpretation <- chainable_analysis$interpretation
    }

    if(next_transform$interpretation != chainable_analysis$interpretation) {
      # swap both space mapping and interpretation to avoid inversing the matrix
      tmp <- next_transform$space_to
      next_transform$space_to <- next_transform$space_from
      next_transform$space_from <- tmp
      next_transform$interpretation <- chainable_analysis$interpretation
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
        interpretation = chainable_analysis$interpretation
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
    accumulated$interpretation <- chainable_analysis$interpretation

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

  transform <- as_ieegio_transform(transform)
  if(transform$interpretation == "passive") {
    # we need active transform, not passive
    tmp <- transform$space_from
    transform$space_to <- tmp
    transform$space_from <- transform$space_to
    transform$interpretation <- "active"
  }

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
