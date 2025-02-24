#' Convert other surface formats to \code{ieegio} surface
#' @param x R object or file path
#' @param vertices \code{n} by 3 matrix, each row is a vertex node position
#' @param faces (optional) face index, either zero or one-indexed
#' (\code{Matlab} and \code{R} start counting from 1 while \code{C} and
#' \code{Python} start indices from 0); one-index face order is recommended
#' @param face_start (optional) either 0 or 1, indicating whether
#' \code{faces} is zero or one-indexed; default is \code{NA},
#' which will check whether the minimum value of \code{faces} is 0.
#' If so, then \code{faces} will be bumped by 1 internally
#' @param transform (optional) a 4 by 4 matrix indicating the vertex position to
#' scanner \code{RAS} transform. Default is missing (identity matrix), i.e.
#' the vertex positions are already in the scanner \code{RAS} coordinate
#' system.
#' @param vertex_colors (optional) integer or color (hex) vector indicating the
#' vertex colors
#' @param annotation_labels (optional) a data frame containing at the following
#' columns. Though optional, \code{annotation_labels} must be provided
#' when \code{annotation_values} is provided
#' \describe{
#' \item{\code{"Key"}}{unique integers to appear in \code{annotation_values},
#' indicating the key of the annotation label}
#' \item{\code{"Label"}}{a character vector (strings) of human-readable labels
#' of the corresponding key}
#' \item{\code{"Color"}}{hex string indicating the color of the key/label}
#' }
#' @param annotation_values (optional) an integer table where each column is
#' a vector of annotation key (for example, 'FreeSurfer' segmentation key)
#' and each row corresponds to a vertex node
#' @param measurements (optional) a numeric table where each column represents
#' a variable (for example, curvature) and each row corresponds to a
#' vertex node. Unlike annotations, which is for discrete node values,
#' \code{measurements} is for continuous values
#' @param time_series_slice_duration (optional) a numeric vector indicating
#' the duration of each slice; default is \code{NA}
#' @param time_series_value (optional) a numeric matrix (\code{n} by \code{m})
#' where \code{n} is the number of vertices and \code{m} is the number of time
#' points, hence each column is a time slice and each row is a vertex node.
#' @param name (optional) name of the geometry
#' @param ... passed to default method
#'
#' @returns An \code{ieeg_surface} object; see \code{\link{read_surface}} or
#' 'Examples'.
#'
#' @examples
#'
#'
#'
#' # ---- Simple usage
#' # vertices only
#' dodecahedron_vert <- matrix(
#'   ncol = 3, byrow = TRUE,
#'   c(-0.62, -0.62, -0.62, 0.62, -0.62, -0.62, -0.62, 0.62, -0.62,
#'     0.62, 0.62, -0.62, -0.62, -0.62, 0.62, 0.62, -0.62, 0.62,
#'     -0.62, 0.62, 0.62, 0.62, 0.62, 0.62, 0.00, -0.38, 1.00,
#'     0.00, 0.38, 1.00, 0.00, -0.38, -1.00, 0.00, 0.38, -1.00,
#'     -0.38, 1.00, 0.00, 0.38, 1.00, 0.00, -0.38, -1.00, 0.00,
#'     0.38, -1.00, 0.00, 1.00, 0.00, -0.38, 1.00, 0.00, 0.38,
#'     -1.00, 0.00, -0.38, -1.00, 0.00, 0.38)
#' )
#'
#' point_cloud <- as_ieegio_surface(dodecahedron_vert)
#' plot(point_cloud, col = "red")
#'
#' # with face index
#' dodecahedron_face <- matrix(
#'   ncol = 3L, byrow = TRUE,
#'   c(1, 11, 2, 1, 2, 16, 1, 16, 15, 1, 15, 5, 1, 5, 20, 1, 20, 19,
#'     1, 19, 3, 1, 3, 12, 1, 12, 11, 2, 11, 12, 2, 12, 4, 2, 4, 17,
#'     2, 17, 18, 2, 18, 6, 2, 6, 16, 3, 13, 14, 3, 14, 4, 3, 4, 12,
#'     3, 19, 20, 3, 20, 7, 3, 7, 13, 4, 14, 8, 4, 8, 18, 4, 18, 17,
#'     5, 9, 10, 5, 10, 7, 5, 7, 20, 5, 15, 16, 5, 16, 6, 5, 6, 9,
#'     6, 18, 8, 6, 8, 10, 6, 10, 9, 7, 10, 8, 7, 8, 14, 7, 14, 13)
#' )
#' mesh <- as_ieegio_surface(dodecahedron_vert,
#'                           faces = dodecahedron_face)
#' plot(mesh)
#'
#' # with vertex colors
#' mesh <- as_ieegio_surface(dodecahedron_vert,
#'                           faces = dodecahedron_face,
#'                           vertex_colors = sample(20))
#' plot(mesh, name = "color")
#'
#' # with annotations
#' mesh <- as_ieegio_surface(
#'   dodecahedron_vert,
#'   faces = dodecahedron_face,
#'   annotation_labels = data.frame(
#'     Key = 1:3,
#'     Label = c("A", "B", "C"),
#'     Color = c("red", "green", "blue")
#'   ),
#'   annotation_values = data.frame(
#'     MyVariable = c(rep(1, 7), rep(2, 7), rep(3, 6))
#'   )
#' )
#' plot(mesh, name = "annotations")
#'
#' # with measurements
#' mesh <- as_ieegio_surface(
#'   dodecahedron_vert,
#'   faces = dodecahedron_face,
#'   measurements = data.frame(
#'     MyVariable = dodecahedron_vert[, 1]
#'   )
#' )
#' plot(mesh, name = "measurements",
#'      col = c("blue", "gray", "red"))
#'
#'
#'
#' @export
as_ieegio_surface <- function(x, ...) {
  UseMethod("as_ieegio_surface")
}

#' @rdname as_ieegio_surface
#' @export
as_ieegio_surface.default <- function(
    x, vertices = x,
    faces = NULL, face_start = NA,
    transform = NULL,
    vertex_colors = NULL,
    annotation_labels = NULL,
    annotation_values = NULL,
    measurements = NULL,
    time_series_slice_duration = NULL,
    time_series_value = NULL,
    name = NULL,
    ...) {

  # DIPSAUS DEBUG START
  # x <- rbind(array(rnorm(30), c(3, 10)), 1)

  if(missing(x)) {
    if(!missing(vertices)) {
      x <- vertices
    } else {
      x <- NULL
    }
  }

  if(is.matrix(transform)) {
    stopifnot(nrow(transform) == 4 && ncol(transform) == 4)
    source_space <- attr(transform, "source_space")
    if(is.null(source_space)) {
      attr(transform, "source_space") <- "Unknown"
    }
    target_space <- attr(transform, "target_space")
    if(is.null(target_space)) {
      attr(transform, "target_space") <- "Unknown"
    }
    transforms <- list(
      ScannerAnat = transform
    )
  } else {
    transforms <- NULL
  }

  n_verts <- NA_integer_
  n_faces <- 0L
  mesh_face_type <- "points"
  if(!is.null(vertices)) {
    if(!is.matrix(vertices)) {
      stop("`vertices` must be a nx3 or nx4 matrix")
    }
    dm_vert <- dim(vertices)
    if(!dm_vert[2] %in% c(3, 4)) {
      stop("`vertices` must be a nx3 or nx4 matrix")
    }

    vertices <- t(cbind(vertices[, c(1, 2, 3), drop = FALSE], 1))
    n_verts <- ncol(vertices)

    if(is.matrix(faces)) {
      dm_faces <- dim(faces)
      if(!dm_faces[2] %in% 3) {
        stop("`faces`, if provided, must be a mx3 matrix")
      }
      if(is.na(face_start)) {
        face_start <- min(faces, na.rm = TRUE)
      }
      if(face_start <= 0) {
        faces <- faces - face_start + 1
      }
      faces <- t(faces)
      storage.mode(faces) <- "integer"
      mesh_face_type <- "tris"
      n_faces <- ncol(faces)
    } else {
      faces <- NULL
      mesh_face_type <- "points"
      n_faces <- 0
    }

    geometry <- list(
      vertices = vertices,
      faces = faces,
      transforms = transforms,
      face_start = 1L,
      meta = list(Name = name)
    )
  } else {
    geometry <- NULL
    n_verts <- NA_integer_
  }

  if(length(vertex_colors) > 0) {
    if(is.na(n_verts)) {
      n_verts <- length(vertex_colors)
    } else {
      nmult <- ceiling(n_verts / length(vertex_colors))
      if(nmult > 1) {
        vertex_colors <- rep(vertex_colors, nmult)
      }
    }
    vertex_colors <- t(grDevices::col2rgb(vertex_colors[seq_len(n_verts)], alpha = FALSE))
  } else {
    vertex_colors <- NULL
  }

  if(length(measurements)) {
    if(!is.data.frame(measurements)) {
      stop("`measurements` must be a numeric table.")
    }
    measurements <- data.table::as.data.table(measurements, keep.rownames = FALSE)
    if(is.na(n_verts)) {
      n_verts <- nrow(measurements)
    }
    meas_names <- names(measurements)
    meas_meta <- structure(lapply(meas_names, function(x) {
      list(intent = "NIFTI_INTENT_SHAPE")
    }), names = meas_names)
    measurements <- list(
      data_table = measurements,
      meta = meas_meta
    )
  } else {
    measurements <- NULL
  }

  if(length(annotation_values)) {
    if(!is.data.frame(annotation_values)) {
      stop("`annotation_values` must be a integer table")
    }
    if(!is.data.frame(annotation_labels)) {
      stop("`annotation_labels`, if provided, must be a table with integer `Key`, annotation `Label`, and `Color` string")
    }
    annotation_labels <- data.table::as.data.table(annotation_labels)
    annot_label_names <- names(annotation_labels)
    if(!all(c("Key", "Label", "Color") %in% annot_label_names)) {
      stop("`annotation_labels`, if provided, must be a table with integer `Key`, annotation `Label`, and `Color` string")
    }
    annotation_values <- data.table::as.data.table(annotation_values)
    if(is.na(n_verts)) {
      n_verts <- nrow(annotation_values)
    }
    annot_names <- names(annotation_values)
    for(nm in annot_names) {
      storage.mode(annotation_values[[nm]]) <- "integer"
    }
    label_colors <- grDevices::col2rgb(annotation_labels$Color, alpha = FALSE) / 255
    label_table <- data.table::data.table(
      Key = as.integer(annotation_labels$Key),
      Label = as.character(annotation_labels$Label),
      Red = label_colors[1, ],
      Green = label_colors[2, ],
      Blue = label_colors[3, ],
      Color = grDevices::rgb(red = label_colors[1, ], green = label_colors[2, ], blue = label_colors[3, ], maxColorValue = 1)
    )

    annotations <- list(
      label_table = label_table,
      data_table = annotation_values,
      meta = structure(lapply(annot_label_names, function(x) {list()}), names = annot_label_names)
    )

  } else {
    annotations <- NULL
  }

  if(length(time_series_value)) {
    if(!is.matrix(time_series_value) || !is.numeric(time_series_value)) {
      stop("`time_series_value` must be numerical matrix")
    }
    n_time <- ncol(time_series_value)
    if(is.na(n_verts)) {
      n_verts <- nrow(time_series_value)
    }
    if(length(time_series_slice_duration) > 0) {
      time_series_slice_duration <- as.double(time_series_slice_duration)
      if(length(time_series_slice_duration) < n_time) {
        n_time1 <- length(time_series_slice_duration)
        time_series_slice_duration <- c(time_series_slice_duration, rep(time_series_slice_duration[[n_time1]], n_time - n_time1))
      }
      time_series_slice_duration <- time_series_slice_duration[seq_len(n_time)]
    } else {
      time_series_slice_duration <- rep(NA_real_, n_time)
    }
    time_series <- list(
      description = "`value`: vertex by time-point matrix",
      slice_duration = time_series_slice_duration,
      value = time_series_value
    )
  } else {
    time_series <- NULL
  }


  surf <- new_surface(
    header = structure(list(
      internal = list(
        num_vertices_expected = n_verts,
        num_faces_expected = n_faces
      ),
      mesh_face_type = mesh_face_type
    ), class = "basic_geometry"),
    geometry = geometry,
    color = vertex_colors,
    measurements = measurements,
    annotations = annotations,
    time_series = time_series
  )

  return(surf)

}


#' @rdname as_ieegio_surface
#' @export
as_ieegio_surface.character <- function(x, ...) {
  stopifnot(file.exists(x))
  return(read_surface(file = x, ...))
}

#' @rdname as_ieegio_surface
#' @export
as_ieegio_surface.ieegio_surface <- function(x, ...) {
  return(x)
}

#' @rdname as_ieegio_surface
#' @export
as_ieegio_surface.mesh3d <- function(x, ...) {


  vertices <- t(x$vb[1:3, , drop = FALSE])
  faces <- x$it

  if(length(faces)) {
    faces <- t(faces[1:3, , drop = FALSE])
  } else {
    faces <- NULL
  }

  surf <- as_ieegio_surface.default(vertices = vertices, faces = faces, face_start = 1L, ...)

  surf
}

#' @rdname as_ieegio_surface
#' @export
as_ieegio_surface.fs.surface <- function(x, ...) {

  vertices <- x$vertices[, c(1, 2, 3), drop = FALSE]
  faces <- x$faces

  if(length(faces)) {
    faces <- faces[1:3, , drop = FALSE]
  } else {
    faces <- NULL
  }

  surf <- as_ieegio_surface.default(vertices = vertices, faces = faces, face_start = NA, ...)
  surf
}


#
#
# if(FALSE) {
#
#   p <- read_surface("~/rave_data/raw_dir/DBS02/rave-imaging/fs/surf/lh.pial")
#   a <- read_surface("~/rave_data/raw_dir/DBS02/rave-imaging/fs/label/lh.aparc.annot")
#   s <- read_surface("~/rave_data/raw_dir/DBS02/rave-imaging/fs/surf/lh.sulc")
#
#   n_verts <- ncol(p$geometry$vertices)
#   surf <- as_ieegio_surface.default(
#     x, vertices = t(p$geometry$vertices),
#     faces = t(p$geometry$faces),
#     transform = p$geometry$transforms$ScannerAnat,
#     vertex_colors = NULL,
#     annotation_labels = a$annotations$label_table,
#     annotation_values = a$annotations$data_table,
#     measurements = s$measurements$data_table,
#     time_series_value = matrix(rnorm(n_verts * 3), nrow = n_verts),
#     name = "merged")
#
#
# }
