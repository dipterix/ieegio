
new_surface <- function(
    header = NULL, geometry = NULL, color = NULL,
    annotations = NULL, measurements = NULL, time_series = NULL,
    sparse_node_index = NULL) {
  has_geometry <- !is.null(geometry)
  has_color <- is.matrix(color)
  has_annotations <- !is.null(annotations)
  has_measurements <- !is.null(measurements)
  has_time_series <- !is.null(time_series)
  has_sparse_node_index <- !is.null(sparse_node_index)

  re <- list(
    header = header,
    sparse = has_sparse_node_index
  )

  contains <- NULL

  # try to get node numbers
  n_vertex <- NA
  n_vert_values <- NA # if sparse, what's the expected number of node values

  validate_n <- function(n) {
    if( is.na(n_vert_values) && is.na(n_vertex) ) { return(TRUE) }
    if(is.na(n_vert_values)) {
      return(isTRUE(n_vertex == n))
    }
    if(is.na(n_vertex)) {
      return(isTRUE(n_vert_values == n))
    }
    isTRUE(n_vert_values == n) || isTRUE(n_vertex == n)
  }

  if( has_sparse_node_index ) {
    node_start <- sparse_node_index$node_index_start
    node_index <- as.integer(sparse_node_index$node_index - node_start + 1L)

    n_vert_values <- length(node_index)
    sparse_node_index <- structure(
      node_index,
      start_index = 1L
    )
    re$sparse_node_index <- sparse_node_index
  }

  if(has_geometry) {
    contains <- "geometry"
    re$geometry <- geometry
    if(is.matrix(geometry$vertices)) {
      n_vertex <- ncol(geometry$vertices)
    } else {
      # what the hell?
      re$geometry <- NULL
      contains <- NULL
    }
    n_vert_values <- n_vertex
  } else if(has_sparse_node_index) {
    n_vertex <- max(sparse_node_index, na.rm = TRUE)
  }



  if( has_color ) {
    if(validate_n(nrow(color))) {
      re$color <- color
      contains <- c(contains, "color")
    } else {
      warning("Vertex `color` attribute has inconsistent length with the ",
              "expected vertex length. The `color` attribute is discarded.")
    }
  }

  if(has_annotations) {
    # list(
    #   label_table = parse_label_table(gii$label),
    #   data_table = data.table::data.table(),
    #   meta = list()
    # )
    if(validate_n( nrow(annotations$data_table) )) {
      re$annotations <- annotations
      contains <- c(contains, "annotations")
    } else {
      warning("Vertex `annotations` attribute has inconsistent length with ",
              "the expected vertex length. ",
              "The `annotations` attribute is discarded.")
    }
  }

  if(has_measurements) {
    # list(
    #   data_table = data.table::data.table(),
    #   meta = list()
    # )
    if(validate_n( nrow(measurements$data_table) )) {
      re$measurements <- measurements
      contains <- c(contains, "measurements")
    } else {
      warning("Vertex `measurements` attribute has inconsistent length with ",
              "the expected vertex length. The `measurements` attribute is ",
              "discarded.")
    }
  }

  if(has_time_series) {
    # list(
    #   description = "`value`: vertex by time-point matrix",
    #   slice_duration = numeric(0L),
    #   value = NULL
    # )
    if(validate_n( nrow(time_series$value) )) {
      re$time_series <- time_series
      contains <- c(contains, "time_series")
    } else {
      warning("Vertex `time_series` attribute has inconsistent length with ",
              "the expected vertex length. The `time_series` attribute ",
              "is discarded.")
    }
  }

  # contains <- geometry color annotations measurements time_series

  structure(
    class = c(
      sprintf("ieegio_surface_contains_%s", as.character(contains)),
      "ieegio_surface"
    ),
    re
  )
}


#' @export
format.ieegio_surface <- function(x, ...) {
  s <- c(
    "<ieegio Surface>",
    sprintf("  Header class: %s", class(x$header))
  )
  contains <- NULL
  ss <- NULL
  if(!is.null(x$geometry)) {
    contains <- "geometry"
    ss <- c(
      sprintf("  Geometry%s : ",
              c(sprintf(" [%s]", x$geometry$meta$Name), "")[[1]]),
      sprintf("    # of Vertex     : %d", ncol(x$geometry$vertices)),
      sprintf("    # of Face index : %d", length(x$geometry$faces) / 3),
      sprintf("    # of transforms : %d", length(x$geometry$transforms)),
      sprintf("      Transform Targets : %s",
              paste(names(x$geometry$transforms), collapse = ", "))
    )
  }
  if(!is.null(x$color)) {
    contains <- c(contains, "color")
  }
  if(!is.null(x$annotations)) {
    contains <- c(contains, "annotations")
    ss <- c(ss, sprintf("  Annotations: %s", paste(
      sprintf("`%s`", names(x$annotations$data_table)), collapse = ", "
    )))
    if(is.data.frame(x$annotations$label_table)) {
      ss <- c(ss, sprintf("    # of labels: %d",
                          nrow(x$annotations$label_table)))
    } else {
      ss <- c(ss, "    No label table found?")
    }
  }

  if(!is.null(x$measurements)) {
    contains <- c(contains, "measurements")
    ss <- c(ss, sprintf("  Measurements: %s", paste(
      sprintf("`%s`", names(x$measurements$data_table)), collapse = ", "
    )))
  }

  if(!is.null(x$time_series)) {
    contains <- c(contains, "time_series")
    slice_duration <- x$time_series$slice_duration
    if(!all(is.na(slice_duration))) {
      avg_duration <- mean(slice_duration, na.rm = TRUE)
    } else {
      avg_duration <- NA_real_
    }
    ss <- c(
      ss,
      sprintf("  Time series:"),
      sprintf("    # of time points: %d", ncol(x$time_series$value)),
      sprintf("    Average slice duration: %g", avg_duration)
    )
  }


  c(s, ss, "",
    sprintf(
      "Contains: %s",
      paste(sprintf("`%s`", contains), collapse = ", ")
    )
  )
}

#' @export
print.ieegio_surface <- function(x, ...) {
  cat(format(x), "", sep = "\n")
}

sparse_to_dense_geometry <- function(x) {
  if(!x$sparse) { return(x) }

  node_index <- x$sparse_node_index
  n_verts <- max(node_index, na.rm = TRUE)

  if(length(x$geometry)) {
    vertices <- array(NA_real_, c(4, n_verts))
    vertices[1:3, node_index] <- x$geometry$vertices[1:3, ]
    vertices[4, ] <- 1
    x$geometry$vertices <- vertices

    if(length(x$geometry$faces)) {
      face_start <- x$geometry$face_start
      if(!length(face_start)) {
        face_start <- min(x$geometry$faces, na.rm = TRUE)
      }
      if(face_start <= 0) {
        x$geometry$faces <- x$geometry$faces + (1 - face_start)
        storage.mode(x$geometry$faces) <- "integer"
      }
      x$geometry$face_start <- 1L
    }
  }

  if(length(x$color)) {
    vertex_color <- array(0, dim = c(n_verts, 4))
    vertex_color[, 4] <- 1
    vertex_color[node_index, seq_len(ncol(x$color))] <- x$color
    x$color <- vertex_color
  }

  if(length(x$time_series)) {
    re <- array(0, dim = c(n_verts, ncol(x$time_series$value)))
    re[node_index, ] <- x$time_series$value
    x$time_series$value <- re
  }

  if(length(x$annotations)) {
    stable <- x$annotations$data_table
    nms <- names(stable)
    x$annotations$data_table <- data.table::as.data.table(structure(
      names = nms,
      lapply(nms, function(nm) {
        item <- rep(0L, n_verts)
        item[node_index] <- stable[[nm]]
      })
    ))
  }

  if(length(x$measurements)) {
    stable <- x$measurements$data_table
    nms <- names(stable)
    x$measurements$data_table <- data.table::as.data.table(structure(
      names = nms,
      lapply(nms, function(nm) {
        item <- rep(0L, n_verts)
        item[node_index] <- stable[[nm]]
      })
    ))
  }

  x$sparse_node_index <- NULL
  x$sparse <- FALSE

  fix_surface_class(x)

}

fix_surface_class <- function(x) {
  tweak_names <- c("geometry", "measurements", "time_series", "annotations", "color")
  tweak_classes <- sprintf("ieegio_surface_contains_%s", tweak_names)

  contain_classes <- sprintf("ieegio_surface_contains_%s", names(x))
  remove_classes <- tweak_classes[!tweak_classes %in% contain_classes]
  contain_classes <- contain_classes[contain_classes %in% tweak_classes]

  cls <- class(x)
  cls <- c(contain_classes, cls[!cls %in% remove_classes])
  class(x) <- unique(cls)

  # also fix the internal header
  if("ieegio_surface_contains_geometry" %in% contain_classes) {
    geometry <- .subset2(x, "geometry")
    nverts <- ncol(geometry$vertices)
    if(length(geometry$faces)) {
      type <- "tris"
      nfaces <- ncol(geometry$faces)
    } else {
      type <- "points"
      nfaces <- 0L
    }
    x$header <- structure(
      class = "basic_geometry",
      list(
        internal = list(
          num_vertices_expected = nverts,
          num_faces_expected = nfaces
        ),
        mesh_face_type = type
      )
    )
  }
  x
}

#' @title Merge two \code{'ieegio'} surfaces
#' @description
#' Either merge surface objects by attributes or merge geometries
#' @param x,y,... \code{'ieegio'} surface objects, see
#' \code{\link{as_ieegio_surface}} or \code{\link{read_surface}}. Object
#' \code{x} must contain geometry information.
#' @param merge_type type of merge:
#' \describe{
#' \item{\code{"attribute"}}{merge \code{y,...} into x by attributes such
#' as color, measurements, annotations, or time-series data, assuming
#' \code{x,y,...} all refer to the same geometry, hence the underlying
#' number of vertices should be the same.}
#' \item{\code{"geometry"}}{merge \code{y,...} into x by geometry; this
#' requires the surfaces to merge have geometries and cannot be only surface
#' attributes. Two mesh objects will be merged into one, and face index will
#' be re-calculated. The merge happens in transformed space, Notice the attributes will be ignored and eventually
#' discarded during merge.}
#' }
#' @param merge_space space to merge the geometries; only used when
#' \code{merge_type} is \code{"geometry"}. Default is to directly merge the
#' surfaces in \code{"model"} space, i.e. assuming the surfaces share the same
#' transform; alternatively, if the model to world transforms are different,
#' users can choose to merge in \code{"world"} space, then all the surfaces
#' will be transformed into world space and mapped back to the model space
#' in \code{x}
#' @param transform_index which local-to-world transform to use when merging
#' geometries in the world space; default is the first transform for each
#' surface object. The transform list can be obtained from
#' \code{surface$geometry$transforms} and \code{transform_index} indicates the
#' index of the transform matrices. The length of \code{transform_index} can be
#' either 1 (same for all surfaces) or the length of all the surfaces, (i.e.
#' length of \code{list(x,y,...)}), when the index needs to be set for each
#' surface respectively. If any index is set to \code{NA}, then it means no
#' transform is to be applied and that surface will be merged assuming its
#' model space is the world space.
#' @param verbose whether to verbose the messages
#'
#' @returns A merged surface object
#' @examples
#'
#'
#' # Construct example geometry
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
#' dodecahedron_face <- matrix(
#'   ncol = 3L, byrow = TRUE,
#'   c(1, 11, 2, 1, 2, 16, 1, 16, 15, 1, 15, 5, 1, 5, 20, 1, 20, 19,
#'     1, 19, 3, 1, 3, 12, 1, 12, 11, 2, 11, 12, 2, 12, 4, 2, 4, 17,
#'     2, 17, 18, 2, 18, 6, 2, 6, 16, 3, 13, 14, 3, 14, 4, 3, 4, 12,
#'     3, 19, 20, 3, 20, 7, 3, 7, 13, 4, 14, 8, 4, 8, 18, 4, 18, 17,
#'     5, 9, 10, 5, 10, 7, 5, 7, 20, 5, 15, 16, 5, 16, 6, 5, 6, 9,
#'     6, 18, 8, 6, 8, 10, 6, 10, 9, 7, 10, 8, 7, 8, 14, 7, 14, 13)
#' )
#'
#' x0 <- as_ieegio_surface(dodecahedron_vert, faces = dodecahedron_face)
#'
#' plot(x0)
#'
#'
#' # ---- merge by attributes -----------------------------------
#'
#' # point-cloud but with vertex measurements
#' y1 <- as_ieegio_surface(
#'   dodecahedron_vert,
#'   measurements = data.frame(MyVariable = dodecahedron_vert[, 1]),
#'   transform = diag(c(2,1,0.5,1))
#' )
#'
#' plot(y1)
#'
#' # the geometry of `y1` will be discarded and only attributes
#' # (in this case, measurements:MyVariable) will be merged to `x`
#'
#' z1 <- merge(x0, y1, merge_type = "attribute")
#'
#' plot(z1)
#'
#' # ---- merge by geometry ----------------------------------------
#'
#' y2 <- as_ieegio_surface(
#'   dodecahedron_vert + 4, faces = dodecahedron_face,
#'   transform = diag(c(2, 1, 0.5, 1))
#' )
#'
#' plot(y2)
#'
#' # merge directly in model space: transform matrix of `y2` will be ignored
#' z2 <- merge(x0, y2, merge_type = "geometry", merge_space = "model")
#'
#' plot(z2)
#'
#' # merge x, y2 in the world space where transforms will be respected
#' z3 <- merge(x0, y2, merge_type = "geometry", merge_space = "world")
#'
#' plot(z3)
#'
#'
#'
#' @export
merge.ieegio_surface <- function(
    x, y, ...,
    merge_type = c("attribute", "geometry"),
    merge_space = c("model", "world"),
    transform_index = 1, verbose = TRUE) {

  merge_type <- match.arg(merge_type)
  merge_space <- match.arg(merge_space)

  base_surface <- x
  if(missing(y)) {
    additional_surfaces <- list(...)
  } else {
    additional_surfaces <- list(y, ...)
  }


  base_surface$header <- NULL

  has_geometry <- !is.null(x$geometry)

  if(!has_geometry) {
    stop("`merge.ieegio_surface`: the first element `x` MUST contain the ",
         "geometry data (surface vertex nodes, face indices).")
  }

  n_verts <- ncol(x$geometry$vertices)

  # if x is sparse, expand
  if( x$sparse ) {
    node_index <- x$sparse_node_index
  } else {
    node_index <- seq_len(n_verts)
  }




  if( merge_type == "geometry" ) {
    # throw all the attributes: they will no longer be valid
    x$annotations <- NULL
    x$measurements <- NULL
    x$color <- NULL
    x$time_series <- NULL

    if(merge_space == "world") {
      if(verbose) {
        message("Merging geometries in the transformed world space indicated by `transform_index` list.")
      }
      n_total_surfs <- length(additional_surfaces) + 1
      if(length(transform_index) == 1) {
        transform_index <- rep(transform_index, n_total_surfs)
      } else {
        if(length(transform_index) != n_total_surfs) {
          stop(
            "`transform_index` length must be either 1 ",
            "(same transform index for all), or its length must equal ",
            "to the total number of surfaces (=", n_total_surfs, ").")
        }
      }
    } else {
      if(verbose) {
        message("Merging geometries directly without checking transforms (assuming the transforms are the same)")
      }
    }
  } else if(verbose) {
    message("Merging geometry attributes, assuming all the surface objects have the same number of vertices.")
  }
  # make dense x and make sure face starts from 1
  x <- sparse_to_dense_geometry(x)

  get_transform <- function(z, idx) {
    transforms <- z$geometry$transforms
    if(is.na(idx) || !length(transforms)) {
      return(diag(1, 4))
    }
    if( idx <= 0 || idx > length(transforms) ) {
      stop(sprintf("Surface has no transform index %s.", idx))
    }
    transforms[[idx]]
  }

  if(merge_type == "geometry" && merge_space == "world") {
    x_world2local <- solve(get_transform(x, transform_index[[1]]))
  } else {
    x_world2local <- NULL
  }

  x <- Reduce(
    seq_along(additional_surfaces),
    init = x,
    f = function(x, kk) {
      # update x data in case y contains geometry
      n_verts <- ncol(x$geometry$vertices)

      y <- additional_surfaces[[ kk ]]

      switch(
        merge_type,
        "geometry" = {
          if( is.null(y$geometry) ) { return(x) }
          y$annotations <- NULL
          y$measurements <- NULL
          y$color <- NULL
          y$time_series <- NULL
          y <- sparse_to_dense_geometry(y)

          if( merge_space == "world" ) {
            t_idx <- transform_index[[ kk + 1 ]]
            y_local2world <- get_transform(y, t_idx)
            transform_y2x <- x_world2local %*% y_local2world

            # bind vertices in the world space
            x$geometry$vertices <- cbind(x$geometry$vertices, transform_y2x %*% y$geometry$vertices)
          } else {

            # bind vertices directly
            x$geometry$vertices <- cbind(x$geometry$vertices, y$geometry$vertices)
          }
          if(length(y$geometry$faces)) {
            x$geometry$faces <- cbind(x$geometry$faces, y$geometry$faces + n_verts)
          }
        },
        {
          y <- sparse_to_dense_geometry(y)

          if( !is.null(y$geometry) && n_verts != ncol(y$geometry$vertices) ) {
            stop(
              "You are trying to merge attributes. ",
              "One of the surface object contains geometry that has ",
              "inconsistent number of vertices. Please check if the surface ",
              "objects share the same number of vertex nodes. If you want to ",
              "merge geomtries, use `merge(..., merge_type = 'geometry')`.")
          }

          # color
          if( !is.null(y$color) ) {
            x$color <- y$color
          }

          # annot
          if(length(y$annotations)) {
            if( length(x$annotations) ) {
              label_table_x <- x$annotations$label_table[, c("Key", "Label")]
              label_table_y <- y$annotations$label_table[, c("Key", "Label")]
              merged <- merge(
                label_table_x,
                label_table_y,
                by = "Key",
                all = FALSE,
                suffixes = c("_x", "_y")
              )
              merged <- merged[!is.na(merged$Key), ]
              sel <- merged$Label_x != merged$Label_y
              if(any(sel)) {
                key_x <- merged$Key[sel][[1]]
                Label_x <- merged$Label_x[sel][[1]]
                Label_y <- merged$Label_y[sel][[1]]
                stop("Unable to merge two annotations with the same key but ",
                     "different labels. For example, key [", key_x,
                     "] represents ", sQuote(Label_x), " in one dataset but ",
                     sQuote(Label_y), " in another.")
              }
              sel <- !label_table_y$Key %in% merged$Key
              x$annotations$label_table <- rbind(x$annotations$label_table, y$annotations$label_table[sel, ])
              x$annotations$data_table <- cbind(
                x$annotations$data_table,
                y$annotations$data_table
              )
              x$annotations$meta <- c(x$annotations$meta, y$annotations$meta)
            } else {
              x$annotations <- y$annotations
            }
          }

          # measurements
          if(length(y$measurements)) {
            if( length(x$measurements) ) {
              x$measurements$data_table <- cbind(
                x$measurements$data_table,
                y$measurements$data_table
              )
              x$measurements$meta <- c(x$measurements$meta, y$measurements$meta)
            } else {
              x$measurements <- y$measurements
            }
          }

          # time_series
          if(length(y$time_series)) {
            if(length(x$time_series)) {
              x$time_series$value <- cbind(
                x$time_series$value,
                y$time_series$value
              )
              x$time_series$slice_duration <- c(
                x$time_series$slice_duration,
                y$time_series$slice_duration
              )
            } else {
              x$time_series <- y$time_series
            }
          }

        }
      )

      return(x)
    }
  )

  fix_surface_class(x)
}

#' @title Plot '3D' surface objects
#' @param x \code{'ieegio_surface'} object, see \code{\link{read_surface}}
#' @param method plot method; \code{'basic'} for just rendering the surfaces;
#' \code{'full'} for rendering with axes and title
#' @param transform which transform to use, can be a 4-by-4 matrix; if
#' the surface contains transform matrix, then this argument can be
#' an integer index of the transform embedded, or the target
#' (transformed) space name; print \code{names(x$transforms)} for
#' choices
#' @param name attribute and name used for colors, options can be \code{'color'}
#' if the surface has color matrix; \code{c('annotations', varname)} for
#' rendering colors from annotations with variable \code{varname};
#' \code{c('measurements', varname)} for rendering colors from measurements
#' with variable \code{varname}; \code{'time_series'} for
#' plotting time series slices; or \code{"flat"} for flat color;
#' default is \code{'auto'}, which will
#' plot the first available data. More details see 'Examples'.
#' @param vlim when plotting with continuous data (\code{name} is measurements
#' or time-series), the value limit used to generate color palette; default is
#' \code{NULL}: the range of the values. This argument can be length of 1 (
#' creating symmetric value range) or 2. If set, then values exceeding the
#' range will be trimmed to the limit
#' @param col color or colors to form the color palette when value data is
#' continuous; when \code{name="flat"}, the last color will be used
#' @param slice_index when plotting the \code{name="time_series"} data,
#' the slice indices to plot; default is to select a maximum of 4 slices
#' @param ... ignored
#'
#' @examples
#'
#'
#'
#'
#' library(ieegio)
#'
#' # geometry
#' geom_file <- "gifti/GzipBase64/sujet01_Lwhite.surf.gii"
#'
#' # measurements
#' shape_file <- "gifti/GzipBase64/sujet01_Lwhite.shape.gii"
#'
#' # time series
#' ts_file <- "gifti/GzipBase64/fmri_sujet01_Lwhite_projection.time.gii"
#'
#' if(ieegio_sample_data(geom_file, test = TRUE)) {
#'
#'   geometry <- read_surface(ieegio_sample_data(geom_file))
#'   measurement <- read_surface(ieegio_sample_data(shape_file))
#'   time_series <- read_surface(ieegio_sample_data(ts_file))
#'   ts_demean <- apply(
#'     time_series$time_series$value,
#'     MARGIN = 1L,
#'     FUN = function(x) {
#'       x - mean(x)
#'     }
#'   )
#'   time_series$time_series$value <- t(ts_demean)
#'
#'   # merge measurement & time_series into geometry (optional)
#'   merged <- merge(geometry, measurement, time_series)
#'   print(merged)
#'
#'   # ---- plot method/style ------------------------------------
#'   plot(merged)
#'
#'   # ---- plot data --------------------------------------------
#'
#'   ## Measurements or annotations
#'
#'   # the first column of `measurements`
#'   plot(merged, name = "measurements")
#'
#'   # equivalent to
#'   plot(merged, name = list("measurements", 1L))
#'
#'   # equivalent to
#'   measurement_names <- names(merged$measurements$data_table)
#'   plot(merged, name = list("measurements", measurement_names[[1]]))
#'
#'   ## Time-series
#'
#'   # automatically select 4 slices, trim the color palette
#'   # from -25 to 25
#'   plot(merged, name = "time_series", vlim = c(-25, 25),
#'        slice_index = 1L)
#'
#'   plot(
#'     merged,
#'     name = "time_series",
#'     vlim = c(-25, 25),
#'     slice_index = 64,
#'     col = c("#053061", "#2166ac", "#4393c3",
#'             "#92c5de", "#d1e5f0", "#ffffff",
#'             "#fddbc7", "#f4a582", "#d6604d",
#'             "#b2182b", "#67001f")
#'   )
#'
#'
#' }
#'
#'
#'
#' @export
plot.ieegio_surface <- function(
    x, method = c("auto", "r3js", "rgl_basic", "rgl_full"), transform = 1L,
    name = "auto", vlim = NULL, col = c("black", "white"),
    slice_index = NULL, ...) {
  method <- match.arg(method)

  # DIPSAUS DEBUG START
  # file <- "/Users/dipterix/rave_data/raw_dir/PAV044/rave-imaging/fs/label/lh.aparc.annot"
  # annot <- read_surface(file)
  # file <- "/Users/dipterix/rave_data/raw_dir/PAV044/rave-imaging/fs/surf/lh.pial"
  # x <- merge(read_surface(file), annot)
  # method <- "fancy"
  # transform <- 1
  # vlim <- NULL
  # name <- c("annot", names(annot$annotations$data_table)[[1]])
  # x <- merge(
  #   io_read_gii("inst/sample_data/gifti/GzipBase64/sujet01_Lwhite.surf.gii")
  #   # io_read_gii("inst/sample_data/gifti/GzipBase64/fmri_sujet01_Lwhite_projection.time.gii")
  # )
  # slice_index <- NULL
  # name <- c("time_series")
  # col = c("black", "white")


  if(!length(x$geometry)) {
    stop("This `ieeg_surface` object does not contain any geometry.")
  }

  if(is.matrix(transform)) {
    stopifnot(nrow(transform) == 4 && ncol(transform) == 4)
  } else if(is.null(transform) || !length(x$geometry$transforms)) {
    transform <- diag(1, 4)
  } else {
    transform <- x$geometry$transforms[[transform]]
    if(!is.matrix(transform)) {
      stop("Unable to find transform matrix ", sQuote(transform))
    }
  }

  # vert
  vertices <- x$geometry$vertices
  if(nrow(vertices) == 3) {
    vertices <- rbind(vertices, 1)
  }
  vertices <- transform %*% vertices
  mesh <- structure(
    class = "mesh3d",
    list(vb = vertices,
         it = x$geometry$faces)
  )

  # name: "color", c(annotation, xxx), c(measure xxx), "flat", "auto"
  if(identical(name, "auto")) {
    if(length(x$color)) {
      name <- "color"
    } else if(length(x$annotations)) {
      name <- c("annot", names(x$annotations$data_table)[[1]])
    } else if(length(x$measurements)) {
      name <- c("meas", names(x$measurements$data_table)[[1]])
    } else if(length(x$time_series)) {
      name <- "time_series"
    } else {
      name <- "flat"
    }
  }
  name[[1]] <- substr(name[[1]], 1, 4)
  cname <- ifelse(length(name) > 1, name[[2]], 1L)

  vert_color <- FALSE
  main <- ""

  vlim <- vlim[!is.na(vlim)]
  if(length(vlim) == 1) {
    vlim <- abs(vlim) * c(-1, 1)
  } else if(length(vlim)){
    vlim <- range(vlim)
  }

  switch(
    name[[1]],
    "colo" = {
      if(length(x$color)) {
        rgb <- x$color[, c(1, 2, 3)]
        max_val <- max(rgb, na.rm = TRUE)
        if(max_val < 1) {
          max_val <- 1
        } else if (max_val > 1.1) {
          max_val <- 255
        }
        col <- grDevices::rgb(red = rgb[,1], green = rgb[,2], blue = rgb[,3],
                              maxColorValue = max_val)
        vert_color <- TRUE
      }
    },
    "anno" = {
      val <- x$annotations$data_table[[cname]]
      if(length(val)) {
        col <- cmap <- structure(
          x$annotations$label_table$Color,
          names = sprintf("%d", x$annotations$label_table$Key)
        )
        col <- cmap[sprintf("%d", val)]
        vert_color <- TRUE
        if(is.numeric(cname)) {
          cname <- names(x$annotations$data_table)[[cname]]
        }
        main <- sprintf("Annotation: %s", cname)
      }
    },
    "meas" = {

      val <- x$measurements$data_table[[cname]]
      if(length(val) && !all(is.na(val))) {
        if(length(vlim) == 0) {
          vlim <- range(val, na.rm = TRUE)
        }
        ncols <- length(col)
        if(ncols < 256) {
          col <- grDevices::colorRampPalette(col)(256)
          ncols <- 256
        }
        idx <- floor((val - vlim[[1]]) /
                       (vlim[[2]] - vlim[[1]]) *
                       (ncols - 0.1)) + 1
        idx[idx <= 1] <- 1
        idx[idx >= ncols] <- ncols
        col <- col[idx]
        vert_color <- TRUE

        if(is.numeric(cname)) {
          cname <- names(x$measurements$data_table)[[cname]]
        }
        main <- sprintf("Measurement: %s", cname)
      }
    },
    "time" = {
      if(length(x$time_series)) {
        dm <- dim(x$time_series$value)
        if(!length(slice_index)) {
          slice_index <- round(seq(1, dm[[2]], length.out = 4))
        }
        slice_index <- unique(slice_index)
        if(length(vlim) != 2) {
          vlim <- range(x$time_series$value, na.rm = TRUE)
        }
        ncols <- length(col)
        if(ncols < 256) {
          col <- grDevices::colorRampPalette(col)(256)
          ncols <- 256
        }
        slice_values <- x$time_series$value[, slice_index, drop = FALSE]
        slice_dim <- dim(slice_values)
        idx <- floor((slice_values - vlim[[1]]) /
                       (vlim[[2]] - vlim[[1]]) *
                       (ncols - 0.1)) + 1
        idx[idx <= 1] <- 1
        idx[idx >= ncols] <- ncols
        col <- col[idx]
        dim(col) <- slice_dim
        col <- t(col)
        vert_color <- TRUE
      }
    }
  )

  if( vert_color ) {
    mesh$meshColor <- "vertices"
    mesh$col <- col
  } else {
    name <- "flat"
    col <- col[[length(col)]]
  }


  if(name[[1]] == "time") {
    n_slices <- length(slice_index)

    if( package_installed("r3js") ) {
      main <- sprintf("Slice %d", slice_index[[1]])
      r3plot <- helper_r3js_render_mesh(mesh, col = col[1, ])
      r3plot <- r3js::legend3js(r3plot, legend = main, fill = NA)
      helper_r3js_plot(r3plot, zoom = 3, title = main)
    } else {
      mfr <- grDevices::n2mfrow(n_slices, asp = 1)
      helper_rgl_view({
        helper_rgl_call("open3d")#, userMatrix = diag(1, 4))
        # view3d(theta = -90, phi = 90, type = "userviewpoint")
        helper_rgl_call(
          "layout3d",
          matrix(seq_len(n_slices * 2), nrow = mfr[[1]] * 2, byrow = FALSE),
          sharedMouse = TRUE, heights = rep(c(3, 1), mfr[[1]])
        )
        # mfrow3d(mfr[[1]], mfr[[2]])
        for (i in seq_len(n_slices)) {
          helper_rgl_call("next3d")
          helper_rgl_call("shade3d", mesh, col = col[i, ])
          helper_rgl_call("next3d")
          helper_rgl_call("text3d", 0, 0, 0,
                          sprintf("Slice %d", slice_index[[i]]))
        }
        # To trigger display


        # helper_rgl_call("highlevel", integer())

        # open3d()
        # mat <- matrix(1:4, 2, 2)
        # mat <- cbind(mat, mat + 4, mat + 8)
        # layout3d(mat, height = rep(c(3, 1), 3), sharedMouse = TRUE)
        # for (i in 1:6) {
        #   next3d()
        #   shade3d(shapes[[i]], col = col[i])
        #   next3d()
        #   text3d(0, 0, 0, names(shapes)[i])
        # }
        # highlevel(integer())
      })
    }
  } else {
    if(identical(method, "auto")) {
      if(package_installed("r3js")) {
        method <- "r3js"
      } else if (package_installed("rgl")) {
        method <- "rgl_basic"
      }
    }

    switch(
      method,
      "rgl_basic" = {
        helper_rgl_view({
          helper_rgl_call("shade3d", mesh, col = col)
        })
      },
      "rgl_full" = {
        helper_rgl_view({
          rg <- apply(mesh$vb, 1, range)[, 1:3]
          helper_rgl_call("shade3d", mesh, col = col)
          helper_rgl_call("arrow3d", rg[1, ], rg[c(2, 3, 5)], s = 0.02,
                          type = "line", col = "red")
          helper_rgl_call("arrow3d", rg[1, ], rg[c(1, 4, 5)], s = 0.02,
                          type = "line", col = "green")
          helper_rgl_call("arrow3d", rg[1, ], rg[c(1, 3, 6)], s = 0.02,
                          type = "line", col = "blue")
          helper_rgl_call("text3d", texts = "Right", x = rg[c(2, 3, 5)],
                          adj = c(1,1,1))
          helper_rgl_call("text3d", texts = "Anterior", x = rg[c(1, 4, 5)],
                          adj = c(1,1,1))
          helper_rgl_call("text3d", texts = "Superior", x = rg[c(1, 3, 6)],
                          adj = c(1,1,1))
          helper_rgl_call("title3d", main = main, cex = 1.2)
        })
      },
      "r3js" = {
        r3plot <- helper_r3js_render_mesh(mesh, col = col)
        helper_r3js_plot(r3plot, zoom = 3, title = main)
      }, {
        # TODO: add RAVE and NiiVue 3D viewers
      }
    )
  }

}




#' @name imaging-surface
#' @title Read and write surface files
#' @description
#' Supports surface geometry, annotation, measurement, and
#' time-series data.
#' Please use the high-level function \code{read_surface}, which calls
#' other low-level functions internally.
#'
#' @param file,con path the file
#' @param x surface (geometry, annotation, measurement) data
#' @param type type of the data; ignored if the file format is 'GIfTI'. For
#' 'FreeSurfer' files, supported types are
#' \describe{
#' \item{\code{'geometry'}}{contains positions of mesh vertex nodes and face
#' indices;}
#' \item{\code{'annotations'}}{annotation file (usually with file extension
#' \code{'annot'}) containing a color look-up table and an array of color keys.
#' These files are used to display discrete values on the surface such as
#' brain atlas;}
#' \item{\code{'measurements'}}{measurement file such as \code{'sulc'} and
#' \code{'curv'} files, containing numerical values (often with continuous
#' domain) for each vertex node}
#' }
#' @param format format of the file, for \code{write_surface}, this is either
#' \code{'gifti'} or \code{'freesurfer'}; for \code{read_surface}, see
#' 'Arguments' section in \code{\link[freesurferformats]{read.fs.surface}}
#' (when file type is \code{'geometry'}) and
#' \code{\link[freesurferformats]{read.fs.curv}}
#' (when file type is \code{'measurements'})
#' @param name name of the data; for \code{io_read_fs}, this argument must be
#' a character, and default is the file name; for \code{write_surface}, this
#' argument can be an integer or a character, representing the
#' index or name of the corresponding measurement or annotation column.
#' @param ... for \code{read_surface}, the arguments will be passed to
#' \code{io_read_fs} if the file is a 'FreeSurfer' file.
#' @returns A surface object container for \code{read_surface}, and
#' the file path for \code{write_surface}
#' @examples
#'
#'
#' library(ieegio)
#'
#' # geometry
#' geom_file <- "gifti/GzipBase64/sujet01_Lwhite.surf.gii"
#'
#' # measurements
#' shape_file <- "gifti/GzipBase64/sujet01_Lwhite.shape.gii"
#'
#' # time series
#' ts_file <- "gifti/GzipBase64/fmri_sujet01_Lwhite_projection.time.gii"
#'
#' if(ieegio_sample_data(geom_file, test = TRUE)) {
#'
#'   geometry <- read_surface(ieegio_sample_data(geom_file))
#'   print(geometry)
#'
#'   measurement <- read_surface(ieegio_sample_data(shape_file))
#'   print(measurement)
#'
#'   time_series <- read_surface(ieegio_sample_data(ts_file))
#'   print(time_series)
#'
#'   # merge measurement & time_series into geometry
#'   merged <- merge(geometry, measurement, time_series)
#'   print(merged)
#'
#'   # make sure you install `rgl` package
#'   plot(merged, name = c("measurements", "Shape001"))
#'
#'   plot(merged, name = "time_series",
#'        slice_index = c(1, 11, 21, 31))
#'
#' }
#'
#'
#' @export
read_surface <- function(file, format = "auto", type = NULL, ...) {
  fname <- basename(file)
  if(endsWith(tolower(fname), "gii") || endsWith(tolower(fname), "gii.gz") ||
     tolower(format) %in% c("gii", "gifti", "gii.gz")) {
    # GIfTI
    return(io_read_gii(file))
  }
  if(!length(type)) {
    # Guess the type
    ext <- tolower(path_ext(fname))

    type <- switch(
      ext,
      "sulc" = "measurements",
      "curv" = "measurements",
      "annot" = "annotations",
      {
        "geometry"
      }
    )
  }

  io_read_fs(file, type = type, format = format, ...)

}

#' @rdname imaging-surface
#' @export
write_surface <- function(
    x, con, format = c("gifti", "freesurfer"),
    type = c("geometry", "annotations", "measurements", "color",
             "time_series"),
    ..., name = 1) {

  format <- match.arg(format)
  x <- as_ieegio_surface(x)


  if(format == "gifti") {
    re <- io_write_gii(x = x, con = con, ...)
    return(invisible(re))
  }

  type <- match.arg(type)

  if( type %in% c("color", "time_series") ) {
    stop("Saving ", type,
         " data in FreeSurfer format has not been implemented.")
  }

  if(!length(x[[type]])) {
    nms <- names(x)
    nms <- nms[nms %in% c("geometry", "annotations", "measurements")]
    stop(
      "The surface object does not contain ",
      sQuote(type),
      " data type. Available data types:",
      paste(sQuote(nms), collapse = ", ")
    )
  }

  if(x$sparse && type == "measurements") {
    warning("Saving ", type, " data with sparse index in ",
            "FreeSurfer format is not supported. ",
            "The result might be wrong. ",
            "Please check it.")
  }


  switch(
    type,
    "geometry" = {
      vertices <- t(x$geometry$vertices[1:3, , drop = FALSE])
      faces <- t(x$geometry$faces)
      face_start <- x$geometry$face_start
      if(length(face_start) == 1 && !is.na(face_start) &&
         is.numeric(face_start) && face_start != 1) {
        faces <- faces - face_start + 1L
      }
      freesurferformats::write.fs.surface(
        filepath = con, vertex_coords = vertices, faces = faces
      )
    },
    "measurements" = {
      n_verts <- 0
      if( x$sparse ) {
        start_index <- attr(x$sparse_node_index, "start_index")
        n_verts <- max(x$sparse_node_index)
        if(length(start_index) == 1 &&
           !is.na(start_index) &&
           is.numeric(start_index)) {
          n_verts <- n_verts - start_index + 1
        }
      }
      n_verts <- max(nrow(x$measurements$data_table), n_verts)

      meas_data <- x$measurements$data_table[[name]]
      if(length(meas_data) != n_verts) {
        stop(sprintf(
          "Number of measurement data points [%d] does not match with the expected number of vertex nodes [%d]",
          length(meas_data), n_verts
        ))
      }
      freesurferformats::write.fs.curv(filepath = con, data = meas_data)

    },
    "annotations" = {
      n_verts <- 0
      if( x$sparse ) {
        start_index <- attr(x$sparse_node_index, "start_index")
        n_verts <- max(x$sparse_node_index)
        if(length(start_index) == 1 &&
           !is.na(start_index) &&
           is.numeric(start_index)) {
          n_verts <- n_verts - start_index + 1
        }
      }
      n_verts <- max(nrow(x$annotations$data_table), n_verts)

      label_table <- x$annotations$label_table
      color_codes <- grDevices::col2rgb(label_table$Color, alpha = FALSE)
      color_codes <- as.integer(colSums(color_codes * c(1, 2^8, 2^16)))

      clut <- structure(
        names = sprintf("%d", label_table$Key),
        color_codes
      )
      coloridx <- unname(clut[sprintf("%d", x$annotations$data_table[[name]])])
      coloridx <- as.integer(coloridx)


      color_max <- max(label_table$Red, label_table$Green, label_table$Blue)
      alpha_max <- max(c(label_table$Alpha, 1))
      if(color_max >= 2) { color_max <- 255 } else { color_max <- 1 }
      if(alpha_max >= 2) { alpha_max <- 255 } else { alpha_max <- 1 }
      colortable <- data.frame(
        struct_name = label_table$Label,
        r = floor(label_table$Red / color_max * 255),
        g = floor(label_table$Green / color_max * 255),
        b = floor(label_table$Blue / color_max * 255),
        a = 0L,
        code = color_codes,
        hex_color_string_rgb = label_table$Color,
        hex_color_string_rgba = sprintf("%s00", label_table$Color),
        struct_index = seq_len(nrow(label_table)) - 1L
      )

      freesurferformats::write.fs.annot(
        filepath = con,
        num_vertices = as.integer(n_verts),
        colortable = colortable,
        labels_as_colorcodes = coloridx,
      )
    },
    {
      stop("Type ", sQuote(type), " is not supported under FreeSurfer format.")
    }
  )

  invisible(con)

}




