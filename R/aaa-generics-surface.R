
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

#' @export
merge.ieegio_surface <- function(x, y, ...) {
  base_surface <- x
  if(missing(y)) {
    additional_surfaces <- list(...)
  } else {
    additional_surfaces <- list(y, ...)
  }


  base_surface$header <- NULL

  has_grometry <- !is.null(x$geometry)

  if(!has_grometry) {
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

  get_color_fill <- function(z, sparse, node_index) {
    if(!sparse || !length(z$color)) { return(z$color) }
    vertex_color <- array(0, dim = c(n_verts, 4))
    vertex_color[, 4] <- 1
    vertex_color[node_index, seq_len(ncol(z$color))] <- z$color
    vertex_color
  }

  get_time_series_full <- function(z, sparse, node_index) {
    if(!length(z$time_series)) { return(NULL) }
    if(!sparse || !length(z$time_series)) { return(z$time_series$value) }
    re <- array(0, dim = c(n_verts, ncol(z$time_series$value)))
    re[node_index, ] <- z$time_series$value
    re
  }

  get_annot_or_meas_full <- function(z, sparse, node_index,
                                     type = "annotations") {
    if(!length(z[[type]])) {
      return(NULL)
    }
    if(!sparse) {
      return(z[[type]]$data_table)
    }
    stable <- z[[type]]$data_table
    nms <- names(stable)
    annot_data <- data.table::data.table(V1 = rep(0L, n_verts))
    names(annot_data) <- nms[[1]]

    data.table::as.data.table(structure(
      names = nms,
      lapply(nms, function(nm) {
        item <- rep(0L, n_verts)
        item[node_index] <- stable[[nm]]
      })
    ))
  }

  if(x$sparse) {
    if(length(x$annotations)) {
      x$annotations$data_table <- get_annot_or_meas_full(
        x, TRUE, node_index, "annotations")
    }
    if(length(x$measurements)) {
      x$measurements$data_table <- get_annot_or_meas_full(
        x, TRUE, node_index, "measurements")
    }
    if(length(x$color)) {
      x$color <- get_color_fill(x, TRUE, node_index)
    }
    if(length(x$time_series)) {
      x$time_series$value <- get_time_series_full(x, TRUE, node_index)
    }
    x$sparse <- FALSE
  }



  for(y in additional_surfaces) {
    if( !is.null(y$geometry) ) {
      if(n_verts != ncol(y$geometry$vertices)) {
        stop("One of the surface object contains geometry that has ",
             "inconsistent number of vertices. Please check if the surface ",
             "objects share the same number of vertex nodes.")
      }
    }

    if(y$sparse) {
      sparse <- TRUE
      node_index <- y$sparse_node_index
    } else {
      sparse <- FALSE
      node_index <- seq_len(n_verts)
    }

    # color
    if( !is.null(y$color) ) {
      if(length(x$color)) {
        x$color[node_index, seq_len(ncol(y$color))] <- y$color
      } else {
        x$color <- get_color_fill(y, sparse, node_index)
      }
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
          get_annot_or_meas_full(y, y$sparse, node_index)
        )
        x$annotations$meta <- c(x$annotations$meta, y$annotations$meta)
      } else {
        x$annotations <- list(
          label_table = y$annotations$label_table,
          data_table = get_annot_or_meas_full(y, y$sparse, node_index),
          meta = y$annotations$meta
        )
      }
    }

    # measurements
    if(length(y$measurements)) {
      if( length(x$measurements) ) {
        x$measurements$data_table <- cbind(
          x$measurements$data_table,
          get_annot_or_meas_full(
            z = y,
            sparse = y$sparse,
            node_index = node_index,
            type = "measurements"
          )
        )
        x$measurements$meta <- c(x$measurements$meta, y$measurements$meta)
      } else {
        x$measurements <- list(
          data_table = get_annot_or_meas_full(
            z = y,
            sparse = y$sparse,
            node_index = node_index,
            type = "measurements"
          ),
          meta = y$measurements$meta
        )
      }
    }

    # time_series
    if(length(y$time_series)) {
      if(length(x$time_series)) {
        x$time_series$value <- cbind(
          x$time_series$value,
          get_time_series_full(y, sparse, node_index)
        )
        x$time_series$slice_duration <- c(
          x$time_series$slice_duration,
          y$time_series$slice_duration
        )
      } else {
        x$time_series <- y$time_series
        x$time_series$value <- get_time_series_full(x, sparse, node_index)
      }
    }
  }

  contains <- names(x)
  contains <- contains[contains %in% c(
    "geometry", "measurements", "time_series", "annotations", "color"
  )]
  cls <- c(
    sprintf("ieegio_surface_contains_%s", contains),
    class(x)
  )
  class(x) <- unique(cls)
  x
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
#' @param format format of the file, see 'Arguments' section in
#' \code{\link[freesurferformats]{read.fs.surface}} (when file type is
#' \code{'geometry'}) and \code{\link[freesurferformats]{read.fs.curv}}
#' (when file type is \code{'measurements'})
#' @param name name of the data; default is the file name
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
    ...) {

  format <- match.arg(format)

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
      coloridx <- unname(clut[sprintf("%d", x$annotations$data_table[[1]])])
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
    }
  )

  invisible(con)

}




