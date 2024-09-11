
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
      warning("Vertex `color` attribute has inconsistent length with the expected vertex length. The `color` attribute is discarded.")
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
      warning("Vertex `annotations` attribute has inconsistent length with the expected vertex length. The `annotations` attribute is discarded.")
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
      warning("Vertex `measurements` attribute has inconsistent length with the expected vertex length. The `measurements` attribute is discarded.")
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
      warning("Vertex `time_series` attribute has inconsistent length with the expected vertex length. The `time_series` attribute is discarded.")
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

#' @rdname read_surface
#' @export
read_surface <- function(file, format = "auto", type = NULL, ...) {
  fname <- basename(file)
  if(endsWith(tolower(fname), "gii") || endsWith(tolower(fname), "gii.gz")) {
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
      sprintf("      Transform Targets : %s", paste(names(x$geometry$transforms), collapse = ", "))
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
      ss <- c(ss, sprintf("    # of labels: %d", nrow(x$annotations$label_table)))
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
    stop("`merge.ieegio_surface`: the first element `x` MUST contain geometry (surface vertex nodes, face indices).")
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
      x$annotations$data_table <- get_annot_or_meas_full(x, TRUE, node_index, "annotations")
    }
    if(length(x$measurements)) {
      x$measurements$data_table <- get_annot_or_meas_full(x, TRUE, node_index, "measurements")
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
        stop("One of the surface object contains geometry that has inconsistent number of vertices. Please check if the surface objects share the same number of vertex nodes.")
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
          stop("Unable to merge two annotations with the same key but different labels. For example, key [", key_x, "] represents ", sQuote(Label_x), " in one dataset but ", sQuote(Label_y), " in another.")
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

  x
}

#' @export
plot.ieegio_surface <- function(
    x, method = c("basic", "fancy"), transform = 1L,
    name = "auto", vlim = NULL, col = c("black", "white"), slice_index = NULL, ...) {
  method <- match.arg(method)

  # DIPSAUS DEBUG START
  # file <- "/Users/dipterix/rave_data/raw_dir/AnonSEEG_old2/rave-imaging/fs/surf/lh.pial.annot.gii"
  # annot <- io_read_gii(file)
  # file <- "/Users/dipterix/rave_data/raw_dir/AnonSEEG_old2/rave-imaging/fs/surf/lh.pial.gii"
  # x <- merge(io_read_gii(file), annot)
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


  if(!length(x$geometry)) {
    stop("This `ieeg_surface` object does not contain any geometry.")
  }

  if(is.null(transform)) {
    transform <- diag(1, 4)
  } else {
    if(is.matrix(transform)) {
      stopifnot(nrow(transform) == 4 && ncol(transform) == 4)
    } else {
      transform <- x$geometry$transforms[[transform]]
      if(!is.matrix(transform)) {
        stop("Unable to find transform matrix ", sQuote(transform))
      }
    }
  }

  # vert
  vertices <- transform %*% x$geometry$vertices
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
        col <- grDevices::rgb(red = rgb[,1], green = rgb[,2], blue = rgb[,3], maxColorValue = max_val)
        vert_color <- TRUE
      }
    },
    "anno" = {
      val <- x$annotations$data_table[[name[[2]]]]
      if(length(val)) {
        col <- cmap <- structure(
          x$annotations$label_table$Color,
          names = sprintf("%d", x$annotations$label_table$Key)
        )
        col <- cmap[sprintf("%d", val)]
        vert_color <- TRUE
        main <- sprintf("Annotation: %s", name[[2]])
      }
    },
    "meas" = {
      val <- x$measurements$data_table[[name[[2]]]]
      if(length(val) && !all(is.na(val))) {
        if(length(vlim) == 0) {
          vlim <- range(val, na.rm = TRUE)
        }
        ncols <- length(col)
        if(ncols < 256) {
          col <- grDevices::colorRampPalette(col)(256)
          ncols <- 256
        }
        idx <- floor((val - vlim[[1]]) / (vlim[[2]] - vlim[[1]]) * (ncols - 0.1)) + 1
        idx[idx <= 1] <- 1
        idx[idx >= ncols] <- ncols
        col <- col[idx]
        vert_color <- TRUE
        main <- sprintf("Measurement: %s", name[[2]])
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
        idx <- floor((slice_values - vlim[[1]]) / (vlim[[2]] - vlim[[1]]) * (ncols - 0.1)) + 1
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
        helper_rgl_call("text3d", 0, 0, 0, sprintf("Slice %d", slice_index[[i]]))
      }
      # To trigger display


      helper_rgl_call("highlevel", integer())

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

  } else {
    switch(
      method,
      "basic" = {
        helper_rgl_view({
          helper_rgl_call("shade3d", mesh, col = col)
        })
      },
      "fancy" = {
        helper_rgl_view({
          rg <- apply(mesh$vb, 1, range)[, 1:3]
          helper_rgl_call("shade3d", mesh, col = col)
          helper_rgl_call("arrow3d", rg[1, ], rg[c(2, 3, 5)], s = 0.02, type = "line", col = "red")
          helper_rgl_call("arrow3d", rg[1, ], rg[c(1, 4, 5)], s = 0.02, type = "line", col = "green")
          helper_rgl_call("arrow3d", rg[1, ], rg[c(1, 3, 6)], s = 0.02, type = "line", col = "blue")
          helper_rgl_call("text3d", texts = "Right", x = rg[c(2, 3, 5)], adj = c(1,1,1))
          helper_rgl_call("text3d", texts = "Anterior", x = rg[c(1, 4, 5)], adj = c(1,1,1))
          helper_rgl_call("text3d", texts = "Superior", x = rg[c(1, 3, 6)], adj = c(1,1,1))
          helper_rgl_call("title3d", main = main, cex = 1.2)
        })
      }, {
        # TODO: add RAVE and NiiVue 3D viewers
      }
    )
  }

}




