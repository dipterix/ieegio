

# DIPSAUS DEBUG START
# file <- "/Users/dipterix/rave_data/raw_dir/AnonSEEG_old2/rave-imaging/fs/surf/lh.pial.annot.gii"
# annot <- io_read_gii(file)
# file <- "inst/sample_data/gifti/GzipBase64/sujet01_Lwhite.shape.gii"
# shape <- io_read_gii(file)
# file <- "inst/sample_data/gifti/GzipBase64/fmri_sujet01_Lwhite_projection.time.gii"
# label <- io_read_gii(file)
# file <- "inst/sample_data/gifti/GzipBase64/fmri_sujet01_Lwhite_projection.time.gii"
# timseries <- io_read_gii(file)
# file <- "/Users/dipterix/rave_data/raw_dir/AnonSEEG_old2/rave-imaging/fs/surf/lh.pial.gii"
# gii <- gifti::read_gifti(file)
# x <- io_read_gii(file)

#' @rdname imaging-surface
#' @export
io_read_gii <- function(file) {

  gii <- gifti::read_gifti(file)

  n_entries <- length(gii$data)

  map <- fastmap::fastmap()
  map$set("sparse_node_index", NULL)

  parse_transforms <- function(transforms, meta = list()) {
    source_space <- unlist(NIFTI_XFORM_CODE[transforms$DataSpace])
    if(!length(source_space)) {
      source_space <- "Unknown"
    }
    nms <- names(transforms)
    tspa <- which(nms == "TransformedSpace")
    tmat <- which(nms == "MatrixData")
    n <- min(length(tspa), length(tmat))
    tspa <- unlist(transforms[tspa[seq_len(n)]])
    xform_name <- unlist(NIFTI_XFORM_CODE[tspa])
    re <- structure(
      names = xform_name,
      lapply(seq_len(n), function(jj) {
        idx <- tmat[jj]
        mat <- transforms[[idx]]
        if(!is.matrix(mat) || nrow(mat) != 4) {
          # The values are listed as a one- dimensional array in row-major order
          mat <- matrix(as.vector(mat), nrow = 4L, byrow = TRUE)
          if(
            any(mat[4, c(1,2,3)] != 0) &&
            all(mat[c(1,2,3), 4] == 0)
          ) {
            warning("Detected column-major transform matrix. This is against GIfTI specification.")
          }
        }
        structure(
          mat,
          source_space = source_space,
          target_space = xform_name[jj]
        )
      })
    )

    # get transform to meta
    # FreeSurfer
    # vol_dim <- as.integer(c(meta$VolGeomWidth, meta$VolGeomHeight, meta$VolGeomDepth))
    # pix_dim <- as.numeric(c(meta$VolGeomXsize, meta$VolGeomYsize, meta$VolGeomZsize))
    # ras_xform <- as.numeric(unlist(meta[c(
    #   "VolGeomX_R", "VolGeomX_A", "VolGeomX_S",
    #   "VolGeomY_R", "VolGeomY_A", "VolGeomY_S",
    #   "VolGeomZ_R", "VolGeomZ_A", "VolGeomZ_S",
    #   "VolGeomC_R", "VolGeomC_A", "VolGeomC_S")]))
    #
    # if(length(ras_xform) == 12) {
    #   dim(ras_xform) <- c(3, 4)
    # }
    #
    # if(all(c("VolGeomWidth", "VolGeomHeight",
    #          "VolGeomDepth") %in% names(meta))) {
    #
    #   vol_dim <- as.integer(vol_dim)
    # } else {
    #   vol_dim <- NULL
    # }

    null_transform <- list(
      Unknown = structure(
        diag(1, 4),
        source_space = unlist(NIFTI_XFORM_CODE["NIFTI_XFORM_UNKNOWN"]),
        target_space = unlist(NIFTI_XFORM_CODE["NIFTI_XFORM_UNKNOWN"])
      )
    )

    if(!length(re)) {
      re <- null_transform
    }


    re
  }


  parse_label_table <- function(lut) {
    if(!length(lut)) { return() }
    if(!is.data.frame(lut) && !is.matrix(lut)) { return() }
    label_names <- rownames(lut)
    lut <- data.table::data.table(lut)
    if(length(label_names) && !all(grepl("^[0-9]{0,}$", label_names))) {
      lut$Label <- label_names
    } else {
      lut$Label <- lut$Key
    }
    # Key – This required attribute contains a non-negative integer value.
    lut$Key <- as.integer(lut$Key)
    # Red, Green, Blue: This optional, but recommended, attribute contains
    # a floating-point value ranging from zero to one.
    if(length(lut$Red)) {
      lut$Red <- as.numeric(lut$Red)
    } else {
      lut$Red <- 0
    }
    if(length(lut$Green)) {
      lut$Green <- as.numeric(lut$Green)
    } else {
      lut$Green <- 0
    }
    if(length(lut$Blue)) {
      lut$Blue <- as.numeric(lut$Blue)
    } else {
      lut$Blue <- 0
    }
    if(length(lut$Alpha)) {
      lut$Alpha <- as.numeric(lut$Alpha)
    } else {
      lut$Alpha <- 1
    }
    lut$Color <- grDevices::rgb(red = lut$Red, green = lut$Green, blue = lut$Blue, maxColorValue = 1.0)
    data.table::setkeyv(lut, c("Key"))
    lut
  }


  lapply(seq_len(n_entries), function(ii) {

    intent <- gii$data_info$Intent[[ii]]
    meta <- gii$data_meta[[ii]]
    data <- gii$data[[ii]]

    if(is.matrix(meta) && nrow(meta)) {
      meta <- structure(
        names = meta[, 1],
        as.list(meta[, 2])
      )
    } else {
      meta <- NULL
    }

    get_meta <- function(name, default = "Unknown") {
      if(!length(meta) || !name %in% names(meta)) { return(default) }
      meta[[name]]
    }

    # ii <- 1
    switch(
      intent,
      "NIFTI_INTENT_NODE_INDEX" = {
        # For storing a subset of node data, a DataArray with intent
        # NIFTI_INTENT_NODE_INDEX may be used. In this case, the data array
        # contains a list of node numbers and it should be the first data array
        # in the file. The remaining data arrays in the file that contain data
        # assigned to nodes must contain the same number of elements as the
        # NIFTI_INTENT_NODE_INDEX array.
        #
        # For example, suppose a NIFTI_INTENT_NODE_INDEX data array contains
        # the elements 7, 13, 38, and 44. A NIFTI_INTENT_TTEST data array
        # would contain only data for the node numbers listed in the
        # NIFTI_INTENT_NODE_INDEX data array.
        map$set("sparse_node_index", list(
          node_index_start = 1L,
          node_index = as.integer(data) + 1L
        ))
      },
      "NIFTI_INTENT_POINTSET" = {
        # The data consists of three-dimensional coordinate data.
        geometry <- map$get("geometry", list())
        geometry$vertices <- rbind(t(data[, 1:3, drop = TRUE]), 1)

        # At least one CoordinateSystemTransformMatrix is required in a
        # DataArray with an intent of NIFTI_INTENT_POINTSET. Multiple
        # CoordinateSystemTransformMatrix elements may be used to describe
        # the transformation to multiple spaces.
        transforms <- parse_transforms(gii$parsed_transformations[[ii]])
        geometry$transforms <- transforms
        geometry$meta <- meta

        map$set("geometry", geometry)
      },
      "NIFTI_INTENT_TRIANGLE" = {
        # The data consists of consecutive triplets that are indices into a
        # NIFTI_INTENT_POI NTSET Array forming a triangle. The three nodes
        # are listed in counter- clockwise winding (right-hand rule).
        geometry <- map$get("geometry", list())
        fidx <- data[, seq_len(3), drop = FALSE] + 1
        geometry$faces <- t(array(as.integer(fidx), dim = dim(fidx)))
        # R start counting from 1
        geometry$face_start <- 1L
        map$set("geometry", geometry)
      },
      "NIFTI_INTENT_LABEL" = {
        # The data consists of indices into the file’s LabelTable.

        # ii <- 1
        # intent <- gii$data_info$Intent[[ii]]
        # meta <- gii$data_meta[[ii]]
        # data <- gii$data[[ii]]

        annotations <- map$get("annotations", list(
          label_table = parse_label_table(gii$label),
          data_table = NULL,
          meta = list()
        ))
        name <- get_meta("Name", default = sprintf("Unnamed%03d", ii))
        if(is.data.frame(annotations$data_table)) {
          annotations$data_table[[name]] <- as.vector(data)
        } else {
          annotations$data_table <- data.table::as.data.table(
            structure(
              names = name,
              list(as.vector(data))
            )
          )
        }
        annotations$meta[[name]] <- as.list(meta)
        map$set("annotations", annotations)
      },
      "NIFTI_INTENT_SHAPE" = {
        # The data consists of shape measurements.

        measurements <- map$get("measurements", list(
          data_table = NULL,
          meta = list()
        ))
        name <- get_meta("Name", default = sprintf("Shape%03d", ii))
        if(is.data.frame(measurements$data_table)) {
          measurements$data_table[[name]] <- as.vector(data)
        } else {
          measurements$data_table <- data.table::as.data.table(
            structure(
              names = name,
              list(as.vector(data))
            )
          )
        }

        measurements$meta[[name]] <- list(
          intent = intent,
          data = meta
        )
        map$set("measurements", measurements)
      },
      "NIFTI_INTENT_RGB_VECTOR" = {
        rgb <- data[, c(1, 2, 3), drop = FALSE]
        map$set("color", rgb)
      },
      "NIFTI_INTENT_RGBA_VECTOR" = {
        rgba <- data[, c(1, 2, 3, 4), drop = FALSE]
        map$set("color", rgba)
      },
      "NIFTI_INTENT_NONE" = {
        # Not supported
      },
      "NIFTI_INTENT_VECTOR" = {
        # Could define normals
        # Not supported
      },
      "NIFTI_INTENT_GENMATRIX" = {
        # field
        # Not supported
      },
      "NIFTI_INTENT_TIME_SERIES" = {
        # Dimensionality is one with the first dimension set to the number
        # of nodes.
        ts_data <- map$get("time_series", list(
          description = "`value`: vertex by time-point matrix",
          slice_duration = numeric(0L),
          value = fastmap::fastqueue()
        ))
        slice_duration <- as.character(get_meta("TimeStep", NA))
        suppressWarnings({
          slice_duration <- as.numeric(slice_duration)
        })
        ts_data$slice_duration <- c(ts_data$slice_duration, slice_duration)
        ts_data$value$add(data)
        map$set("time_series", ts_data)
      },
      {
        # NIFTI_INTENT_*
        stat_name <- gifti::convert_intent(intent)
        measurements <- map$get("measurements", list(
          data_table = data.table::data.table(),
          meta = list()
        ))
        measurements$data_table[, stat_name] <- as.vector(data)
        measurements$meta[[name]] <- as.list(
          intent = intent,
          data = meta
        )
        map$set("measurements", measurements)
      }
    )
  })
  # map <- map$as_list()
  gii$data <- NULL
  time_series <- map$get("time_series")
  if(!is.null(time_series)) {
    time_series$value <- do.call("cbind", time_series$value$as_list())
  }
  new_surface(
    header = gii,
    geometry = map$get("geometry"),
    color = map$get("color"),
    annotations = map$get("annotations"),
    measurements = map$get("measurements"),
    time_series = time_series,
    sparse_node_index = map$get("sparse_node_index")
  )
}

#' @rdname imaging-surface
#' @export
io_write_gii <- function(x, con, ...) {

  UseMethod("io_write_gii")

}

#' @export
io_write_gii.ieegio_surface <- function(
    x, con, force = FALSE, ...) {

  data_sets <- list()
  datatypes <- c()
  intents <- c()
  transform_matrix <- list()

  if( x$sparse ) {
    node_index <- x$sparse_node_index
    node_index_start <- attr(node_index, "start_index")
    if(length(node_index)) {
      if(length(node_index_start) == 1 && !is.na(node_index_start) && is.numeric(node_index_start)) {
        node_index <- node_index - node_index_start
      } else {
        node_index <- node_index - 1L
      }
      node_index <- matrix(node_index, ncol = 1L)
      storage.mode(node_index) <- "integer"
      data_sets <- c(data_sets, list(node_index))
      datatypes <- c(datatypes, "NIFTI_TYPE_INT32")
      intents <- c(intents, "NIFTI_INTENT_NODE_INDEX")
      transform_matrix <- c(transform_matrix, list(NA))
    }
  }

  if(length(x$geometry)) {
    # NIFTI_INTENT_SHAPE
    vertex_coords <- t(x$geometry$vertices[1:3, , drop = FALSE])
    faces <- t(x$geometry$faces)
    face_start <- x$geometry$face_start
    if(length(face_start) == 1 && is.numeric(face_start) && !is.na(face_start)) {
      faces <- faces - face_start
    } else {
      faces <- faces - 1L
    }
    storage.mode(faces) <- "integer"

    transform <- x$geometry$transforms[[1]]
    source_space <- attr(transform, "source_space")
    if(!isTRUE(source_space %in% names(NIFTI_XFORM_CODE))) {
      source_space <- "NIFTI_XFORM_UNKNOWN"
    }
    target_space <- attr(transform, "target_space")
    if(!isTRUE(target_space %in% names(NIFTI_XFORM_CODE))) {
      sel <- NIFTI_XFORM_CODE %in% names(x$geometry$transforms)[[1]]
      if(any(sel)) {
        target_space <- names(NIFTI_XFORM_CODE)[sel][[1]]
      } else {
        target_space <- "NIFTI_XFORM_UNKNOWN"
      }
    }


    data_sets <- c(data_sets, list(vertex_coords, faces))
    datatypes <- c(datatypes, "NIFTI_TYPE_FLOAT32", "NIFTI_TYPE_INT32")
    intents <- c(intents, "NIFTI_INTENT_POINTSET", "NIFTI_INTENT_TRIANGLE")
    transform_matrix <- c(transform_matrix, list(
      list(
        'transform_matrix' = transform,
        'data_space' = source_space,
        'transformed_space' = target_space
      ),
      NA
    ))
  }

  label_table <- NULL
  if(length(x$annotations)) {
    nms <- names(x$annotations$data_table)
    intent <- "NIFTI_INTENT_LABEL"
    for(nm in nms) {
      v <- matrix(x$annotations$data_table[[nm]], ncol = 1L)
      storage.mode(v) <- "integer"

      data_sets <- c(data_sets, list(v))
      datatypes <- c(datatypes, "NIFTI_TYPE_INT32")
      intents <- c(intents, 'NIFTI_INTENT_LABEL')
      transform_matrix <- c(transform_matrix, list(NA))

      # <Label Key="0" Red="0" Green="0" Blue="0" Alpha="1"><![CDATA[Unknown]]></Label>
      # <Label Key="1" Red="0.0901961" Green="0.862745" Blue="0.235294" Alpha="1"><![CDATA[G_and_S_frontomargin]]></Label>
      label_table <- x$annotations$label_table
    }
  }

  if(length(x$measurements)) {
    nms <- names(x$measurements$data_table)
    for(nm in nms) {
      meta <- as.list(x$measurements$meta[[nm]])
      intent <- paste(as.character(meta$intent), collapse = "")
      if(gifti::convert_intent(intent) == "unknown") {
        intent <- "NIFTI_INTENT_SHAPE"
      }
      v <- matrix(x$measurements$data_table[[nm]], ncol = 1L)
      data_sets <- c(data_sets, list(v))
      datatypes <- c(datatypes, "NIFTI_TYPE_FLOAT32")
      intents <- c(intents, intent)
      transform_matrix <- c(transform_matrix, list(NA))
    }
  }

  if(length(x$color) && is.matrix(x$color) && ncol(x$color) >= 3) {
    cmat <- x$color
    nc <- ncol(x$color)
    if(nc >= 4) {
      nc <- nc[, 1:4, drop = FALSE]
      intent <- "NIFTI_INTENT_RGBA_VECTOR"
    } else {
      intent <- "NIFTI_INTENT_RGB_VECTOR"
    }
    if(max(cmat, na.rm = TRUE) >= 2) {
      cmat <- cmat / 255
    }

    data_sets <- c(data_sets, list(cmat))
    datatypes <- c(datatypes, "NIFTI_TYPE_FLOAT32")
    intents <- c(intents, intent)
    transform_matrix <- c(transform_matrix, list(NA))
  }

  # time series
  if(length(x$time_series)) {

    v <- x$time_series$value
    slice_duration <- x$time_series$slice_duration

    data_sets <- c(
      data_sets,
      lapply(seq_along(slice_duration), function(ii) {
        duration <- slice_duration[[ii]]
        if(is.na(duration)) { duration <- 0 }
        structure(
          v[, ii, drop = FALSE],
          meta = list(TimeStep = duration)
        )
      })
    )
    intents <- c(intents, rep("NIFTI_INTENT_TIME_SERIES", length(slice_duration)))
    datatypes <- c(datatypes, rep("NIFTI_TYPE_FLOAT32", length(slice_duration)))
    transform_matrix <- c(transform_matrix, as.list(rep(NA, length(slice_duration))))
  }

  xmltree <- create_gii_xml(
    data_array = data_sets,
    intent = intents,
    datatype = datatypes,
    transform_matrix = transform_matrix,
    label_table = label_table,
    force = force
  )

  # if(validate) {
  #   xsd <- system.file("validator", "gifti.xsd", package = "ieegio")
  #   xml2::xml_validate(xmltree, xml2::read_xml(gifti_xsd))
  # }

  xml2::write_xml(xmltree, file = con, options = c("as_xml", "format"))

  invisible(con)

}

