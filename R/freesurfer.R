
fs_lut <- local({
  lut <- NULL
  impl <- function() {
    path <- system.file("lookup_table", "FreeSurferColorLUT.json", package = "ieegio")
    lut <- io_read_json(path)
    data.table::rbindlist(lut$`__global_data__.VolumeColorLUT`$map)
  }
  function() {
    if(is.null(lut)) {
      lut <<- impl()
    }
    lut
  }
})

remove_freesurfer_cortical_prefix <- function(x, type = c("hemisphere", "tissue")) {
  x <- tolower(x)

  if("hemisphere" %in% type) {
    x <- gsub("^(left|right)[_-]", "", x)
    x <- gsub("^(lh|rh)[_-]", "", x)
    x <- gsub("([_-])(lh|rh)[_-]", "\\1", x)
  } else {
    x <- gsub("^left[_-]", "lh.", x)
    x <- gsub("^right[_-]", "rh.", x)
  }

  if("tissue" %in% type) {
    x <- gsub("^(ctx|wm)[_-]", "", x)
    x <- gsub("^(ctx|wm)[_-]", "", x)
  }
  x
}


#' @rdname imaging-surface
#' @export
io_read_fs <- function(file, type = c("geometry", "annotations", "measurements"),
                       format = "auto", name = basename(file), ...) {

  type <- match.arg(type)

  # DIPSAUS DEBUG START
  # f_geom <- "/Users/dipterix/rave_data/raw_dir/AnonSEEG_old2/rave-imaging/fs/surf/lh.pial"
  # f_annot <- "/Users/dipterix/rave_data/raw_dir/AnonSEEG_old2/rave-imaging/fs/label/lh.aparc.annot"
  #
  # file <- f_geom
  # format <- "auto"
  # name = basename(file)



  re <- switch(
    type,
    "geometry" = {
      surf <- freesurferformats::read.fs.surface(file, format = format)
      header <- structure(
        class = "fs_geometry",
        list(internal = surf$internal, mesh_face_type = surf$mesh_face_type)
      )
      # try to get c_ras
      transforms <- list(
        Unknown = structure(
          diag(1, 4),
          source_space = "Unknown",
          target_space = "Unknown"
        )
      )

      suppressWarnings({
        tryCatch(
          {
            line1 <- readLines(file, n = 1L, encoding = "ascii")
            n <- c(
              nchar(line1, type = "bytes"), 8,
              length(surf$vertices) * 4,
              length(surf$faces) * 4
            )
            extra_bytes <- file.size(file) - sum(n)
            if(length(extra_bytes)) {
              conn <- base::file(file, "rb")
              on.exit({ close(conn) })
              invisible({ readBin(conn, "raw", n = sum(n)) })
              extra <- readBin(conn, "raw", n = extra_bytes, endian = "big")
              extra <- intToUtf8(extra)
              extra <- trimws(strsplit(extra, "\n")[[1]])
              extra <- extra[startsWith(extra, "cras ")]
              cras <- strsplit(extra, "=", fixed = TRUE)[[1]][[2]]
              cras <- strsplit(trimws(cras), " ")[[1]]
              cras <- cras[cras != ""]
              cras <- as.numeric(cras)
              if(length(cras) == 3 && !anyNA(cras)) {
                names(transforms) <- "ScannerAnat"
                transforms$ScannerAnat[1:3, 4] <- cras
              }
            }
          },
          error = function(...) {}
        )
      })

      return(new_surface(header = header, geometry = list(
        vertices = t(cbind(surf$vertices[, c(1, 2, 3)], 1)),
        faces = t(surf$faces),
        face_start = 1L,
        transforms = transforms,
        meta = list(Name = name)
      )))
    },
    "annotations" = {
      # list(
      #   label_table = parse_label_table(gii$label),
      #   data_table = NULL,
      #   meta = list()
      # )
      annot <- freesurferformats::read.fs.annot(file, default_label_name = "Unlabeled")
      # lut <- fs_lut()

      annot_name <- name

      label_table <- data.table::data.table(
        Key = annot$colortable_df$struct_index,
        Label = annot$colortable_df$struct_name,
        Red = annot$colortable_df$r / 255,
        Green = annot$colortable_df$g / 255,
        Blue = annot$colortable_df$b / 255,
        Color = annot$colortable_df$hex_color_string_rgb
      )

      # annot$vertices
      lut <- structure(
        names = as.character(label_table$Label),
        label_table$Key
      )
      unames <- as.character(unique(annot$label_names))
      unames <- unames[!unames %in% label_table$Label]
      if(length(unames)) {
        lut[unames] <- 0
      }
      data_table <- data.table::data.table(V = unname(lut[annot$label_names]))

      names(data_table) <- annot_name
      annotations <- list(
        label_table = label_table,
        data_table = data_table,
        meta = structure(names = annot_name, list(annot$metadata))
      )
      node_index <- annot$vertices
      if(length(node_index)) {
        node_index <- list(
          node_index = as.integer(node_index) + 1L,
          node_index_start = 1L
        )
      } else {
        node_index <- NULL
      }
      return(
        new_surface(
          header = structure(
            class = "fs_annot",
            list(orig_ctab = annot$colortable_df)
          ),
          annotations = annotations,
          sparse_node_index = node_index
        )
      )
    },
    "measurements" = {

      is_sparse <- TRUE
      meas <- tryCatch({
        meas <- freesurferformats::read.fs.curv(file, format = format, with_header = TRUE)
        is_sparse <- FALSE
        meas
      }, error = function(e) {
        meas <- freesurferformats::read.fs.weight(file, format = format)
        meas
      })

      if( is_sparse ) {
        # 1-based
        data_table <- data.table::data.table(V = meas$values)
        node_index <- list(
          node_index = meas$vertex_indices,
          node_index_start = 1L
        )
        meas$header <- list()
      } else {
        data_table <- data.table::data.table(V = meas$data)
        node_index <- NULL
      }
      names(data_table) <- name


      measurements <- list(
        data_table = data_table,
        meta = list(
          intent = "NIFTI_INTENT_SHAPE"
        )
      )
      return(
        new_surface(
          header = structure(
            class = "fs_measurement",
            meas$header
          ),
          sparse_node_index = node_index,
          measurements = measurements
        )
      )

    },
    "color" = {

    },
    "time_series" = {

    }
  )


  # if(FALSE) {
  #   f_geom <- "/Users/dipterix/rave_data/raw_dir/AnonSEEG_old2/rave-imaging/fs/surf/lh.pial"
  #   f_annot <- "/Users/dipterix/rave_data/raw_dir/AnonSEEG_old2/rave-imaging/fs/label/lh.aparc.annot"
  #   f_meas <- "/Users/dipterix/rave_data/raw_dir/AnonSEEG_old2/rave-imaging/fs/surf/lh.curv"
  #
  #   file <- f_meas
  #   x <- merge(
  #     io_read_fs(f_geom, "geometry"),
  #     io_read_fs(f_annot, "annot"),
  #     io_read_fs(f_meas, "mea")
  #   )
  #   plot(x, name = c("measure", "lh.curv"), col = c("black", "white", "black"))
  # }


  re

}

io_write_fs <- function(x, con, type = c("geometry", "annotations", "measurements"),
                        name = 1L, ...) {

  type <- match.arg(type)

  if( type %in% c("color", "time_series") ) {
    stop("Saving ", type, " data in FreeSurfer format has not been implemented.")
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

  # if(x$sparse && type == "measurements") {
  #   warning("Saving ", type, " data with sparse index in FreeSurfer format is not supported. The result might be wrong. Please check it.")
  # }

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
      freesurferformats::write.fs.surface(filepath = con, vertex_coords = vertices, faces = faces)
    },
    "annotations" = {
      nms <- names(x$annotations$data_table)
      if(is.character(name) && !isTRUE(name %in% nms)) {
        stop("Cannot find annotation ", sQuote(name), " in the table. Available names are ",
             paste(sQuote(nms), collapse = ", "))
      }
      n_verts <- 0
      if( x$sparse ) {
        start_index <- attr(x$sparse_node_index, "start_index")
        n_verts <- max(x$sparse_node_index)
        if(length(start_index) == 1 && !is.na(start_index) && is.numeric(start_index)) {
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
    "measurements" = {
      # l <- freesurferformats::read.fs.label.native("~/rave_data/raw_dir/AnonSEEG2/rave-imaging/fs/label/lh.MT_exvivo.label", full = TRUE)

      nms <- names(x$measurements$data_table)
      if(is.character(name) && !isTRUE(name %in% nms)) {
        stop("Cannot find measurement ", sQuote(name), " in the table. Available names are ",
             paste(sQuote(nms), collapse = ", "))
      }

      vec <- x$measurements$data_table[[name]]

      if(x$sparse) {
        # write as weights
        start_index <- attr(x$sparse_node_index, start_index)
        node_index <- x$sparse_node_index
        if(length(start_index) == 1 && !is.na(start_index) && is.numeric(start_index)) {
          node_index <- node_index - start_index + 1L
        }
        freesurferformats::write.fs.weight(
          filepath = con,
          vertex_indices = as.integer(node_index),
          values = vec
        )
      } else {
        freesurferformats::write.fs.curv(con, vec)
      }

    }
  )

}
