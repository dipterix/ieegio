
fs_lut <- local({
  lut <- NULL
  impl <- function() {
    # /Users/dipterix/Dropbox (Personal)/projects/ieegio/inst/lookup_table
    "inst/lookup_table/FreeSurferColorLUT.json"
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

#' @rdname read_surface
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
      meas <- freesurferformats::read.fs.curv(file, format = format, with_header = TRUE)

      data_table <- data.table::data.table(V = meas$data)
      names(data_table) <- name
      measurements <- list(
        data_table = data_table,
        meta = list()
      )
      return(
        new_surface(
          header = structure(
            class = "fs_measurement",
            meas$header
          ),
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

