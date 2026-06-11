
# ---- Format-specific internal readers --------------------------------------

# Parse an attribute value from an XML-like tag string.
cmap_niml_attr <- function(tag, attr) {
  pattern <- sprintf('%s\\s*=\\s*["\']([^"\']*)["\']', attr)
  m <- regmatches(tag, regexpr(pattern, tag, perl = TRUE))
  if (!length(m)) return(NA_character_)
  sub(pattern, "\\1", m, perl = TRUE)
}

# FreeSurfer color LUT: txt format
# Lines: <index>  <label>  <R>  <G>  <B>  <A>
# Comments start with #
io_read_cmap_fs <- function(file, ...) {
  lines <- readLines(file, warn = FALSE)
  lines <- lines[!grepl("^\\s*#", lines) & nzchar(trimws(lines))]
  parts <- strsplit(trimws(lines), "\\s+")
  # Filter to lines with >=6 tokens (index label R G B A)
  parts <- Filter(function(p) { length(p) >= 6L }, parts)
  if (!length(parts)) stop("No valid entries found in FreeSurfer LUT file: ", file)
  idx   <- as.integer(vapply(parts, `[[`, 1L, FUN.VALUE = ""))
  lbl   <- vapply(parts, `[[`, 2L, FUN.VALUE = "")
  R_v   <- as.integer(vapply(parts, `[[`, 3L, FUN.VALUE = ""))
  G_v   <- as.integer(vapply(parts, `[[`, 4L, FUN.VALUE = ""))
  B_v   <- as.integer(vapply(parts, `[[`, 5L, FUN.VALUE = ""))
  A_v   <- as.integer(vapply(parts, `[[`, 6L, FUN.VALUE = ""))
  ct    <- new_colortable(data.frame(Key = idx, R = R_v, G = G_v, B = B_v, A = A_v))
  lut   <- new_lookup_discrete(data.frame(Key = idx, Label = lbl))
  new_colormap(ct, lookup = lut, colorspace = "RGB",
               meta = list(name = tools::file_path_sans_ext(basename(file)),
                           source = "FreeSurfer"))
}

# FreeSurfer annotation (.annot): extract colortable from existing io_read_fs()
io_read_cmap_fs_annot <- function(file, ...) {
  surf <- io_read_fs(file, type = "annotations")
  lt   <- surf$annotations$label_table
  if (is.null(lt)) stop("No label table found in annotation file: ", file)
  as_ieegio_colormap(lt)
}

# AFNI NIML: auto-detect labeltable vs colorscale inside the file
io_read_cmap_afni <- function(file, ...) {
  text <- paste(readLines(file, warn = FALSE), collapse = "\n")

  # Try labeltable first
  lt_match <- regexpr("<AFNI_labeltable[^>]*>.*?</AFNI_labeltable>",
                       text, perl = TRUE)
  cs_match <- regexpr("<AFNI_colorscale[^>]*>.*?</AFNI_colorscale>",
                       text, perl = TRUE)

  if (lt_match > 0L) {
    return(cmap_parse_afni_labeltable(regmatches(text, lt_match), file))
  }
  if (cs_match > 0L) {
    return(cmap_parse_afni_colorscale(regmatches(text, cs_match), file))
  }
  stop("No AFNI labeltable or colorscale found in file: ", file)
}

cmap_parse_afni_labeltable <- function(xml_text, file = "") {
  entries <- regmatches(
    xml_text,
    gregexpr("<AFNI_labeltable_entry[^/]*/?>", xml_text, perl = TRUE)
  )[[1L]]
  if (!length(entries)) stop("Empty AFNI labeltable in: ", file)

  idx <- as.integer(cmap_niml_attr(entries, "index"))
  lbl <- cmap_niml_attr(entries, "label")
  R_v <- as.integer(cmap_niml_attr(entries, "r"))
  G_v <- as.integer(cmap_niml_attr(entries, "g"))
  B_v <- as.integer(cmap_niml_attr(entries, "b"))
  A_v <- cmap_niml_attr(entries, "a")
  A_v <- ifelse(is.na(A_v), 255L, as.integer(A_v))

  ct  <- new_colortable(data.frame(Key = idx, R = R_v, G = G_v, B = B_v, A = A_v))
  lut <- new_lookup_discrete(data.frame(Key = idx, Label = lbl))
  new_colormap(ct, lookup = lut, colorspace = "RGB",
               meta = list(name = tools::file_path_sans_ext(basename(file)),
                           source = "AFNI"))
}

cmap_parse_afni_colorscale <- function(xml_text, file = "") {
  # Extract colorscale name
  nm <- cmap_niml_attr(xml_text, "Name")
  if (is.na(nm)) nm <- tools::file_path_sans_ext(basename(file))

  # Extract body between tags
  body <- sub("<AFNI_colorscale[^>]*>\\s*", "", xml_text)
  body <- sub("\\s*</AFNI_colorscale>", "", body)
  lines <- strsplit(trimws(body), "\\n")[[1L]]
  lines <- lines[nzchar(trimws(lines))]
  parts <- strsplit(trimws(lines), "\\s+")
  parts <- Filter(function(p) { length(p) >= 3L }, parts)
  if (!length(parts)) stop("No color entries in AFNI colorscale: ", file)

  n   <- length(parts)
  R_v <- as.integer(round(as.double(vapply(parts, `[[`, 1L, FUN.VALUE = "")) * 255))
  G_v <- as.integer(round(as.double(vapply(parts, `[[`, 2L, FUN.VALUE = "")) * 255))
  B_v <- as.integer(round(as.double(vapply(parts, `[[`, 3L, FUN.VALUE = "")) * 255))
  # Keys evenly 0..n-1; Key 0 is black (enforced by constructor)
  idx <- seq_len(n) - 1L

  ct  <- new_colortable(data.frame(
    Key = idx, R = R_v, G = G_v, B = B_v, A = rep(255L, n)
  ))
  lut <- new_lookup_continuous(data.frame(Value = c(0, 1), Scaled = c(0, 1)))
  new_colormap(ct, lookup = lut, colorspace = "RGB", data_range = NULL,
               meta = list(name = nm, source = "AFNI"))
}

# FSL .lut (VEST format) or plain .cmap
io_read_cmap_fsl <- function(file, ...) {
  lines <- readLines(file, warn = FALSE)
  text  <- paste(lines, collapse = "\n")

  if (grepl("%!VEST-LUT", text, fixed = TRUE)) {
    # VEST format: extract <-color{R,G,B}-> patterns
    m <- regmatches(text, gregexpr("<-color\\{([^}]*)\\}->", text, perl = TRUE))[[1L]]
    if (!length(m)) stop("No color entries in FSL VEST LUT: ", file)
    rgbs <- regmatches(m, regexpr("\\{([^}]*)\\}", m, perl = TRUE))
    rgbs <- sub("\\{", "", sub("\\}", "", rgbs))
    parts <- strsplit(rgbs, ",")
    n <- length(parts)
    R_v <- as.integer(round(as.double(vapply(parts, `[[`, 1L, FUN.VALUE = "")) * 255))
    G_v <- as.integer(round(as.double(vapply(parts, `[[`, 2L, FUN.VALUE = "")) * 255))
    B_v <- as.integer(round(as.double(vapply(parts, `[[`, 3L, FUN.VALUE = "")) * 255))
  } else {
    # Plain .cmap: lines of "R G B" in [0,1]
    data_lines <- lines[!grepl("^\\s*#", lines) & nzchar(trimws(lines))]
    parts      <- strsplit(trimws(data_lines), "\\s+")
    parts      <- Filter(function(p) { length(p) >= 3L }, parts)
    if (!length(parts)) stop("No color entries in FSL cmap: ", file)
    n   <- length(parts)
    R_v <- as.integer(round(as.double(vapply(parts, `[[`, 1L, FUN.VALUE = "")) * 255))
    G_v <- as.integer(round(as.double(vapply(parts, `[[`, 2L, FUN.VALUE = "")) * 255))
    B_v <- as.integer(round(as.double(vapply(parts, `[[`, 3L, FUN.VALUE = "")) * 255))
  }

  idx <- seq_len(n) - 1L
  ct  <- new_colortable(data.frame(
    Key = idx, R = R_v, G = G_v, B = B_v, A = rep(255L, n)
  ))
  lut <- new_lookup_continuous(data.frame(Value = c(0, 1), Scaled = c(0, 1)))
  new_colormap(ct, lookup = lut, colorspace = "RGB", data_range = NULL,
               meta = list(name = tools::file_path_sans_ext(basename(file)),
                           source = "FSL"))
}

# threeBrain / RAVE JSON colormap
io_read_cmap_threebrain <- function(file, ...) {
  obj <- io_read_json(file)

  # ieegio-native format: has "colortable" field
  if (!is.null(obj$colortable)) {
    return(cmap_parse_threebrain_native(obj, file))
  }

  # Original threeBrain format
  type <- tolower(obj$type %||% "continuous")

  if (type == "discrete") {
    vals   <- as.integer(obj$mapValues)
    labels <- as.character(obj$mapLabels)
    colors <- obj$mapColors

    rgba <- cmap_hex_to_rgba(colors)
    ct   <- new_colortable(data.frame(
      Key = vals, R = rgba[, 1L], G = rgba[, 2L], B = rgba[, 3L], A = rgba[, 4L]
    ))
    lut  <- new_lookup_discrete(data.frame(Key = vals, Label = labels))
    new_colormap(ct, lookup = lut, colorspace = obj$colorspace %||% "RGB",
                 meta = list(name = obj$alias %||% basename(file), source = "threeBrain"))
  } else {
    keys   <- as.double(obj$colorKeys)
    colors <- obj$colorValues
    dr     <- cmap_parse_data_range_json(obj$dataRange)

    rgba <- cmap_hex_to_rgba(colors)
    # Rescale 0-1 keys to integer range 0..length-1 for storage
    n    <- length(keys)
    idx  <- as.integer(round(keys * (n - 1L)))
    ct   <- new_colortable(data.frame(
      Key = idx, R = rgba[, 1L], G = rgba[, 2L], B = rgba[, 3L], A = rgba[, 4L]
    ))
    lut  <- new_lookup_continuous(data.frame(Value = c(0, 1), Scaled = c(0, 1)))
    new_colormap(ct, lookup = lut, colorspace = obj$colorspace %||% "RGB",
                 data_range = dr,
                 meta = list(name = obj$alias %||% basename(file), source = "threeBrain"))
  }
}

cmap_parse_threebrain_native <- function(obj, file = "") {
  type <- tolower(obj$type %||% "continuous")
  ct_data <- obj$colortable
  ct <- new_colortable(data.frame(
    Key = as.integer(ct_data$Key),
    R   = as.integer(ct_data$R),
    G   = as.integer(ct_data$G),
    B   = as.integer(ct_data$B),
    A   = as.integer(ct_data$A)
  ))

  lut_data <- obj$lookup
  if (!is.null(lut_data)) {
    if (lut_data$type == "discrete") {
      lut <- new_lookup_discrete(data.frame(
        Key   = as.integer(lut_data$Key),
        Label = as.character(lut_data$Label)
      ))
    } else {
      lut <- new_lookup_continuous(data.frame(
        Value  = as.double(lut_data$Value),
        Scaled = as.double(lut_data$Scaled)
      ))
    }
  } else {
    lut <- NULL
  }

  dr <- cmap_validate_data_range(cmap_parse_data_range_json(obj$dataRange))
  new_colormap(ct, lookup = lut, colorspace = obj$colorspace %||% "RGB",
               data_range = dr,
               meta = list(name = obj$name %||% basename(file), source = "ieegio"))
}

cmap_hex_to_rgba <- function(hex_colors) {
  # Parse #RRGGBB or #RRGGBBAA strings -> integer matrix nx4
  hex_colors <- trimws(hex_colors)
  # Pad 6-digit hex to 8-digit
  short <- nchar(hex_colors) == 7L   # #RRGGBB
  hex8  <- ifelse(short, paste0(hex_colors, "ff"), hex_colors)
  hex8  <- sub("^#", "", hex8)
  R <- strtoi(substr(hex8, 1L, 2L), 16L)
  G <- strtoi(substr(hex8, 3L, 4L), 16L)
  B <- strtoi(substr(hex8, 5L, 6L), 16L)
  A <- strtoi(substr(hex8, 7L, 8L), 16L)
  matrix(as.integer(c(R, G, B, A)), ncol = 4L, dimnames = list(NULL, c("R", "G", "B", "A")))
}

cmap_parse_data_range_json <- function(x) {
  if (is.null(x)) return(NULL)
  dr <- as.double(x)
  dr[!is.finite(dr)] <- NA_real_
  if (all(is.na(dr))) return(NULL)
  dr
}



# ---- Unified reader / writer ------------------------------------------------

#' @title Read a color map file
#' @description
#' Read a color map or color lookup table from a file.  The format is
#' auto-detected from the file extension and content.
#' @param file path to the color map file
#' @param type one of \code{"auto"} (default), \code{"fs_lut"},
#'   \code{"fs_annot"}, \code{"afni"}, \code{"fsl"}, \code{"threebrain"}
#' @param ... additional arguments passed to the format-specific reader
#' @return An \code{ieegio_colormap} or \code{ieegio_colortable} object.
#' @examples
#' # Write a colormap to a temp file and read it back
#' cm <- as_ieegio_colormap(as_ieegio_colortable(data.frame(
#'   Key = 1:3,
#'   R = c(255L, 0L, 0L), G = c(0L, 200L, 0L),
#'   B = c(0L, 0L, 180L), A = c(255L, 255L, 255L)
#' )))
#' tmp <- tempfile(fileext = ".json")
#' write_colormap(cm, tmp)
#' cm2 <- read_colormap(tmp)
#' print(cm2)
#' unlink(tmp)
#' @export
read_colormap <- function(file, type = "auto", ...) {
  file <- normalizePath(file, mustWork = TRUE)
  type <- match.arg(type, c("auto", "fs_lut", "fs_annot", "afni", "fsl", "threebrain"))

  if (type == "auto") {
    type <- cmap_detect_type(file)
  }

  switch(type,
    fs_lut     = io_read_cmap_fs(file, ...),
    fs_annot   = io_read_cmap_fs_annot(file, ...),
    afni       = io_read_cmap_afni(file, ...),
    fsl        = io_read_cmap_fsl(file, ...),
    threebrain = io_read_cmap_threebrain(file, ...),
    stop("Unknown colormap type: ", type)
  )
}

cmap_detect_type <- function(file) {
  base <- tolower(basename(file))
  ext  <- tolower(tools::file_ext(file))

  if (ext == "annot") return("fs_annot")
  if (ext == "json")  return("threebrain")
  if (ext == "cmap")  return("fsl")

  if (ext == "lut") {
    # Could be FSL VEST or AFNI labeltable
    first_lines <- readLines(file, n = 5L, warn = FALSE)
    if (any(grepl("%!VEST-LUT", first_lines, fixed = TRUE))) return("fsl")
    return("afni")
  }

  if (ext %in% c("niml", "dset") || grepl("\\.niml\\.dset$", base)) return("afni")

  if (ext == "txt") {
    # FreeSurfer LUT if starts with number then a label
    first_data <- Filter(function(l) { !grepl("^\\s*#", l) && nzchar(trimws(l)) },
                         readLines(file, n = 20L, warn = FALSE))
    if (length(first_data)) {
      parts <- strsplit(trimws(first_data[1L]), "\\s+")[[1L]]
      if (length(parts) >= 6L && !is.na(suppressWarnings(as.integer(parts[1L])))) {
        return("fs_lut")
      }
    }
  }

  stop("Cannot determine colormap format from file: ", file,
       "\nSupply 'type' argument explicitly.")
}

#' @title Write a color map to file
#' @description
#' Write an \code{ieegio_colormap} or \code{ieegio_colortable} to disk.
#' The default format is \pkg{threeBrain}/RAVE JSON, which preserves all information
#' and is readable by both ieegio and \pkg{threeBrain}.
#' @param x an \code{ieegio_colormap} or \code{ieegio_colortable}
#' @param con file path to write
#' @param format output format; currently only \code{"threebrain"} (default)
#'   and \code{"fs_lut"} are supported
#' @param ... additional arguments
#' @examples
#' # Build a discrete colormap with region labels
#' ct <- as_ieegio_colortable(data.frame(
#'   Key = 1:3,
#'   R = c(255L, 0L, 0L), G = c(0L, 200L, 0L),
#'   B = c(0L, 0L, 180L), A = c(255L, 255L, 255L)
#' ))
#' lut <- as_ieegio_lookup(data.frame(
#'   Key   = 1:3,
#'   Label = c("Cortex", "White matter", "CSF")
#' ))
#' cm <- as_ieegio_colormap(ct, lookup = lut)
#'
#' # Write as threeBrain JSON (default)
#' tmp_json <- tempfile(fileext = ".json")
#' write_colormap(cm, tmp_json)
#'
#' # Write as FreeSurfer LUT
#' tmp_lut <- tempfile(fileext = ".txt")
#' write_colormap(cm, tmp_lut, format = "fs_lut")
#'
#' unlink(c(tmp_json, tmp_lut))
#' @export
write_colormap <- function(x, con, format = "threebrain", ...) {
  format <- match.arg(format, c("threebrain", "fs_lut"))
  if (inherits(x, "ieegio_colortable")) {
    # Wrap bare colortable into a continuous colormap
    x <- as_ieegio_colormap(x)
  }
  if (!inherits(x, "ieegio_colormap")) {
    stop("`x` must be an ieegio_colormap or ieegio_colortable")
  }

  switch(format,
    threebrain = write_cmap_threebrain(x, con),
    fs_lut     = write_cmap_fs_lut(x, con),
    stop("Unknown format: ", format)
  )
  invisible(con)
}

write_cmap_threebrain <- function(x, con) {
  ct <- x$colors$color_table

  ct_list <- list(
    Key = as.list(ct$Key),
    R   = as.list(ct$R),
    G   = as.list(ct$G),
    B   = as.list(ct$B),
    A   = as.list(ct$A)
  )

  lut_list <- NULL
  if (!is.null(x$lookup)) {
    lt <- x$lookup$lookup_table
    if (inherits(x$lookup, "ieegio_lookup_discrete")) {
      lut_list <- list(type = "discrete",
                       Key   = as.list(lt$Key),
                       Label = as.list(lt$Label))
    } else {
      lut_list <- list(type = "continuous",
                       Value  = as.list(lt$Value),
                       Scaled = as.list(lt$Scaled))
    }
  }

  dr <- NULL
  if (!is.null(x$data_range)) {
    dr <- lapply(x$data_range, function(v) { if (is.finite(v)) v else NULL })
  }

  type_str <- if (inherits(x, "ieegio_colormap_discrete")) "discrete" else "continuous"

  obj <- drop_nulls(list(
    type       = type_str,
    colorspace = x$colorspace,
    dataRange  = dr,
    colortable = ct_list,
    lookup     = lut_list
  ))

  io_write_json(obj, con = con, serialize = FALSE)
}

write_cmap_fs_lut <- function(x, con) {
  if (!inherits(x, "ieegio_colormap_discrete")) {
    stop("fs_lut format requires a discrete colormap")
  }
  ct  <- x$colors$color_table
  lut <- x$lookup
  if (is.null(lut)) {
    lbl <- sprintf("%d", ct$Key)
  } else {
    lt  <- lut$lookup_table
    m   <- match(ct$Key, lt$Key)
    lbl <- ifelse(is.na(m), sprintf("%d", ct$Key), lt$Label[m])
  }

  lines <- c(
    "# Color LUT created by ieegio",
    "#No.    Label                           R    G    B    A",
    "",
    vapply(seq_len(nrow(ct)), function(i) {
      sprintf("%-7d %-40s %-4d %-4d %-4d %d",
              ct$Key[[i]], lbl[[i]], ct$R[[i]], ct$G[[i]], ct$B[[i]], ct$A[[i]])
    }, FUN.VALUE = "")
  )
  writeLines(lines, con = con)
}
