
# ---- Internal helpers -------------------------------------------------------

colormap_hex <- function(R, G, B, A = 255L) {
  R <- as.integer(round(R))
  G <- as.integer(round(G))
  B <- as.integer(round(B))
  A <- as.integer(round(A))
  ifelse(
    A == 255L,
    grDevices::rgb(R, G, B, maxColorValue = 255L),
    grDevices::rgb(R, G, B, A, maxColorValue = 255L)
  )
}

# sRGB gamma decode: gamma-corrected [0,1] -> linear light [0,1]
cmap_srgb_decode <- function(c) {
  ifelse(c <= 0.04045, c / 12.92, ((c + 0.055) / 1.055)^2.4)
}

# sRGB gamma encode: linear light [0,1] -> gamma-corrected [0,1]
cmap_srgb_encode <- function(c) {
  c <- pmax(0, pmin(1, c))
  ifelse(c <= 0.0031308, c * 12.92, 1.055 * c ^ (1 / 2.4) - 0.055)
}

cmap_enforce_key0_black <- function(dt) {
  idx <- which(dt$Key == 0L)
  if (length(idx)) {
    dt$R[idx] <- 0L
    dt$G[idx] <- 0L
    dt$B[idx] <- 0L
  } else {
    dt <- rbind(
      data.frame(Key = 0L, R = 0L, G = 0L, B = 0L, A = 0L),
      dt
    )
  }
  dt <- dt[order(dt$Key), ]
  rownames(dt) <- NULL
  dt
}

cmap_validate_data_range <- function(data_range) {
  if (is.null(data_range)) return(NULL)
  dr <- as.double(data_range)
  if (length(dr) == 1L) {
    dr <- abs(dr) * c(-1, 1)
  }
  if (length(dr) != 2L) {
    warning("data_range must have length 1 or 2; ignoring")
    return(NULL)
  }
  dr
}

# Vectorized hue interpolation with shortest-path wrap-around.
# hue_range: 1 for HSV (h in [0,1]), 360 for HCL (H in [0,360)).
cmap_approx_hue <- function(x_stops, h_stops, x_query, hue_range = 360) {
  n <- length(h_stops)
  if (n <= 1L) return(rep(h_stops[1L], length(x_query)))

  # Unwrap: accumulate shortest-path deltas
  h_unwrapped <- h_stops
  for (i in seq_len(n - 1L) + 1L) {
    dh <- ((h_stops[i] - h_unwrapped[i - 1L] + hue_range * 1.5) %% hue_range) - hue_range * 0.5
    h_unwrapped[i] <- h_unwrapped[i - 1L] + dh
  }

  h_interp <- stats::approx(x_stops, h_unwrapped, xout = x_query,
                             method = "linear", rule = 2L)$y
  (h_interp %% hue_range + hue_range) %% hue_range
}

# Interpolate an nx4 RGBA table at `positions` (0-1) using the given colorspace.
# ct_keys: sorted numeric key positions (0-1 rescaled), same length as ct rows.
# ct: data.table with Key, R, G, B, A (integer 0-255).
cmap_interpolate_rgba <- function(ct_keys, ct, positions, colorspace) {
  A_out <- as.integer(round(
    stats::approx(ct_keys, ct$A, xout = positions, method = "linear", rule = 2L)$y
  ))

  if (colorspace == "RGB") {
    R_out <- as.integer(round(stats::approx(ct_keys, ct$R, xout = positions, method = "linear", rule = 2L)$y))
    G_out <- as.integer(round(stats::approx(ct_keys, ct$G, xout = positions, method = "linear", rule = 2L)$y))
    B_out <- as.integer(round(stats::approx(ct_keys, ct$B, xout = positions, method = "linear", rule = 2L)$y))
    return(list(R = R_out, G = G_out, B = B_out, A = A_out))
  }

  rgb_mat <- cbind(ct$R, ct$G, ct$B)
  cs_mat <- colorspace_from_rgb(rgb_mat, to = colorspace)

  if (colorspace %in% c("HSV", "HCL")) {
    hue_range <- if (colorspace == "HSV") 1 else 360
    ch1 <- cmap_approx_hue(ct_keys, cs_mat[, 1L], positions, hue_range)
  } else {
    ch1 <- stats::approx(ct_keys, cs_mat[, 1L], xout = positions, method = "linear", rule = 2L)$y
  }
  ch2 <- stats::approx(ct_keys, cs_mat[, 2L], xout = positions, method = "linear", rule = 2L)$y
  ch3 <- stats::approx(ct_keys, cs_mat[, 3L], xout = positions, method = "linear", rule = 2L)$y

  cs_interp <- matrix(c(ch1, ch2, ch3), ncol = 3L)
  rgb_out <- colorspace_to_rgb(cs_interp, from = colorspace)
  rgb_out <- pmax(0L, pmin(255L, round(rgb_out)))

  list(
    R = as.integer(rgb_out[, 1L]),
    G = as.integer(rgb_out[, 2L]),
    B = as.integer(rgb_out[, 3L]),
    A = A_out
  )
}


# ---- Constructors -----------------------------------------------------------

new_colortable <- function(color_table) {
  color_table <- as.data.frame(color_table)
  missing_cols <- setdiff(c("Key", "R", "G", "B", "A"), names(color_table))
  if (length(missing_cols)) {
    stop("color_table missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  color_table$Key <- as.integer(color_table$Key)
  color_table$R   <- as.integer(color_table$R)
  color_table$G   <- as.integer(color_table$G)
  color_table$B   <- as.integer(color_table$B)
  color_table$A   <- as.integer(color_table$A)
  # All-zero alpha means the format does not use transparency; treat as opaque.
  if (all(color_table$A == 0L)) color_table$A[] <- 255L
  color_table <- color_table[order(color_table$Key), ]
  rownames(color_table) <- NULL
  structure(list(color_table = color_table), class = "ieegio_colortable")
}

new_lookup_discrete <- function(lookup_table) {
  lookup_table <- as.data.frame(lookup_table, stringsAsFactors = FALSE)
  missing_cols <- setdiff(c("Key", "Label"), names(lookup_table))
  if (length(missing_cols)) {
    stop("lookup_table missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  lookup_table <- data.frame(
    Key   = as.integer(lookup_table[["Key"]]),
    Label = as.character(lookup_table[["Label"]]),
    stringsAsFactors = FALSE
  )
  if (!0L %in% lookup_table$Key) {
    lookup_table <- rbind(
      data.frame(Key = 0L, Label = "Unknown", stringsAsFactors = FALSE),
      lookup_table
    )
  } else {
    lookup_table[lookup_table$Key == 0L, "Label"] <- "Unknown"
  }
  lookup_table <- lookup_table[order(lookup_table$Key), ]
  rownames(lookup_table) <- NULL
  structure(
    list(type = "discrete", lookup_table = lookup_table),
    class = c("ieegio_lookup_discrete", "ieegio_lookup")
  )
}

new_lookup_continuous <- function(lookup_table) {
  lookup_table <- as.data.frame(lookup_table)
  missing_cols <- setdiff(c("Value", "Scaled"), names(lookup_table))
  if (length(missing_cols)) {
    stop("lookup_table missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  lookup_table <- data.frame(
    Value  = as.double(lookup_table[["Value"]]),
    Scaled = as.double(lookup_table[["Scaled"]])
  )
  lookup_table <- lookup_table[order(lookup_table$Value), ]
  rownames(lookup_table) <- NULL
  structure(
    list(type = "continuous", lookup_table = lookup_table),
    class = c("ieegio_lookup_continuous", "ieegio_lookup")
  )
}

new_colormap <- function(colors, lookup = NULL, colorspace = "RGB",
                         data_range = NULL,
                         type = c("auto", "discrete", "continuous"), ...) {
  if (!inherits(colors, "ieegio_colortable")) {
    stop("`colors` must be an ieegio_colortable")
  }
  colorspace <- match.arg(colorspace, c("RGB", "sRGB", "HSV", "HCL", "Lab"))
  data_range <- cmap_validate_data_range(data_range)
  type       <- match.arg(type)

  # When no lookup supplied, use type/data_range to decide the class.
  # type="continuous" or a non-NULL data_range both signal continuous intent.
  if (is.null(lookup)) {
    want_continuous <- identical(type, "continuous") ||
      (!identical(type, "discrete") && !is.null(data_range))
    if (want_continuous) {
      lookup <- new_lookup_continuous(
        data.frame(Value = c(0, 1), Scaled = c(0, 1),
                   stringsAsFactors = FALSE)
      )
    }
  }

  if (is.null(lookup) || inherits(lookup, "ieegio_lookup_discrete")) {
    structure(
      list(lookup = lookup, colors = colors, colorspace = colorspace),
      class = c("ieegio_colormap_discrete", "ieegio_colormap")
    )
  } else if (inherits(lookup, "ieegio_lookup_continuous")) {
    structure(
      list(lookup = lookup, colors = colors, colorspace = colorspace,
           data_range = data_range),
      class = c("ieegio_colormap_continuous", "ieegio_colormap")
    )
  } else {
    stop("`lookup` must be an ieegio_lookup object or NULL")
  }
}


# ---- format / print ---------------------------------------------------------

#' @export
format.ieegio_colortable <- function(x, ...) {
  ct <- x$color_table
  n  <- nrow(ct)
  s  <- c(
    "<ieegio Color Table>",
    sprintf("  %d color stops  |  Key range: [%d, %d]", n, min(ct$Key), max(ct$Key))
  )
  show <- if (n <= 6L) seq_len(n) else c(1:3, (n - 2L):n)
  rows <- vapply(show, function(i) {
    sprintf("    [%d]  %s", ct$Key[[i]],
            colormap_hex(ct$R[[i]], ct$G[[i]], ct$B[[i]], ct$A[[i]]))
  }, FUN.VALUE = "")
  if (n > 6L) rows <- append(rows, "    ...", after = 3L)
  c(s, rows)
}

#' @export
print.ieegio_colortable <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
format.ieegio_lookup <- function(x, ...) {
  lt <- x$lookup_table
  if (inherits(x, "ieegio_lookup_discrete")) {
    n     <- nrow(lt)
    n_show <- min(n, 5L)
    rows  <- vapply(seq_len(n_show), function(i) {
      sprintf("    [%d]  %s", lt$Key[[i]], lt$Label[[i]])
    }, FUN.VALUE = "")
    more  <- if (n > n_show) sprintf("    ... (%d more)", n - n_show) else NULL
    c("<ieegio Lookup [discrete]>", sprintf("  %d labels", n), rows, more)
  } else {
    c(
      "<ieegio Lookup [continuous]>",
      sprintf("  %d breakpoints  |  Value: [%g, %g]  ->  Scaled: [%g, %g]",
              nrow(lt), min(lt$Value), max(lt$Value), min(lt$Scaled), max(lt$Scaled))
    )
  }
}

#' @export
print.ieegio_lookup <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
format.ieegio_colormap <- function(x, ...) {
  type_str <- if (inherits(x, "ieegio_colormap_discrete")) "discrete" else "continuous"
  ct <- x$colors$color_table
  s  <- c(
    sprintf("<ieegio Colormap [%s]>", type_str),
    sprintf("  Colorspace   : %s", x$colorspace),
    sprintf("  Color stops  : %d  (keys %d to %d)", nrow(ct), min(ct$Key), max(ct$Key))
  )
  if (!is.null(x$data_range)) {
    dr <- x$data_range
    dr_str <- paste0("[",
      if (is.finite(dr[1])) dr[1] else "auto", ", ",
      if (is.finite(dr[2])) dr[2] else "auto", "]")
    s <- c(s, sprintf("  Data range   : %s", dr_str))
  }
  if (!is.null(x$lookup)) {
    lt <- x$lookup$lookup_table
    if (inherits(x$lookup, "ieegio_lookup_discrete")) {
      s <- c(s, sprintf("  Lookup       : %d labels", nrow(lt)))
    } else {
      s <- c(s, sprintf("  Lookup       : %d breakpoints", nrow(lt)))
    }
  }
  s
}

#' @export
print.ieegio_colormap <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}


# ---- as_ieegio_colortable ---------------------------------------------------

#' @title Convert to ieegio color table
#' @description
#' Convert a data frame or existing label table to an \code{ieegio_colortable}
#' object.
#' @param x object to convert
#' @param ... additional arguments passed to methods
#' @return An \code{ieegio_colortable} object.
#' @examples
#' ct <- as_ieegio_colortable(data.frame(
#'   Key = 1:3,
#'   R   = c(255L, 0L, 0L),
#'   G   = c(0L, 255L, 0L),
#'   B   = c(0L, 0L, 255L),
#'   A   = c(255L, 255L, 255L)
#' ))
#' print(ct)
#' @export
as_ieegio_colortable <- function(x, ...) { UseMethod("as_ieegio_colortable") }

#' @export
as_ieegio_colortable.ieegio_colortable <- function(x, ...) { x }

#' @export
as_ieegio_colortable.data.frame <- function(x, ...) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!"Key" %in% names(x) && "ColorID" %in% names(x)) x$Key <- x$ColorID
  # R/G/B detection
  if (all(c("R", "G", "B") %in% names(x))) {
    R <- as.integer(x$R)
    G <- as.integer(x$G)
    B <- as.integer(x$B)
  } else if (all(c("Red", "Green", "Blue") %in% names(x))) {
    # 0-1 scale from existing label_table format
    max_val <- max(c(x$Red, x$Green, x$Blue), na.rm = TRUE)
    scale   <- if (max_val <= 1) 255 else 1
    R <- as.integer(round(x$Red   * scale))
    G <- as.integer(round(x$Green * scale))
    B <- as.integer(round(x$Blue  * scale))
  } else {
    stop("Cannot find RGB columns; expected 'R','G','B' or 'Red','Green','Blue'")
  }
  # Alpha
  if ("A" %in% names(x)) {
    A <- as.integer(x$A)
  } else if ("Alpha" %in% names(x)) {
    max_a <- max(x$Alpha, na.rm = TRUE)
    A <- if (max_a <= 1) as.integer(round(x$Alpha * 255)) else as.integer(x$Alpha)
  } else {
    A <- rep(255L, nrow(x))
  }
  new_colortable(data.frame(Key = as.integer(x$Key), R = R, G = G, B = B, A = A))
}


# ---- as_ieegio_lookup -------------------------------------------------------

#' @title Convert to ieegio lookup table
#' @description
#' Convert a data frame to an \code{ieegio_lookup} object.
#' @param x object to convert
#' @param type \code{"discrete"} or \code{"continuous"} (or \code{"auto"} to detect)
#' @param ... additional arguments
#' @return An \code{ieegio_lookup} object.
#' @examples
#' # Discrete lookup: integer keys mapped to region labels
#' lut_disc <- as_ieegio_lookup(data.frame(
#'   Key   = 1:3,
#'   Label = c("Cortex", "White matter", "CSF")
#' ))
#' print(lut_disc)
#'
#' # Continuous lookup: piecewise value-to-scaled mapping
#' lut_cont <- as_ieegio_lookup(data.frame(
#'   Value  = c(-5, 0, 5),
#'   Scaled = c(0, 0.5, 1)
#' ))
#' print(lut_cont)
#' @export
as_ieegio_lookup <- function(x, type = "auto", ...) { UseMethod("as_ieegio_lookup") }

#' @export
as_ieegio_lookup.ieegio_lookup <- function(x, ...) { x }

#' @export
as_ieegio_lookup.data.frame <- function(x, type = "auto", ...) {
  x    <- as.data.frame(x, stringsAsFactors = FALSE)
  type <- match.arg(type, c("auto", "discrete", "continuous"))
  if (type == "auto") {
    if (all(c("Key", "Label") %in% names(x))) {
      type <- "discrete"
    } else if (all(c("Value", "Scaled") %in% names(x))) {
      type <- "continuous"
    } else {
      stop("Cannot auto-detect lookup type; supply 'type' argument")
    }
  }
  if (type == "discrete") {
    new_lookup_discrete(x)
  } else {
    new_lookup_continuous(x)
  }
}


# ---- as_ieegio_colormap -----------------------------------------------------

#' @title Convert to \code{ieegio_colormap}
#' @description
#' Combine or convert objects into an \code{ieegio_colormap}.
#' @param x an \code{ieegio_colortable}, existing \code{ieegio_colormap}, or
#'   label-table \code{data.frame}
#' @param lookup optional \code{ieegio_lookup} to attach; replaces existing
#'   lookup when \code{x} is already an \code{ieegio_colormap}
#' @param colorspace one of \code{"RGB"} (default), \code{"sRGB"},
#'   \code{"HSV"}, \code{"HCL"}, \code{"Lab"}
#' @param data_range optional numeric(1 or 2) fixed data range for continuous
#'   color maps; \code{NA}/\code{Inf} positions are filled from data at call
#'   time; \code{NULL} defers to the lookup table
#' @param type one of \code{"auto"} (default), \code{"discrete"}, or
#'   \code{"continuous"}.  When \code{lookup = NULL}, this controls whether a
#'   discrete or continuous color map is returned.  \code{"auto"} produces
#'   discrete unless \code{data_range} is also supplied.
#' @param ... additional arguments
#' @return An \code{ieegio_colormap} object.
#' @examples
#' # From a color table alone (discrete colormap, no labels)
#' ct <- as_ieegio_colortable(data.frame(
#'   Key = 1:3,
#'   R = c(255L, 0L, 0L), G = c(0L, 200L, 0L),
#'   B = c(0L, 0L, 180L), A = c(255L, 255L, 255L)
#' ))
#' cm <- as_ieegio_colormap(ct)
#' print(cm)
#'
#' # Continuous colormap from the same color table
#' cm_cont <- as_ieegio_colormap(ct, type = "continuous", data_range = c(-10, 10))
#' print(cm_cont)
#'
#' # From a label table data.frame (adds both colors and a discrete lookup)
#' lt <- data.frame(
#'   Key   = 1:2,
#'   Label = c("Region A", "Region B"),
#'   R = c(200L, 50L), G = c(50L, 200L),
#'   B = c(50L,  50L), A = c(255L, 255L)
#' )
#' cm2 <- as_ieegio_colormap(lt)
#' print(cm2)
#' @export
as_ieegio_colormap <- function(x, lookup = NULL, colorspace = "RGB",
                               data_range = NULL,
                               type = c("auto", "discrete", "continuous"), ...) {
  UseMethod("as_ieegio_colormap")
}

#' @export
as_ieegio_colormap.ieegio_colormap <- function(
    x, lookup = NULL, colorspace = NULL, data_range = NULL,
    type = c("auto", "discrete", "continuous"), ...) {
  type <- match.arg(type)

  # Resolve fields: caller overrides win, otherwise inherit from x.
  lut  <- if (!is.null(lookup)) lookup else x$lookup
  cs   <- if (!is.null(colorspace)) {
    match.arg(colorspace, c("RGB", "sRGB", "HSV", "HCL", "Lab"))
  } else {
    x$colorspace
  }
  dr   <- if (!is.null(data_range)) data_range else x$data_range

  if (identical(type, "auto")) {
    # In auto mode: keep the existing class; only patch what was supplied.
    x$lookup     <- lut
    x$colorspace <- cs
    if (inherits(x, "ieegio_colormap_continuous")) x$data_range <- dr
    return(x)
  }

  # Explicit type requested — rebuild so the class reflects the new type.
  # When switching to discrete, drop any continuous lookup (use NULL so
  # new_colormap produces a label-free discrete map).
  if (identical(type, "discrete") &&
      inherits(lut, "ieegio_lookup_continuous")) {
    lut <- NULL
  }
  new_colormap(x$colors, lookup = lut, colorspace = cs,
               data_range = dr, type = type)
}

#' @export
as_ieegio_colormap.ieegio_colortable <- function(x, lookup = NULL,
                                                  colorspace = "RGB",
                                                  data_range = NULL,
                                                  type = c("auto", "discrete", "continuous"),
                                                  ...) {
  new_colormap(x, lookup = lookup, colorspace = colorspace,
               data_range = data_range, type = type)
}

#' @export
as_ieegio_colormap.data.frame <- function(x, lookup = NULL, colorspace = "RGB",
                                           data_range = NULL,
                                           type = c("auto", "discrete", "continuous"),
                                           ...) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  # Label-table (from existing ieegio_surface$annotations$label_table)
  if (all(c("Key", "Label") %in% names(x))) {
    ct  <- as_ieegio_colortable(x)
    lut <- new_lookup_discrete(data.frame(
      Key   = as.integer(x[["Key"]]),
      Label = as.character(x[["Label"]]),
      stringsAsFactors = FALSE
    ))
    return(new_colormap(ct, lookup = lut, colorspace = colorspace))
  }
  stop("Cannot convert data.frame to ieegio_colormap; missing 'Key'/'Label' columns")
}


# ---- Colorspace helpers -----------------------------------------------------

#' @title Color space conversion helpers
#' @description
#' Convert an n-by-3 RGB integer matrix (values 0-255) to a
#' target color space, or convert back.  All conversions use base-R
#' \code{grDevices} - no additional packages required.
#' @param rgb_mat integer matrix with columns R, G, B (values 0-255)
#' @param mat color space matrix returned by \code{colorspace_from_rgb}
#' @param to,from one of \code{"RGB"}, \code{"sRGB"}, \code{"HSV"},
#'   \code{"HCL"}, \code{"Lab"}
#' @return A numeric matrix with 3 columns in the target color space.
#'   For \code{colorspace_to_rgb}: an integer matrix (0-255).
#' @examples
#' # RGB primaries as a 3x3 matrix (rows = colors, columns = R, G, B)
#' rgb_mat <- matrix(c(
#'   255,   0,   0,
#'     0, 255,   0,
#'     0,   0, 255
#' ), ncol = 3, byrow = TRUE)
#'
#' # Convert to HSV and back
#' hsv_mat  <- colorspace_from_rgb(rgb_mat, to = "HSV")
#' rgb_back <- colorspace_to_rgb(hsv_mat, from = "HSV")
#' print(rgb_back)
#'
#' # Convert to HCL (perceptually uniform)
#' hcl_mat <- colorspace_from_rgb(rgb_mat, to = "HCL")
#' print(hcl_mat)
#' @export
colorspace_from_rgb <- function(rgb_mat, to = c("RGB", "sRGB", "HSV", "HCL", "Lab")) {
  to      <- match.arg(to)
  rgb_mat <- matrix(as.double(rgb_mat), ncol = 3L)
  switch(to,
    "RGB"  = rgb_mat,
    "sRGB" = {
      # Linearise sRGB (remove gamma) -> linear light [0,1]
      cmap_srgb_decode(rgb_mat / 255)
    },
    "HSV"  = {
      # rgb2hsv needs 3xn (rows = R,G,B), values in [0,255]
      hsv_mat <- grDevices::rgb2hsv(t(rgb_mat))  # 3xn; rows h,s,v in [0,1]
      t(hsv_mat)                                  # nx3
    },
    "HCL"  = {
      luv <- grDevices::convertColor(rgb_mat / 255, from = "sRGB", to = "Luv")
      H   <- (atan2(luv[, 3L], luv[, 2L]) * 180 / pi + 360) %% 360
      C   <- sqrt(luv[, 2L]^2 + luv[, 3L]^2)
      L   <- luv[, 1L]
      matrix(c(H, C, L), ncol = 3L, dimnames = list(NULL, c("H", "C", "L")))
    },
    "Lab"  = {
      grDevices::convertColor(rgb_mat / 255, from = "sRGB", to = "Lab")
    }
  )
}

#' @rdname colorspace_from_rgb
#' @export
colorspace_to_rgb <- function(mat, from = c("RGB", "sRGB", "HSV", "HCL", "Lab")) {
  from <- match.arg(from)
  mat  <- matrix(as.double(mat), ncol = 3L)
  switch(from,
    "RGB"  = round(mat),
    "sRGB" = {
      # Re-apply sRGB gamma -> scale to 0-255
      round(cmap_srgb_encode(mat) * 255)
    },
    "HSV"  = {
      hex <- grDevices::hsv(h = mat[, 1L], s = mat[, 2L], v = mat[, 3L])
      t(grDevices::col2rgb(hex))  # nx3
    },
    "HCL"  = {
      H_rad <- mat[, 1L] * pi / 180
      C     <- mat[, 2L]
      L     <- mat[, 3L]
      U     <- C * cos(H_rad)
      V     <- C * sin(H_rad)
      luv   <- matrix(c(L, U, V), ncol = 3L)
      rgb01 <- grDevices::convertColor(luv, from = "Luv", to = "sRGB")
      round(pmax(0, pmin(1, rgb01)) * 255)
    },
    "Lab"  = {
      rgb01 <- grDevices::convertColor(mat, from = "Lab", to = "sRGB")
      round(pmax(0, pmin(1, rgb01)) * 255)
    }
  )
}


# ---- calculate_color --------------------------------------------------------

#' @title Map data values or atlas keys to hex colors
#' @description
#' Apply an \code{ieegio_colormap} to a vector of values or atlas keys, returning
#' a character vector of hex color strings.
#' @param x numeric or integer vector of data values / atlas keys
#' @param colormap an \code{ieegio_colormap} object
#' @param type one of \code{"auto"} (default), \code{"discrete"},
#'   \code{"continuous"}.  Auto-detection: integer class or any value outside
#'   the 0-1 range maps to discrete; numeric values all within 0-1 map
#'   to continuous.
#' @param colorspace override the color space stored in \code{colormap}; one of
#'   \code{"RGB"}, \code{"sRGB"}, \code{"HSV"}, \code{"HCL"}, \code{"Lab"}.
#'   \code{NULL} (default) inherits from \code{colormap$colorspace}.
#' @param keep_alpha logical; if \code{TRUE} (default), colors with alpha below
#'   255 are returned as \code{#RRGGBBAA}. Set to \code{FALSE} to always return
#'   \code{#RRGGBB}, dropping the alpha channel (needed for formats that do not
#'   support alpha in hex).
#' @param na color string used to replace \code{NA} inputs in the output;
#'   defaults to \code{"#00000000"} (transparent black). Set to \code{NA} to
#'   leave \code{NA} inputs as \code{NA} in the output.
#' @return Character vector of \code{#RRGGBB} (or \code{#RRGGBBAA} when
#'   \code{keep_alpha=TRUE} and alpha is below 255) color strings, same length
#'   as \code{x}.
#' @examples
#' # Build a red-to-green color table with 5 stops
#' ct <- as_ieegio_colortable(data.frame(
#'   Key = c(1L, 5L),
#'   R = c(255L, 0L), G = c(0L, 255L), B = c(0L, 0L), A = c(255L, 255L)
#' ))
#' cm <- as_ieegio_colormap(ct)
#'
#' # Discrete: Key 0 is always black; intermediate keys are interpolated
#' calculate_color(c(0L, 1L, 3L, 5L), cm)
#'
#' # Continuous: attach a value-to-scaled lookup and map numeric data
#' lut <- as_ieegio_lookup(data.frame(Value = c(0, 1), Scaled = c(0, 1)))
#' cm_cont <- as_ieegio_colormap(ct, lookup = lut)
#' calculate_color(c(0, 0.25, 0.5, 0.75, 1), cm_cont)
#'
#' # NA handling: default fills NA with transparent black
#' calculate_color(c(1L, NA_integer_, 5L), cm)
#' # Pass na = NA to keep NA in the output
#' calculate_color(c(1L, NA_integer_, 5L), cm, na = NA)
#' # Drop alpha channel for formats that do not support it
#' calculate_color(c(1L, 5L), cm, keep_alpha = FALSE)
#' @export
calculate_color <- function(x, colormap,
                             type       = c("auto", "discrete", "continuous"),
                             colorspace = NULL,
                             keep_alpha = TRUE,
                             na         = "#00000000") {
  if (!inherits(colormap, "ieegio_colormap")) {
    stop("`colormap` must be an ieegio_colormap object")
  }
  type <- match.arg(type)
  cs   <- colorspace %||% colormap$colorspace %||% "RGB"
  cs   <- match.arg(cs, c("RGB", "sRGB", "HSV", "HCL", "Lab"))

  if (type == "auto") {
    # Prefer the colormap's own class; fall back to inspecting x only when the
    # colormap carries no type information (should not normally occur).
    if (inherits(colormap, "ieegio_colormap_continuous")) {
      type <- "continuous"
    } else if (inherits(colormap, "ieegio_colormap_discrete")) {
      type <- "discrete"
    } else {
      type <- if (is.integer(x) ||
                  (is.numeric(x) && !all(is.na(x)) &&
                   any(x > 1 | x < 0, na.rm = TRUE))) "discrete" else "continuous"
    }
  }

  ct <- colormap$colors$color_table

  result <- if (type == "discrete") {
    cmap_calc_discrete(as.integer(x), ct, cs)
  } else {
    cmap_calc_continuous(as.double(x), ct, colormap$lookup,
                            colormap$data_range, cs)
  }

  if (!is.na(na)) result[is.na(result)] <- na
  if (!keep_alpha) result <- substr(result, 1L, 7L)
  result
}

cmap_calc_discrete <- function(x_int, ct, cs) {
  na_mask <- is.na(x_int)
  result  <- character(length(x_int))
  result[na_mask] <- NA_character_
  if (all(na_mask)) return(result)

  x_valid <- x_int[!na_mask]
  # Key=0 is special in discrete mode: always black (RGB forced to 0).
  ct <- cmap_enforce_key0_black(ct)
  all_keys <- ct$Key
  key_min  <- min(all_keys)
  key_max  <- max(all_keys)
  key_rng  <- key_max - key_min
  if (key_rng == 0L) key_rng <- 1L
  key_pos  <- (all_keys - key_min) / key_rng
  x_pos    <- (x_valid  - key_min) / key_rng

  rgba <- cmap_interpolate_rgba(key_pos, ct, x_pos, cs)
  result[!na_mask] <- colormap_hex(rgba$R, rgba$G, rgba$B, rgba$A)
  result
}

cmap_calc_continuous <- function(x_dbl, ct, lookup, data_range, cs) {
  na_mask <- is.na(x_dbl)
  result  <- character(length(x_dbl))
  result[na_mask] <- NA_character_
  if (all(na_mask)) return(result)

  x_valid <- x_dbl[!na_mask]

  # Resolve data_range (may contain NA/Inf to fill from x_valid)
  use_dr <- FALSE
  if (!is.null(data_range)) {
    dr <- data_range
    if (length(dr) == 1L) dr <- abs(dr) * c(-1, 1)
    x_rng <- range(x_valid, na.rm = TRUE)
    if (!is.finite(dr[1L]) || is.na(dr[1L])) dr[1L] <- x_rng[1L]
    if (!is.finite(dr[2L]) || is.na(dr[2L])) dr[2L] <- x_rng[2L]
    use_dr <- all(is.finite(dr))
  }

  # Map x -> scaled [0, 1]
  if (use_dr) {
    rng    <- dr[2L] - dr[1L]
    scaled <- if (rng == 0) rep(0.5, length(x_valid)) else (x_valid - dr[1L]) / rng
    scaled <- pmax(0, pmin(1, scaled))
  } else if (!is.null(lookup)) {
    lt     <- lookup$lookup_table
    scaled <- stats::approx(lt$Value, lt$Scaled, xout = x_valid,
                             method = "linear", rule = 2L)$y
    scaled <- pmax(0, pmin(1, scaled))
  } else {
    scaled <- pmax(0, pmin(1, x_valid))
  }

  # Map scaled position -> color via colortable
  all_keys <- ct$Key
  key_min  <- min(all_keys)
  key_max  <- max(all_keys)
  key_rng  <- key_max - key_min
  if (key_rng == 0L) key_rng <- 1L
  key_pos  <- (all_keys - key_min) / key_rng

  rgba <- cmap_interpolate_rgba(key_pos, ct, scaled, cs)
  result[!na_mask] <- colormap_hex(rgba$R, rgba$G, rgba$B, rgba$A)
  result
}


# ---- plot methods -----------------------------------------------------------

#' @title Plot a color map legend
#' @description
#' Plot a visual legend for an \code{ieegio_colormap} object.
#'
#' The continuous method draws a vertical gradient bar with tick marks on the
#' requested axis side (default left); the bar is rendered flush to the
#' opposite edge.  Ticks are placed at the minimum, maximum, and zero (when
#' zero falls strictly inside the range).
#'
#' The discrete method draws vertically stacked labeled color squares.  Labels
#' appear to the right of each square.  When there are more labels than
#' \code{max_labels}, the excess are shown as \code{"... (N omitted)"}.
#'
#' @param x an \code{ieegio_colormap_continuous} or
#'   \code{ieegio_colormap_discrete} object
#' @param data_range numeric(2); overrides the color bar range.  Falls back to
#'   \code{x$data_range}, then to \code{c(0, 1)} if still missing.
#' @param side integer; axis side for tick marks (see \code{\link{axis}}).
#'   \code{2} (left, default) places ticks on the left; \code{4} on the right.
#' @param las integer; label orientation passed to \code{\link{axis}}.
#'   Default \code{1} (always horizontal).
#' @param n_colors integer; number of color samples used to render the
#'   gradient bar.  Default \code{256}.
#' @param max_labels integer or \code{NA}; maximum number of labels to display
#'   in the discrete legend.  \code{NA} (default) auto-sizes from the current
#'   plot-region height.
#' @param cex numeric; character expansion for label text.  Default \code{1}.
#' @param border color for the box border in the discrete legend; \code{NA}
#'   (default) draws no border.
#' @param ... currently unused; reserved for future arguments.
#' @return Invisibly returns \code{x}.
#' @examples
#' # Continuous legend --------------------------------------------------
#' ct <- as_ieegio_colortable(data.frame(
#'   Key = c(0L, 50L, 100L),
#'   R = c(0L, 255L, 255L), G = c(0L, 255L, 0L),
#'   B = c(255L, 0L, 0L),   A = c(255L, 255L, 255L)
#' ))
#' lut <- as_ieegio_lookup(data.frame(
#'   Value = c(-3, 0, 3), Scaled = c(0, 0.5, 1)
#' ))
#' cm_cont <- as_ieegio_colormap(ct, lookup = lut, data_range = c(-3, 3))
#' grDevices::pdf(NULL)
#' plot(cm_cont)
#' grDevices::dev.off()
#'
#' # Discrete legend ----------------------------------------------------
#' ct2 <- as_ieegio_colortable(data.frame(
#'   Key = 1:4,
#'   R = c(255L, 0L, 0L, 128L), G = c(0L, 200L, 0L, 0L),
#'   B = c(0L, 0L, 180L, 180L), A = c(255L, 255L, 255L, 255L)
#' ))
#' lut2 <- as_ieegio_lookup(data.frame(
#'   Key   = 1:4,
#'   Label = c("Cortex", "White matter", "CSF", "Hippocampus")
#' ))
#' cm_disc <- as_ieegio_colormap(ct2, lookup = lut2)
#' grDevices::pdf(NULL)
#' plot(cm_disc)
#' grDevices::dev.off()
#' @rdname plot_ieegio_colormap
#' @export
plot.ieegio_colormap_continuous <- function(x, data_range = NULL,
                                             side = 2L, las = 1L,
                                             n_colors = 256L, ...) {
  # Resolve data range
  dr <- if (!is.null(data_range)) {
    cmap_validate_data_range(data_range)
  } else {
    x$data_range
  }
  if (is.null(dr) || !all(is.finite(dr))) dr <- c(0, 1)

  n_colors <- max(2L, as.integer(n_colors))
  vals     <- seq(dr[1L], dr[2L], length.out = n_colors)
  hex_cols <- calculate_color(vals, x, type = "continuous", keep_alpha = FALSE)

  # as.raster stores rows top-to-bottom; reverse so min is at the bottom
  ras <- grDevices::as.raster(matrix(rev(hex_cols), ncol = 1L))

  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = dr, xaxs = "i", yaxs = "i")
  graphics::rasterImage(ras, xleft = 0, ybottom = dr[1L], xright = 1, ytop = dr[2L])

  # Ticks at min, max, and 0 if strictly inside the range
  at_vals <- c(dr[1L], dr[2L])
  if (dr[1L] < 0 && dr[2L] > 0) at_vals <- c(at_vals, 0)
  graphics::axis(side = as.integer(side), at = sort(unique(at_vals)), las = as.integer(las))

  graphics::box()
  invisible(x)
}

#' @rdname plot_ieegio_colormap
#' @export
plot.ieegio_colormap_discrete <- function(x, max_labels = NA, cex = 1,
                                           border = NA, ...) {
  # Collect labels: from lookup if present, otherwise use key numbers
  ct <- x$colors$color_table
  if (!is.null(x$lookup) && inherits(x$lookup, "ieegio_lookup_discrete")) {
    lt      <- x$lookup$lookup_table
    keys    <- lt$Key
    labels  <- lt$Label
  } else {
    keys   <- ct$Key
    labels <- paste0("Key ", keys)
  }
  n_total <- length(keys)

  # Open the plot frame first so par("pin") reflects actual device geometry
  graphics::plot.new()

  # Auto-size max_labels from plot-region height
  if (is.na(max_labels)) {
    pin        <- graphics::par("pin")
    line_h_in  <- graphics::strheight("M", units = "inches", cex = cex)
    max_labels <- max(1L, floor(pin[2L] / (line_h_in * 1.5)))
  }
  max_labels <- min(as.integer(max_labels), n_total)

  n_omit <- max(0L, n_total - max_labels)
  n_show <- n_total - n_omit
  n_rows <- n_show + (if (n_omit > 0L) 1L else 0L)

  show_keys   <- keys[seq_len(n_show)]
  show_labels <- labels[seq_len(n_show)]
  hex_cols    <- calculate_color(show_keys, x, type = "discrete",
                                  keep_alpha = FALSE)

  graphics::plot.window(xlim = c(0, 1), ylim = c(0, n_rows), xaxs = "i", yaxs = "i")

  # Make the colored square approximately square in device units
  usr      <- graphics::par("usr")
  pin      <- graphics::par("pin")
  x_per_in <- (usr[2L] - usr[1L]) / pin[1L]
  y_per_in <- (usr[4L] - usr[3L]) / pin[2L]
  box_h    <- 0.7   # height of each square in user-coordinate rows
  box_w    <- min((box_h / y_per_in) * x_per_in, 0.2)
  gap      <- box_w * 0.25

  for (i in seq_len(n_show)) {
    y_mid <- n_rows - i + 0.5
    graphics::rect(xleft   = 0,
                   ybottom = y_mid - box_h / 2,
                   xright  = box_w,
                   ytop    = y_mid + box_h / 2,
                   col     = hex_cols[i],
                   border  = border)
    graphics::text(x      = box_w + gap,
                   y      = y_mid,
                   labels = show_labels[i],
                   adj    = c(0, 0.5),
                   cex    = cex)
  }

  if (n_omit > 0L) {
    graphics::text(x      = 0,
                   y      = 0.5,
                   labels = sprintf("... (%d omitted)", n_omit),
                   adj    = c(0, 0.5),
                   cex    = cex * 0.85,
                   col    = "gray50",
                   font   = 3L)
  }

  invisible(x)
}
