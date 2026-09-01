# Handles text, binary (lsb/msb first) and base64 data, one or many columns,
# and mixed column types, including complex, rgb, RGBA, and String columns.

# ---------- type table -------------------------------------------------------

# `size` is the number of bytes one value occupies in binary/base64 form;
# `n_token` is the number of whitespace-separated tokens one value occupies in
# text form. String/Line have no fixed binary width (AFNI's `NI_write_element`
# forces text mode whenever an element carries one), hence `size = NA`.
NIML_DATATYPES <-  list(
  byte    = list(what = "integer",   size = 1L,  signed = FALSE, n_token = 1L),
  short   = list(what = "integer",   size = 2L,  signed = TRUE,  n_token = 1L),
  int     = list(what = "integer",   size = 4L,  signed = TRUE,  n_token = 1L),
  float   = list(what = "double",    size = 4L,  signed = TRUE,  n_token = 1L),
  double  = list(what = "double",    size = 8L,  signed = TRUE,  n_token = 1L),
  complex = list(what = "complex",   size = 8L,  signed = TRUE,  n_token = 2L),
  rgb     = list(what = "rgb",       size = 3L,  signed = FALSE, n_token = 3L),
  rgba    = list(what = "rgb",       size = 4L,  signed = FALSE, n_token = 4L),
  string  = list(what = "character", size = NA_integer_, signed = NA, n_token = 1L),
  line    = list(what = "character", size = NA_integer_, signed = NA, n_token = 1L)
)

# Single-letter aliases are case-sensitive: `s` is short but `S` is String,
# `r` is rgb but `R` is RGBA.
NIML_DATATYPE_ALIAS <- list(
  b = "byte", s = "short", i = "int", f = "float", d = "double",
  c = "complex", r = "rgb", R = "rgba", S = "string", L = "line"
)

niml_type_info <- function(t) {
  stopifnot(
    "NIML data type must be a length of one non-empty string" = is.character(t) &&
      length(t) == 1 &&
      !is.na(t) &&
      nzchar(t)
  )

  t1 <- tolower(t)
  if (nchar(t) == 1) {
    t1 <- NIML_DATATYPE_ALIAS[[t]]
  }
  if (!isTRUE(t1 %in% names(NIML_DATATYPES))) {
    stop("unsupported NIML data type: ", paste(t, collapse = ""))
  }
  NIML_DATATYPES[[t1]]
}

# "3*float" -> c("float","float","float");  "int,float" -> c("int","float")
niml_types <- function(s) {
  parts <- strsplit(gsub("[[:space:]]", "", s), "[,.]")[[1]]
  parts <- parts[nzchar(parts)]
  out <- character(0)
  for (p in parts) {
    m <- regmatches(p, regexec("^([0-9]+)\\*?(.+)$", p))[[1]]
    if (length(m) == 3L) out <- c(out, rep(m[3], as.integer(m[2])))
    else                 out <- c(out, p)
  }
  out
}

# Canonical lower-case type names, so downstream code can switch on them.
niml_type_names <- function(s) {
  types <- niml_types(s)
  vapply(types, function(t) {
    t1 <- tolower(t)
    if (nchar(t) == 1) {
      t1 <- NIML_DATATYPE_ALIAS[[t]]
    }
    if (!isTRUE(t1 %in% names(NIML_DATATYPES))) {
      stop("unsupported NIML data type: ", paste(t, collapse = ""))
    }
    t1
  }, character(1), USE.NAMES = FALSE)
}

# ---------- base64 -----------------------------------------------------------

# NIML base64 payloads use the standard alphabet and are wrapped at 72 columns,
# so strip anything outside the alphabet before decoding. `jsonlite` is already
# a hard dependency, so there is no need for a hand-rolled decoder here.
niml_b64_decode <- function(txt) {
  jsonlite::base64_dec(gsub("[^A-Za-z0-9+/=]", "", txt))
}

# ---------- header parsing ---------------------------------------------------

niml_attrs <- function(hdr) {
  pat <- "([A-Za-z_][A-Za-z0-9_.-]*)[[:space:]]*=[[:space:]]*(\"[^\"]*\"|'[^']*'|[^[:space:]>]+)"
  s <- regmatches(hdr, gregexpr(pat, hdr))[[1]]
  if (!length(s)) {
    return(character(0))
  }
  k <- sub("[[:space:]]*=.*$", "", s)
  v <- sub("^[^=]*=[[:space:]]*", "", s)
  v <- sub("^[\"']", "", sub("[\"']$", "", v))

  structure(v, names = k)
}

# Unescape the five XML entities NIML uses. `&amp;` must come last, otherwise
# a literal "&amp;lt;" would decode twice.
niml_unescape <- function(s) {
  s <- gsub("&lt;", "<", s, fixed = TRUE)
  s <- gsub("&gt;", ">", s, fixed = TRUE)
  s <- gsub("&quot;", "\"", s, fixed = TRUE)
  s <- gsub("&apos;", "'", s, fixed = TRUE)
  gsub("&amp;", "&", s, fixed = TRUE)
}

# Locate the "</" that closes a text/base64 element, skipping any that falls
# inside a quoted string value. The first candidate is almost always the right
# one, so this normally costs a single scan.
niml_text_end <- function(buf, start, n) {
  pos <- start
  repeat {
    e <- grepRaw("</", buf, offset = pos, fixed = TRUE)
    if (!length(e)) {
      return(n)
    }
    if (e <= start) {
      return(start - 1L)
    }
    # A closing tag is only real when an even number of quotes precedes it.
    # Count double quotes only: NIML quotes String values with `"`, and a
    # literal apostrophe inside such a value would otherwise skew the parity.
    chunk <- buf[start:(e - 1L)]
    if (sum(chunk == as.raw(34)) %% 2L == 0L) {
      return(e - 1L)
    }
    pos <- e + 2L
    if (pos > n) {
      return(n)
    }
  }
}

# Walk the file and return the element tree, preserving `ni_group` nesting.
# Nesting matters: an AFNI_labeltable group carries its own SPARSE_DATA and
# AFNI_atr elements that must not be confused with the dataset's own.
niml_elements <- function(buf) {
  n <- length(buf)
  pos <- 1L

  LT <- as.raw(60)
  GT <- as.raw(62)
  SL <- as.raw(47)

  DQ <- as.raw(34)
  SQ <- as.raw(39)
  NUL <- as.raw(0)

  root <- list(name = "", attributes = character(0), is_group = TRUE,
               children = list())
  # stack of open groups; the last entry is the current parent
  stack <- list(root)

  # nodes accumulate on the innermost open group; `stack` is rebound in place
  # rather than through a closure so the assignment stays local to this frame

  while (pos <= n) {
    lt <- grepRaw("<", buf, offset = pos, fixed = TRUE)
    if (!length(lt)) {
      break
    }

    # "</..." closing tag. Only pop when the name matches the innermost open
    # group: data elements such as </SPARSE_DATA> also land here, and popping
    # on those would close the enclosing group far too early.
    if (lt < n && buf[lt + 1L] == SL) {
      gt <- grepRaw(">", buf, offset = lt, fixed = TRUE)
      if (!length(gt)) {
        break
      }
      close_name <- rawToChar(buf[(lt + 2L):(gt - 1L)])
      close_name <- gsub("[[:space:]]", "", close_name)
      depth <- length(stack)
      if (depth > 1L && identical(close_name, stack[[depth]]$name)) {
        closed <- stack[[depth]]
        stack[[depth]] <- NULL
        depth <- depth - 1L
        stack[[depth]]$children[[length(stack[[depth]]$children) + 1L]] <- closed
      }
      pos <- gt + 1L
      next
    }

    # find header's ">"
    i <- lt + 1L
    q <- NUL
    while (i <= n) {
      b <- buf[[i]]
      if (q != NUL) {
        if (b == q) {
          q <- NUL
        }
      } else if (b == DQ || b == SQ) {
        q <- b
      } else if (b == GT) {
        break
      }
      i <- i + 1L
    }
    if (i > n) {
      break
    }
    hdr  <- rawToChar(buf[lt:i])
    name <- substring(regmatches(hdr, regexpr("^<[A-Za-z][A-Za-z0-9_.-]*", hdr)), 2)
    a    <- niml_attrs(hdr)
    pos  <- i + 1L

    form <- if (is.na(a["ni_form"])) {
      "text"
    } else {
      a[["ni_form"]]
    }

    if (grepl("/>$", hdr)) {
      # empty element: no payload, and it never opens a group
      depth <- length(stack)
      stack[[depth]]$children[[length(stack[[depth]]$children) + 1L]] <- list(
        name = name, attributes = a, is_group = FALSE,
        form = form, start = NA_integer_, end = NA_integer_
      )
      next
    }
    if (identical(form, "ni_group")) {
      # group open: becomes the parent for everything until its closing tag
      stack[[length(stack) + 1L]] <- list(
        name = name, attributes = a, is_group = TRUE, children = list()
      )
      next
    }
    if (is.na(a["ni_type"])) {
      next
    }

    start <- i + 1L
    is_binary <- grepl("^binary", form)
    is_b64 <- grepl("^base64", form)

    if (is_binary) {                                     # length is known
      if (is.na(a["ni_dimen"])) {
        stop("NIML element <", name, "> is binary but has no `ni_dimen`; ",
             "its length cannot be determined.")
      }
      nrows <- prod(as.numeric(strsplit(a[["ni_dimen"]], ",")[[1]]))
      sizes <- vapply(niml_types(a[["ni_type"]]),
                      function(t) niml_type_info(t)$size, integer(1))
      if (anyNA(sizes)) {
        stop("NIML element <", name, "> declares String/Line data in ",
             sQuote(form), " form, which has no fixed byte width.")
      }
      end <- start + nrows * sum(sizes) - 1L
    } else {                                             # text / base64
      end <- niml_text_end(buf, start, n)
    }
    end <- min(end, n)
    depth <- length(stack)
    stack[[depth]]$children[[length(stack[[depth]]$children) + 1L]] <- list(
      name = name, attributes = a, is_group = FALSE,
      form = form, start = start, end = end
    )
    pos <- end + 1L
  }

  # anything still open at EOF is closed implicitly
  while (length(stack) > 1L) {
    closed <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    depth <- length(stack)
    stack[[depth]]$children[[length(stack[[depth]]$children) + 1L]] <- closed
  }

  stack[[1]]$children
}

# ---------- turn one element into numbers / a string -------------------------

niml_string <- function(e, buf) {
  if (is.na(e$start) || e$end < e$start) {
    return("")
  }
  s <- rawToChar(buf[e$start:e$end])
  s <- gsub("^[[:space:]]+|[[:space:]]+$", "", s)
  s <- sub("^\"", "", sub("\"$", "", s))
  niml_unescape(s)
}

# Text payloads are whitespace-separated, with String values optionally
# wrapped in single or double quotes.
NIML_TOKEN_PATTERN <- paste0(
  '"(?:[^"\\\\]|\\\\.)*"', "|",
  "'(?:[^'\\\\]|\\\\.)*'", "|",
  "[^[:space:]]+"
)

niml_tokenize <- function(txt) {
  regmatches(txt, gregexpr(NIML_TOKEN_PATTERN, txt, perl = TRUE))[[1]]
}

niml_strip_quotes <- function(x) {
  dq <- grepl('^".*"$', x)
  x[dq] <- substr(x[dq], 2L, nchar(x[dq]) - 1L)
  sq <- grepl("^'.*'$", x)
  x[sq] <- substr(x[sq], 2L, nchar(x[sq]) - 1L)
  x
}

niml_column_names <- function(e, ncol) {
  nms <- sprintf("V%d", seq_len(ncol))
  labs <- e$attributes["COLMS_LABS"]
  if (!is.na(labs)) {
    labs <- strsplit(niml_unescape(labs[[1]]), ";", fixed = TRUE)[[1]]
    labs <- trimws(labs)
    # only trust the labels when they line up with the actual column count
    if (length(labs) == ncol && all(nzchar(labs))) {
      nms <- labs
    }
  }
  make.unique(nms)
}

niml_as_data_frame <- function(columns, nms, nrow) {
  structure(columns, names = nms, class = "data.frame",
            row.names = .set_row_names(nrow))
}

niml_values <- function(e, buf) {
  types <- niml_type_names(e$attributes[["ni_type"]])
  ncol  <- length(types)
  info  <- NIML_DATATYPES[types]
  per   <- vapply(info, function(k) { k$n_token }, integer(1))

  form <- e$form
  is_binary <- grepl("^binary", form)
  is_b64 <- grepl("^base64", form)

  nrow <- NA_integer_
  if (!is.na(e$attributes["ni_dimen"])) {
    nrow <- as.integer(prod(as.numeric(
      strsplit(e$attributes[["ni_dimen"]], ",")[[1]]
    )))
  }

  if (is.na(e$start) || e$end < e$start) {
    columns <- lapply(types, function(t) {
      switch(NIML_DATATYPES[[t]]$what,
             "integer" = integer(0), "double" = numeric(0),
             "complex" = complex(0), character(0))
    })
    return(niml_as_data_frame(columns, niml_column_names(e, ncol), 0L))
  }

  # ---- text -----------------------------------------------------------------
  if (!is_binary && !is_b64) {
    txt <- rawToChar(buf[e$start:e$end])
    has_text_col <- any(types %in% c("string", "line"))

    if (!has_text_col) {
      v <- scan(text = txt, quiet = TRUE, na.strings = c("NA", "nan", "NaN"))
      tot <- sum(per)
      if (is.na(nrow)) {
        nrow <- as.integer(length(v) %/% tot)
      }
      m <- matrix(v[seq_len(nrow * tot)], nrow = tot, ncol = nrow)
    } else {
      tok <- niml_tokenize(txt)
      tot <- sum(per)
      if (is.na(nrow)) {
        nrow <- as.integer(length(tok) %/% tot)
      }
      if (ncol == 1L && nrow == 1L && length(tok) > tot) {
        # an unquoted multi-word string: the whole payload is one value
        tok <- niml_string(e, buf)
      }
      if (length(tok) < nrow * tot) {
        stop("NIML element <", e$name, "> declares ", nrow,
             " rows but only ", length(tok) %/% tot, " could be parsed.")
      }
      m <- matrix(tok[seq_len(nrow * tot)], nrow = tot, ncol = nrow)
    }

    offs <- c(0L, cumsum(per))
    columns <- lapply(seq_len(ncol), function(j) {
      rows <- m[offs[[j]] + seq_len(per[[j]]), , drop = FALSE]
      switch(
        types[[j]],
        "string" = ,
        "line" = niml_unescape(niml_strip_quotes(rows[1, ])),
        "rgb" = grDevices::rgb(
          as.integer(rows[1, ]), as.integer(rows[2, ]), as.integer(rows[3, ]),
          maxColorValue = 255
        ),
        "rgba" = grDevices::rgb(
          as.integer(rows[1, ]), as.integer(rows[2, ]), as.integer(rows[3, ]),
          alpha = as.integer(rows[4, ]), maxColorValue = 255
        ),
        "complex" = complex(
          real = as.numeric(rows[1, ]), imaginary = as.numeric(rows[2, ])
        ),
        {
          v <- as.numeric(rows[1, ])
          if (info[[j]]$what == "integer") { as.integer(v) } else { v }
        }
      )
    })
    return(niml_as_data_frame(columns, niml_column_names(e, ncol), nrow))
  }

  # ---- binary / base64 ------------------------------------------------------
  if (is.na(nrow)) {
    stop("NIML element <", e$name, "> is ", sQuote(form),
         " but has no `ni_dimen`; its length cannot be determined.")
  }
  bytes <- buf[e$start:e$end]
  if (is_b64) {
    bytes <- niml_b64_decode(rawToChar(bytes))
  }
  # NIML swaps only when the declared order differs from the writer's native
  # order; with no suffix at all, native order is implied.
  endian <- if (grepl("lsbfirst", form)) {
    "little"
  } else if (grepl("msbfirst", form)) {
    "big"
  } else {
    .Platform$endian
  }

  sizes <- vapply(info, function(k) { k$size }, integer(1))
  rowsz <- sum(sizes)
  offs  <- c(0L, cumsum(sizes))

  columns <- lapply(seq_len(ncol), function(j) {
    k <- info[[j]]
    if (ncol == 1L) {
      chunk <- bytes[seq_len(min(length(bytes), nrow * rowsz))]
    } else {
      heads <- offs[[j]] + (seq_len(nrow) - 1L) * rowsz + 1L
      idx <- rep(heads, each = k$size) + rep(seq_len(k$size) - 1L, times = nrow)
      chunk <- bytes[idx]
    }
    switch(
      types[[j]],
      "rgb" = {
        v <- matrix(as.integer(chunk), nrow = 3L)
        grDevices::rgb(v[1, ], v[2, ], v[3, ], maxColorValue = 255)
      },
      "rgba" = {
        v <- matrix(as.integer(chunk), nrow = 4L)
        grDevices::rgb(v[1, ], v[2, ], v[3, ], alpha = v[4, ],
                       maxColorValue = 255)
      },
      "complex" = {
        v <- readBin(chunk, "double", n = nrow * 2L, size = 4L, endian = endian)
        complex(real = v[c(TRUE, FALSE)], imaginary = v[c(FALSE, TRUE)])
      },
      readBin(chunk, k$what, n = nrow, size = k$size, signed = k$signed,
              endian = endian)
    )
  })
  niml_as_data_frame(columns, niml_column_names(e, ncol), nrow)
}

# ---------- tree accessors ---------------------------------------------------

# Depth-first search for data elements by name. `recursive = FALSE` restricts
# the search to the immediate children, which is how the dataset's own
# SPARSE_DATA is told apart from the one inside an AFNI_labeltable group.

#' @rdname io_read_niml
#' @param x an \code{'ieegio_niml'} object, or an element node within one
#' @param name element names to look for, such as \code{'SPARSE_DATA'} or
#' \code{'AFNI_atr'}
#' @param recursive whether to descend into nested \verb{ni_group} elements;
#' default is true. Use \code{FALSE} to restrict the search to the immediate
#' children, for example to select the data element belonging to a dataset
#' itself rather than the one inside its label table
#' @param groups whether to return \verb{ni_group} elements instead of data
#' elements; default is false
#' @export
niml_find <- function(x, name, recursive = TRUE, groups = FALSE) {
  nodes <- if (is.list(x) && !is.null(x$children)) { x$children } else { x }
  out <- list()
  for (node in nodes) {
    if (isTRUE(node$is_group)) {
      if (groups && node$name %in% name) {
        out[[length(out) + 1L]] <- node
      }
      if (recursive) {
        out <- c(out, niml_find(node$children, name, recursive = TRUE,
                                groups = groups))
      }
    } else if (!groups && node$name %in% name) {
      out[[length(out) + 1L]] <- node
    }
  }
  out
}

# AFNI stores an element's column labels in a *sibling* `AFNI_atr` named
# COLMS_LABS rather than on the element itself.
niml_sibling_labs <- function(group, ncol) {
  atrs <- niml_find(group, "AFNI_atr", recursive = FALSE)
  for (a in atrs) {
    if (isTRUE(a$attributes[["atr_name"]] == "COLMS_LABS")) {
      labs <- trimws(strsplit(a$value[[1]][[1]], ";", fixed = TRUE)[[1]])
      labs <- labs[nzchar(labs)]
      if (length(labs) == ncol) {
        return(labs)
      }
    }
  }
  NULL
}

# ---------- main entry point -------------------------------------------------

#' @title Read an \verb{AFNI}/\verb{SUMA} \verb{NIML} dataset
#' @description
#' Reads a \verb{NIML} (\code{'.niml.dset'}) file into a nested tree of
#' elements. All \verb{NIML} storage forms are supported: plain text,
#' \verb{binary.lsbfirst}, \verb{binary.msbfirst}, \verb{base64.lsbfirst}, and
#' \verb{base64.msbfirst}; the file may additionally be \verb{gzip} compressed.
#' All \verb{NIML} column types are supported, including \code{'String'},
#' \code{'Line'}, \code{'complex'}, \code{'rgb'}, and \code{'rgba'}.
#'
#' Most users should call \code{\link{read_surface}} or
#' \code{\link{read_colormap}} instead, which build surface annotation and
#' color map objects on top of this function.
#' @param file path to a \verb{NIML} file
#' @returns \code{io_read_niml} returns an \code{'ieegio_niml'} object: a list
#' of element nodes. Each node has a \code{name}, an \code{attributes}
#' character vector, and either \code{children} (for \verb{ni_group} elements)
#' or a \code{value} \code{data.frame} with one column per \verb{ni_type} entry
#' and \verb{ni_dimen} rows. \code{niml_find} returns a list of the matching
#' element nodes.
#' @examples
#'
#' # Build a small NIML dataset with a nested label table
#' path <- tempfile(fileext = ".niml.dset")
#' writeLines(c(
#'   '<AFNI_dataset dset_type="Node_Label" ni_form="ni_group" >',
#'   '<SPARSE_DATA ni_type="int" ni_dimen="4" >',
#'   ' 0 1 2 1',
#'   '</SPARSE_DATA>',
#'   '<AFNI_labeltable ni_form="ni_group" >',
#'   '<SPARSE_DATA ni_type="4*float,int,String" ni_dimen="3" >',
#'   ' 0 0 0 1 0 "Unknown"',
#'   ' 1 0 0 1 1 "Left Insula"',
#'   ' 0 0 1 1 2 "Right Insula"',
#'   '</SPARSE_DATA>',
#'   '</AFNI_labeltable>',
#'   '</AFNI_dataset>'
#' ), path)
#'
#' x <- io_read_niml(path)
#' print(x)
#'
#' # the data belonging to the dataset itself, not to the label table
#' dset <- x[[1]]
#' niml_find(dset, "SPARSE_DATA", recursive = FALSE)[[1]]$value
#'
#' unlink(path)
#'
#' @export
io_read_niml <- function(file) {
  sz <- file.info(file)$size
  if (is.na(sz)) {
    stop("cannot open file: ", file)
  }

  if (sz < 2) {
    stop("NIML dataset cannot be less than 2 bytes. The data is empty.")
  }

  # gzfile handles it when the data is just plain text
  con <- gzfile(file, "rb")
  on.exit({
    close(con)
  }, add = TRUE)

  buf <- list()
  repeat {
    b <- readBin(con, "raw", n = 1e6)
    if (!length(b)) break
    buf[[length(buf) + 1L]] <- b
  }
  buf <- do.call(c, buf)

  decode <- function(nodes) {
    lapply(nodes, function(node) {
      if (isTRUE(node$is_group)) {
        node$children <- decode(node$children)
      } else {
        node$value <- niml_values(node, buf)
        node$start <- NULL
        node$end <- NULL
      }
      node
    })
  }

  structure(decode(niml_elements(buf)), class = "ieegio_niml")
}

#' @export
print.ieegio_niml <- function(x, ...) {
  cat("<ieegio NIML>\n")
  show <- function(nodes, depth) {
    for (node in nodes) {
      pad <- strrep("  ", depth + 1L)
      if (isTRUE(node$is_group)) {
        cat(sprintf("%s+ %s [group: %d element(s)]\n", pad, node$name,
                    length(node$children)))
        show(node$children, depth + 1L)
      } else {
        extra <- ""
        if (!is.na(node$attributes["atr_name"])) {
          extra <- sprintf(" (%s)", node$attributes[["atr_name"]])
        }
        cat(sprintf("%s- %s%s: %s [%s] %d x %d\n", pad, node$name, extra,
                    node$attributes[["ni_type"]], node$form,
                    nrow(node$value), ncol(node$value)))
      }
    }
  }
  show(x, 0L)
  invisible(x)
}

# ---------- conversion to `ieegio_surface` -----------------------------------

# SUMA additionally wraps label strings in escaped single quotes, so a decoded
# value can arrive as 'Unknown'. Strip only a symmetric pair.
niml_strip_label_quotes <- function(x) {
  sel <- grepl("^'.*'$", x)
  x[sel] <- substr(x[sel], 2L, nchar(x[sel]) - 1L)
  x
}

# Build the `label_table` from an AFNI_labeltable group. Its SPARSE_DATA holds
# "R;G;B;A;key;name" with R/G/B/A as floating point values in [0, 1].
niml_label_table <- function(group) {
  els <- niml_find(group, c("SPARSE_DATA", "DATA"), recursive = FALSE)
  if (!length(els)) {
    return(NULL)
  }
  tbl <- els[[1]]$value
  labs <- niml_sibling_labs(group, ncol(tbl))
  if (length(labs)) {
    names(tbl) <- labs
  }
  pick <- function(nm, idx) {
    if (!is.null(tbl[[nm]])) { tbl[[nm]] } else if (ncol(tbl) >= idx) { tbl[[idx]] } else { NULL }
  }
  key <- pick("key", 5L)
  name <- pick("name", 6L)
  if (is.null(key) || is.null(name)) {
    return(NULL)
  }
  red <- pick("R", 1L)
  green <- pick("G", 2L)
  blue <- pick("B", 3L)
  alpha <- pick("A", 4L)
  if (is.null(red)) { red <- rep(0, length(key)) }
  if (is.null(green)) { green <- rep(0, length(key)) }
  if (is.null(blue)) { blue <- rep(0, length(key)) }
  if (is.null(alpha)) { alpha <- rep(1, length(key)) }

  out <- data.table::data.table(
    Key = as.integer(key),
    Label = niml_strip_label_quotes(as.character(name)),
    Red = as.numeric(red),
    Green = as.numeric(green),
    Blue = as.numeric(blue),
    Alpha = as.numeric(alpha)
  )
  # AFNI pads label tables with repeated placeholder rows; keep the first row
  # per key so downstream look-ups stay unambiguous.
  out <- out[!duplicated(out$Key), ]
  out$Color <- grDevices::rgb(
    red = pmin(pmax(out$Red, 0), 1),
    green = pmin(pmax(out$Green, 0), 1),
    blue = pmin(pmax(out$Blue, 0), 1),
    maxColorValue = 1
  )
  data.table::setkeyv(out, "Key")
  out
}

# Locate the dataset group; a NIML file may or may not wrap its elements in an
# outer AFNI_dataset group.
niml_dataset_root <- function(x) {
  for (node in x) {
    if (isTRUE(node$is_group) && length(niml_find(node, c("SPARSE_DATA", "DATA"),
                                                  recursive = FALSE))) {
      return(node)
    }
  }
  list(name = "", attributes = character(0), is_group = TRUE, children = x)
}

# Collect `AFNI_atr` elements (both text and binary forms) keyed by `atr_name`.
niml_attributes <- function(group) {
  out <- list()
  for (a in niml_find(group, "AFNI_atr", recursive = FALSE)) {
    nm <- a$attributes["atr_name"]
    if (is.na(nm)) { next }
    v <- a$value
    # a 1 x 1 String attribute is far more useful as a plain scalar
    out[[nm[[1]]]] <- if (nrow(v) == 1L && ncol(v) == 1L) { v[[1]][[1]] } else { v }
  }
  out
}

niml_as_surface <- function(file, type = NULL, name = basename(file)) {
  x <- io_read_niml(file)
  dset <- niml_dataset_root(x)

  els <- niml_find(dset, c("SPARSE_DATA", "DATA"), recursive = FALSE)
  if (!length(els)) {
    stop("NIML file contains no SPARSE_DATA or DATA element: ", file)
  }
  data <- els[[1]]$value
  atr <- niml_attributes(dset)

  # column names: prefer the dataset's own COLMS_LABS, else name the single
  # column after the file so `merge()` on surfaces stays readable
  labs <- niml_sibling_labs(dset, ncol(data))
  if (length(labs)) {
    names(data) <- labs
  } else if (ncol(data) == 1L) {
    names(data) <- name
  } else {
    names(data) <- sprintf("%s.%d", name, seq_len(ncol(data)))
  }

  # node indices are 0-based in NIML
  node_index <- NULL
  idx_els <- niml_find(dset, "INDEX_LIST", recursive = FALSE)
  if (length(idx_els) && nrow(idx_els[[1]]$value)) {
    node_index <- list(
      node_index = as.integer(idx_els[[1]]$value[[1]]) + 1L,
      node_index_start = 1L
    )
  }

  lt_groups <- niml_find(dset, "AFNI_labeltable", recursive = TRUE,
                         groups = TRUE)
  label_table <- NULL
  if (length(lt_groups)) {
    label_table <- niml_label_table(lt_groups[[1]])
  }

  # Resolve the data type from the object, not from the values: `dset_type`
  # first, then whether the file carries a label table.
  if (!length(type) || identical(type, "auto")) {
    dset_type <- dset$attributes["dset_type"]
    type <- if (!is.na(dset_type) && grepl("label|roi", dset_type, ignore.case = TRUE)) {
      "annotations"
    } else if (!is.null(label_table)) {
      "annotations"
    } else {
      "measurements"
    }
  }

  header <- structure(
    class = "niml_dset",
    list(dset_type = unname(dset$attributes["dset_type"]), attributes = atr)
  )

  if (identical(type, "annotations")) {
    if (is.null(label_table)) {
      # an annotation without a table: synthesize keys so the object is usable
      keys <- sort(unique(as.integer(data[[1]])))
      label_table <- data.table::data.table(
        Key = keys, Label = as.character(keys),
        Red = 0, Green = 0, Blue = 0, Alpha = 1,
        Color = grDevices::rgb(0, 0, 0)
      )
      data.table::setkeyv(label_table, "Key")
    }
    for (nm in names(data)) {
      data[[nm]] <- as.integer(data[[nm]])
    }
    return(new_surface(
      header = header,
      annotations = list(
        label_table = label_table,
        data_table = data.table::as.data.table(data),
        meta = structure(names = names(data),
                         rep(list(atr), ncol(data)))
      ),
      sparse_node_index = node_index
    ))
  }

  new_surface(
    header = header,
    measurements = list(
      data_table = data.table::as.data.table(data),
      meta = list(intent = "NIFTI_INTENT_SHAPE")
    ),
    sparse_node_index = node_index
  )
}
