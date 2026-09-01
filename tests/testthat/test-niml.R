# Fixtures are generated here rather than downloaded so the encoding matrix
# (text / binary lsb+msb / base64 lsb+msb) can be exercised offline.

niml_test_encode_binary <- function(columns, types, endian) {
  n <- length(columns[[1]])
  chunks <- lapply(seq_len(n), function(i) {
    per_col <- lapply(seq_along(types), function(j) {
      v <- columns[[j]][[i]]
      switch(
        types[[j]],
        "byte" = as.raw(v),
        "short" = writeBin(as.integer(v), raw(), size = 2L, endian = endian),
        "int" = writeBin(as.integer(v), raw(), size = 4L, endian = endian),
        "float" = writeBin(as.double(v), raw(), size = 4L, endian = endian),
        "double" = writeBin(as.double(v), raw(), size = 8L, endian = endian),
        "complex" = writeBin(c(Re(v), Im(v)), raw(), size = 4L, endian = endian),
        "rgb" = as.raw(grDevices::col2rgb(v)[1:3, 1]),
        "rgba" = as.raw(grDevices::col2rgb(v, alpha = TRUE)[1:4, 1]),
        stop("test writer cannot encode ", types[[j]], " in binary")
      )
    })
    do.call(c, per_col)
  })
  do.call(c, chunks)
}

niml_test_encode_text <- function(columns, types) {
  n <- length(columns[[1]])
  lines <- vapply(seq_len(n), function(i) {
    per_col <- vapply(seq_along(types), function(j) {
      v <- columns[[j]][[i]]
      switch(
        types[[j]],
        "String" = ,
        "Line" = sprintf('"%s"', v),
        "complex" = sprintf("%.9g %.9g", Re(v), Im(v)),
        "rgb" = paste(grDevices::col2rgb(v)[1:3, 1], collapse = " "),
        "rgba" = paste(grDevices::col2rgb(v, alpha = TRUE)[1:4, 1], collapse = " "),
        "float" = ,
        "double" = sprintf("%.9g", v),
        sprintf("%d", as.integer(v))
      )
    }, character(1))
    paste(per_col, collapse = " ")
  }, character(1))
  paste0(" ", paste(lines, collapse = "\n"), "\n")
}

# Write one NIML element. `columns` is a list of equal-length vectors, one per
# entry in `types`. `types` are the canonical names the encoder understands;
# `declare` is what actually lands in `ni_type`, so alias spellings such as
# "c,r,R" can be exercised without teaching the encoder about them.
niml_test_element <- function(con, name, types, columns, form = "text",
                              extra = character(0), declare = NULL) {
  n <- length(columns[[1]])
  attrs <- c(
    sprintf('ni_type="%s"', declare %||% paste(types, collapse = ",")),
    sprintf('ni_dimen="%d"', n),
    if (!identical(form, "text")) { sprintf('ni_form="%s"', form) },
    extra
  )
  writeBin(charToRaw(sprintf("<%s\n  %s >", name, paste(attrs, collapse = "\n  "))), con)
  if (identical(form, "text")) {
    writeBin(charToRaw(niml_test_encode_text(columns, types)), con)
  } else {
    endian <- if (grepl("msbfirst", form)) { "big" } else { "little" }
    payload <- niml_test_encode_binary(columns, types, endian)
    if (grepl("^base64", form)) {
      payload <- charToRaw(jsonlite::base64_enc(payload))
    }
    writeBin(payload, con)
  }
  writeBin(charToRaw(sprintf("</%s>\n", name)), con)
}

niml_test_file <- function(elements, dset_type = "Node_Label") {
  path <- tempfile(fileext = ".niml.dset")
  con <- file(path, "wb")
  on.exit({ close(con) }, add = TRUE)
  writeBin(charToRaw(sprintf(
    '<AFNI_dataset\n  dset_type="%s"\n  ni_form="ni_group" >\n', dset_type
  )), con)
  for (el in elements) {
    if (identical(el$name, "__group_open__")) {
      writeBin(charToRaw(sprintf(
        '<%s\n  ni_form="ni_group" >\n', el$group
      )), con)
    } else if (identical(el$name, "__group_close__")) {
      writeBin(charToRaw(sprintf("</%s>\n", el$group)), con)
    } else {
      niml_test_element(con, el$name, el$types, el$columns,
                        form = el$form %||% "text",
                        extra = el$extra %||% character(0),
                        declare = el$declare)
    }
  }
  writeBin(charToRaw("</AFNI_dataset>\n"), con)
  path
}

`%||%` <- function(x, y) { if (is.null(x)) { y } else { x } }


testthat::test_that("NIML encodings agree: text, binary lsb/msb, base64 lsb/msb", {

  ints <- c(0L, 1L, -5L, 112L, 2147483647L, -2147483647L)
  dbls <- c(0, 0.5, -0.25, 1.5, 1024, -3.75)

  forms <- c("text", "binary.lsbfirst", "binary.msbfirst",
             "base64.lsbfirst", "base64.msbfirst")

  results <- lapply(forms, function(form) {
    path <- niml_test_file(list(list(
      name = "SPARSE_DATA", types = c("int", "double"),
      columns = list(ints, dbls), form = form
    )))
    on.exit({ unlink(path) }, add = TRUE)
    x <- io_read_niml(path)
    niml_find(x, "SPARSE_DATA")[[1]]$value
  })
  names(results) <- forms

  for (form in forms) {
    testthat::expect_identical(results[[form]][[1]], ints, info = form)
    testthat::expect_equal(results[[form]][[2]], dbls, info = form)
  }

  # all five decode to exactly the same table
  for (form in forms[-1]) {
    testthat::expect_equal(results[[form]], results[["text"]], info = form)
  }
})


testthat::test_that("NIML byte/short/float round-trip in every binary form", {

  bytes <- c(0L, 1L, 127L, 128L, 255L)   # NI_BYTE is unsigned
  shorts <- c(0L, -32768L, 32767L, 5L, -5L)
  floats <- c(0, 0.5, -0.25, 1.5, 64)    # exact in float32

  for (form in c("binary.lsbfirst", "binary.msbfirst",
                 "base64.lsbfirst", "base64.msbfirst", "text")) {
    path <- niml_test_file(list(list(
      name = "SPARSE_DATA", types = c("byte", "short", "float"),
      columns = list(bytes, shorts, floats), form = form
    )))
    on.exit({ unlink(path) }, add = TRUE)
    v <- niml_find(io_read_niml(path), "SPARSE_DATA")[[1]]$value
    testthat::expect_identical(v[[1]], bytes, info = form)
    testthat::expect_identical(v[[2]], shorts, info = form)
    testthat::expect_equal(v[[3]], floats, info = form)
  }
})


testthat::test_that("NIML complex, rgb and rgba columns decode", {

  cx <- c(complex(real = 1, imaginary = -2), complex(real = 0.5, imaginary = 0))
  rgb_v <- c("#FF0000", "#0A141E")
  rgba_v <- c("#FF000080", "#0A141EFF")

  for (form in c("text", "binary.lsbfirst", "binary.msbfirst",
                 "base64.lsbfirst")) {
    path <- niml_test_file(list(list(
      name = "SPARSE_DATA", types = c("complex", "rgb", "rgba"),
      columns = list(cx, rgb_v, rgba_v), form = form
    )))
    on.exit({ unlink(path) }, add = TRUE)
    v <- niml_find(io_read_niml(path), "SPARSE_DATA")[[1]]$value
    testthat::expect_equal(v[[1]], cx, info = form)
    testthat::expect_identical(toupper(v[[2]]), rgb_v, info = form)
    testthat::expect_identical(toupper(v[[3]]), rgba_v, info = form)
  }

  # single-letter aliases: c=complex, r=rgb, R=rgba (case sensitive)
  path <- niml_test_file(list(list(
    name = "SPARSE_DATA", types = c("complex", "rgb", "rgba"),
    declare = "c,r,R",
    columns = list(cx, rgb_v, rgba_v), form = "text"
  )))
  on.exit({ unlink(path) }, add = TRUE)
  v <- niml_find(io_read_niml(path), "SPARSE_DATA")[[1]]$value
  testthat::expect_equal(v[[1]], cx)
  testthat::expect_identical(toupper(v[[2]]), rgb_v)
  testthat::expect_identical(toupper(v[[3]]), rgba_v)
})


testthat::test_that("NIML String columns decode XML entities", {

  raw_labels <- c("&apos;Unknown&apos;", "A &amp; B", "&lt;tag&gt;",
                  "say &quot;hi&quot;")
  path <- niml_test_file(list(list(
    name = "SPARSE_DATA", types = c("int", "String"),
    columns = list(seq_along(raw_labels), raw_labels), form = "text"
  )))
  on.exit({ unlink(path) }, add = TRUE)

  v <- niml_find(io_read_niml(path), "SPARSE_DATA")[[1]]$value
  testthat::expect_identical(
    v[[2]],
    c("'Unknown'", "A & B", "<tag>", 'say "hi"')
  )
})


testthat::test_that("NIML nested groups keep the dataset's own data", {

  # regression: the AFNI_labeltable's SPARSE_DATA used to overwrite the
  # dataset's SPARSE_DATA because the element list was flat
  path <- niml_test_file(list(
    list(name = "SPARSE_DATA", types = "int",
         columns = list(c(0L, 1L, 2L, 1L)), form = "binary.lsbfirst"),
    list(name = "INDEX_LIST", types = "int",
         columns = list(0:3), form = "binary.lsbfirst"),
    list(name = "__group_open__", group = "AFNI_labeltable"),
    list(name = "SPARSE_DATA", types = c("float", "float", "float", "float",
                                         "int", "String"),
         columns = list(c(0, 1, 0), c(0, 0, 1), c(0, 0, 0), c(1, 1, 1),
                        c(0L, 1L, 2L),
                        c("Unknown", "Left Insula", "Right Insula")),
         form = "text"),
    list(name = "AFNI_atr", types = "String",
         columns = list("R;G;B;A;key;name"),
         extra = 'atr_name="COLMS_LABS"'),
    list(name = "__group_close__", group = "AFNI_labeltable")
  ))
  on.exit({ unlink(path) }, add = TRUE)

  x <- io_read_niml(path)
  dset <- x[[1]]
  testthat::expect_true(isTRUE(dset$is_group))

  # non-recursive search must find only the dataset's own element
  own <- niml_find(dset, "SPARSE_DATA", recursive = FALSE)
  testthat::expect_length(own, 1L)
  testthat::expect_identical(own[[1]]$value[[1]], c(0L, 1L, 2L, 1L))

  # recursive search finds both
  testthat::expect_length(niml_find(dset, "SPARSE_DATA", recursive = TRUE), 2L)

  # and the group survives as a group
  groups <- niml_find(dset, "AFNI_labeltable", recursive = TRUE, groups = TRUE)
  testthat::expect_length(groups, 1L)
  testthat::expect_identical(
    niml_find(groups[[1]], "SPARSE_DATA", recursive = FALSE)[[1]]$value[[6]],
    c("Unknown", "Left Insula", "Right Insula")
  )
})


testthat::test_that("NIML binary AFNI_atr is decoded, not read as text", {

  # regression: a binary AFNI_atr used to hit rawToChar() and raise
  # "embedded nul in string"
  path <- niml_test_file(list(
    list(name = "SPARSE_DATA", types = "int",
         columns = list(c(5L, 6L)), form = "text"),
    list(name = "AFNI_atr", types = "int", columns = list(0:180),
         form = "binary.lsbfirst",
         extra = 'atr_name="UNIQUE_VALS_000000"')
  ))
  on.exit({ unlink(path) }, add = TRUE)

  x <- testthat::expect_no_error(io_read_niml(path))
  atrs <- niml_find(x, "AFNI_atr")
  testthat::expect_length(atrs, 1L)
  testthat::expect_identical(atrs[[1]]$value[[1]], 0:180)
})


testthat::test_that("NIML respects ni_dimen and ignores alloc_max", {

  # alloc_max is a writer-side allocation hint. AFNI emits values such as
  # alloc_max="8196" against a 16-character string, so it must never be used
  # to size, pad or truncate the value.
  path <- niml_test_file(list(
    list(name = "SPARSE_DATA", types = "int", columns = list(1:2)),
    list(name = "AFNI_atr", types = "String",
         columns = list("R;G;B;A;key;name"),
         extra = c('atr_name="COLMS_LABS"', 'alloc_max="8196"')),
    list(name = "AFNI_atr", types = "String",
         columns = list("this value is much longer than alloc_max claims"),
         extra = c('atr_name="SHORT_CLAIM"', 'alloc_max="3"'))
  ))
  on.exit({ unlink(path) }, add = TRUE)

  atrs <- niml_find(io_read_niml(path), "AFNI_atr")
  testthat::expect_identical(atrs[[1]]$value[[1]], "R;G;B;A;key;name")
  testthat::expect_identical(
    atrs[[2]]$value[[1]],
    "this value is much longer than alloc_max claims"
  )
  # the hint itself is preserved verbatim
  testthat::expect_identical(unname(atrs[[1]]$attributes[["alloc_max"]]), "8196")

  # ni_dimen drives the row count
  path2 <- niml_test_file(list(
    list(name = "SPARSE_DATA", types = "int", columns = list(1:2)),
    list(name = "AFNI_atr", types = "String",
         columns = list(c("one", "two", "three")),
         extra = 'atr_name="MULTI"')
  ))
  on.exit({ unlink(path2) }, add = TRUE)
  v <- niml_find(io_read_niml(path2), "AFNI_atr")[[1]]$value
  testthat::expect_identical(nrow(v), 3L)
  testthat::expect_identical(v[[1]], c("one", "two", "three"))
})


testthat::test_that("NIML unquoted multi-word String with ni_dimen=1", {

  path <- tempfile(fileext = ".niml.dset")
  on.exit({ unlink(path) }, add = TRUE)
  writeLines(c(
    '<AFNI_dataset dset_type="Node_Label" ni_form="ni_group" >',
    '<SPARSE_DATA ni_type="int" ni_dimen="2" >',
    " 1 2",
    "</SPARSE_DATA>",
    '<AFNI_atr ni_type="String" ni_dimen="1" atr_name="COLMS_LABS" >',
    " node label",
    "</AFNI_atr>",
    "</AFNI_dataset>"
  ), path)

  v <- niml_find(io_read_niml(path), "AFNI_atr")[[1]]$value
  testthat::expect_identical(nrow(v), 1L)
  testthat::expect_identical(v[[1]], "node label")
})


testthat::test_that("NIML text element without ni_dimen infers the row count", {

  path <- tempfile(fileext = ".niml.dset")
  on.exit({ unlink(path) }, add = TRUE)
  writeLines(c(
    '<AFNI_dataset dset_type="Node_Label" ni_form="ni_group" >',
    '<SPARSE_DATA ni_type="int,float" >',
    " 1 0.5",
    " 2 1.5",
    " 3 2.5",
    "</SPARSE_DATA>",
    "</AFNI_dataset>"
  ), path)

  v <- niml_find(io_read_niml(path), "SPARSE_DATA")[[1]]$value
  testthat::expect_identical(nrow(v), 3L)
  testthat::expect_identical(v[[1]], 1:3)
  testthat::expect_equal(v[[2]], c(0.5, 1.5, 2.5))
})


testthat::test_that("NIML binary element with String column errors clearly", {

  path <- tempfile(fileext = ".niml.dset")
  on.exit({ unlink(path) }, add = TRUE)
  writeLines(c(
    '<AFNI_dataset ni_form="ni_group" >',
    '<SPARSE_DATA ni_type="int,String" ni_dimen="2" ni_form="binary.lsbfirst" >',
    "xxxx",
    "</SPARSE_DATA>",
    "</AFNI_dataset>"
  ), path)

  testthat::expect_error(io_read_niml(path), "no fixed byte width")
})


testthat::test_that("NIML dataset converts to an ieegio surface", {

  path <- niml_test_file(list(
    list(name = "SPARSE_DATA", types = "int",
         columns = list(c(0L, 1L, 2L, 1L)), form = "binary.lsbfirst"),
    list(name = "INDEX_LIST", types = "int",
         columns = list(c(10L, 11L, 12L, 13L)), form = "binary.lsbfirst"),
    list(name = "__group_open__", group = "AFNI_labeltable"),
    list(name = "SPARSE_DATA",
         types = c("float", "float", "float", "float", "int", "String"),
         columns = list(c(0, 1, 0), c(0, 0, 1), c(0, 0, 0), c(1, 1, 1),
                        c(0L, 1L, 2L),
                        c("&apos;Unknown&apos;", "Left Insula", "Right Insula")),
         form = "text"),
    list(name = "AFNI_atr", types = "String",
         columns = list("R;G;B;A;key;name"),
         extra = 'atr_name="COLMS_LABS"'),
    list(name = "__group_close__", group = "AFNI_labeltable")
  ))
  on.exit({ unlink(path) }, add = TRUE)

  surf <- read_surface(path)
  testthat::expect_true("annotations" %in% names(surf))

  dt <- surf$annotations$data_table
  testthat::expect_identical(nrow(dt), 4L)
  testthat::expect_identical(dt[[1]], c(0L, 1L, 2L, 1L))

  lt <- surf$annotations$label_table
  testthat::expect_identical(lt$Key, c(0L, 1L, 2L))
  # entity-decoded, and SUMA's extra single quotes stripped
  testthat::expect_identical(lt$Label, c("Unknown", "Left Insula", "Right Insula"))
  testthat::expect_identical(toupper(lt$Color), c("#000000", "#FF0000", "#00FF00"))

  # NIML node indices are 0-based; ieegio surfaces are 1-based, so the
  # INDEX_LIST 10,11,12,13 addresses vertices 11,12,13,14
  testthat::expect_identical(as.integer(surf$sparse_node_index),
                             c(11L, 12L, 13L, 14L))

  cmap <- read_colormap(path)
  testthat::expect_identical(cmap$colors$color_table$Key, c(0L, 1L, 2L))
  testthat::expect_identical(cmap$colors$color_table$R, c(0L, 255L, 0L))
})


testthat::test_that("NIML dataset without a label table reads as measurements", {

  path <- niml_test_file(list(
    list(name = "SPARSE_DATA", types = "float",
         columns = list(c(0.5, -1.5, 2.25)), form = "binary.lsbfirst")
  ), dset_type = "Node_Bucket")
  on.exit({ unlink(path) }, add = TRUE)

  surf <- read_surface(path)
  testthat::expect_true("measurements" %in% names(surf))
  testthat::expect_equal(surf$measurements$data_table[[1]], c(0.5, -1.5, 2.25))
})


testthat::test_that("NIML reader handles the shipped AFNI sample", {

  fname <- "niml/std.141.lh.aparc.a2009s.annot.niml.dset"
  testthat::skip_if_not(ieegio_sample_data(fname, test = TRUE))

  path <- ieegio_sample_data(fname)

  x <- io_read_niml(path)
  testthat::expect_s3_class(x, "ieegio_niml")

  dset <- x[[1]]
  own <- niml_find(dset, "SPARSE_DATA", recursive = FALSE)
  testthat::expect_length(own, 1L)
  testthat::expect_identical(nrow(own[[1]]$value), 198812L)

  surf <- read_surface(path)
  testthat::expect_identical(nrow(surf$annotations$data_table), 198812L)
  testthat::expect_identical(length(surf$sparse_node_index), 198812L)
  testthat::expect_true(nrow(surf$annotations$label_table) > 0L)
  testthat::expect_true(all(grepl("^wm_lh_|^Unknown$",
                                  surf$annotations$label_table$Label)))

  cmap <- read_colormap(path)
  testthat::expect_true(nrow(cmap$colors$color_table) > 0L)
})
