library(testthat)

test_that("ieegio_colortable construction stores colors as-is, sorted by Key", {
  ct <- new_colortable(data.frame(
    Key = c(3L, 1L, 2L), R = c(0L, 255L, 0L),
    G = c(0L, 0L, 255L), B = c(255L, 0L, 0L), A = c(255L, 255L, 255L)
  ))
  expect_s3_class(ct, "ieegio_colortable")
  expect_equal(ct$color_table$Key, c(1L, 2L, 3L))
  # Key=0 is NOT inserted by the constructor
  expect_false(0L %in% ct$color_table$Key)
})

test_that("ieegio_colortable: Key=0 in input is preserved as-is by constructor", {
  ct <- new_colortable(data.frame(
    Key = c(0L, 1L), R = c(200L, 100L),
    G = c(100L, 50L), B = c(50L, 25L), A = c(255L, 255L)
  ))
  k0 <- ct$color_table[ct$color_table$Key == 0L, ]
  # Constructor does not override RGB for Key=0
  expect_equal(k0$R, 200L)
  expect_equal(k0$G, 100L)
  expect_equal(k0$B, 50L)
})

test_that("ieegio_colortable: all-zero alpha promoted to opaque (FreeSurfer convention)", {
  ct <- new_colortable(data.frame(
    Key = c(1L, 2L, 3L), R = c(255L, 0L, 128L),
    G = c(0L, 200L, 64L), B = c(0L, 100L, 32L), A = c(0L, 0L, 0L)
  ))
  expect_true(all(ct$color_table$A == 255L))
})

test_that("ieegio_lookup_discrete: Key=0 always Unknown", {
  lut <- new_lookup_discrete(data.frame(
    Key = c(1L, 2L), Label = c("Region A", "Region B")
  ))
  expect_s3_class(lut, "ieegio_lookup_discrete")
  expect_true(0L %in% lut$lookup_table$Key)
  expect_equal(lut$lookup_table[lut$lookup_table$Key == 0L, "Label"][[1L]], "Unknown")
})

test_that("ieegio_lookup_discrete: Key=0 label overridden to Unknown", {
  lut <- new_lookup_discrete(data.frame(
    Key = c(0L, 1L), Label = c("Background", "Region A")
  ))
  expect_equal(lut$lookup_table[lut$lookup_table$Key == 0L, "Label"][[1L]], "Unknown")
})

test_that("ieegio_lookup_continuous constructor", {
  lut <- new_lookup_continuous(data.frame(
    Value = c(0, 1), Scaled = c(0, 1)
  ))
  expect_s3_class(lut, "ieegio_lookup_continuous")
  expect_equal(lut$type, "continuous")
})

test_that("new_colormap discrete vs continuous dispatch", {
  ct <- new_colortable(data.frame(
    Key = c(0L, 1L, 2L), R = c(0L, 255L, 0L),
    G = c(0L, 0L, 255L), B = c(0L, 0L, 0L), A = c(0L, 255L, 255L)
  ))
  cm_disc <- new_colormap(ct)
  expect_s3_class(cm_disc, "ieegio_colormap_discrete")
  expect_s3_class(cm_disc, "ieegio_colormap")

  lut_cont <- new_lookup_continuous(data.frame(
    Value = c(0, 1), Scaled = c(0, 1)
  ))
  cm_cont <- new_colormap(ct, lookup = lut_cont)
  expect_s3_class(cm_cont, "ieegio_colormap_continuous")
})

test_that("calculate_color discrete: Key=0 forced black regardless of stored color", {
  # Even though Key=0 has R=200 in the colortable, discrete mode overrides it to black
  ct <- new_colortable(data.frame(
    Key = c(0L, 1L, 2L), R = c(200L, 255L, 0L),
    G = c(100L, 0L, 255L), B = c(50L, 0L, 0L), A = c(255L, 255L, 255L)
  ))
  cm <- new_colormap(ct)
  cols <- calculate_color(0L, cm)
  rgb_vals <- grDevices::col2rgb(cols, alpha = FALSE)
  expect_equal(as.integer(rgb_vals["red", 1L]), 0L)
  expect_equal(as.integer(rgb_vals["green", 1L]), 0L)
  expect_equal(as.integer(rgb_vals["blue", 1L]), 0L)
})

test_that("calculate_color discrete: missing key interpolated", {
  ct <- new_colortable(data.frame(
    Key = c(0L, 1L, 3L), R = c(0L, 255L, 0L),
    G = c(0L, 0L, 255L), B = c(0L, 0L, 0L), A = c(0L, 255L, 255L)
  ))
  cm <- new_colormap(ct)
  # Key 2 is midpoint of Key 1 (255,0,0) and Key 3 (0,255,0)
  cols <- calculate_color(2L, cm)
  expect_match(cols, "^#[0-9A-Fa-f]{6,8}$")
  rgb2 <- grDevices::col2rgb(cols)
  expect_equal(as.integer(rgb2["red", 1L]), 128L, tolerance = 1)
  expect_equal(as.integer(rgb2["green", 1L]), 128L, tolerance = 1)
})

test_that("calculate_color continuous: linear scaling", {
  ct <- new_colortable(data.frame(
    Key = c(0L, 100L), R = c(0L, 255L),
    G = c(0L, 0L), B = c(0L, 0L), A = c(255L, 255L)
  ))
  lut <- new_lookup_continuous(data.frame(
    Value = c(0, 1), Scaled = c(0, 1)
  ))
  cm <- new_colormap(ct, lookup = lut)
  cols <- calculate_color(c(0, 0.5, 1), cm, type = "continuous")
  expect_length(cols, 3L)
  expect_match(cols[1L], "^#[0-9A-Fa-f]{6}")
  rgb_mid <- grDevices::col2rgb(cols[2L])
  expect_equal(as.integer(rgb_mid["red", 1L]), 128L, tolerance = 1)
})

test_that("calculate_color: NA inputs filled with na= color by default", {
  ct <- new_colortable(data.frame(
    Key = c(0L, 1L), R = c(0L, 255L),
    G = c(0L, 0L), B = c(0L, 0L), A = c(255L, 255L)
  ))
  cm <- new_colormap(ct)
  cols <- calculate_color(c(1L, NA_integer_, 0L), cm)
  expect_false(is.na(cols[2L]))
  expect_equal(cols[2L], "#00000000")

  # na = NA preserves NA outputs
  cols_na <- calculate_color(c(1L, NA_integer_, 0L), cm, na = NA)
  expect_true(is.na(cols_na[2L]))
  expect_false(is.na(cols_na[1L]))

  # na with keep_alpha=FALSE strips alpha from the fill color too
  cols_no_alpha <- calculate_color(c(1L, NA_integer_), cm, keep_alpha = FALSE)
  expect_equal(cols_no_alpha[2L], "#000000")
})

test_that("data_range shorthand: length-1 becomes symmetric", {
  ct <- new_colortable(data.frame(
    Key = c(0L, 100L), R = c(0L, 255L),
    G = c(0L, 0L), B = c(0L, 0L), A = c(255L, 255L)
  ))
  lut <- new_lookup_continuous(data.frame(
    Value = c(0, 1), Scaled = c(0, 1)
  ))
  cm <- new_colormap(ct, lookup = lut, data_range = 5)
  expect_equal(cm$data_range, c(-5, 5))
})

test_that("colorspace_from_rgb / colorspace_to_rgb round-trip", {
  rgb_in <- matrix(c(255L, 0L, 0L,
                     0L, 255L, 0L,
                     0L, 0L, 255L), ncol = 3L, byrow = TRUE)
  for (cs in c("sRGB", "HSV", "HCL", "Lab")) {
    cs_mat <- colorspace_from_rgb(rgb_in, to = cs)
    rgb_out <- colorspace_to_rgb(cs_mat, from = cs)
    expect_equal(as.integer(round(rgb_out)), as.integer(round(rgb_in)),
                 tolerance = 2, label = paste("round-trip", cs))
  }
})

test_that("as_ieegio_colormap.data.frame from label table", {
  lt <- data.frame(
    Key   = c(0L, 1L, 2L),
    Label = c("Unknown", "Region A", "Region B"),
    R     = c(0L, 255L, 0L),
    G     = c(0L, 0L, 200L),
    B     = c(0L, 50L, 100L),
    A     = c(255L, 255L, 255L)
  )
  cm <- as_ieegio_colormap(lt)
  expect_s3_class(cm, "ieegio_colormap_discrete")
  expect_s3_class(cm$lookup, "ieegio_lookup_discrete")
  expect_equal(nrow(cm$lookup$lookup_table), 3L)
})

test_that("write_colormap and read_colormap threebrain round-trip", {
  ct <- new_colortable(data.frame(
    Key = c(0L, 1L, 2L), R = c(0L, 200L, 50L),
    G = c(0L, 100L, 180L), B = c(0L, 30L, 90L), A = c(0L, 255L, 255L)
  ))
  lut <- new_lookup_discrete(data.frame(
    Key = c(0L, 1L, 2L), Label = c("Unknown", "Alpha", "Beta")
  ))
  cm <- new_colormap(ct, lookup = lut)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  write_colormap(cm, tmp)
  expect_true(file.exists(tmp))

  cm2 <- read_colormap(tmp)
  expect_s3_class(cm2, "ieegio_colormap_discrete")
  expect_equal(nrow(cm2$colors$color_table), nrow(ct$color_table))
  expect_equal(cm2$lookup$lookup_table$Label,
               lut$lookup_table$Label)
})

test_that("calculate_color keep_alpha=FALSE always returns #RRGGBB", {
  ct <- new_colortable(data.frame(
    Key = c(0L, 1L), R = c(0L, 255L),
    G = c(0L, 0L), B = c(0L, 0L), A = c(128L, 255L)
  ))
  cm <- new_colormap(ct)
  cols_with    <- calculate_color(c(0L, 1L), cm, keep_alpha = TRUE)
  cols_without <- calculate_color(c(0L, 1L), cm, keep_alpha = FALSE)
  # With alpha: Key=0 has A=128 so should be 9-char #RRGGBBAA
  expect_equal(nchar(cols_with[1L]), 9L)
  # Without alpha: always 7-char #RRGGBB
  expect_true(all(nchar(na.omit(cols_without)) == 7L))
  # NA stays NA when na = NA
  cols_na <- calculate_color(NA_integer_, cm, keep_alpha = FALSE, na = NA)
  expect_true(is.na(cols_na))
})

test_that("print and format methods do not error", {
  ct <- new_colortable(data.frame(
    Key = 0L:5L, R = 0L:5L * 50L, G = rep(0L, 6L),
    B = rep(0L, 6L), A = rep(255L, 6L)
  ))
  lut <- new_lookup_discrete(data.frame(
    Key = 0L:5L, Label = paste0("L", 0L:5L)
  ))
  cm <- new_colormap(ct, lookup = lut)
  expect_output(print(ct))
  expect_output(print(lut))
  expect_output(print(cm))
})
