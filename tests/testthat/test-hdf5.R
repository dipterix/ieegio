require(testthat)

# ---------------------------------------------------------------------------
# Shared fixtures for the backend coverage tests at the bottom of this file
# ---------------------------------------------------------------------------

# Backends selectable through `IEEGIO_USE_H5`. `readNSx` ships in `Imports` and
# is the resolved default; the others are optional.
h5_all_backends <- function() {
  c("hdf5r", "h5lite", "readNSx", "h5py")
}

h5_backend_ready <- function(backend) {
  if (identical(backend, "h5py")) {
    if (nzchar(Sys.getenv("IEEGIO_NO_PYTHON", unset = ""))) { return(FALSE) }
    old <- Sys.getenv("IEEGIO_USE_H5", unset = NA_character_)
    on.exit({
      if (is.na(old)) {
        Sys.unsetenv("IEEGIO_USE_H5")
      } else {
        Sys.setenv("IEEGIO_USE_H5" = old)
      }
    }, add = TRUE)
    Sys.setenv("IEEGIO_USE_H5" = "h5py")
    return(inherits(ensure_hdf5_backend(), "python.builtin.module"))
  }
  nzchar(system.file(package = backend))
}

# Switches the active backend and reports which one actually got resolved.
# Callers must restore `IEEGIO_USE_H5` through `on.exit()`.
h5_use_backend <- function(backend) {
  Sys.unsetenv("IEEGIO_USE_H5PY")
  Sys.setenv("IEEGIO_USE_H5" = backend)
  hdf5_backend_type(ensure_hdf5_backend())
}

# One sample per storage type the backends are expected to handle. Every string
# is ASCII, so `hdf5r` (which writes CSET_ASCII) and `readNSx` (CSET_UTF8) put
# the same bytes on disk and only the character-set label differs.
h5_type_samples <- function() {
  list(
    "integer"          = 1:24,
    "integer array"    = array(1:24, c(1, 2, 3, 1, 4, 1)),
    "integer NA"       = c(1L, NA, 3L),
    "double"           = as.numeric(1:24),
    "double special"   = c(1.5, NA, Inf, -Inf, NaN),
    "logical"          = c(TRUE, FALSE, NA),
    "character"        = c("a", "bb", "ccc"),
    "character matrix" = matrix(c("a", "bb", "ccc", "dddd", "e", "ff"), 2, 3),
    "character blank"  = c("", "a", ""),
    "character long"   = paste(rep("abcXYZ0123", 50), collapse = ""),
    "character NA"     = c("a", NA, "c"),
    "complex"          = complex(real = 1:3, imaginary = 4:6),
    "raw"              = as.raw(1:10),
    "empty double"     = numeric(0),
    "empty character"  = character(0)
  )
}

# `raw` needs a bit-field writer that only `h5lite` has, and a complex compound
# datatype is beyond `hdf5r`. Everything else every backend must store.
h5_can_store <- function(backend, name) {
  if (identical(name, "raw")) { return(identical(backend, "h5lite")) }
  if (identical(name, "complex")) { return(!identical(backend, "hdf5r")) }
  TRUE
}

# `hdf5r` writes `NA_character_` as the literal text "NA", so the value that
# comes back is not the value that went in.
h5_expected_value <- function(backend, name, value) {
  if (identical(name, "character NA") && identical(backend, "hdf5r")) {
    value[is.na(value)] <- "NA"
  }
  value
}

# `get_type()` reports the R storage mode wherever the backend can recover it.
# `hdf5r` is the exception: `H5T_ENUM` also inherits `H5T_INTEGER` there, so the
# FALSE/TRUE/NA enum is reported as an integer type.
h5_expected_type <- function(backend, value) {
  type <- storage.mode(value)
  if (identical(type, "logical") && identical(backend, "hdf5r")) {
    return("integer")
  }
  type
}

# Cross-backend divergences that are known and deliberate; anything not named
# here has to survive a write with one backend and a read with another.
#
#   * `h5lite` records the R type in an `R_type` attribute that no other backend
#     writes or reads, so `logical`, NA-carrying `integer`, `complex` and `raw`
#     only round trip when `h5lite` sits on both ends.
#   * `hdf5r` stores `NA_character_` as the text "NA" and reads a stored NULL
#     string back as "", so a missing string never survives it either way.
#   * `hdf5r` can neither read nor write a complex compound datatype.
#   * `h5py` reads the R backends' variable-length strings as bytes, and its
#     `bool` is a 2-member enum where the R backends write FALSE/TRUE/NA.
h5_cross_gap <- function(write_backend, read_backend, name) {
  pair <- c(write_backend, read_backend)
  mixed <- !identical(write_backend, read_backend)
  h5lite_reason <-
    "h5lite encodes R types in an `R_type` attribute no other backend writes"

  if (identical(name, "raw")) {
    if (!identical(pair, c("h5lite", "h5lite"))) {
      return("only h5lite can store `raw`; hdf5r and readNSx reject it")
    }
    return(NULL)
  }
  if (identical(name, "complex")) {
    if ("hdf5r" %in% pair) {
      return("hdf5r cannot read or write a complex compound datatype")
    }
    # h5py complex round trips crash reticulate's finalizer, so stay away
    if ("h5py" %in% pair) {
      return("h5py complex round trips are not exercised here")
    }
    if (identical(write_backend, "readNSx") && identical(read_backend, "h5lite")) {
      return(h5lite_reason)
    }
    return(NULL)
  }
  if (name %in% c("logical", "integer NA") && mixed && "h5lite" %in% pair) {
    return(h5lite_reason)
  }
  if (identical(name, "logical") && "h5py" %in% pair) {
    return("h5py bool is a 2-member enum; the R backends write FALSE/TRUE/NA")
  }
  if (identical(name, "character NA")) {
    if ("hdf5r" %in% pair) {
      return("hdf5r stores NA_character_ as the text NA and reads a NULL string as blank")
    }
    if ("h5py" %in% pair) {
      return("h5py does not preserve NA_character_")
    }
    return(NULL)
  }
  if (startsWith(name, "character") && identical(read_backend, "h5py")) {
    return("h5py reads variable-length strings written by the R backends as bytes")
  }
  NULL
}

test_that("HDF5 IO with R-hdf5r backend", {

  testthat::skip_if_not(nzchar(system.file(package = "hdf5r")))

  Sys.unsetenv("IEEGIO_USE_H5PY")
  Sys.setenv("IEEGIO_USE_H5" = "hdf5r")
  old_opt <- options("ieegio.debug.emscripten" = FALSE)
  on.exit({
    options(old_opt)
    Sys.unsetenv("IEEGIO_USE_H5")
  }, add = TRUE)

  h5backend <- ensure_hdf5_backend()
  expect_true(getNamespaceName(h5backend) == "hdf5r")

  x <- array(1:24, c(1, 2, 3, 1, 4, 1))

  f <- tempfile()
  on.exit({ unlink(f) }, add = TRUE)

  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")

  y <- io_read_h5(file = f, name = "data")
  expect_equal(
    dim(y),
    dim(x)
  )

  # `dim()` closes the dataset pointer but leaves the file pointer valid; the
  # second call must not fail on a stale `has_data`
  expect_equal(
    dim(y),
    dim(x)
  )

  expect_equal(
    dim(io_read_h5(file = f, name = "data", ram = TRUE)),
    dim(x)
  )
  expect_equal(
    dim(y[]),
    dim(x)
  )
  expect_equal(
    dim(y[drop = TRUE]),
    dim(drop(x))
  )

  env <- new.env()
  env$idx <- c(FALSE, TRUE, TRUE)
  expect_equal(
    with(env, {
      y[1, , idx, , , ]
    }),
    x[1, , c(2, 3), , , , drop = FALSE]
  )
  expect_equal(
    y[1, , 1, , 4, , drop = TRUE],
    x[1, , 1, , 4, , drop = TRUE]
  )

  x <- 1:24
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- array(1:24, c(1, 24))
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- array(1:24, c(24, 1))
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- numeric(0)
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

  # `subset()` must not leak a writable file connection
  o <- LazyH5$new(f, "data", read_only = FALSE)
  invisible(o$subset())
  ptr <- environment(o$subset)$private$file_ptr
  expect_true(is.null(ptr) || !ptr$is_valid)

})

test_that("HDF5 IO with R-h5lite backend", {

  testthat::skip_if_not(nzchar(system.file(package = "h5lite")))
  # the fixture below is written with hdf5r, to check cross-backend reads
  testthat::skip_if_not(nzchar(system.file(package = "hdf5r")))

  Sys.unsetenv("IEEGIO_USE_H5PY")
  Sys.setenv("IEEGIO_USE_H5" = "hdf5r")
  old_opt <- options("ieegio.debug.emscripten" = FALSE)
  on.exit({
    options(old_opt)
    Sys.unsetenv("IEEGIO_USE_H5")
  }, add = TRUE)

  h5backend <- ensure_hdf5_backend()
  expect_true(getNamespaceName(h5backend) == "hdf5r")


  x <- array(1:24, c(1, 2, 3, 1, 4, 1))

  f <- tempfile()
  on.exit({ unlink(f) }, add = TRUE)

  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")

  Sys.setenv("IEEGIO_USE_H5" = "h5lite")

  h5backend <- ensure_hdf5_backend()
  expect_true(getNamespaceName(h5backend) == "h5lite")

  y <- io_read_h5(file = f, name = "data")
  expect_equal(
    dim(y),
    dim(x)
  )

  expect_equal(
    dim(io_read_h5(file = f, name = "data", ram = TRUE)),
    dim(x)
  )
  expect_equal(
    dim(y[]),
    dim(x)
  )
  expect_equal(
    dim(y[drop = TRUE]),
    dim(drop(x))
  )

  env <- new.env()
  env$idx <- c(FALSE, TRUE, TRUE)
  expect_equal(
    with(env, {
      y[1, , idx, , , ]
    }),
    x[1, , c(2, 3), , , , drop = FALSE]
  )
  expect_equal(
    y[1, , 1, , 4, , drop = TRUE],
    x[1, , 1, , 4, , drop = TRUE]
  )

  x <- 1:24
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- array(1:24, c(1, 24))
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- array(1:24, c(24, 1))
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- numeric(0)
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

})

test_that("HDF5 IO with R-readNSx backend", {

  # the fixture below is written with hdf5r, to check cross-backend reads
  testthat::skip_if_not(nzchar(system.file(package = "hdf5r")))

  Sys.unsetenv("IEEGIO_USE_H5PY")
  Sys.setenv("IEEGIO_USE_H5" = "hdf5r")
  old_opt <- options("ieegio.debug.emscripten" = FALSE)
  on.exit({
    options(old_opt)
    Sys.unsetenv("IEEGIO_USE_H5")
  }, add = TRUE)

  h5backend <- ensure_hdf5_backend()
  expect_true(getNamespaceName(h5backend) == "hdf5r")


  x <- array(1:24, c(1, 2, 3, 1, 4, 1))

  f <- tempfile()
  on.exit({ unlink(f) }, add = TRUE)

  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")

  Sys.setenv("IEEGIO_USE_H5" = "readNSx")

  h5backend <- ensure_hdf5_backend()
  expect_true(getNamespaceName(h5backend) == "readNSx")

  y <- io_read_h5(file = f, name = "data")
  expect_equal(
    dim(y),
    dim(x)
  )

  expect_equal(
    dim(io_read_h5(file = f, name = "data", ram = TRUE)),
    dim(x)
  )
  expect_equal(
    dim(y[]),
    dim(x)
  )
  expect_equal(
    dim(y[drop = TRUE]),
    dim(drop(x))
  )

  env <- new.env()
  env$idx <- c(FALSE, TRUE, TRUE)
  expect_equal(
    with(env, {
      y[1, , idx, , , ]
    }),
    x[1, , c(2, 3), , , , drop = FALSE]
  )
  expect_equal(
    y[1, , 1, , 4, , drop = TRUE],
    x[1, , 1, , 4, , drop = TRUE]
  )

  x <- 1:24
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- array(1:24, c(1, 24))
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- array(1:24, c(24, 1))
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- numeric(0)
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

})


test_that("h5lite backend write path", {

  testthat::skip_if_not(nzchar(system.file(package = "h5lite")))

  Sys.unsetenv("IEEGIO_USE_H5PY")
  Sys.setenv("IEEGIO_USE_H5" = "h5lite")
  old_opt <- options("ieegio.debug.emscripten" = FALSE)
  f <- tempfile()
  on.exit({
    options(old_opt)
    Sys.unsetenv("IEEGIO_USE_H5")
    unlink(f)
  }, add = TRUE)

  expect_true(getNamespaceName(ensure_hdf5_backend()) == "h5lite")

  # written *and* read by h5lite: value, storage mode, and reported type
  # must all survive the round trip
  samples <- list(
    "integer"   = list(array(1:24, c(1, 2, 3, 1, 4, 1)), "integer"),
    "double"    = list(as.numeric(1:24), "double"),
    "logical"   = list(c(TRUE, FALSE, NA), "logical"),
    "character" = list(c("a", "bb", "ccc"), "character"),
    "empty"     = list(numeric(0), "double"),
    "specials"  = list(c(1.5, NA, Inf, -Inf, NaN), "double")
  )

  for (nm in names(samples)) {
    value <- samples[[nm]][[1]]
    expected_type <- samples[[nm]][[2]]

    io_write_h5(value, file = f, name = "data", quiet = TRUE, new_file = TRUE)
    y <- io_read_h5(file = f, name = "data")

    expect_equal(y[], value, info = nm)
    expect_equal(storage.mode(y[]), storage.mode(value), info = nm)
    expect_equal(y$get_type(), expected_type, info = nm)
    expect_equal(dim(y), dim(value), info = nm)
  }

  # `level` must reach h5lite's `compress`
  big <- as.numeric(rep(1:10, 1e4))
  f0 <- tempfile()
  f9 <- tempfile()
  on.exit({ unlink(c(f0, f9)) }, add = TRUE)
  io_write_h5(big, file = f0, name = "d", quiet = TRUE, level = 0)
  io_write_h5(big, file = f9, name = "d", quiet = TRUE, level = 9)
  expect_gt(file.size(f0), file.size(f9))

  # dataset names round-trip through nested groups
  io_write_h5(1:5, file = f, name = "g1/g2/d", quiet = TRUE, new_file = TRUE)
  expect_true("g1/g2/d" %in% io_h5_names(f))

})


test_that("h5lite backend validity checks on write-protected files", {

  testthat::skip_if_not(nzchar(system.file(package = "h5lite")))
  testthat::skip_on_os("windows")

  Sys.unsetenv("IEEGIO_USE_H5PY")
  Sys.setenv("IEEGIO_USE_H5" = "h5lite")
  old_opt <- options("ieegio.debug.emscripten" = FALSE)
  f <- tempfile()
  on.exit({
    options(old_opt)
    Sys.unsetenv("IEEGIO_USE_H5")
    Sys.chmod(f, "0644")
    unlink(f)
  }, add = TRUE)

  x <- array(1:24, c(2, 3, 4))
  io_write_h5(x, file = f, name = "data", quiet = TRUE)

  Sys.chmod(f, "0444")

  # `h5_open()` creates the root group, i.e. it writes, so a write-protected
  # file cannot be opened through an h5lite handle; the validity check must
  # report that rather than claiming the file is writable
  expect_true(io_h5_valid(f, "r"))
  expect_false(io_h5_valid(f, "w"))

  Sys.chmod(f, "0644")
  expect_true(io_h5_valid(f, "w"))

  # opening read-only must never create a file
  missing_file <- file.path(tempdir(), "ieegio-must-not-appear.h5")
  unlink(missing_file)
  expect_error(io_read_h5(file = missing_file, name = "data", quiet = TRUE))
  expect_false(file.exists(missing_file))

})


test_that("filearray fallback backend", {

  Sys.unsetenv("IEEGIO_USE_H5PY")
  Sys.unsetenv("IEEGIO_USE_H5")
  old_opt <- options("ieegio.debug.emscripten" = TRUE)
  f <- tempfile()
  on.exit({
    options(old_opt)
    unlink(f, recursive = TRUE)
    unlink(sprintf("%s.farr", f), recursive = TRUE)
  }, add = TRUE)

  expect_null(ensure_hdf5_backend())

  x <- array(1:24, c(2, 3, 4))
  io_write_h5(x, file = f, name = "data", quiet = TRUE)

  # data lives in `<file>.farr/`, the plain path never exists - validity and
  # listing must not depend on it
  expect_false(file.exists(f))
  expect_true(io_h5_valid(f, "r"))
  expect_true("data" %in% io_h5_names(f))
  expect_equal(io_read_h5(file = f, name = "data", ram = TRUE), x)

})


test_that("HDF5 IO with Python backend", {

  testthat::skip_on_cran()
  testthat::skip_on_bioc()
  testthat::skip_if(nzchar(Sys.getenv("IEEGIO_NO_PYTHON", unset = "")))

  Sys.setenv("IEEGIO_USE_H5" = "h5py")
  old_opt <- options("ieegio.debug.emscripten" = FALSE)
  on.exit({
    Sys.unsetenv("IEEGIO_USE_H5")
    Sys.unsetenv("IEEGIO_USE_H5PY")
    options(old_opt)
  }, add = TRUE)

  h5py <- ensure_hdf5_backend()

  # Skip if h5py is null (no python)
  testthat::skip_if(is.null(h5py))

  testthat::expect_true(inherits(h5py, "python.builtin.module"))

  x <- array(1:24, c(1, 2, 3, 1, 4, 1))

  f <- tempfile()
  on.exit({ unlink(f) }, add = TRUE)

  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")

  y <- io_read_h5(file = f, name = "data")
  expect_equal(
    dim(y),
    dim(x)
  )

  expect_equal(
    dim(io_read_h5(file = f, name = "data", ram = TRUE)),
    dim(x)
  )
  expect_equal(
    dim(y[]),
    dim(x)
  )
  expect_equal(
    dim(y[drop = TRUE]),
    dim(drop(x))
  )

  env <- new.env()
  env$idx <- c(FALSE, TRUE, TRUE)
  expect_equal(
    with(env, {
      y[1, , idx, , , ]
    }),
    x[1, , c(2, 3), , , , drop = FALSE]
  )
  expect_equal(
    y[1, , 1, , 4, , drop = TRUE],
    x[1, , 1, , 4, , drop = TRUE]
  )

  x <- 1:24
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- array(1:24, c(1, 24))
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- array(1:24, c(24, 1))
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- numeric(0)
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

})

test_that("HDF5 IO with no backend", {

  testthat::skip_on_cran()
  testthat::skip_on_bioc()

  old_opt <- options("ieegio.debug.emscripten" = TRUE)
  f <- tempfile()

  on.exit({
    options(old_opt)
    unlink(f)
  }, add = TRUE)

  h5backend <- ensure_hdf5_backend()

  # Skip if h5py is null (no python)
  testthat::expect_true(is.null(h5backend))

  x <- array(1:24, c(1, 2, 3, 1, 4, 1))

  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")

  y <- io_read_h5(file = f, name = "data")
  expect_equal(
    dim(y),
    dim(x)
  )

  expect_equal(
    dim(io_read_h5(file = f, name = "data", ram = TRUE)),
    dim(x)
  )
  expect_equal(
    dim(y[]),
    dim(x)
  )
  expect_equal(
    dim(y[drop = TRUE]),
    dim(drop(x))
  )

  env <- new.env()
  env$idx <- c(FALSE, TRUE, TRUE)
  expect_equal(
    with(env, {
      y[1, , idx, , , ]
    }),
    x[1, , c(2, 3), , , , drop = FALSE]
  )
  expect_equal(
    y[1, , 1, , 4, , drop = TRUE],
    x[1, , 1, , 4, , drop = TRUE]
  )

  x <- 1:24
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- array(1:24, c(1, 24))
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- array(1:24, c(24, 1))
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- numeric(0)
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_length(x, 0)
  expect_length(y[], 0)
  expect_true(is.numeric(y[]))


  x <- character(0)
  io_write_h5(x, file = f, name = "data/str", quiet = TRUE, ctype = "character")
  y <- io_read_h5(file = f, name = "data/str")

  expect_length(x, 0)
  expect_length(y[], 0)
  expect_true(is.character(y[]))


  x <- "2312313asdkahdbq"
  io_write_h5(x, file = f, name = "data/str", quiet = TRUE, ctype = "character")
  y <- io_read_h5(file = f, name = "data/str")

  expect_length(x, 1)
  expect_length(y[], 1)
  expect_equal(y[], x)


})


# ---------------------------------------------------------------------------
# Type coverage: every backend, every storage type, written and read by itself
# ---------------------------------------------------------------------------

for (h5_backend in c("hdf5r", "h5lite", "readNSx")) {

  test_that(sprintf("HDF5 type coverage with the %s backend", h5_backend), {

    testthat::skip_if_not(h5_backend_ready(h5_backend))

    Sys.unsetenv("IEEGIO_USE_H5PY")
    old_opt <- options("ieegio.debug.emscripten" = FALSE)
    f <- tempfile()
    on.exit({
      options(old_opt)
      Sys.unsetenv("IEEGIO_USE_H5")
      unlink(f)
    }, add = TRUE)

    expect_equal(h5_use_backend(h5_backend), h5_backend)

    samples <- h5_type_samples()
    for (nm in names(samples)) {
      value <- samples[[nm]]

      if (!h5_can_store(h5_backend, nm)) {
        # The backend has no datatype for this; the failure must be loud, and
        # it may come from either side of the round trip
        expect_error(
          {
            io_write_h5(value, file = f, name = "data", quiet = TRUE, new_file = TRUE)
            io_read_h5(file = f, name = "data", ram = TRUE)
          },
          info = nm
        )
        next
      }

      io_write_h5(value, file = f, name = "data", quiet = TRUE, new_file = TRUE)
      y <- io_read_h5(file = f, name = "data")

      expected <- h5_expected_value(h5_backend, nm, value)
      expect_equal(y[], expected, info = nm)
      expect_equal(storage.mode(y[]), storage.mode(expected), info = nm)
      expect_equal(dim(y), dim(expected), info = nm)
      expect_equal(y$get_type(), h5_expected_type(h5_backend, expected), info = nm)
    }

  })

}


# ---------------------------------------------------------------------------
# Cross-backend IO in both directions. The existing tests above only ever write
# with `hdf5r`, so the write path of every other backend went untested.
# ---------------------------------------------------------------------------

for (h5_writer in h5_all_backends()) {
  for (h5_reader in h5_all_backends()) {

    test_that(sprintf("HDF5 cross-backend IO: write %s, read %s", h5_writer, h5_reader), {

      testthat::skip_if_not(h5_backend_ready(h5_writer))
      testthat::skip_if_not(h5_backend_ready(h5_reader))

      Sys.unsetenv("IEEGIO_USE_H5PY")
      old_opt <- options("ieegio.debug.emscripten" = FALSE)
      f <- tempfile()
      on.exit({
        options(old_opt)
        Sys.unsetenv("IEEGIO_USE_H5")
        unlink(f)
      }, add = TRUE)

      samples <- h5_type_samples()
      for (nm in names(samples)) {
        value <- samples[[nm]]

        reason <- h5_cross_gap(h5_writer, h5_reader, nm)
        if (length(reason)) {
          testthat::succeed(sprintf("%s: %s", nm, reason))
          next
        }

        expect_equal(h5_use_backend(h5_writer), h5_writer, info = nm)
        io_write_h5(value, file = f, name = "g1/g2/d", quiet = TRUE, new_file = TRUE)

        expect_equal(h5_use_backend(h5_reader), h5_reader, info = nm)
        y <- io_read_h5(file = f, name = "g1/g2/d")

        expect_equal(y[], value, info = nm)
        expect_equal(storage.mode(y[]), storage.mode(value), info = nm)
        expect_equal(dim(y), dim(value), info = nm)

        # nested groups have to survive the hand-off too
        expect_true("g1/g2/d" %in% io_h5_names(f), info = nm)
      }

    })

  }
}


# ---------------------------------------------------------------------------
# readNSx write path
# ---------------------------------------------------------------------------

test_that("readNSx backend write path", {

  Sys.unsetenv("IEEGIO_USE_H5PY")
  old_opt <- options("ieegio.debug.emscripten" = FALSE)
  f <- tempfile()
  on.exit({
    options(old_opt)
    Sys.unsetenv("IEEGIO_USE_H5")
    unlink(f)
  }, add = TRUE)

  expect_equal(h5_use_backend("readNSx"), "readNSx")

  # `open(new_dataset = TRUE)` must register the name even without data, the
  # same contract the `hdf5r` and `h5lite` branches follow
  o <- LazyH5$new(f, "g/d", read_only = FALSE, quiet = TRUE)
  o$open(new_dataset = TRUE)
  expect_true("g/d" %in% io_h5_names(f))
  unlink(f)

  # so must a plain writable `open()` on a file that has no such dataset yet
  o <- LazyH5$new(f, "g/d", read_only = FALSE, quiet = TRUE)
  o$open()
  expect_true("g/d" %in% io_h5_names(f))
  unlink(f)

  # `ctype` is resolved against `robj`, not against a variable that never
  # existed in `open()`
  o <- LazyH5$new(f, "d", read_only = FALSE, quiet = TRUE)
  o$open(new_dataset = TRUE, robj = 1:5, ctype = "numeric")
  expect_equal(io_read_h5(file = f, name = "d", ram = TRUE), as.numeric(1:5))
  unlink(f)

  # a rank-1 dataset reads back as a plain vector, as it does under `hdf5r`
  io_write_h5(1:24, file = f, name = "d", quiet = TRUE, new_file = TRUE)
  y <- io_read_h5(file = f, name = "d")
  expect_null(dim(y[3:5]))
  expect_equal(y[3:5], 3:5)
  expect_equal(y$get_type(), "integer")

  # `level` must reach `h5_native_write`; `save()` forwards it as `gzip_level`
  big <- as.numeric(rep(1:10, 1e4))
  f0 <- tempfile()
  f9 <- tempfile()
  on.exit({ unlink(c(f0, f9)) }, add = TRUE)
  io_write_h5(big, file = f0, name = "d", quiet = TRUE, level = 0)
  io_write_h5(big, file = f9, name = "d", quiet = TRUE, level = 9)
  expect_gt(file.size(f0), file.size(f9))

  # per-dimension and scalar `chunk` are both accepted; a scalar is recycled
  # across dimensions, which `hdf5r` rejects outright
  m <- matrix(as.numeric(1:20000), nrow = 20)
  io_write_h5(m, file = f, name = "d", quiet = TRUE, new_file = TRUE, chunk = c(20, 1024))
  expect_equal(io_read_h5(file = f, name = "d", ram = TRUE), m)
  io_write_h5(m, file = f, name = "d", quiet = TRUE, new_file = TRUE, chunk = 1024)
  expect_equal(io_read_h5(file = f, name = "d", ram = TRUE), m)

  # writing a second dataset must not drop the first
  io_write_h5(1:5, file = f, name = "keepme", quiet = TRUE, new_file = TRUE)
  io_write_h5(6:10, file = f, name = "g1/g2/d", quiet = TRUE)
  expect_setequal(io_h5_names(f), c("g1/g2/d", "keepme"))
  expect_equal(io_read_h5(file = f, name = "keepme", ram = TRUE), 1:5)

})


test_that("readNSx backend out-of-bound indices are padded with NA", {

  # RAVE epochs an electrode with `tp <- round(onset * srate) + tidx`, where
  # `tidx` spans a pre-stimulus window. Nothing clamps `tp`, so a trial near a
  # block boundary asks for samples that do not exist, and the result must keep
  # the requested shape so that `dim(voltage) <- dim(tp)` still lines up.
  Sys.unsetenv("IEEGIO_USE_H5PY")
  old_opt <- options("ieegio.debug.emscripten" = FALSE)
  f <- tempfile()
  on.exit({
    options(old_opt)
    Sys.unsetenv("IEEGIO_USE_H5")
    unlink(f)
  }, add = TRUE)

  expect_equal(h5_use_backend("readNSx"), "readNSx")

  v <- as.numeric(1:100)
  io_write_h5(v, file = f, name = "raw/b1", quiet = TRUE, new_file = TRUE)
  y <- io_read_h5(file = f, name = "raw/b1")

  # two trials sitting on the block edges
  tp <- matrix(c(-2:2, 98:102), ncol = 2)
  out <- y[tp]
  expect_length(out, length(tp))
  expect_equal(sum(is.na(out)), 5L)
  expect_equal(out[!is.na(out)], v[tp[tp >= 1 & tp <= 100]])

  # in-range requests keep matching base R, and every backend agrees on them
  tp_ok <- matrix(c(10:14, 50:54), ncol = 2)
  expect_equal(as.vector(y[tp_ok]), v[tp_ok])

  # the 2-D form used for `/raw/power/<block>` behaves the same way
  m <- matrix(as.numeric(1:200), nrow = 10)
  io_write_h5(m, file = f, name = "raw/power/b1", quiet = TRUE)
  y2 <- io_read_h5(file = f, name = "raw/power/b1")
  expect_equal(y2[, 3:6], m[, 3:6, drop = FALSE])

  tp2 <- matrix(c(-1:2, 17:20), ncol = 2)
  out2 <- y2[, tp2]
  expect_equal(dim(out2), c(nrow(m), length(tp2)))
  expect_equal(sum(is.na(out2)), 2L * nrow(m))

})


for (h5_backend in c("hdf5r", "h5lite", "readNSx")) {

  test_that(sprintf("io_h5_valid does not destroy data with the %s backend", h5_backend), {

    testthat::skip_if_not(h5_backend_ready(h5_backend))

    Sys.unsetenv("IEEGIO_USE_H5PY")
    old_opt <- options("ieegio.debug.emscripten" = FALSE)
    f <- tempfile()
    plain <- tempfile()
    on.exit({
      options(old_opt)
      Sys.unsetenv("IEEGIO_USE_H5")
      unlink(c(f, plain))
    }, add = TRUE)

    expect_equal(h5_use_backend(h5_backend), h5_backend)

    # asking whether a file is writable must never write to it - `hdf5r`'s "w"
    # is `H5F_ACC_TRUNC`, so it has to be mapped to "r+"
    io_write_h5(1:5, file = f, name = "keepme", quiet = TRUE)
    expect_true(io_h5_valid(f, "w"))
    expect_true("keepme" %in% io_h5_names(f))
    expect_equal(io_read_h5(file = f, name = "keepme", ram = TRUE), 1:5)

    # the same goes for a file that is not HDF5 at all
    writeLines("hello world", plain)
    before <- readBin(plain, "raw", n = 1024)
    expect_false(io_h5_valid(plain, "w"))
    expect_equal(readBin(plain, "raw", n = 1024), before)

  })

}


test_that("readNSx backend validity checks on write-protected files", {

  testthat::skip_on_os("windows")

  Sys.unsetenv("IEEGIO_USE_H5PY")
  old_opt <- options("ieegio.debug.emscripten" = FALSE)
  f <- tempfile()
  on.exit({
    options(old_opt)
    Sys.unsetenv("IEEGIO_USE_H5")
    Sys.chmod(f, "0644")
    unlink(f)
  }, add = TRUE)

  expect_equal(h5_use_backend("readNSx"), "readNSx")

  x <- array(1:24, c(2, 3, 4))
  io_write_h5(x, file = f, name = "data", quiet = TRUE)

  Sys.chmod(f, "0444")
  expect_true(io_h5_valid(f, "r"))
  expect_false(io_h5_valid(f, "w"))

  Sys.chmod(f, "0644")
  expect_true(io_h5_valid(f, "w"))

  # opening read-only must never create a file
  missing_file <- file.path(tempdir(), "ieegio-readnsx-must-not-appear.h5")
  unlink(missing_file)
  expect_error(io_read_h5(file = missing_file, name = "data", quiet = TRUE))
  expect_false(file.exists(missing_file))

})


test_that("io_h5_names is sorted and backend independent", {

  Sys.unsetenv("IEEGIO_USE_H5PY")
  old_opt <- options("ieegio.debug.emscripten" = FALSE)
  f <- tempfile()
  on.exit({
    options(old_opt)
    Sys.unsetenv("IEEGIO_USE_H5")
    unlink(f)
  }, add = TRUE)

  # `hdf5r` and `readNSx` list datasets in name order while `h5lite` lists them
  # in insertion order, so the names are sorted before they are returned
  expected <- c("notch/b1", "raw/b2", "reference")

  for (backend in c("hdf5r", "h5lite", "readNSx")) {
    if (!h5_backend_ready(backend)) { next }

    unlink(f)
    expect_equal(h5_use_backend(backend), backend)

    io_write_h5(1:3, file = f, name = "/raw/b2", quiet = TRUE)
    io_write_h5(1:3, file = f, name = "notch/b1", quiet = TRUE)
    io_write_h5("x", file = f, name = "reference", quiet = TRUE, ctype = "character")

    expect_equal(io_h5_names(f), expected, info = backend)
  }

})
