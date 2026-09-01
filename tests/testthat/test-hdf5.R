require(testthat)

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
