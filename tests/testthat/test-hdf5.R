require(testthat)
test_that("HDF5 IO with R backend", {

  Sys.unsetenv("IEEGIO_USE_H5PY")
  old_opt <- options("ieegio.debug.emscripten" = FALSE)
  on.exit({ options(old_opt) }, add = TRUE)

  x <- array(1:24, c(1,2,3,1,4,1))

  f <- tempfile()
  on.exit({ unlink(f) })

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
      y[1,,idx,,,]
    }),
    x[1,,c(2,3),,,,drop=FALSE]
  )
  expect_equal(
    y[1,,1,,4,,drop=TRUE],
    x[1,,1,,4,,drop=TRUE]
  )

  x <- 1:24
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- array(1:24, c(1,24))
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- array(1:24, c(24,1))
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- numeric(0)
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

})


test_that("HDF5 IO with Python backend", {

  testthat::skip_on_cran()
  testthat::skip_on_bioc()
  testthat::skip_if(nzchar(Sys.getenv("IEEGIO_NO_PYTHON", unset = "")))

  Sys.setenv("IEEGIO_USE_H5PY" = "TRUE")
  old_opt <- options("ieegio.debug.emscripten" = FALSE)
  on.exit({
    Sys.unsetenv("IEEGIO_USE_H5PY")
    options(old_opt)
  }, add = TRUE)

  h5py <- ensure_hdf5_backend()

  # Skip if h5py is null (no python)
  testthat::skip_if(is.null(h5py))

  testthat::expect_true(inherits(h5py, "python.builtin.module"))

  x <- array(1:24, c(1,2,3,1,4,1))

  f <- tempfile()
  on.exit({ unlink(f) })

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
      y[1,,idx,,,]
    }),
    x[1,,c(2,3),,,,drop=FALSE]
  )
  expect_equal(
    y[1,,1,,4,,drop=TRUE],
    x[1,,1,,4,,drop=TRUE]
  )

  x <- 1:24
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- array(1:24, c(1,24))
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- array(1:24, c(24,1))
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

  x <- array(1:24, c(1,2,3,1,4,1))

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
      y[1,,idx,,,]
    }),
    x[1,,c(2,3),,,,drop=FALSE]
  )
  expect_equal(
    y[1,,1,,4,,drop=TRUE],
    x[1,,1,,4,,drop=TRUE]
  )

  x <- 1:24
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- array(1:24, c(1,24))
  io_write_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- io_read_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- array(1:24, c(24,1))
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
