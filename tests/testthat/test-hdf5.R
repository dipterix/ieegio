require(testthat)
test_that("HDF5 IO with R backend", {

  Sys.unsetenv("IEEGIO_USE_H5PY")

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


test_that("HDF5 IO with R backend", {

  testthat::skip_on_cran()
  testthat::skip_on_bioc()

  Sys.setenv("IEEGIO_USE_H5PY" = "TRUE")
  on.exit({
    Sys.unsetenv("IEEGIO_USE_H5PY")
  })

  h5py <- ensure_hdf5_backend()

  # Skip if h5py is null (no python)
  testthat::skip_if(is.null(h5py))

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
