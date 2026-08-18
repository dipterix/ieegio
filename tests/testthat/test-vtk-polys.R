skip_if_no_vtk <- function() {
  testthat::skip_if_not(
    requireNamespace("rpymat", quietly = TRUE) &&
      dir.exists(rpymat::env_path()),
    message = "Python environment is not configured"
  )
  testthat::skip_if_not(
    isTRUE(tryCatch({
      rpymat::import("vtk")
      TRUE
    }, error = function(e) { FALSE })),
    message = "Python `vtk` module is unavailable"
  )
}

sample_mesh <- function() {
  vertices <- matrix(
    ncol = 3, byrow = TRUE,
    as.double(c(0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0,
                0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1))
  )
  faces <- matrix(
    ncol = 3, byrow = TRUE,
    c(1, 2, 3, 1, 3, 4, 5, 6, 7, 5, 7, 8,
      1, 2, 6, 1, 6, 5, 2, 3, 7, 2, 7, 6,
      3, 4, 8, 3, 8, 7, 4, 1, 5, 4, 5, 8)
  )
  as_ieegio_surface(vertices, faces = faces)
}

test_that("io-vtk-polys: geometry round trip", {

  skip_if_no_vtk()

  mesh <- sample_mesh()

  for (ext in c(".vtk", ".vtp")) {
    for (binary in c(TRUE, FALSE)) {
      tfile <- tempfile(fileext = ext)
      io_write_vtk_polys(mesh, con = tfile, binary = binary)
      expect_true(file.exists(tfile))

      re_read <- io_read_vtk_polys(tfile)
      expect_true(inherits(re_read, "ieegio_surface_contains_geometry"))
      expect_equal(re_read$geometry$vertices, mesh$geometry$vertices)
      expect_equal(re_read$geometry$faces, mesh$geometry$faces)
      expect_equal(re_read$geometry$face_start, 1L)

      unlink(tfile)
    }
  }

})

test_that("io-vtk-polys: color and measurement round trip", {

  skip_if_no_vtk()

  vertices <- t(sample_mesh()$geometry$vertices[c(1, 2, 3), , drop = FALSE])
  mesh <- as_ieegio_surface(
    vertices,
    faces = t(sample_mesh()$geometry$faces),
    vertex_colors = c("red", "green", "blue", "white",
                      "black", "yellow", "cyan", "magenta"),
    measurements = data.frame(
      curv = vertices[, 1],
      sulc = vertices[, 3]
    )
  )

  tfile <- tempfile(fileext = ".vtk")
  io_write_vtk_polys(mesh, con = tfile)
  re_read <- io_read_vtk_polys(tfile)

  expect_true(inherits(re_read, "ieegio_surface_contains_color"))
  expect_true(inherits(re_read, "ieegio_surface_contains_measurements"))

  expect_equal(unname(re_read$color), unname(mesh$color))
  expect_equal(
    as.data.frame(re_read$measurements$data_table),
    as.data.frame(mesh$measurements$data_table)
  )

  unlink(tfile)

})

test_that("io-vtk-polys: transform is applied when writing", {

  skip_if_no_vtk()

  vertices <- t(sample_mesh()$geometry$vertices[c(1, 2, 3), , drop = FALSE])
  mesh <- as_ieegio_surface(
    vertices,
    faces = t(sample_mesh()$geometry$faces),
    transform = diag(c(-1, -1, 2, 1))
  )

  # by default the first transform is applied
  tfile <- tempfile(fileext = ".vtk")
  io_write_vtk_polys(mesh, con = tfile)
  re_read <- io_read_vtk_polys(tfile)
  expect_equal(
    re_read$geometry$vertices,
    diag(c(-1, -1, 2, 1)) %*% mesh$geometry$vertices
  )
  # the resulting surface is in the transformed space
  expect_equal(re_read$geometry$transforms$Unknown, diag(1, 4),
               ignore_attr = TRUE)
  unlink(tfile)

  # `transform = NULL` writes the vertex positions as-is
  tfile <- tempfile(fileext = ".vtk")
  io_write_vtk_polys(mesh, con = tfile, transform = NULL)
  re_read <- io_read_vtk_polys(tfile)
  expect_equal(re_read$geometry$vertices, mesh$geometry$vertices)
  unlink(tfile)

  # explicit matrix
  tfile <- tempfile(fileext = ".vtk")
  io_write_vtk_polys(mesh, con = tfile, transform = diag(c(1, 1, 1, 1)))
  re_read <- io_read_vtk_polys(tfile)
  expect_equal(re_read$geometry$vertices, mesh$geometry$vertices)
  unlink(tfile)

})

test_that("io-vtk-polys: non-triangular polygons are triangulated", {

  skip_if_no_vtk()

  tfile <- tempfile(fileext = ".vtk")
  writeLines(
    c("# vtk DataFile Version 3.0", "quad", "ASCII", "DATASET POLYDATA",
      "POINTS 4 float", "0 0 0", "1 0 0", "1 1 0", "0 1 0",
      "POLYGONS 1 5", "4 0 1 2 3"),
    tfile
  )

  quad <- io_read_vtk_polys(tfile)

  # one quadrilateral becomes two triangles
  expect_equal(dim(quad$geometry$faces), c(3L, 2L))
  expect_equal(ncol(quad$geometry$vertices), 4L)
  expect_true(all(quad$geometry$faces >= 1L))
  expect_true(all(quad$geometry$faces <= 4L))

  unlink(tfile)

})

test_that("io-vtk-polys: point cloud and unstructured grid", {

  skip_if_no_vtk()

  # point cloud: no polygon at all
  tfile <- tempfile(fileext = ".vtk")
  writeLines(
    c("# vtk DataFile Version 3.0", "pts", "ASCII", "DATASET POLYDATA",
      "POINTS 3 float", "0 0 0", "1 0 0", "1 1 0"),
    tfile
  )

  point_cloud <- io_read_vtk_polys(tfile)
  expect_equal(ncol(point_cloud$geometry$vertices), 3L)
  expect_null(point_cloud$geometry$faces)
  unlink(tfile)

  # unstructured grid: surface is extracted
  tfile <- tempfile(fileext = ".vtk")
  writeLines(
    c("# vtk DataFile Version 3.0", "ug", "ASCII",
      "DATASET UNSTRUCTURED_GRID",
      "POINTS 4 float", "0 0 0", "1 0 0", "1 1 0", "0 1 0",
      "CELLS 2 8", "3 0 1 2", "3 0 2 3",
      "CELL_TYPES 2", "5", "5"),
    tfile
  )

  grid <- io_read_vtk_polys(tfile)
  expect_equal(ncol(grid$geometry$vertices), 4L)
  expect_equal(ncol(grid$geometry$faces), 2L)
  unlink(tfile)

})

test_that("io-vtk-polys: read_surface and write_surface dispatch", {

  skip_if_no_vtk()

  mesh <- sample_mesh()

  tfile <- tempfile(fileext = ".vtp")
  write_surface(mesh, con = tfile)
  expect_true(file.exists(tfile))

  re_read <- read_surface(tfile)
  expect_equal(re_read$geometry$vertices, mesh$geometry$vertices)
  expect_equal(re_read$geometry$faces, mesh$geometry$faces)

  unlink(tfile)

  # explicit format is honored when the file name has no VTK extension
  tfile <- tempfile(fileext = ".dat")
  write_surface(mesh, con = tfile, format = "vtk")
  expect_true(file.exists(tfile))
  expect_equal(
    read_surface(tfile, format = "vtk")$geometry$faces,
    mesh$geometry$faces
  )
  unlink(tfile)

})
