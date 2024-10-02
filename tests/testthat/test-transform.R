library(testthat)
test_that("mat_to_quaternion", {

  # trace > 0
  m1 <- matrix(
    nrow = 3, byrow = TRUE,
    c(
      1, 0, 0,
      0, 0, 1,
      0, 1, 0
    )
  )
  # dput(ravetools::new_quaternion()$set_from_rotation_matrix(m1)[])
  expect_equal(mat_to_quaternion(m1), c(0, 0, 0, 0.707106781186548),
               tolerance = 1e-5, ignore_attr = TRUE)

  # m11 > m22 && m11 > m33
  m2 <- matrix(
    nrow = 3, byrow = TRUE,
    c(
      1, 0, 0,
      0, cos(0.5), -sin(0.5),
      0, sin(0.5), cos(0.5)
    )
  )
  # dput(ravetools::new_quaternion()$set_from_rotation_matrix(m2)[])
  expect_equal(mat_to_quaternion(m2), c(0.247403959254523, 0, 0, 0.968912421710645),
               tolerance = 1e-5, ignore_attr = TRUE)

  # m22 > m33
  m3 <- matrix(
    nrow = 3, byrow = TRUE,
    c(
      cos(0.5), 0, -sin(0.5),
      0, 1, 0,
      sin(0.5), 0, cos(0.5)
    )
  )
  # dput(ravetools::new_quaternion()$set_from_rotation_matrix(m3)[])
  expect_equal(mat_to_quaternion(m3), c(0, -0.247403959254523, 0, 0.968912421710645),
               tolerance = 1e-5, ignore_attr = TRUE)

  # else
  m4 <- matrix(
    nrow = 3, byrow = TRUE,
    c(
      -1, 0, 0,
      0, 0, 1,
      0, 1, 0
    )
  )
  # dput(ravetools::new_quaternion()$set_from_rotation_matrix(m4)[])
  expect_equal(mat_to_quaternion(m4), c(0, 0.707106781186547, 0.707106781186548, 0),
               tolerance = 1e-5, ignore_attr = TRUE)


  # unnormalized
  m5 <- structure(c(3.40019986015226, -0.664561369499719, 0.0876456630228239,
                    2.06023861419986, 1.79269006456597, 2.53913216184044, -0.308197463106615,
                    -2.15728003710697, -0.198188173835187), dim = c(3L, 3L))
  # dput(ravetools::new_quaternion()$set_from_rotation_matrix(apply(m5, 2, function(x) { x / sqrt(sum(x^2)) }))[])
  expect_equal(
    mat_to_quaternion(m5),
    c(0.541202408480409, -0.0539439967319966, -0.241654474993762, 0.769949006914233),
    tolerance = 1e-5, ignore_attr = TRUE)

})


test_that("get_vox2ras_tkr", {

  # trace > 0

  vox2ras <- matrix(
    nrow = 4, byrow = 4,
    c(
      1, 0, 0, -128,
      0, 0, -1, 128,
      0, -1, 0, 128,
      0, 0,  0, 1
    )
  )
  expect_equal(get_vox2ras_tkr(vox2ras, c(128, 128, 128)), vox2ras)

  vox2ras <- matrix(
    nrow = 4, byrow = 4,
    c(
      -1, 0, 0, 128,
      0, 0, 1, -128,
      0, 1, 0, -128,
      0, 0,  0, 1
    )
  )
  expect_equal(get_vox2ras_tkr(vox2ras, c(128, 128, 128)), vox2ras)

})
