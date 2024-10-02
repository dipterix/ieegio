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

})
