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

# Test helper for are_transforms_chainable
create_dummy_transform <- function(type = c("affine", "deformation"),
                                   interpretation = c("active", "passive"),
                                   space_from = "space1",
                                   space_to = "space2",
                                   orientation_from = "RAS",
                                   orientation_to = "LPS") {
  type <- match.arg(type)
  interpretation <- match.arg(interpretation)
  
  if(type == "affine") {
    # Create a simple identity-like affine transform
    data <- diag(4)
  } else {
    # For deformation, use a dummy ANTs-like object (mock)
    # In real tests, this would be an actual ANTsTransform
    data <- structure(list(transform_type = "DisplacementFieldTransform"),
                     class = "ANTsTransform")
  }
  
  ieegio:::new_transform(
    data = data,
    type = type,
    space_from = new_space(space_from, orientation = orientation_from),
    space_to = new_space(space_to, orientation = orientation_to),
    interpretation = interpretation
  )
}

test_that("are_transforms_chainable: matching interpretations, matching spaces", {
  # Both active, affine + affine
  t1 <- create_dummy_transform("affine", "active", "A", "B")
  t2 <- create_dummy_transform("affine", "active", "B", "C")
  result <- ieegio:::are_transforms_chainable(t1, t2)
  expect_true(result$chainable)
  expect_equal(result$interpretation, "active")
  expect_equal(as.character(result$space_from), "A")
  expect_equal(as.character(result$space_to), "C")
  
  # Both passive, affine + affine
  t1 <- create_dummy_transform("affine", "passive", "A", "B")
  t2 <- create_dummy_transform("affine", "passive", "B", "C")
  result <- ieegio:::are_transforms_chainable(t1, t2)
  expect_true(result$chainable)
  expect_equal(result$interpretation, "passive")
  expect_equal(as.character(result$space_from), "A")
  expect_equal(as.character(result$space_to), "C")
})

test_that("are_transforms_chainable: mismatching interpretations", {
  # A->B (active), B->C (passive) - t2 needs inversion to match t1's active

# After inverting t2: C->B (active), but B != C so space mismatch
  t1 <- create_dummy_transform("affine", "active", "A", "B")
  t2 <- create_dummy_transform("affine", "passive", "B", "C")
  result <- ieegio:::are_transforms_chainable(t1, t2)
  expect_false(result$chainable)
  expect_match(result$reason, "space mismatch")
  
  # A->B (active), C->B (passive) - t2 inverted becomes B->C (active)
  # A->B chains with B->C = A->C
  t1 <- create_dummy_transform("affine", "active", "A", "B")
  t2 <- create_dummy_transform("affine", "passive", "C", "B")
  result <- ieegio:::are_transforms_chainable(t1, t2)
  expect_true(result$chainable)
  expect_equal(result$interpretation, "active")
  expect_equal(as.character(result$space_from), "A")
  expect_equal(as.character(result$space_to), "C")
})

test_that("are_transforms_chainable: deformation forces its interpretation", {
  # Deformation (passive) + affine (active) - must use passive
  # t1 passive, t2 inverted from active to passive: A->B becomes B->A
  t1 <- create_dummy_transform("deformation", "passive", "A", "B")
  t2 <- create_dummy_transform("affine", "active", "A", "B")
  result <- ieegio:::are_transforms_chainable(t1, t2)
  # t1 is deformation, so interpretation = passive
  # t2 inverted: B->A (passive), t1 ends at B, t2 starts at B -> chains!
  expect_true(result$chainable)
  expect_equal(result$interpretation, "passive")
  expect_equal(as.character(result$space_from), "A")
  expect_equal(as.character(result$space_to), "A")
  
  # Affine (active) + deformation (passive) - must use passive (t2 is deformation)
  t1 <- create_dummy_transform("affine", "active", "A", "B")
  t2 <- create_dummy_transform("deformation", "passive", "B", "C")
  result <- ieegio:::are_transforms_chainable(t1, t2)
  # t2 is deformation, so interpretation = passive
  # t1 inverted: B->A (passive) - doesn't match t2's B->C
  expect_false(result$chainable)
  expect_match(result$reason, "space mismatch")
})

test_that("are_transforms_chainable: wildcard spaces", {
  # Wildcard in first transform's space_to
  t1 <- create_dummy_transform("affine", "active", "A", "")
  t2 <- create_dummy_transform("affine", "active", "B", "C")
  result <- ieegio:::are_transforms_chainable(t1, t2)
  expect_true(result$chainable)
  expect_equal(as.character(result$space_from), "A")
  expect_equal(as.character(result$space_to), "C")
  
  # Wildcard in second transform's space_from
  t1 <- create_dummy_transform("affine", "active", "A", "B")
  t2 <- create_dummy_transform("affine", "active", "", "C")
  result <- ieegio:::are_transforms_chainable(t1, t2)
  expect_true(result$chainable)
  expect_equal(as.character(result$space_from), "A")
  expect_equal(as.character(result$space_to), "C")
  
  # Both wildcards
  t1 <- create_dummy_transform("affine", "active", "A", "")
  t2 <- create_dummy_transform("affine", "active", "", "C")
  result <- ieegio:::are_transforms_chainable(t1, t2)
  expect_true(result$chainable)
  expect_equal(as.character(result$space_from), "A")
  expect_equal(as.character(result$space_to), "C")
})

test_that("are_transforms_chainable: deformation + deformation mismatched interpretation", {
  # Both deformations with different interpretations -> can't invert either
  t1 <- create_dummy_transform("deformation", "active", "A", "B")
  t2 <- create_dummy_transform("deformation", "passive", "B", "C")
  result <- ieegio:::are_transforms_chainable(t1, t2)
  expect_false(result$chainable)
  expect_match(result$reason, "non-invertible deformation")
})

test_that("are_transforms_chainable: completely incompatible spaces", {
  t1 <- create_dummy_transform("affine", "active", "A", "B")
  t2 <- create_dummy_transform("affine", "active", "X", "Y")
  result <- ieegio:::are_transforms_chainable(t1, t2)
  expect_false(result$chainable)
  expect_match(result$reason, "space mismatch")
})

test_that("are_transforms_chainable: orientation differences are allowed", {
  # Same space name but different orientations -> should be chainable
  t1 <- create_dummy_transform("affine", "active", "scanner", "scanner", "RAS", "LPS")
  t2 <- create_dummy_transform("affine", "active", "scanner", "scanner", "LPS", "RAS")
  result <- ieegio:::are_transforms_chainable(t1, t2)
  expect_true(result$chainable)
  expect_equal(result$interpretation, "active")
})

test_that("are_transforms_chainable: same spaces, opposite directions, same interpretation", {
  # A->B (active) + B->A (active)
  # Direct chain works: A->B ends at B, B->A starts at B -> A->A
  t1 <- create_dummy_transform("affine", "active", "A", "B")
  t2 <- create_dummy_transform("affine", "active", "B", "A")
  result <- ieegio:::are_transforms_chainable(t1, t2)
  expect_true(result$chainable)
  expect_equal(result$interpretation, "active")
  expect_equal(as.character(result$space_from), "A")
  expect_equal(as.character(result$space_to), "A")
  
  # B->A (passive) + A->B (passive)
  t1 <- create_dummy_transform("affine", "passive", "B", "A")
  t2 <- create_dummy_transform("affine", "passive", "A", "B")
  result <- ieegio:::are_transforms_chainable(t1, t2)
  expect_true(result$chainable)
  expect_equal(result$interpretation, "passive")
  expect_equal(as.character(result$space_from), "B")
  expect_equal(as.character(result$space_to), "B")
})

test_that("are_transforms_chainable: same spaces, same direction, different interpretations", {
  # A->B (active) + A->B (passive)
  # t2 inverted: B->A (active), A->B chains with B->A = A->A
  t1 <- create_dummy_transform("affine", "active", "A", "B")
  t2 <- create_dummy_transform("affine", "passive", "A", "B")
  result <- ieegio:::are_transforms_chainable(t1, t2)
  expect_true(result$chainable)
  expect_equal(result$interpretation, "active")
  expect_equal(as.character(result$space_from), "A")
  expect_equal(as.character(result$space_to), "A")
  
  # A->B (passive) + A->B (active)
  # t2 inverted: B->A (passive), A->B chains with B->A = A->A
  t1 <- create_dummy_transform("affine", "passive", "A", "B")
  t2 <- create_dummy_transform("affine", "active", "A", "B")
  result <- ieegio:::are_transforms_chainable(t1, t2)
  expect_true(result$chainable)
  expect_equal(result$interpretation, "passive")
  expect_equal(as.character(result$space_from), "A")
  expect_equal(as.character(result$space_to), "A")
})

test_that("are_transforms_chainable: expected_interpretation parameter", {
  # Force active interpretation
  t1 <- create_dummy_transform("affine", "passive", "A", "B")
  t2 <- create_dummy_transform("affine", "passive", "A", "C")
  result <- ieegio:::are_transforms_chainable(t1, t2, expected_interpretation = "active")
  # t1 inverted: B->A (active), t2 inverted: C->A (active)
  # B->A ends at A, C->A starts at C - no match!
  expect_false(result$chainable)
  
  # Force passive interpretation - both already passive
  t1 <- create_dummy_transform("affine", "passive", "A", "B")
  t2 <- create_dummy_transform("affine", "passive", "B", "C")
  result <- ieegio:::are_transforms_chainable(t1, t2, expected_interpretation = "passive")
  expect_true(result$chainable)
  expect_equal(result$interpretation, "passive")
})

test_that("are_transforms_chainable: expected_interpretation with deformation fails", {
  # Deformation can't be inverted to match expected interpretation
  t1 <- create_dummy_transform("deformation", "passive", "A", "B")
  t2 <- create_dummy_transform("affine", "passive", "B", "C")
  result <- ieegio:::are_transforms_chainable(t1, t2, expected_interpretation = "active")
  expect_false(result$chainable)
  expect_match(result$reason, "non-invertible deformation")
})

test_that("are_transforms_chainable: expected_interpretation with space mismatch", {
  # After aligning interpretations, spaces don't match
  t1 <- create_dummy_transform("affine", "active", "A", "B")
  t2 <- create_dummy_transform("affine", "passive", "C", "D")
  result <- ieegio:::are_transforms_chainable(t1, t2, expected_interpretation = "active")
  expect_false(result$chainable)
  expect_match(result$reason, "space mismatch")
})

test_that("are_transforms_chainable: expected_interpretation with wildcards", {
  # Wildcard spaces should work with expected interpretation
  t1 <- create_dummy_transform("affine", "passive", "A", "")
  t2 <- create_dummy_transform("affine", "active", "", "C")
  result <- ieegio:::are_transforms_chainable(t1, t2, expected_interpretation = "active")
  expect_true(result$chainable)
  expect_equal(result$interpretation, "active")
  
  # Both wildcards
  t1 <- create_dummy_transform("affine", "active", "", "")
  t2 <- create_dummy_transform("affine", "passive", "", "")
  result <- ieegio:::are_transforms_chainable(t1, t2, expected_interpretation = "passive")
  expect_true(result$chainable)
  expect_equal(result$interpretation, "passive")
  
  # Wildcard to in t1, matches with anything in t2
  t1 <- create_dummy_transform("affine", "active", "X", "")
  t2 <- create_dummy_transform("affine", "active", "Y", "Z")
  result <- ieegio:::are_transforms_chainable(t1, t2, expected_interpretation = "active")
  expect_true(result$chainable)
  expect_equal(result$interpretation, "active")
})
