# tests/testthat/test-apply-transform-to-points.R

# --- Local Helper: Create affine transform ---
make_affine_transform <- function(mat = NULL, space_from = "", space_to = "",
                                  orientation_from = "RAS", orientation_to = "RAS",
                                  interpretation = "active") {
  if (is.null(mat)) mat <- diag(4)
  ieegio:::new_transform(
    data = mat, type = "affine",
    space_from = new_space(space_from, orientation = orientation_from, dimension = 3L),
    space_to = new_space(space_to, orientation = orientation_to, dimension = 3L),
    dimension = 3L, interpretation = interpretation
  )
}

# --- Local Helper: Generate random invertible affine matrix ---
rand_affine <- function(seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  mat <- matrix(rnorm(16), 4, 4)
  mat[4, ] <- c(0, 0, 0, 1)
  # Ensure invertibility
  while(abs(det(mat[1:3, 1:3])) < 0.01) {
    mat <- matrix(rnorm(16), 4, 4)
    mat[4, ] <- c(0, 0, 0, 1)
  }
  mat
}

# ============================================================
# TEST: Basic Active Affine Transforms
# ============================================================

test_that("apply_transform_to_points: identity transform", {
  transform <- make_affine_transform(diag(4), "A", "B")
  
  # Single point as vector
  point <- c(10, 20, 30)
  result <- ieegio:::apply_transform_to_points(point, transform)
  expect_equal(result[1, 1:3], point, tolerance = 1e-10)
  expect_equal(result[1, 4], 1)
  
  # Multiple points as matrix
  points <- matrix(c(10, 20, 30,
                     15, 25, 35), nrow = 2, byrow = TRUE)
  result <- ieegio:::apply_transform_to_points(points, transform)
  expect_equal(result[, 1:3], points, tolerance = 1e-10)
})

test_that("apply_transform_to_points: translation matrix", {
  translation <- diag(4)
  translation[1:3, 4] <- c(5, 10, 15)
  
  transform <- make_affine_transform(translation, "A", "B")
  
  point <- c(10, 20, 30)
  result <- ieegio:::apply_transform_to_points(point, transform)
  
  # Expect point + translation
  expected <- c(15, 30, 45)
  expect_equal(result[1, 1:3], expected, tolerance = 1e-10)
})

test_that("apply_transform_to_points: rotation matrix", {
  # 90-degree rotation around z-axis: (x,y,z) -> (-y,x,z)
  rotation <- matrix(c(
    0, -1, 0, 0,
    1,  0, 0, 0,
    0,  0, 1, 0,
    0,  0, 0, 1
  ), nrow = 4, byrow = TRUE)
  
  transform <- make_affine_transform(rotation, "A", "B")
  
  point <- c(10, 20, 30)
  result <- ieegio:::apply_transform_to_points(point, transform)
  
  # (10, 20, 30) -> (-20, 10, 30)
  expected <- c(-20, 10, 30)
  expect_equal(result[1, 1:3], expected, tolerance = 1e-10)
})

# ============================================================
# TEST: Passive-to-Active Conversion for Regular Transforms
# ============================================================

test_that("apply_transform_to_points: passive transform different spaces", {
  # Create a general affine matrix with rotation, scaling, and translation
  affine <- matrix(c(
    0.8, -0.6, 0.2, 5,
    0.6,  0.8, 0.1, 10,
    0.1, -0.2, 0.9, 15,
    0,    0,   0,   1
  ), nrow = 4, byrow = TRUE)
  
  # Create as passive A->B
  transform <- make_affine_transform(affine, "A", "B", interpretation = "passive")
  
  point <- c(10, 20, 30)
  result <- ieegio:::apply_transform_to_points(point, transform)
  
  # For passive->active conversion: uses matrix inversion + interpretation toggle
  # Expected: solve(affine) applied to point
  expected_matrix <- solve(affine)
  expected_point <- as.vector((expected_matrix %*% c(point, 1))[1:3])
  
  expect_equal(result[1, 1:3], expected_point, tolerance = 1e-10)
})

# ============================================================
# TEST: Same-Space Transforms (Critical for Bug Fix)
# ============================================================

test_that("apply_transform_to_points: active same-space transform", {
  # Create rotation A->A (active)
  rotation <- matrix(c(
    0, -1, 0, 0,
    1,  0, 0, 0,
    0,  0, 1, 0,
    0,  0, 0, 1
  ), nrow = 4, byrow = TRUE)
  
  transform <- make_affine_transform(rotation, "scanner", "scanner", interpretation = "active")
  
  point <- c(10, 20, 30)
  result <- ieegio:::apply_transform_to_points(point, transform)
  
  # Active: apply matrix directly
  expected <- c(-20, 10, 30)
  expect_equal(result[1, 1:3], expected, tolerance = 1e-10)
})

test_that("apply_transform_to_points: passive same-space transform", {
  # Create same rotation A->A (passive)
  rotation <- matrix(c(
    0, -1, 0, 0,
    1,  0, 0, 0,
    0,  0, 1, 0,
    0,  0, 0, 1
  ), nrow = 4, byrow = TRUE)
  
  transform <- make_affine_transform(rotation, "scanner", "scanner", interpretation = "passive")
  
  point <- c(10, 20, 30)
  result <- ieegio:::apply_transform_to_points(point, transform)
  
  # Passive: apply solve(matrix) (due to atomic operations: matrix inversion + interpretation toggle)
  expected_matrix <- solve(rotation)
  expected <- as.vector((expected_matrix %*% c(point, 1))[1:3])
  expect_equal(result[1, 1:3], expected, tolerance = 1e-10)
})

test_that("apply_transform_to_points: passive differs from active for same-space", {
  # This is the key bug fix: passive(A->A) != active(A->A)
  # Use general affine with rotation + shear + scaling
  affine <- matrix(c(
    0.9, -0.3, 0.15, 2,
    0.4,  0.85, -0.1, -1,
    0.05, 0.2, 0.95, 0.5,
    0,    0,   0,    1
  ), nrow = 4, byrow = TRUE)
  
  transform_active <- make_affine_transform(affine, "scanner", "scanner", interpretation = "active")
  transform_passive <- make_affine_transform(affine, "scanner", "scanner", interpretation = "passive")
  
  point <- c(10, 20, 30)
  result_active <- ieegio:::apply_transform_to_points(point, transform_active)
  result_passive <- ieegio:::apply_transform_to_points(point, transform_passive)
  
  # Results should be different
  expect_false(isTRUE(all.equal(result_active[1, 1:3], result_passive[1, 1:3], tolerance = 1e-10)))
  
  # Compute expected results manually
  expected_active <- as.vector((affine %*% c(point, 1))[1:3])
  expected_passive <- as.vector((solve(affine) %*% c(point, 1))[1:3])
  
  expect_equal(result_active[1, 1:3], expected_active, tolerance = 1e-10)
  expect_equal(result_passive[1, 1:3], expected_passive, tolerance = 1e-10)
})

# ============================================================
# TEST: Wildcard Same-Space Transforms
# ============================================================

test_that("apply_transform_to_points: active wildcard-to-wildcard", {
  # General affine with rotation and shear
  affine <- matrix(c(
    0.7, -0.5, 0.3, 1,
    0.6,  0.8, -0.2, 2,
    0.1,  0.3, 0.9, 3,
    0,    0,   0,   1
  ), nrow = 4, byrow = TRUE)
  transform <- make_affine_transform(affine, "", "", interpretation = "active")
  
  point <- c(10, 20, 30)
  result <- ieegio:::apply_transform_to_points(point, transform)
  
  expected <- as.vector((affine %*% c(point, 1))[1:3])
  expect_equal(result[1, 1:3], expected, tolerance = 1e-10)
})

test_that("apply_transform_to_points: passive wildcard-to-wildcard", {
  # General affine with rotation and shear
  affine <- matrix(c(
    0.7, -0.5, 0.3, 1,
    0.6,  0.8, -0.2, 2,
    0.1,  0.3, 0.9, 3,
    0,    0,   0,   1
  ), nrow = 4, byrow = TRUE)
  transform <- make_affine_transform(affine, "", "", interpretation = "passive")
  
  point <- c(10, 20, 30)
  result <- ieegio:::apply_transform_to_points(point, transform)
  
  # Passive: apply solve(affine)
  expected <- as.vector((solve(affine) %*% c(point, 1))[1:3])
  expect_equal(result[1, 1:3], expected, tolerance = 1e-10)
})

test_that("apply_transform_to_points: half-wildcard A-to-empty active", {
  scaling <- diag(c(2, 3, 4, 1))
  transform <- make_affine_transform(scaling, "scanner", "", interpretation = "active")
  
  point <- c(10, 20, 30)
  result <- ieegio:::apply_transform_to_points(point, transform)
  
  expected <- c(20, 60, 120)
  expect_equal(result[1, 1:3], expected, tolerance = 1e-10)
})

test_that("apply_transform_to_points: half-wildcard A-to-empty passive", {
  scaling <- diag(c(2, 3, 4, 1))
  transform <- make_affine_transform(scaling, "scanner", "", interpretation = "passive")
  
  point <- c(10, 20, 30)
  result <- ieegio:::apply_transform_to_points(point, transform)
  
  # Passive: apply inverse
  expected <- c(5, 20/3, 7.5)
  expect_equal(result[1, 1:3], expected, tolerance = 1e-10)
})

test_that("apply_transform_to_points: half-wildcard empty-to-A active", {
  scaling <- diag(c(2, 3, 4, 1))
  transform <- make_affine_transform(scaling, "", "target", interpretation = "active")
  
  point <- c(10, 20, 30)
  result <- ieegio:::apply_transform_to_points(point, transform)
  
  expected <- c(20, 60, 120)
  expect_equal(result[1, 1:3], expected, tolerance = 1e-10)
})

test_that("apply_transform_to_points: half-wildcard empty-to-A passive", {
  scaling <- diag(c(2, 3, 4, 1))
  transform <- make_affine_transform(scaling, "", "target", interpretation = "passive")
  
  point <- c(10, 20, 30)
  result <- ieegio:::apply_transform_to_points(point, transform)
  
  # Passive: apply inverse
  expected <- c(5, 20/3, 7.5)
  expect_equal(result[1, 1:3], expected, tolerance = 1e-10)
})

# ============================================================
# TEST: Coordinate Format Handling
# ============================================================

test_that("apply_transform_to_points: 3D vector input", {
  transform <- make_affine_transform(diag(c(2, 2, 2, 1)), "A", "B")
  
  # Input: 3D vector
  point <- c(10, 20, 30)
  result <- ieegio:::apply_transform_to_points(point, transform)
  
  expect_equal(dim(result), c(1, 4))
  expect_equal(result[1, 1:3], c(20, 40, 60), tolerance = 1e-10)
  expect_equal(result[1, 4], 1)
})

test_that("apply_transform_to_points: 4D homogeneous vector input", {
  transform <- make_affine_transform(diag(c(2, 2, 2, 1)), "A", "B")
  
  # Input: 4D homogeneous vector
  point <- c(10, 20, 30, 1)
  result <- ieegio:::apply_transform_to_points(point, transform)
  
  expect_equal(dim(result), c(1, 4))
  expect_equal(result[1, 1:3], c(20, 40, 60), tolerance = 1e-10)
  expect_equal(result[1, 4], 1)
})

test_that("apply_transform_to_points: matrix with 3 columns", {
  transform <- make_affine_transform(diag(c(2, 2, 2, 1)), "A", "B")
  
  # Input: matrix with 3 columns
  points <- matrix(c(10, 20, 30,
                     15, 25, 35), nrow = 2, byrow = TRUE)
  result <- ieegio:::apply_transform_to_points(points, transform)
  
  expect_equal(dim(result), c(2, 4))
  expect_equal(result[1, 1:3], c(20, 40, 60), tolerance = 1e-10)
  expect_equal(result[2, 1:3], c(30, 50, 70), tolerance = 1e-10)
  expect_equal(result[, 4], c(1, 1))
})

test_that("apply_transform_to_points: matrix with 4 columns", {
  transform <- make_affine_transform(diag(c(2, 2, 2, 1)), "A", "B")
  
  # Input: matrix with 4 columns (homogeneous)
  points <- matrix(c(10, 20, 30, 1,
                     15, 25, 35, 1), nrow = 2, byrow = TRUE)
  result <- ieegio:::apply_transform_to_points(points, transform)
  
  expect_equal(dim(result), c(2, 4))
  expect_equal(result[1, 1:3], c(20, 40, 60), tolerance = 1e-10)
  expect_equal(result[2, 1:3], c(30, 50, 70), tolerance = 1e-10)
  expect_equal(result[, 4], c(1, 1))
})

# ============================================================
# TEST: Wildcard Transform Chains
# ============================================================

test_that("apply_transform_to_points: chain A-to-wildcard, wildcard-to-B, B-to-C", {
  # Three transforms: A->"", ""->"B", "B"->C
  m1 <- rand_affine(2001)
  m2 <- rand_affine(2002)
  m3 <- rand_affine(2003)
  
  t1 <- make_affine_transform(m1, "A", "")
  t2 <- make_affine_transform(m2, "", "B")
  t3 <- make_affine_transform(m3, "B", "C")
  
  chain <- new_transform_chain(t1, t2, t3)
  
  point <- c(10, 20, 30)
  result <- ieegio:::apply_transform_to_points(point, chain)
  
  # Expected: m3 %*% m2 %*% m1
  expected_matrix <- m3 %*% m2 %*% m1
  expected <- as.vector((expected_matrix %*% c(point, 1))[1:3])
  expect_equal(result[1, 1:3], expected, tolerance = 1e-10)
})

test_that("apply_transform_to_points: chain all wildcards", {
  # Three transforms: ""->"", ""->"", ""->""
  m1 <- rand_affine(2004)
  m2 <- rand_affine(2005)
  m3 <- rand_affine(2006)
  
  t1 <- make_affine_transform(m1, "", "")
  t2 <- make_affine_transform(m2, "", "")
  t3 <- make_affine_transform(m3, "", "")
  
  chain <- new_transform_chain(t1, t2, t3)
  
  point <- c(10, 20, 30)
  result <- ieegio:::apply_transform_to_points(point, chain)
  
  # Expected: m3 %*% m2 %*% m1
  expected_matrix <- m3 %*% m2 %*% m1
  expected <- as.vector((expected_matrix %*% c(point, 1))[1:3])
  expect_equal(result[1, 1:3], expected, tolerance = 1e-10)
})

test_that("apply_transform_to_points: chain A-to-B, wildcard-to-wildcard, wildcard-to-C", {
  # Three transforms: A->B, ""->"", ""->C
  m1 <- rand_affine(2007)
  m2 <- rand_affine(2008)
  m3 <- rand_affine(2009)
  
  t1 <- make_affine_transform(m1, "A", "B")
  t2 <- make_affine_transform(m2, "", "")
  t3 <- make_affine_transform(m3, "", "C")
  
  chain <- new_transform_chain(t1, t2, t3)
  
  point <- c(10, 20, 30)
  result <- ieegio:::apply_transform_to_points(point, chain)
  
  # Expected: m3 %*% m2 %*% m1 applied
  expected_matrix <- m3 %*% m2 %*% m1
  expected <- as.vector((expected_matrix %*% c(point, 1))[1:3])
  
  expect_equal(result[1, 1:3], expected, tolerance = 1e-10)
})

test_that("apply_transform_to_points: chain wildcards with passive interpretation", {
  # Three transforms with mixed interpretations: A->"" (passive), ""->B (active), B->C (passive)
  m1 <- rand_affine(2010)
  m2 <- rand_affine(2011)
  m3 <- rand_affine(2012)
  
  t1 <- make_affine_transform(m1, "A", "", interpretation = "passive")
  t2 <- make_affine_transform(m2, "", "B", interpretation = "active")
  t3 <- make_affine_transform(m3, "B", "C", interpretation = "passive")
  
  # Chain with active interpretation - should convert all to active
  chain <- new_transform_chain(t1, t2, t3, interpretation = "active")
  
  point <- c(10, 20, 30)
  result <- ieegio:::apply_transform_to_points(point, chain)
  
  # t1 passive->active: solve(m1) + interp toggle
  # t2 already active
  # t3 passive->active: solve(m3) + interp toggle
  # Actually: solve(m3) %*% m2 %*% solve(m1)
  expected_matrix <- solve(m3) %*% m2 %*% solve(m1)
  expected <- as.vector((expected_matrix %*% c(point, 1))[1:3])
  
  expect_equal(result[1, 1:3], expected, tolerance = 1e-10)
})

test_that("apply_transform_to_points: chain same-space wildcards", {
  # Three transforms: ""->"" (all same space with different interpretations)
  affine1 <- rand_affine(2013)
  affine2 <- rand_affine(2014)
  affine3 <- rand_affine(2015)
  
  t1 <- make_affine_transform(affine1, "", "", interpretation = "active")
  t2 <- make_affine_transform(affine2, "", "", interpretation = "passive")
  t3 <- make_affine_transform(affine3, "", "", interpretation = "active")
  
  chain <- new_transform_chain(t1, t2, t3, interpretation = "active")
  
  point <- c(10, 20, 30)
  result <- ieegio:::apply_transform_to_points(point, chain)
  
  # t1 active (no change)
  # t2 passive->active: solve(affine2) + interp toggle
  # t3 active (no change)
  # Expected: affine3 %*% solve(affine2) %*% affine1
  expected_matrix <- affine3 %*% solve(affine2) %*% affine1
  expected <- as.vector((expected_matrix %*% c(point, 1))[1:3])
  
  expect_equal(result[1, 1:3], expected, tolerance = 1e-10)
})

# ============================================================
# TEST: Transform Chains
# ============================================================

test_that("apply_transform_to_points: chained affine transforms", {
  # Two transforms: A->B and B->C
  m1 <- diag(c(2, 2, 2, 1))
  m2 <- diag(c(3, 3, 3, 1))
  
  t1 <- make_affine_transform(m1, "A", "B")
  t2 <- make_affine_transform(m2, "B", "C")
  
  # Chain them
  chain <- new_transform_chain(t1, t2)
  
  point <- c(10, 20, 30)
  result <- ieegio:::apply_transform_to_points(point, chain)
  
  # Expected: m2 %*% m1 applied = 6x scaling
  expected <- c(60, 120, 180)
  expect_equal(result[1, 1:3], expected, tolerance = 1e-10)
})

test_that("apply_transform_to_points: chain with orientation bridge", {
  # A->B (LPS) and B->C (RAS) - should insert orientation bridge
  m1 <- rand_affine(1001)
  m2 <- rand_affine(1002)
  
  t1 <- make_affine_transform(m1, "A", "B", orientation_to = "LPS")
  t2 <- make_affine_transform(m2, "B", "C", orientation_from = "RAS")
  
  chain <- new_transform_chain(t1, t2)
  
  point <- c(10, 20, 30)
  result <- ieegio:::apply_transform_to_points(point, chain)
  
  # Should have applied: m2 %*% bridge %*% m1
  bridge <- ieegio:::orientation_transform("LPS", "RAS")
  expected_matrix <- m2 %*% bridge %*% m1
  expected <- as.vector((expected_matrix %*% c(point, 1))[1:3])
  
  expect_equal(result[1, 1:3], expected, tolerance = 1e-10)
})

test_that("apply_transform_to_points: chain with passive transform", {
  # A->B (passive) and B->C (active)
  m1 <- diag(c(2, 3, 4, 1))
  m2 <- diag(c(5, 6, 7, 1))
  
  t1 <- make_affine_transform(m1, "A", "B", interpretation = "passive")
  t2 <- make_affine_transform(m2, "B", "C", interpretation = "active")
  
  # Chain with active interpretation
  chain <- new_transform_chain(t1, t2, interpretation = "active")
  
  point <- c(10, 20, 30)
  result <- ieegio:::apply_transform_to_points(point, chain)
  
  # t1 passive->active: solve(m1) + interpretation toggle
  # Then chain: m2 %*% solve(m1)
  m1_inverted <- solve(m1)
  expected_matrix <- m2 %*% m1_inverted
  expected <- as.vector((expected_matrix %*% c(point, 1))[1:3])
  
  expect_equal(result[1, 1:3], expected, tolerance = 1e-10)
})
