test_that("orientation_transform creates correct matrices for common orientations", {
  
  # Test 1: RAS to LPS (flip x and y)
  mat <- ieegio:::orientation_transform("RAS", "LPS")
  expected <- diag(c(-1, -1, 1, 1))
  expect_equal(mat, expected)
  
  # Test point transform (active)
  point_ras <- c(10, 20, 30, 1)  # Point in RAS: R=10, A=20, S=30
  point_lps <- as.vector(mat %*% point_ras)
  expect_equal(point_lps, c(-10, -20, 30, 1))  # L=-10, P=-20, S=30
  
  # Test 2: RAS to RAS (identity)
  mat <- ieegio:::orientation_transform("RAS", "RAS")
  expect_equal(mat, diag(4))
  
  # Test 3: RAS to LAI (flip x and y, keep z)
  mat <- ieegio:::orientation_transform("RAS", "LAI")
  expected <- diag(c(-1, 1, -1, 1))
  expect_equal(mat, expected)
  
  # Test 4: LPS to RAS (inverse of test 1)
  mat <- ieegio:::orientation_transform("LPS", "RAS")
  expected <- diag(c(-1, -1, 1, 1))
  expect_equal(mat, expected)
  
  # Test 5: RAS to LAS (flip only x)
  mat <- ieegio:::orientation_transform("RAS", "LAS")
  expected <- diag(c(-1, 1, 1, 1))
  expect_equal(mat, expected)
  
  # Test 6: RAS to RPS (flip only y)
  mat <- ieegio:::orientation_transform("RAS", "RPS")
  expected <- diag(c(1, -1, 1, 1))
  expect_equal(mat, expected)
  
  # Test 7: RAS to RAI (flip only z)
  mat <- ieegio:::orientation_transform("RAS", "RAI")
  expected <- diag(c(1, 1, -1, 1))
  expect_equal(mat, expected)
  
  # Test 8: LPI to RAI (flip x and z, keep y)
  mat <- ieegio:::orientation_transform("LPI", "RAI")
  expected <- diag(c(-1, -1, 1, 1))
  expect_equal(mat, expected)
  
  # Test 9: RAS to PIR (axis permutation: P from -A, I from -S, R from R)
  mat <- ieegio:::orientation_transform("RAS", "PIR")
  expected <- matrix(c(
    0, -1,  0, 0,
    0,  0, -1, 0,
    1,  0,  0, 0,
    0,  0,  0, 1
  ), nrow = 4, byrow = TRUE)
  expect_equal(mat, expected)
  
  # Test point with axis permutation
  point_ras <- c(10, 20, 30, 1)  # R=10, A=20, S=30
  point_pir <- as.vector(mat %*% point_ras)
  # In PIR: x=P (was -A=-20), y=I (was -S=-30), z=R (was R=10)
  expect_equal(point_pir, c(-20, -30, 10, 1))
  
})

test_that("orientation_transform composition works correctly", {
  
  # Transform chain: RAS -> LPS -> RAS should be identity
  mat1 <- ieegio:::orientation_transform("RAS", "LPS")
  mat2 <- ieegio:::orientation_transform("LPS", "RAS")
  mat_composed <- mat2 %*% mat1
  expect_equal(mat_composed, diag(4))
  
  # Three-way chain: RAS -> LPS -> LAI -> RAS
  mat1 <- ieegio:::orientation_transform("RAS", "LPS")
  mat2 <- ieegio:::orientation_transform("LPS", "LAI")
  mat3 <- ieegio:::orientation_transform("LAI", "RAS")
  mat_composed <- mat3 %*% mat2 %*% mat1
  expect_equal(mat_composed, diag(4), tolerance = 1e-10)
  
})

test_that("orientation_transform handles all common 8 orientations", {
  
  common_orientations <- c("RAS", "LAS", "LPS", "RPS", "LPI", "RPI", "LAI", "RAI")
  
  # Test all pairs can be transformed
  for(from_orient in common_orientations) {
    for(to_orient in common_orientations) {
      mat <- ieegio:::orientation_transform(from_orient, to_orient)
      
      # Matrix should be 4x4
      expect_equal(dim(mat), c(4, 4))
      
      # Bottom row should be [0, 0, 0, 1]
      expect_equal(mat[4, ], c(0, 0, 0, 1))
      
      # Top-left 3x3 should be a permutation with sign flips
      # (each row and column should have exactly one non-zero entry of ±1)
      top_left <- mat[1:3, 1:3]
      
      # Row sums of absolute values should be 1
      expect_equal(rowSums(abs(top_left)), c(1, 1, 1))
      
      # Column sums of absolute values should be 1
      expect_equal(colSums(abs(top_left)), c(1, 1, 1))
      
      # All entries should be -1, 0, or 1
      expect_true(all(top_left %in% c(-1, 0, 1)))
    }
  }
  
})

test_that("orientation_transform preserves distances", {
  
  # Distance should be preserved for orthogonal transforms
  mat <- ieegio:::orientation_transform("RAS", "LPS")
  
  # Test point
  p1 <- c(10, 20, 30, 1)
  p2 <- c(15, 25, 35, 1)
  
  # Transform points
  p1_transformed <- mat %*% p1
  p2_transformed <- mat %*% p2
  
  # Calculate distances
  dist_original <- sqrt(sum((p1[1:3] - p2[1:3])^2))
  dist_transformed <- sqrt(sum((p1_transformed[1:3] - p2_transformed[1:3])^2))
  
  expect_equal(dist_original, dist_transformed)
  
})
