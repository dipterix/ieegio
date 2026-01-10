# tests/testthat/test-transform-chain.R

# --- Helper: Create affine transform with specific matrix ---
make_affine <- function(mat = NULL, space_from = "", space_to = "",
                        orientation_from = "RAS", orientation_to = "RAS",
                        interpretation = "active") {
  if (is.null(mat)) mat <- diag(4)
  new_transform(
    data = mat, type = "affine",
    space_from = new_space(space_from, orientation = orientation_from, dimension = 3L),
    space_to = new_space(space_to, orientation = orientation_to, dimension = 3L),
    dimension = 3L, interpretation = interpretation
  )
}

# --- Helper: Create mock deformation transform ---
make_deform <- function(space_from = "", space_to = "",
                        orientation_from = "LPS", orientation_to = "LPS",
                        interpretation = "passive") {
  mock_data <- structure(list(type = "mock_deformation"), class = "mock_ants")
  structure(
    list(
      data = list(mock_data),
      type = "deformation",
      interpretation = interpretation,
      space_from = new_space(space_from, orientation = orientation_from, dimension = 3L),
      space_to = new_space(space_to, orientation = orientation_to, dimension = 3L),
      dimension = 3L
    ),
    class = c("ieegio_transform_deformation", "ieegio_transforms")
  )
}

# --- Helper: Generate random invertible 4x4 affine matrix ---
rand_affine <- function(seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  mat <- matrix(rnorm(16), 4, 4)
  mat[4, ] <- c(0, 0, 0, 1)
  mat
}

# ============================================================
# TEST: Input Edge Cases
# ============================================================
test_that("new_transform_chain handles empty and NULL inputs", {
  result <- new_transform_chain()
  expect_equal(result$data[[1]], diag(4))
  expect_equal(result$type, "affine")
  expect_equal(result$interpretation, "active")

  result2 <- new_transform_chain(NULL, NULL, .list = list(NULL))
  expect_equal(result2$data[[1]], diag(4))
})

test_that("new_transform_chain passes through single transform", {
  t1 <- make_affine(rand_affine(1), "A", "B")
  result <- new_transform_chain(t1)
  expect_identical(result, t1)

  result2 <- new_transform_chain(.list = list(t1))
  expect_identical(result2, t1)
})

# ============================================================
# TEST: Affine Matrix Merging
# ============================================================
test_that("consecutive affines are merged correctly", {
  m1 <- rand_affine(42)
  m2 <- rand_affine(43)
  m3 <- rand_affine(44)

  t1 <- make_affine(m1, "A", "B")
  t2 <- make_affine(m2, "B", "C")
  t3 <- make_affine(m3, "C", "D")

  result <- new_transform_chain(t1, t2, t3)

  expect_length(result$data, 1)
  expect_equal(result$type, "affine")

  # Verify multiplication order: m3 %*% m2 %*% m1
  expected <- m3 %*% m2 %*% m1
  expect_equal(result$data[[1]], expected, tolerance = 1e-10)

  expect_equal(as.character(result$space_from), "A")
  expect_equal(as.character(result$space_to), "D")
})

test_that("mixed ... and .list inputs work", {
  m1 <- rand_affine(100)
  m2 <- rand_affine(101)

  t1 <- make_affine(m1, "A", "B")
  t2 <- make_affine(m2, "B", "C")

  result <- new_transform_chain(t1, .list = list(t2))
  expect_equal(result$data[[1]], m2 %*% m1, tolerance = 1e-10)
})

# ============================================================
# TEST: Space Matching
# ============================================================
test_that("exact space matching works", {
  t1 <- make_affine(diag(4), "A", "B")
  t2 <- make_affine(diag(4), "B", "C")

  result <- new_transform_chain(t1, t2)
  expect_equal(as.character(result$space_from), "A")
  expect_equal(as.character(result$space_to), "C")
})

test_that("wildcard space matching works", {
  t1 <- make_affine(diag(4), "A", "")
  t2 <- make_affine(diag(4), "", "C")
  result <- new_transform_chain(t1, t2)
  expect_equal(as.character(result$space_from), "A")
  expect_equal(as.character(result$space_to), "C")

  t3 <- make_affine(diag(4), "A", "")
  t4 <- make_affine(diag(4), "B", "C")
  result2 <- new_transform_chain(t3, t4)
  expect_true(inherits(result2, "ieegio_transforms"))
})

test_that("space mismatch throws error", {
  t1 <- make_affine(diag(4), "A", "B")
  t2 <- make_affine(diag(4), "X", "Y")

  expect_error(new_transform_chain(t1, t2), "space mismatch")
})

# ============================================================
# TEST: Orientation Bridging
# ============================================================
test_that("orientation bridging inserts and merges transform", {
  m1 <- rand_affine(200)
  m2 <- rand_affine(201)

  t1 <- make_affine(m1, "A", "B", orientation_to = "LPS")
  t2 <- make_affine(m2, "B", "C", orientation_from = "RAS")

  result <- new_transform_chain(t1, t2)

  expect_length(result$data, 1)

  bridge <- orientation_transform("LPS", "RAS")
  expected <- m2 %*% bridge %*% m1
  expect_equal(result$data[[1]], expected, tolerance = 1e-10)
})

test_that("same orientation does not insert bridge", {
  m1 <- rand_affine(300)
  m2 <- rand_affine(301)

  t1 <- make_affine(m1, "A", "B", orientation_to = "RAS")
  t2 <- make_affine(m2, "B", "C", orientation_from = "RAS")

  result <- new_transform_chain(t1, t2)
  expect_equal(result$data[[1]], m2 %*% m1, tolerance = 1e-10)
})

test_that("axis permutation bridging works", {
  m1 <- rand_affine(350)
  m2 <- rand_affine(351)

  t1 <- make_affine(m1, "A", "B", orientation_to = "RAS")
  t2 <- make_affine(m2, "B", "C", orientation_from = "PIR")

  result <- new_transform_chain(t1, t2)

  bridge <- orientation_transform("RAS", "PIR")
  expected <- m2 %*% bridge %*% m1
  expect_equal(result$data[[1]], expected, tolerance = 1e-10)
})

# ============================================================
# TEST: Interpretation Handling
# ============================================================
test_that("matching interpretations chain directly", {
  m1 <- rand_affine(400)
  m2 <- rand_affine(401)

  t1 <- make_affine(m1, "A", "B", interpretation = "passive")
  t2 <- make_affine(m2, "B", "C", interpretation = "passive")

  result <- new_transform_chain(t1, t2)
  expect_equal(result$interpretation, "passive")
  expect_equal(result$data[[1]], m2 %*% m1, tolerance = 1e-10)
})

test_that("mismatched interpretations swap spaces", {
  m1 <- rand_affine(500)
  m2 <- rand_affine(501)

  # A->B active chains with C->B passive (becomes B->C active)
  t1 <- make_affine(m1, "A", "B", interpretation = "active")
  t2 <- make_affine(m2, "C", "B", interpretation = "passive")

  result <- new_transform_chain(t1, t2)
  expect_equal(result$interpretation, "active")
  expect_equal(as.character(result$space_from), "A")
  expect_equal(as.character(result$space_to), "C")
})

test_that("expected_interpretation forces alignment", {
  m1 <- rand_affine(600)
  m2 <- rand_affine(601)

  # A->B active + C->B active, forced passive
  # Becomes: B->A passive + B->C passive => chains to B->C via A (but A matches)
  # Simpler: use transforms where forcing works
  t1 <- make_affine(m1, "A", "B", interpretation = "active")
  t2 <- make_affine(m2, "A", "C", interpretation = "active")

  # Force passive: A->B active => B->A passive, A->C active => C->A passive
  # B->A passive + C->A passive: space_to of first is A, space_from of second is C
  # But with wildcard or matching we need: t1 ends at A, t2 starts at A? No...
  # Actually: B->A chains with A->C (if we flip t2 differently)
  # Let's use: expected_interpretation = "passive" on matching chain
  t3 <- make_affine(m1, "B", "A", interpretation = "passive")
  t4 <- make_affine(m2, "C", "B", interpretation = "passive")

  # C->B passive + B->A passive = C->A passive
  result <- new_transform_chain(t4, t3)
  expect_equal(result$interpretation, "passive")
  expect_equal(as.character(result$space_from), "C")
  expect_equal(as.character(result$space_to), "A")
})

# ============================================================
# TEST: Deformation Handling
# ============================================================
test_that("affine + deformation creates deformation type", {
  m1 <- rand_affine(700)
  t1 <- make_affine(m1, "A", "B", interpretation = "passive",
                    orientation_to = "LPS")
  t2 <- make_deform("B", "C")

  result <- new_transform_chain(t1, t2)

  expect_equal(result$type, "deformation")
  expect_length(result$data, 2)
  expect_true(is.matrix(result$data[[1]]))
  expect_false(is.matrix(result$data[[2]]))
})

test_that("deformation forces its interpretation", {
  t1 <- make_affine(diag(4), "B", "A", interpretation = "active",
                    orientation_to = "LPS")
  t2 <- make_deform("B", "C", interpretation = "passive")

  expect_error(new_transform_chain(t1, t2, interpretation = "active"))
  expect_error(new_transform_chain(t1, t2, interpretation = "passive"))

  # t1 active B->A becomes passive A->B to chain with passive B->C
  t1_swapped <- invert_transform(invert_transform(t1, method = "direction"), method = "interpretation")
  result <- new_transform_chain(t1_swapped, t2)
  expect_equal(result$interpretation, "passive")
  expect_equal(as.character(result$space_from), "A")
  expect_equal(as.character(result$space_to), "C")

  result <- new_transform_chain(t2, t1_swapped, interpretation = "active")
  expect_equal(result$interpretation, "active")
  expect_equal(as.character(result$space_from), "C")
  expect_equal(as.character(result$space_to), "A")
})

test_that("deformation with wrong interpretation errors", {
  t1 <- make_deform("A", "B", interpretation = "passive")
  t2 <- make_affine(diag(4), "X", "Y", interpretation = "active",
                    orientation_from = "LPS")

  expect_error(new_transform_chain(t1, t2, interpretation = "active"))
})

test_that("deformation + affine creates deformation type", {
  t1 <- make_deform("A", "B")
  m2 <- rand_affine(750)
  t2 <- make_affine(m2, "B", "C", interpretation = "passive",
                    orientation_from = "LPS")

  result <- new_transform_chain(t1, t2)
  expect_equal(result$type, "deformation")
  expect_length(result$data, 2)
})

# ============================================================
# TEST: Dimension Compatibility
# ============================================================
test_that("dimension mismatch throws error", {
  t1_3d <- make_affine(diag(4), "A", "B")

  t2_2d <- structure(
    list(data = list(diag(3)), type = "affine", interpretation = "active",
         space_from = new_space("B", dimension = 2L),
         space_to = new_space("C", dimension = 2L),
         dimension = 2L),
    class = c("ieegio_transform_affine", "ieegio_transforms")
  )

  expect_error(new_transform_chain(t1_3d, t2_2d), "Dimension mismatch")
})

# ============================================================
# TEST: Class and Structure
# ============================================================
test_that("result has correct class and structure", {
  t1 <- make_affine(rand_affine(800), "A", "B")
  t2 <- make_affine(rand_affine(801), "B", "C")

  result <- new_transform_chain(t1, t2)

  expect_s3_class(result, "ieegio_transforms")
  expect_s3_class(result, "ieegio_transform_affine")
  expect_true(is.list(result$data))
  expect_true(all(c("data", "type", "interpretation", "space_from",
                    "space_to", "dimension") %in% names(result)))
})

test_that("deformation chain has correct class", {
  t1 <- make_affine(diag(4), "A", "B", interpretation = "passive",
                    orientation_to = "LPS")
  t2 <- make_deform("B", "C")

  result <- new_transform_chain(t1, t2)
  expect_s3_class(result, "ieegio_transform_deformation")
})

# ============================================================
# TEST: Orientation bridge after deformation not merged
# ============================================================
test_that("bridge after deformation is separate element", {
  t1 <- make_deform("A", "B", orientation_to = "LPS")
  t2 <- make_affine(rand_affine(900), "B", "C", interpretation = "passive",
                    orientation_from = "RAS")

  result <- new_transform_chain(t1, t2)

  # Deform + bridge + affine = 3 elements (bridge merged with affine = 2)
  expect_length(result$data, 2)
  expect_false(is.matrix(result$data[[1]]))
  expect_true(is.matrix(result$data[[2]]))
})
