test_that("surface_to_surface handles basic orientation changes", {
  skip_if_not(ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii", test = TRUE))
  
  surf_ras <- read_surface(ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii"))
  
  # RAS to LPS conversion
  surf_lps <- surface_to_surface(
    surf_ras,
    space_from = new_space("", orientation = "RAS"),
    space_to = new_space("", orientation = "LPS")
  )
  
  # Expected: flip X and Y axes
  expected_verts <- diag(c(-1, -1, 1, 1)) %*% surf_ras$geometry$vertices
  
  expect_equal(
    surf_lps$geometry$vertices[1:3, ],
    expected_verts[1:3, ],
    tolerance = 1e-10
  )
})

test_that("surface_to_surface handles active transforms", {
  skip_if_not(ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii", test = TRUE))
  
  surf <- read_surface(ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii"))
  
  # Active transform
  transform_mat <- matrix(
    byrow = TRUE, nrow = 4,
    c(
      0.5, 0, 0.3, 1,
      0, -1, 0.2, 2,
      0, 0.7, -0.5, 4,
      0, 0, 0, 1
    )
  )
  
  surf_transformed <- surface_to_surface(surf, transform = transform_mat)
  
  # Expected: direct matrix multiplication
  expected_verts <- transform_mat %*% surf$geometry$vertices
  
  expect_equal(
    surf_transformed$geometry$vertices[1:3, ],
    expected_verts[1:3, ],
    tolerance = 1e-10
  )
})

test_that("surface_to_surface converts passive transforms to active", {
  skip_if_not(ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii", test = TRUE))
  
  surf <- read_surface(ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii"))
  
  # Create a passive transform (scanner -> T1)
  passive_mat <- matrix(
    byrow = TRUE, nrow = 4,
    c(
      2, 0.1, 0, 10,
      0, 2, 0.1, 20,
      0.1, 0, 2, 30,
      0, 0, 0, 1
    )
  )
  
  passive_transform <- as_ieegio_transform(
    passive_mat,
    space_from = new_space("scanner"),
    space_to = new_space("T1"),
    interpretation = "passive"
  )
  
  surf_transformed <- surface_to_surface(surf, transform = passive_transform)
  
  # For passive transform: need to invert matrix AND toggle interpretation
  # This is equivalent to active(T1 -> scanner)
  active_equivalent_mat <- solve(passive_mat)
  expected_verts <- active_equivalent_mat %*% surf$geometry$vertices
  
  expect_equal(
    surf_transformed$geometry$vertices[1:3, ],
    expected_verts[1:3, ],
    tolerance = 1e-10
  )
})

test_that("surface_to_surface handles same-space transforms with different orientations", {
  skip_if_not(ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii", test = TRUE))
  
  surf <- read_surface(ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii"))
  
  # Active same-space transform (scanner RAS -> scanner RAS)
  active_mat <- matrix(
    byrow = TRUE, nrow = 4,
    c(
      1.5, 0.2, 0.1, 5,
      0.1, 1.5, 0.2, 10,
      0.2, 0.1, 1.5, 15,
      0, 0, 0, 1
    )
  )
  
  active_transform <- as_ieegio_transform(
    active_mat,
    space_from = new_space("scanner", orientation = "RAS"),
    space_to = new_space("scanner", orientation = "RAS"),
    interpretation = "active"
  )
  
  surf_active <- surface_to_surface(
    surf,
    space_from = new_space("scanner", orientation = "RAS"),
    space_to = new_space("scanner", orientation = "RAS"),
    transform = active_transform
  )
  
  # Passive same-space transform (scanner RAS -> scanner RAS)
  passive_transform <- as_ieegio_transform(
    active_mat,
    space_from = new_space("scanner", orientation = "RAS"),
    space_to = new_space("scanner", orientation = "RAS"),
    interpretation = "passive"
  )
  
  surf_passive <- surface_to_surface(
    surf,
    space_from = new_space("scanner", orientation = "RAS"),
    space_to = new_space("scanner", orientation = "RAS"),
    transform = passive_transform
  )
  
  # Active and passive should give DIFFERENT results for same-space transforms
  expect_false(isTRUE(all.equal(
    surf_active$geometry$vertices,
    surf_passive$geometry$vertices,
    tolerance = 1e-10
  )))
  
  # Active should apply matrix directly
  expected_active <- active_mat %*% surf$geometry$vertices
  expect_equal(
    surf_active$geometry$vertices[1:3, ],
    expected_active[1:3, ],
    tolerance = 1e-10
  )
  
  # Passive should apply inverse matrix
  expected_passive <- solve(active_mat) %*% surf$geometry$vertices
  expect_equal(
    surf_passive$geometry$vertices[1:3, ],
    expected_passive[1:3, ],
    tolerance = 1e-10
  )
})

test_that("surface_to_surface handles orientation changes in transform chain", {
  skip_if_not(ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii", test = TRUE))
  
  surf <- read_surface(ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii"))
  
  # Transform from scanner(RAS) to T1(LPS)
  transform_mat <- matrix(
    byrow = TRUE, nrow = 4,
    c(
      1.2, 0.1, 0, 5,
      0, 1.2, 0.1, 10,
      0.1, 0, 1.2, 15,
      0, 0, 0, 1
    )
  )
  
  transform <- as_ieegio_transform(
    transform_mat,
    space_from = new_space("scanner", orientation = "RAS"),
    space_to = new_space("T1", orientation = "LPS"),
    interpretation = "active"
  )
  
  surf_transformed <- surface_to_surface(
    surf,
    space_from = new_space("scanner", orientation = "RAS"),
    space_to = new_space("T1", orientation = "LPS"),
    transform = transform
  )
  
  # Manual calculation:
  # 1. pre_affine: RAS->RAS (identity)
  # 2. transform: scanner(RAS) -> T1(LPS)
  # 3. post_affine: LPS->LPS (identity)
  # Result should just be the transform applied
  expected_verts <- transform_mat %*% surf$geometry$vertices
  
  expect_equal(
    surf_transformed$geometry$vertices[1:3, ],
    expected_verts[1:3, ],
    tolerance = 1e-10
  )
})

test_that("surface_to_surface handles wildcard spaces", {
  skip_if_not(ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii", test = TRUE))
  
  surf <- read_surface(ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii"))
  
  # Transform with wildcard spaces
  transform_mat <- matrix(
    byrow = TRUE, nrow = 4,
    c(
      1.1, 0.05, 0, 2,
      0, 1.1, 0.05, 4,
      0.05, 0, 1.1, 6,
      0, 0, 0, 1
    )
  )
  
  # Wildcard -> wildcard transform
  transform <- as_ieegio_transform(
    transform_mat,
    space_from = "",
    space_to = "",
    interpretation = "active"
  )
  
  surf_transformed <- surface_to_surface(
    surf,
    space_from = "",
    space_to = "",
    transform = transform
  )
  
  expected_verts <- transform_mat %*% surf$geometry$vertices
  
  expect_equal(
    surf_transformed$geometry$vertices[1:3, ],
    expected_verts[1:3, ],
    tolerance = 1e-10
  )
})

test_that("surface_to_surface handles complex orientation bridging", {
  skip_if_not(ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii", test = TRUE))
  
  surf <- read_surface(ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii"))
  
  # Surface in RAS, transform expects LPS input, outputs RAS, final target is LPS
  transform_mat <- matrix(
    byrow = TRUE, nrow = 4,
    c(
      1.3, 0, 0, 3,
      0, 1.3, 0, 6,
      0, 0, 1.3, 9,
      0, 0, 0, 1
    )
  )
  
  transform <- as_ieegio_transform(
    transform_mat,
    space_from = new_space("scanner", orientation = "LPS"),
    space_to = new_space("T1", orientation = "RAS"),
    interpretation = "active"
  )
  
  surf_transformed <- surface_to_surface(
    surf,
    space_from = new_space("scanner", orientation = "RAS"),
    space_to = new_space("T1", orientation = "LPS"),
    transform = transform
  )
  
  # Manual calculation:
  # 1. pre_affine: RAS -> LPS (flip X, Y)
  # 2. transform: scanner(LPS) -> T1(RAS)
  # 3. post_affine: RAS -> LPS (flip X, Y)
  
  pre_mat <- diag(c(-1, -1, 1, 1))
  post_mat <- diag(c(-1, -1, 1, 1))
  
  expected_verts <- post_mat %*% transform_mat %*% pre_mat %*% surf$geometry$vertices
  
  expect_equal(
    surf_transformed$geometry$vertices[1:3, ],
    expected_verts[1:3, ],
    tolerance = 1e-10
  )
})

test_that("surface_to_surface handles passive transform without orientation changes", {
  skip_if_not(ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii", test = TRUE))
  
  surf <- read_surface(ieegio_sample_data("gifti/GzipBase64/sujet01_Lwhite.surf.gii"))
  
  # Passive transform without orientation change (RAS -> RAS)
  passive_mat <- matrix(
    byrow = TRUE, nrow = 4,
    c(
      1.4, 0.1, 0, 4,
      0.1, 1.4, 0, 8,
      0, 0, 1.4, 12,
      0, 0, 0, 1
    )
  )
  
  passive_transform <- as_ieegio_transform(
    passive_mat,
    space_from = new_space("scanner", orientation = "RAS"),
    space_to = new_space("T1", orientation = "RAS"),
    interpretation = "passive"
  )
  
  surf_transformed <- surface_to_surface(
    surf,
    space_from = new_space("scanner", orientation = "RAS"),
    space_to = new_space("T1", orientation = "RAS"),
    transform = passive_transform
  )
  
  # Passive(scanner -> T1) should apply inverse matrix
  # (since new_transform_chain converts it to active with atomic operations)
  expected_verts <- solve(passive_mat) %*% surf$geometry$vertices
  
  expect_equal(
    surf_transformed$geometry$vertices[1:3, ],
    expected_verts[1:3, ],
    tolerance = 1e-10
  )
})
