mat_to_quaternion <- function(m, fix_qfac = TRUE) {
  # normalize it
  m <- m[1:3, 1:3]
  m <- apply(m, 2, function(x) {
    l2 <- sum(x^2)
    if ( l2 > 0 ) {
      x <- x / sqrt(l2)
    }
    x
  })

  # NIfTI spec (and FreeSurfer's nifti_mat44_to_quatern): if the matrix is
  # left-handed (det < 0), negate the z-column before extracting the
  # quaternion. qfac = -1 is stored in pixdim[0] by the caller and used
  # during reconstruction to undo the flip (zd *= qfac).
  if ( fix_qfac && det(m) < 0 ) {
    m[, 3] <- -m[, 3]
  }

  m11 <- m[1, 1]
  m21 <- m[2, 1]
  m31 <- m[3, 1]

  m12 <- m[1, 2]
  m22 <- m[2, 2]
  m32 <- m[3, 2]

  m13 <- m[1, 3]
  m23 <- m[2, 3]
  m33 <- m[3, 3]

  trace <- m11 + m22 + m33

  if ( trace > 0 ) {
    s <- 0.5 / sqrt(trace + 1)
    w <- 0.25 / s
    x <- (m32 - m23) * s
    y <- (m13 - m31) * s
    z <- (m21 - m12) * s
  } else if (m11 > m22 && m11 > m33) {
    s <- 2.0 * sqrt(1.0 + m11 - m22 - m33)
    w <- (m32 - m23) / s
    x <- 0.25 * s
    y <- (m12 + m21) / s
    z <- (m13 + m31) / s
  } else if (m22 > m33) {
    s <- 2.0 * sqrt(1.0 + m22 - m11 - m33)
    w <- (m13 - m31) / s
    x <- (m12 + m21) / s
    y <- 0.25 * s
    z <- (m23 + m32) / s
  } else {
    s <- 2.0 * sqrt(1.0 + m33 - m11 - m22)
    w <- (m21 - m12) / s
    x <- (m13 + m31) / s
    y <- (m23 + m32) / s
    z <- 0.25 * s
  }

  re <- c(x = x, y = y, z = z, w = w)

  if (w < 0) {
    # make sure w is positive as described: https://nifti.nimh.nih.gov/pub/dist/src/niftilib/nifti1.h
    # Requiring a >= 0 is equivalent to requiring -Pi <= h <= Pi.  (Note that
    # [-a,-b,-c,-d] represents the same rotation as [a,b,c,d]; there are 2
    # quaternions that can be used to represent a given rotation matrix R.)
    # To rotate a 3-vector (x,y,z) using quaternions, we compute the
    # quaternion product
    re <- re * -1
  }
  re
}

# Reconstruct a 4x4 vox2ras matrix from a NIfTI quaternion (Method 2 / qform).
# quaternion : named vector with elements x (quatern_b), y (quatern_c), z (quatern_d)
# dx, dy, dz : voxel sizes (pixdim[1:3]); default 1
# qfac       : pixdim[0], either +1 or -1; sign(det(vox2ras[1:3,1:3])); default 1
# qoffset_*  : translation (column 4); default 0
quaternion_to_mat <- function(quaternion,
                              dx = 1, dy = 1, dz = 1,
                              qfac = 1,
                              qoffset_x = 0, qoffset_y = 0, qoffset_z = 0,
                              tol = 1e-7) {
  b <- quaternion[[1]]   # quatern_b
  c <- quaternion[[2]]   # quatern_c
  d <- quaternion[[3]]   # quatern_d

  # Derive a from b, c, d; normalise if numerical error pushes a^2 below tol
  a_sq <- 1 - (b * b + c * c + d * d)
  if (a_sq < tol) {                     # 180-degree rotation edge case
    s <- 1 / sqrt(b * b + c * c + d * d)
    b <- b * s
    c <- c * s
    d <- d * s
    a <- 0
  } else {
    a <- sqrt(a_sq)
  }

  # Voxel-size scaling; qfac flips z for left-handed (radiological) matrices
  xd <- if (dx > 0) dx else 1
  yd <- if (dy > 0) dy else 1
  zd <- if (dz > 0) dz * qfac else qfac   # applies qfac sign

  # Build in one allocation (column-major order) to avoid copy-on-write overhead
  matrix(
    c(
      (a * a + b * b - c * c - d * d) * xd,
      2 * (b * c + a * d) * xd,
      2 * (b * d - a * c) * xd,
      0, # col 1

      2 * (b * c - a * d) * yd,
      (a * a + c * c - b * b - d * d) * yd,
      2 * (c * d + a * b) * yd,
      0, # col 2

      2 * (b * d + a * c) * zd,
      2 * (c * d - a * b) * zd,
      (a * a + d * d - c * c - b * b) * zd,
      0, # col 3

      qoffset_x,
      qoffset_y,
      qoffset_z,
      1 # col 4
    ),
    nrow = 4,
    ncol = 4
  )
}
