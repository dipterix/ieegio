mat_to_quaternion <- function(m) {
  # normalize it
  m <- m[1:3, 1:3]
  m <- apply(m, 2, function(x) {
    l2 <- sum(x^2)
    if( l2 > 0 ) {
      x <- x / sqrt(l2)
    }
    x
  })
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

  if( trace > 0 ) {
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

  if(w < 0) {
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

#
# quaternion_to_mat <- function() {
#   quaternion2mat44 <- function(nim, tol=1e-7) {
#     qb <- nim@"quatern_b"
#     qc <- nim@"quatern_c"
#     qd <- nim@"quatern_d"
#     qx <- nim@"qoffset_x"
#     qy <- nim@"qoffset_y"
#     qz <- nim@"qoffset_z"
#     dx <- pixdim(nim)[2]
#     dy <- pixdim(nim)[3]
#     dz <- pixdim(nim)[4]
#     qfac <- pixdim(nim)[1]
#     R <- matrix(0, nrow=4, ncol=4)
#     b <- qb
#     c <- qc
#     d <- qd
#     ## last row is always [ 0 0 0 1 ]
#     R[4,1] <- R[4,2] <- R[4,3] <- 0.0
#     R[4,4] <- 1.0
#     ## compute a parameter from b,c,d
#     a <- 1 - (b*b + c*c + d*d)
#     if (a < tol) {                      # special case
#       a <- 1 / sqrt(b*b + c*c +d*d)
#       b <- a * b
#       c <- a * c
#       d <- a * d                        # normalize (b,c,d) vector
#       a <- 0                            # a = 0 ==> 180 degree rotation
#     } else {
#       a <- sqrt(a)                     # angle = 2*arccos(a)
#     }
#     ## load rotation matrix, including scaling factors for voxel sizes
#     xd <- ifelse(dx > 0, dx, 1)         # make sure are positive
#     yd <- ifelse(dy > 0, dy, 1)
#     zd <- ifelse(dz > 0, dz, 1)
#     if (qfac < 0) {
#       zd <- -zd                         # left handedness?
#     }
#     R[1,1] <- (a*a + b*b - c*c - d*d) * xd
#     R[1,2] <- 2 * (b*c - a*d) * yd
#     R[1,3] <- 2 * (b*d + a*c) * zd
#     R[2,1] <- 2 * (b*c + a*d) * xd
#     R[2,2] <- (a*a + c*c - b*b - d*d) * yd
#     R[2,3] <- 2 * (c*d - a*b) * zd
#     R[3,1] <- 2 * (b*d - a*c) * xd
#     R[3,2] <- 2 * (c*d + a*b) * yd
#     R[3,3] <- (a*a + d*d - c*c - b*b) * zd
#     ## load offsets
#     R[1,4] <- qx
#     R[2,4] <- qy
#     R[3,4] <- qz
#     return(R)
#   }
# }
