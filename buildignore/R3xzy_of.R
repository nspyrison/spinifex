## R3 rotation for an angle in the x-dimension
R3x_of <- function(mat, angle){ ## https://en.wikipedia.org/wiki/Rotation_matrix#In_three_dimensions
  angle <- -angle ## Orientation as I imagine it defined, double check.
  mat <- as.matrix(mat)
  c <- cos(angle)
  s <- sin(angle)
  rot <- matrix(c(1L, 0L, 0L,
                  0L,  c, -s,
                  0L,  s,  c), ncol = 3L, byrow = TRUE)
  as_xyz_df(mat %*% rot)
}
## R3 rotation for an angle in the y-dimension
R3y_of <- function(mat, angle){ ## https://en.wikipedia.org/wiki/Rotation_matrix#In_three_dimensions
  angle <- -angle ## Orientation as I imagine it defined, double check.
  mat <- as.matrix(mat)
  c <- cos(angle)
  s <- sin(angle)
  rot <- matrix(c(  c, 0L, s,
                   0L, 1L, 0L,
                   -s, 0L, c), ncol = 3L, byrow = TRUE)
  as_xyz_df(mat %*% rot)
}
## R3 rotation for an angle in the z-dimension
R3z_of <- function(mat, angle){ ## https://en.wikipedia.org/wiki/Rotation_matrix#In_three_dimensions
  angle <- -angle ## Orientation as I imagine it defined, double check.
  mat <- as.matrix(mat)
  c <- cos(angle)
  s <- sin(angle)
  rot <- matrix(c(c,  -s, 0L,
                  s,   c, 0L,
                  0L, 0L, 1L), ncol = 3, byrow = T)
  as_xyz_df(mat %*% rot)
}