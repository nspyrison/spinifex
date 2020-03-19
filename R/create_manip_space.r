#' Create a manipulation space
#'
#' Typically called by `manual_tour()`. Creates a (p, d) orthonormal matrix,
#' the manipulation space from the given basis right concatenated with a zero 
#' vector, with manip_var set to 1.
#'
#' @param basis A (p, d) orthonormal matrix.
#' @param manip_var Number of the column/dimension to rotate.
#' @return A (p, d+1) orthonormal matrix, the manipulation space.
#' @import tourr
#' @export
#' @examples
#' flea_std <- tourr::rescale(tourr::flea[,1:6])
#' 
#' rb <- basis_random(n = ncol(flea_std))
#' create_manip_space(basis = rb, manip_var = 4)
create_manip_space <- function(basis, 
                               manip_var) {
  if (is.null(basis) | nrow(basis) == 0) stop("non empty basis required.")
  if (!is.matrix(basis)) as.matrix(basis)
  
  manip_space <- cbind(basis, rep(0, len = nrow(basis) ))
  manip_space[manip_var, 3] <- 1
  manip_space <- apply(manip_space, 2, as.numeric)
  
  manip_space  <- tourr::orthonormalise(manip_space)
  colnames(manip_space) <- NULL
  
  manip_space
}

