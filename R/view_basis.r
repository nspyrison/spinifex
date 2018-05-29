#' View basis axes and table
#' 
#' For internal use mainly 
#' 
#' @param basis [p, d=2] basis, xy contributions of the var. 
#' @param data optional, [n, p], applies colnames to the rows of the basis.
#' 
#' @export
view_basis <- function(basis, data = NULL) {
  
  tmp <- as.data.frame(basis)
  tmp <- cbind(tmp, sqrt(tmp[,1]^2 + tmp[,2]^2), atan(tmp[,2]/tmp[,1]))
  colnames(tmp) <- c("X", "Y", "H_xy", "theta")
  rownames(tmp) <- colnames(data)
  view_basis <- tmp
  
  plot(0,type='n',axes=FALSE,ann=FALSE,xlim=c(-1, 1), ylim=c(-1, 1),asp=1)
  segments(0,0, basis[, 1], basis[, 2], col="grey50")
  theta <- seq(0, 2 * pi, length = 50)
  lines(cos(theta), sin(theta), col = "grey50")
  text(basis[, 1], basis[, 2], label = colnames(data), col = "grey50")
  
  return(view_basis)
}
