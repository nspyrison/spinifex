#' Project data onto a rotated space
#'
#' Project [n, p] data onto a [p, 3] rotated manipulation space into the [n, 3] projection. Stores the data projection(s) in a data structure for plotting
#'
#' @param data [n, p] data to project, consisting of only numeric variables (for coercion into matrix)
#' @param manip character string of the type of manipulation to use. Defaults to "radial". Alternatively use "horizontal" or "vertical" (fixed theta=0, pi/2 respectively)
#' @param basis the [p, 2] orthonormal starting basis. Default to the identity basis 
#' @param manip_var integer column or string name of the variable to manipulate. Required, does not default
#' @param theta the angle that the manipvar should be rotated by in parameter space
#' @param from value of phi to start the projection. Defaults to 0
#' @param to value of phi to end the projection. Defaults to 0
#' @param by the increment phi should change by between projections. Defaults to pi/20
#' @export
#' @examples
#' data <- flea[, 1:6]
#' p <- ncol(data)
#' b_rand <- basis_random(p = p)
#' 
#' data_proj(data=data[1:3, ], manip_var=1, manip="horizontal", to=pi/10)
#' 
#' dp <- data_proj(data=data, basis=b_rand, manip_var=3, manip="radial",
#'                 from=0, to=pi, theta=pi/4)
#' head(dp)
#' 
data_proj <- function(data, manip = "radial", basis = basis_identity(p = ncol(data)),
                      manip_var, theta = NULL, center = T, scale = T,
                      from = 0, to = 0, by = (to - from)/9 ) {
  if (!substr(manip, 1, 3) %in% c("rad", "hor", "ver")) {
    stop(cat(manip, " manipulation not found."))
  }
  if (!is.null(theta) & manip %in% c("hor", "ver")) {
    message("Non null theta used with horizontal or vertical manip. 
            Note that theta is forced to 0, pi/2 respectively.")
  }
  
  ### redundant explicit try catch
  #tc_seq <- tryCatch(seq(from, to, by)
  #                   ,error=function(e) e, warning=function(w) w)
  #if (any(class(tc_seq) == "error")) stop(tc_seq)
  if (is.character(manip_var)) manip_var <- match(manip_var, names(data)) #chr to num
  if (!is.matrix(data)) data <- as.matrix(data)
  if (center) data <- scale(data, center = T, scale = F)
  if (scale)  data <- scale(data, center = F, scale = T)
  if (is.null(theta)) theta <- atan(basis[manip_var,2]/basis[manip_var,1]) #sets radial theta
  
  index <- 0
  projected_data <- NULL
  manip <- tolower(substr(manip, 1, 3))
  manip_space = create_manip_space(basis = basis, manip_var = manip_var)
  
  for (phi in seq(from, to, by)) {
    index <- index+1
    if (manip == "rad") {
      delta <- cbind(data %*% radial_manip(manip_space, phi, theta), 
                     index, manip_var, phi, theta)
    }
    else if (manip == "hor") {
      delta <- cbind(data %*% horizontal_manip(manip_space, phi), 
                     index, manip_var, phi, theta)
    }
    else if (manip == "ver") {
      delta <- cbind(data %*% vertical_manip(manip_space, phi), 
                     index, manip_var, phi, theta)
    }
    projected_data <- rbind(projected_data, delta)
  }
  
  return(projected_data)
}


#' Slide show of the projected data.
#'
#' Takes the result of data_proj() and uses base graphics to view each index with delay in base grahics
#'
#' @param projected_data the output of data_proj(), a series data projections being rotated.
###' @param delay the delay (in seconds) between projections. Defaults to .5
###' @param ggplot TRUE plots with ggplot2::ggplot(), FALSE plots with plot(). Defaults to ##TRUE
###' @param from index to start the slideshow at. Defaults to 1
###' @param to index to end the slideshow at. Defaults to max(index)
###' @param by number of indexs to increment by between plots. Defaults to 1
#' @import ggplot2
#' @export
#' @examples
#' data <- flea[, 1:6]
#' p <- ncol(data)
#' b_rand <- basis_random(p = p)
#' 
#' dp <- data_proj(data=data, basis=b_rand, manip_var=3, manip="radial",
#'                from=0, to=pi, theta=pi/4)
#' slideshow(dp)

slideshow <- function(projected_data, col=NULL) {

  d <- as.data.frame(projected_data)
  if (is.null(col)) {}
  else if (length(col) != 1 & nrow(d) %% length(col) == 0) 
    {c <- rep(col, max(d[,4]))}
  else message("arg col expected of length data or proj_data(data).")
  
  plotly::plot_ly(d,
    x = ~x,
    y = ~y,
    frame = ~index,
    text = ~paste0('phi for this index: ', round(phi,2)),
    color = ~c,
    type = 'scatter',
    mode = 'markers',
    showlegend = F
  )
  
}


