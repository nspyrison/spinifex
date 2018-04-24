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
#'                 from=0, to=pi, by=pi/10, theta=pi/4)
#' str(dp)
#' 
data_proj <- function(data, manip = "radial", basis = basis_identity(p = ncol(data)),
                      manip_var, theta = 0, center=T, from = 0, to = 0, by = pi/20 ) {
  ### redundant explicit try catch
  #tc_seq <- tryCatch(seq(from, to, by)
  #                   ,error=function(e) e, warning=function(w) w)
  #if (any(class(tc_seq) == "error")) stop(tc_seq)
  if (is.character(manip_var)) manip_var <- match(manip_var, names(data))
  if (!is.matrix(data)) data <- as.matrix(data)
  
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
      delta <- cbind(data %*% horizontal_manip(manip_space, phi, theta), 
                     index, manip_var, phi, theta)
    }
    else if (manip == "ver") {
      delta <- cbind(data %*% vertical_manip(manip_space, phi, theta), 
                     index, manip_var, phi, theta)
    }
    projected_data <- rbind(projected_data, delta)
  }
  
  if (!substr(manip, 1, 3) %in% c("rad", "hor", "ver")) {
    message(cat(manip, " manipulation not found."))
  }
  if (theta != 0 & manip %in% c("hor", "ver")) {
    message("Non zero theta used with horizontal or vertical manip. Note that theta is forced to 0, pi/2 respectively.")
  }
  
  return(projected_data)
}


#' Slide show of the projected data.
#'
#' Takes the result of data_proj() and uses base graphics to view each index with delay in base grahics
#'
#' @param projected_data the output of data_proj(), a series data projections being rotated.
#' @param delay the delay (in seconds) between projections. Defaults to .5
#' @param ggplot TRUE plots with ggplot2::ggplot(), FALSE plots with plot(). Defaults to TRUE
#' @param from index to start the slideshow at. Defaults to 1
#' @param to index to end the slideshow at. Defaults to max(index)
#' @param by number of indexs to increment by between plots. Defaults to 1
#' @import ggplot2
#' @export
#' @examples
#' data <- flea[, 1:6]
#' p <- ncol(data)
#' basis <- basis_random(p = p)
#' 
#' dp <- data_proj(data=data, basis=b_rand, manip_var=3, manip="radial",
#'                from=0, to=pi, by=pi/10, theta=pi/4)
#' slideshow(dp, ggplot=T)
#' slideshow(dp, ggplot=F, from=2, to=8, by=2, delay=2)

slideshow <- function(projected_data, delay=.5, ggplot = T,
                      from = 1, to = max(projected_data[, 4]), by = 1,
                      col = "black", pch = 16) {
  xmin <- min(projected_data[, 1])
  xmax <- max(projected_data[, 1])
  ymin <- min(projected_data[, 2])
  ymax <- max(projected_data[, 2])
  imax <- max(projected_data[, 4])
  
  for (i in seq(from, to, by)) {
    d <- as.data.frame(projected_data[which(projected_data[, 4] == i), ]) 
    t <- paste("Projected data. index=",i,"of",imax,". phi=",round(d$phi[1],2))
    if (ggplot == T) {
      print( ggplot2::ggplot(d, ggplot2::aes(x = d$x, y = d$y)) + 
               ggplot2::geom_point(color=col) + ggplot2::ggtitle(t) + 
               ggplot2::ylab("") + ggplot2::xlab("") + 
               ggplot2::xlim(c(xmin, xmax)) + ggplot2::ylim(c(ymin, ymax)) )
    } 
    else {
      plot(x = d$x, y = d$y, main = t, ylab = "", xlab = "",
          xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=col, pch=pch)
    }
    Sys.sleep(delay)
  }
  
  return("end")
}

