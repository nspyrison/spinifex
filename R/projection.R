#' Project data onto a rotated space
#'
#' Project [n, p] data onto a [p, 3] rotated manipulation space into the [n, 3] projection. Stores the data projection(s) in a data structure for plotting
#'
#' @param data [n, p] data to project, consisting of only numeric variables (for coercion into matrix)
#' @param r_space [p, 3] rotated manipulation space 
#' @export
#' @examples
#' data <- flea[, 1:6]
#' p <- ncol(data)
#' basis <- basis_random(p = p)
#' m_sp <- create_manip_space(basis = b, manip_var = 3)
#' 
#' data_proj(data=flea[1:3, 1:6], manip="radial", to=2)
#' dp <- data_proj(data=data,  manip_var=3, manip="radial",
#'                 from=0, to=pi, by=pi/10,
#'                 manip_space = m_sp, theta = pi/4)
#' str(dp)
#' 
data_proj <- function(data, manip="radial", manip_var = 1,
                manip_space = create_manip_space(basis = basis_random(p = p),
                                                 manip_var = manip_var), 
                theta = 0, #phi controled by to, from, by.
                from = 0, to = 0, by = pi / 20) {
  ### redundant explicit try catch
  #tc_seq <- tryCatch(seq(from, to, by)  
  #                   ,error=function(e) e, warning=function(w) w)
  #if (any(class(tc_seq) == "error")) stop(tc_seq)
  if (is.character(manip_var)) manip_var <- match("manip_var",names(data))
  if (!is.matrix(data)) data <- as.matrix(data)
  
  index <- 0
  projected_data <- NULL
  manip <- tolower(substr(manip, 1, 3))
  
  for (phi in seq(from, to, by)) {
    index <- index+1
    if (manip == "rad") {
      delta <- cbind(data %*% radial_manip(manip_space, phi, theta), index, phi)
    }
    else if (manip == "hor") {
      delta <- cbind(data %*% horizontal_manip(manip_space, phi, theta), index, phi)
    }
    else if (manip == "ver") {
      delta <- cbind(data %*% vertical_manip(manip_space, phi, theta), index, phi)
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
#' @param from index to start the slideshow at. Defaults to 1
#' @param to index to start the slideshow at. Defaults to max(index)
#' @param by index to start the slideshow at. Defaults to 1
#' @import ggplot2
#' @export
#' @examples
#' data <- flea[, 1:6]
#' p <- ncol(data)
#' basis <- basis_random(p = p)
#' m_sp <- create_manip_space(basis = b, manip_var = 3)
#' 
#' dp <- data_proj(data=data,  manip_var=3, manip="radial",
#'                 from=0, to=pi, by=pi/10,
#'                 manip_space = m_sp, theta = pi/4)
#' slideshow(dp, ggplot=T)

slideshow <- function(projected_data, delay=.5, ggplot = T,
                      from = 1, to = max(projected_data[, 4]), by = 1) {
  xmin <- min(projected_data[, 1])
  xmax <- max(projected_data[, 1])
  ymin <- min(projected_data[, 2])
  ymax <- max(projected_data[, 2])
  imax <- max(projected_data[, 4])
  
  for (i in seq(from, to, by)) {
    d <- as.data.frame(projected_data[which(projected_data[, 4] == i), ]) 
    t <- paste("Projected data. index=",i,"/",imax,". phi=",round(phi,2))
    if (ggplot == T) {
      print( ggplot2::ggplot(d, aes(x = d$x, y = d$y)) + geom_point() +
        ggtitle(t) + ylab("") + xlab("") + 
        xlim(c(xmin, xmax)) + ylim(c(ymin, ymax)) )
    } 
    else {
      print( plot(x = d$x, y = d$y, main = t, ylab = "", xlab = "",
                  xlim=c(xmin, xmax), ylim=c(ymin, ymax)) )
    }
    Sys.sleep(delay)
  }
  
  return("end")
}

