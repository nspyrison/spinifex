#' Project data onto a rotated space
#'
#' Project [n, p] data onto a [p, 3] rotated manipulation space into the [n, 3] projection. Stores the data projection(s) in a data structure for plotting
#'
#' @param data [n, p] data to project, consisting of only numeric variables (for coercion into matrix)
#' @param r_space [p, 3] rotated manipulation space 
#' @export
#' @examples
#' ###NEED TO CHANGE EXAMPLE###
#' this_r_space <- horizontal_manip(manip_space = this_manip_space, phi = i)
#' proj <- data_proj(data = quakes, r_space = this_r_space)
#' plot(proj[,1], proj[,2], main="Projected data")
#' 
#' for (i in seq(0, pi, pi/20)) {
#' this_r_space <- horizontal_manip(manip_space = this_manip_space, phi = i)
#' proj <- data_proj(data = quakes, r_space = this_r_space)
#' plot(proj[,1], proj[,2], main="Projected data")
#' Sys.sleep(time=.5)
#' print(i/pi)
#' }
#' 
data_proj <- 
  function(data, manip="radial", manip_var = 1,
           manip_space = create_manip_space(basis = basis_random(p = p),
                                            manip_var = manip_var), 
           phi = 0, theta = 0, #manip args
           from = 0, to = 0, by = pi / 20) {
    tc_seq <- tryCatch(seq(from, to, by)
                       ,error=function(e) e, warning=function(w) w)
    if (any(class(tc_seq) == "error")) stop(tc_seq)
    if (!is.matrix(data)) data <- as.matrix(data)
    
    index <- 0
    projected_data <- NULL
    manip <- tolower(substr(manip, 1, 3))
    
    if (manip == "rad") {
      for (i in seq(from, to, by)) {
        index <- index+1
        delta <- cbind(data %*% radial_manip(manip_space, phi = i, theta), index)
        projected_data <- rbind(projected_data, delta)
      }
    }
    if (manip == "hor") {
      for (i in seq(from, to, by)) {
        index <- index+1
        delta <- cbind(data %*% horizontal_manip(manip_space, phi = i, theta), index)
        projected_data <- rbind(projected_data, delta)
      }
    }
    if (substr(manip, 1, 3) == "ver") {
      for (i in seq(from, to, by)) {
        index <- index+1
        delta <- cbind(data %*% vertical_manip(manip_space, phi = i, theta), index)
        projected_data <- rbind(projected_data, delta)
      }
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
#' @export
#' @examples
#' ###NEED TO CHANGE EXAMPLE###

slideshow <- 
  function(projected_data, delay=.5,
           from = 1, to = max(projected_data[,4]), by = 1) {
    tc_seq <- tryCatch(seq(from, to, by)
                       ,error=function(e) e, warning=function(w) w)
    if (any(class(tc_seq) == "error")) stop(tc_seq)
    if (!is.matrix(data)) data <- as.matrix(data)
    
    for (i in seq(from, to, by)) {
      plot(projected_data[which(projected_data[,4] == i),1],
           projected_data[which(projected_data[,4] == i),2],
           main = paste("Projected data, index = ",i,"/",max(projected_data[,4])),
           ylab = NULL, xlab = NULL)
      Sys.sleep(delay)
    }

    return("slideshow over")
  }