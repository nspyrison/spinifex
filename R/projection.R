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
  if (is.character(manip_var)) manip_var <- match(manip_var, names(data)) #char to num
  if (!is.matrix(data)) data <- as.matrix(data)
  if (center) data <- scale(data, center = T, scale = F)
  if (scale)  data <- scale(data, center = F, scale = T)
  if (is.null(theta)) theta <- atan(basis[manip_var,2]/basis[manip_var,1]) #sets radial theta
  
  index <- 0
  proj_data <- NULL
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
    proj_data <- rbind(proj_data, delta)
  }
  
  index <- 0
  proj_basis <- NULL
  for (phi in seq(from, to, by)) {
    index <- index+1
    if (manip == "rad") {
      delta <- cbind(radial_manip(manip_space, phi, theta), index, phi)
    }
    else if (manip == "hor") {
      delta <- cbind(horizontal_manip(manip_space, phi), index, phi)
    }
    else if (manip == "ver") {
      delta <- cbind(vertical_manip(manip_space, phi), index, phi)
    }
    proj_basis <- rbind(proj_basis, delta)
  }
  n_index <- max(proj_basis[, 4])
  proj_basis <- cbind(proj_basis, manip_var, theta, "var_num" = 1:ncol(data))
  if (is.null(rownames(proj_basis))) 
    {rownames(proj_basis) <- rep(colnames(data),n_index)}
  
  proj_list <- list("proj_data" = proj_data, 
                    "proj_basis" = proj_basis
                    )
  return(proj_list)
}


#' Slide show of the projected data and basis.
#'
#' Takes the result of data_proj() and uses base graphics to view each index with delay in base grahics
#'
#' @param proj_list the output of data_proj(), list of projected data and basis by index.
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

slideshow <- function(proj_list, col=NULL) {
  
  proj_data <- proj_list$proj_data
  proj_basis <- proj_list$proj_basis
  proj_basis <- proj_basis[order(row.names(proj_basis), proj_basis[, 4]), ] 
  if (is.null(col)) {stop()
    } else
  if (length(col) != 1 & nrow(proj_data) %% length(col) == 0)
    {col <- rep(col, nrow(proj_data) / length(col))
    }else message("arg col (color) expected of length data.")
  
  
  plot_data <- plotly::plot_ly(as.data.frame(proj_data),
    x = ~x,
    y = ~y,
    frame = ~index,
    text = ~paste0('phi for this index: ', round(phi,2)),
    color = ~col,
    type = 'scatter',
    mode = 'markers',
    showlegend = F
  )
  
  n_index = max(proj_basis[, 4])
  n_var = max(proj_basis[, 8])
  sp_mat <- matrix(0, ncol = 4, nrow = n_index) #spacer matrix
  plotly_basis <- NULL
  for (i in 1:n_var) {
    rowend <- i * n_index
    row <- rowend - n_index + 1
    delta <- rbind(sp_mat, proj_basis[row:rowend, 1:4])
    plotly_basis <- rbind(plotly_basis, delta)
  }
  
  plot_basis <- plotly::plot_ly(as.data.frame(proj_basis),
                               x = ~x,
                               y = ~y,
                               frame = ~index,
                               text = ~row.names(proj_basis),
                               color = I("grey50"),
                               type = 'scatter',
                               mode = 'markers',
                               showlegend = F
  ) %>% add_text(x = ~x, y = ~y,
                 text = row.names(proj_basis), 
                 textposition = 'middle right')
  
  
  subplot(plot_basis, plot_data, nrows = 1, widths = c(0.3, 0.7))
  
}

#' Find the basis after the data is projected.
#' 
#' Solve for the basis after data projection. data^(-1) %*% projected_data
#' 
#' @param data the [n, p] data used for projection.
#' @param projected_data the results of data_proj.
#' @param index the index of the `projected_data` to find the basis of. defaults to max(projected_data$index)
#' 
#' DO NOT @ export
proj_basis <- function(data, projected_data, index = NULL) {
  pd <- projected_data
  if (is.null(index)) index <- max(pd[, 4])
  pd <- pd[which(pd[, 4]==index), 1:2] #for 1 index, only x,y contributions
  
  proj_basis <- MASS::ginv(as.matrix(data)) %*% pd #issue with finding the inverse of data

  return(proj_basis)
}


