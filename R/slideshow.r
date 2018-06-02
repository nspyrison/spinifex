#' Slide show of the projected data and basis.
#'
#' Takes the result of data_proj() and uses base graphics to view each index with delay in base grahics
#'
#' @param proj_list the output of data_proj(), list of projected data and basis by index.
#' @param col color of the data, 1 color or length of the data
#' @import ggplot2
#' @import plotly
#' @export
#' @examples
#' 
#' data <- flea[, 1:5]
#' proj <- proj_data(data, manip_var=2)
#' slideshow(proj)
#' 
#' slideshow(proj, col=flea$species)

slideshow <- function(proj_list, col = "black") {
  if (length(col) != 1 &
      nrow(proj_list$proj_data) %% length(col) == 0)
  {
    col <- rep(col, nrow(proj_list$proj_data) / length(col))
  } else
    stop("length of col (color) expected to be 1 or length of data.")
  
  proj_data <- proj_list$proj_data
  proj_data <- as.data.frame(proj_data)
  proj_basis <- proj_list$proj_basis
  proj_basis <- proj_basis[order(row.names(proj_basis), proj_basis[, 4]),]
  proj_basis <- as.data.frame(proj_basis)
  
  ### STRUCTURE BASIS
  n_index = length(unique(proj_basis[, 4]))
  n_var = length(unique(proj_basis[, 8]))
  sp_mat <- matrix(0, ncol = 4, nrow = n_index)
  colnames(sp_mat) <- colnames(proj_basis[, 1:4])
  spaced_basis <- NULL
  for (i in 1:n_var) {
    rowend <- i * n_index
    row <- rowend - n_index + 1
    delta <- rbind(sp_mat, proj_basis[row:rowend, 1:4])
    spaced_basis <- rbind(spaced_basis, delta)
  }
  
  #### PLOTTING
  ggplotly_data <- suppressWarnings(ggplotly((
    ggplot(proj_data, aes(x = x, y = y)) +
      geom_point(color = "black", aes(frame = index)) +
      ylab("") + xlab("") + coord_fixed()
    )))
  #return(ggplotly_data)
  
  ggplotly_basis <- suppressWarnings(ggplotly((
    ggplot(spaced_basis, aes(x = x, y = y)) +
      geom_point(color = "grey50", aes(frame =index)) +
      ylab("") + xlab("") + coord_fixed()
    )))
  #return(ggplotly_basis)
  
  slideshow <- 
    subplot(ggplotly_basis, ggplotly_data, nrows = 1, widths = c(0.3, 0.7)) 
    #subplot(ggplotly_basis, ggplotly_basis, nrows = 1, widths = c(0.3, 0.7)) 
    #subplot(ggplotly_data, ggplotly_data, nrows = 1, widths = c(0.3, 0.7)) 
  #a,b doesn't work, but aa and bb do....
  
  stopifnot(is.list(slideshow))
  stopifnot(length(slideshow) == 8)
  return(slideshow)
}
  #subplot(ggplotly_basis, ggplotly_data, nrows = 1, widths = c(0.3, 0.7))
  
  #gridExtra::grid.arrange(ggplotly_basis, ggplotly_data, ncol = 2)
  
  ### RAW PLOTLY
  #plotly_data <- plotly::plot_ly(proj_data,
  #  x = ~x,
  #  y = ~y,
  #  frame = ~index,
  #  text = ~paste0('phi for index ', index, ' is ', round(phi,2)),
  #  color = ~col,
  #  type = 'scatter',
  #  mode = 'markers',
  #  showlegend = F
  #)
  #
  #plotly_basis <- plotly::plot_ly(spaced_basis,
  #                             x = ~x,
  #                             y = ~y,
  #                             frame = ~index,
  #                             text = ~row.names(spaced_basis),
  #                             color = I("grey50"),
  #                             type = 'scatter',
  #                             mode = 'markers',
  #                             showlegend = F
  #) %>% add_text(x = ~x, y = ~y,
  #               text = row.names(spaced_basis),
  #               textposition = 'middle right')
  #
  #subplot(plotly_basis, plotly_data, nrows = 1, widths = c(0.3, 0.7))
