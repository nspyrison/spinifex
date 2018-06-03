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
  stopifnot(is.list(proj_list))
  stopifnot(length(proj_list) == 2)
  if (length(col) != 1 & nrow(proj_list$proj_data) %% length(col) == 0)
    {col <- rep(col, nrow(proj_list$proj_data) / length(col))}
  stopifnot(length(col) == 1 | length(col) == nrow(proj_list$proj_data))
  
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
  
  #### PLOTTING #frame needs to be in a geom_(aes()).
  ggplotly_data <- suppressWarnings(ggplotly((
    ggplot(proj_data, aes(x = x, y = y)) +
      geom_point(color = "black", aes(frame = index)) +
      ylab("") + xlab("") + coord_fixed()
    )))
  
  cn <- as.character(proj_basis$var_name)
  label <- paste0(substr(cn, 1, 2), substr(cn, nchar(cn), nchar(cn)))
  ggplotly_basis <- suppressWarnings(config(
    ggplotly(
      ggplot(spaced_basis, aes(x = x, y = y)) +
        geom_segment(
          aes(xend = 0, yend = 0, frame = index),
          color = "grey70",
          size = .3
        ) +
        geom_text(
          aes(label = label, frame = index),
          color = "grey50",
          hjust = 0,
          vjust = 0
        ) +
        geom_path(
          data = circ,
          aes(x, y),
          size = .3,
          color = "grey70"
        ) +
        ylab("") + xlab("") + coord_fixed()
    )
  ))
  
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
